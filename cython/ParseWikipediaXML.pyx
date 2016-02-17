# -*- coding: utf-8 -*-

from __future__ import print_function
import sys
import argparse
import re
import threading
from collections import defaultdict
import MeCab
import redis
import time
import math

if sys.version_info[0] == 2:
    import Queue
    import msgpack
else:
    import queue as Queue
    from functools import cmp_to_key, reduce

def python_sorted(lst):
    if sys.version_info[0] == 2:
        return sorted(lst,cmp=cmp_dict)
    else:
        return sorted(lst,key=cmp_to_key(cmp_dict))

def cmp_dict(a,b):
    if a[1]>b[1]:
        return -1
    elif a[1]<b[1]:
        return 1
    else:
        if a[0]>b[0]:
            return 1
        else:
            return -1

class bofwThread(threading.Thread):

    # static variables
    lock = threading.Lock()
    pages = 0
    saved = 0

    def __init__(self, parser, idx):
        super(bofwThread,self).__init__()
        self.parser = parser
        self.idx = idx

    def run(self):

        args = self.parser.args
        queue = self.parser.queue

        while True:

            page = queue.get()

            # This may finish main thread before parseText()
            #queue.task_done()

            if page == "Finished":

                # Be sure that this can let queue.join() go
                queue.task_done()
                break

            elif re.search(args.recateg, page):

                titlema = re.search("<title[^<>]*>([^<>]+)</title>",page)
                textma = re.search("<text[^<>]*>([^<>]+)</text>",page)

                if titlema and textma:

                    title = titlema.group(1)
                    text = textma.group(1)

                    dictBofw = self.parser.parseText(text)

                    if self.parser.post_process(self.parser, dictBofw, title):
                        bofwThread.saved+=1

            self.report()

            # put() counts up and task_done() counts down
            queue.task_done()


    def report(self):
        m = " > Execute %(class)s" % { "class": self.__class__.__name__ }

        bofwThread.lock.acquire()
        bofwThread.pages += 1
        print( "%(message)s [#page(saved/parsed) %(saved)s/%(count)s @ thread %(index)s]" %
                { "message" : m,
                  "count"   : bofwThread.pages,
                  "saved"   : bofwThread.saved,
                  "index"   : self.idx
                },
                "\r", end="" )
        bofwThread.lock.release()

def store_redis(func):
    import functools
    @functools.wraps(func)
    def wrapper(*args,**kwargs):
        try:
            if args[0].client == None:
                raise redis.exceptions.ConnectionError
            else:
                args[0].client.ping()
        except redis.exceptions.ConnectionError:
            result = func(*args,**kwargs)
        except Exception as e:
            print(e)
        else:
            args[0].client.set("start_time",time.time())
            result = func(*args,**kwargs)
            args[0].client.set("finish_time",time.time())
        return result
    return wrapper

def check_time(func):
    import functools
    @functools.wraps(func)
    def wrapper(*args,**kwargs):
        import time
        start = time.time()
        #print("Begin %s."%(func.__name__))
        result = func(*args,**kwargs)
        print(" > Finished %s in %.2f sec."%(func.__name__,time.time()-start))
        return result
    return wrapper

def writeToFile(self, dictBofw, title):

    docCount=sum(dictBofw.values())
    listTupleBofw = python_sorted(dictBofw.items())

    if docCount >= self.args.minw and docCount <= self.args.maxw:

        # Make string from list of tuples of bag-of-words
        cont = reduce(
            lambda _cont, _bofw: _cont+_bofw[0]+" "+str(_bofw[1])+" ",
            [ _bofw for _bofw in listTupleBofw if _bofw[1] >= self.args.minc ],
            ""
        ).rstrip()

        if len(cont) > 1:
            self.lockb.acquire()
            with open(self.args.ofcont,'a') as f:
                f.write(cont+"\n")
            self.lockb.release()
            self.writeTitleToFile(title)

            return True

    return False

class AbstParser():

    def __init__(self,args):

        self.lockb = threading.Lock()
        self.lockt = threading.Lock()
        self.args = args
        self.queue = Queue.Queue()
        self.client = redis.StrictRedis()
        self.redis_check()
        self.post_process = writeToFile

    def redis_check(self):
        try:
            self.client.ping()
        except redis.exceptions.ConnectionError:
            self.client = None
        except:
            sys.exit(2)

    def stopWorkers(self):
        for i in range(self.args.workers):
            self.queue.put("Finished")
            #print("Final flag is put at thread %(idx)s" % { "idx":i })

    @store_redis
    @check_time
    def startParse(self):

        bofwthreads = []

        for i in range(self.args.workers):
            bofwthreads.append(bofwThread(self,i))
            bofwthreads[i].start()

        page=""
        startFlag=endFlag=False
        try:
            for line in open(self.args.ifwiki,'r'):

                if line.find("<page>")>=0: startFlag=True
                if line.find("</page>")>=0: endFlag=True

                if startFlag: page+=line

                if startFlag and endFlag:
                    self.queue.put(page)
                    startFlag=endFlag=False
                    page=""

        except IOError as e:
            print(e,file=sys.stderr)
            self.stopWorkers()
            return

        # Leave out of this join() when the count becomes zero
        self.stopWorkers()
        self.queue.join()

        print( "" )

    # This forces inheritances to implement this method
    def parseText(self,text):
        return NotImplementedError

    def writeTitleToFile(self,title):
        if self.args.oftitle is None: return

        self.lockt.acquire()
        with open(self.args.oftitle,'a') as f:
            f.write(title+"\n")
        self.lockt.release()

    def saveWordCountsToRedis(self,total,num):
        if self.client == None: return

        try:
            # This may take long time like seconds
            #self.client.ping()
            self.client.zincrby("sorted_total",self.getSetVal(total))
            self.client.zincrby("sorted_num",self.getSetVal(num))
        except redis.exceptions.ConnectionError:
            pass
        except Exception as e:
            print(e)

    def getSetVal(self,n):
        return str(n/100*100)

    def applyTfIdf(self):
        if self.args.oftfidf is None: return
        self.getDfCorpus()

    def getDfCorpus(self):
        m = " > Execute %(func)s" % { "func": sys._getframe().f_code.co_name }
        c = 0

        docs = 0
        dictDf = defaultdict(int)
        for line in open(self.args.ofcont,'r'):
            if len(line.split(" ")) % 2 == 1:
                print("")
                print("Wrong bag-of-words : %s" % (line))
                raise Exception

            terms = line.split(" ")[::2]
            for term in terms:
                dictDf[term] += 1

            docs += 1

            c+=1
            print( "%(message)s [ # page %(count)s ]" %
                    { "message" : m, "count": c }, "\r", end="" )

        print( "" )

        self.getTfIdf(dictDf, docs)

    def getTfIdf(self, dictDf, docs):
        m = " > Execute %(func)s" % { "func": sys._getframe().f_code.co_name }
        c = 0

        for line in open(self.args.ofcont,'r'):

            terms = line.split(" ")[::2]
            freqs = list(map(int, line.split(" ")[1::2]))

            tfidf = {}
            for (term, freq) in zip(terms, freqs):
                # python 2.x does not give float unless specified
                tf = float(freq) / sum(freqs)
                idf = math.log10( float(docs) / dictDf[term] )+1
                tfidf[term] = tf * idf
                if tf*idf<0.0:
                    print("%s:%d, sum(freqs)=%d, docs=%d, tf=%.5f ,idf=%.5f, dictDf=%d" %
                            (term,freq,sum(freqs),docs,tf,idf,dictDf[term]))
                    raise Exception

            self.writeTfIdfToFile( self.normalize(tfidf) )

            c+=1
            print( "%(message)s [ # page %(count)s ]" %
                    { "message" : m, "count": c }, "\r", end="" )

        print( "" )


    def writeTfIdfToFile(self, dictTfIdf):
        listTupleTfIdf = python_sorted(dictTfIdf.items())

        output = reduce(
            lambda strs, tpl: strs+tpl[0]+" "+str(tpl[1])+" ", listTupleTfIdf, ""
        ).rstrip()

        if len(output) > 1:
            self.lockb.acquire()
            with open(self.args.oftfidf,'a') as f:
                f.write(output+"\n")
            self.lockb.release()

    def normalize(self, tfidf):
        norm= 1.0 / min(tfidf.values())
        normdict = {}
        for k,v in tfidf.items():
            normdict[k] = int(round(v * norm, 0))
        return normdict

class JapParser(AbstParser):

    def __init__(self,args):
        AbstParser.__init__(self,args)
        self.mecab = MeCab.Tagger()

        stopwords = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"
        self.stopwords=stopwords.split(",")

    def parseText(self,text):
        dictBofw=defaultdict(int) # python >= 2.5
        for word_info in self.mecab.parse(text).split("\n"):
            if word_info == "EOS": break
            word = word_info.split("\t")[0]
            if word in self.stopwords: continue
            info = word_info.split("\t")[1].split(",")
            speech = info[0]
            speech_info = info[1]
            baseform = info[6]
            if speech in ["形容詞","動詞","副詞"] or\
                (speech == "名詞" and speech_info == "サ変接続" ) :
                if baseform == "*": continue
                dictBofw[baseform] += 1 # defaultdict initializes the fist value of a key

        return dictBofw

class EngParser(AbstParser):

    def __init__(self,args):
        AbstParser.__init__(self,args)

        stopwords="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
        self.stopwords=stopwords.split(",")
        self.setRegexp(r" ", r"^[a-z][a-z0-9'-]*[a-z0-9]$")

    def setRegexp(self, separator="", termexp=""):
        if separator:
            self.separator = separator
        if termexp:
            self.termexp = termexp

    @check_time
    def readDictionary(self):
        self.dictMap = {}
        if self.args.ifdict == "": return

        # try message pack format first
        try:
            hdlr = open(self.args.ifdict,'r')
        except IOError as e:
            print(e,file=sys.stderr)
            sys.exit(1)

        if sys.version_info[0] == 2:
            unpacker = msgpack.Unpacker(hdlr)
            for msg in unpacker:
                if type(msg) == type({}):
                    self.dictMap = msg
                    return

        m = " > Execute %(func)s" % { "func": sys._getframe().f_code.co_name }
        c = 0
        try:
            for line in open(self.args.ifdict,'r'):
                if line.find(";;;")>=0: continue
                words = line.split("\t")
                if words[0] == words[2]: continue
                if " " in words[2]: continue
                self.dictMap[words[0].rstrip()] = words[2].rstrip()
                c+=1
                print( "%(message)s [ # word %(count)s ]" %
                        { "message" : m, "count": c }, "\r", end="" )
        except IOError as e:
            print(e,file=sys.stderr)
            sys.exit(1)
        else:
            print( "" )

    # This decorator is for line_profiler to get line-based profile
    #@profile
    def parseText(self,text):

        totalNumOfWords = 0
        dictBofw=defaultdict(int) # python >= 2.5

        for word in re.sub(self.separator," ",text).split():
            word = word.lower()
            if word in self.stopwords: continue
            if re.search(self.termexp,word) is None: continue
             # defaultdict initializes the fist value of a key
            dictBofw[self.dictMap.get(word,word)] += 1
            totalNumOfWords +=1

        if totalNumOfWords > 0:
            self.saveWordCountsToRedis(totalNumOfWords, len(dictBofw.keys()))

        return dictBofw

class Parser():

    def __init__(self,argv):
        self.parseArgs(argv)

    def parseArgs(self,argv):

        parser = argparse.ArgumentParser(description="Parse WikipediaXML to make bag-of-words.")
        parser.add_argument('-i','--ifwiki',required=True)
        parser.add_argument('-d','--ifdict',default="")
        parser.add_argument('-s','--ofcont',required=True)
        parser.add_argument('-t','--oftitle')
        parser.add_argument('-f','--oftfidf')
        parser.add_argument('-m','--minw',default=1,type=int)
        parser.add_argument('-x','--maxw',default=65535,type=int)
        parser.add_argument('-c','--minc',default=1,type=int)
        parser.add_argument('-g','--recateg',default=".*")
        parser.add_argument('-j','--isjapanese',action="store_const",const=True,default=False)
        parser.add_argument('-w','--workers',default=1,type=int)

        self.args = parser.parse_args(argv[1:])

    def new(self):
        if self.args.isjapanese:
            parser = JapParser(self.args)
        else:
            parser = EngParser(self.args)

        return parser

    def start(self):

        if self.args.isjapanese:
            parser = JapParser(self.args)
        else:
            parser = EngParser(self.args)
            parser.readDictionary()

        parser.startParse()
        parser.applyTfIdf()


if __name__ == "__main__":

    Parser(sys.argv).start()
