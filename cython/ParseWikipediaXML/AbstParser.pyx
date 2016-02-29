# -*- coding: utf-8 -*-

from __future__ import print_function

from ParseWikipediaXML.BofwThread import BofwThread
from ParseWikipediaXML.util_funcs import *

import sys
import threading
from collections import defaultdict
import redis
import math

if sys.version_info[0] == 2:
    import Queue
else:
    import queue as Queue

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
            bofwthreads.append(BofwThread(self,i))
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


