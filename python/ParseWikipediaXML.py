# -*- coding: utf-8 -*-

import sys
import argparse
import re
import threading
import Queue
from collections import defaultdict
import MeCab

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

	def __init__(self, parser, queue):
		super(bofwThread,self).__init__()
		self.parser = parser
		self.queue = queue

	def run(self):

		args = self.parser.args

		while True:

			page = self.queue.get()
			if page == "Finished": break

			# Do not use re.match for this purpose. Instead, we can use re.search as well.
			if re.search("<category[^<>]*>[^<>]+</category>",page) is not None:
				if re.search("<category[^<>]*>%s</category>"%args.recateg,page) is None: continue

			match = re.search("<text[^<>]*>([^<>]+)</text>",page)
			text = match.group(1)

			dictBofw = parser.parseText(text)
			parser.writeToFile(dictBofw)

			# put() counts up and task_done() counts down
			self.queue.task_done()

class AbstParser():

	def __init__(self,args):
		self.lock = threading.Lock()
		self.args = args

	def startParse(self):
		queue = Queue.Queue()
		bowthread = bofwThread(self,queue)
		bowthread.start()

		page=""
		startFlag=endFlag=False
		for line in open(self.args.ifwiki,'r'):
			if line.find("<page>")>=0:
				startFlag=True
			if line.find("</page>")>=0:
				endFlag=True
			if startFlag:
				page+=line
			if endFlag:
				queue.put(page)
				startFlag=endFlag=False
				page=""

		# Leave out when count is zero
		queue.join()
		queue.put("Finished")

	# This forces inheritances to implement this method
	def parseText(self,text):
		return NotImplementedError

	def writeToFile(self,dictBofw):

		docCount=sum(dictBofw.values())
		listTupleBofw = sorted(dictBofw.items(), cmp=cmp_dict)

		if docCount >= args.minw and docCount <= args.maxw:
			cont = reduce(lambda i,t: i+t[0]+" "+str(t[1])+" " if t[1] >= args.minc else i+"", listTupleBofw, "").rstrip()

			if len(cont) > 1:
				parser.lock.acquire()
				with open(args.ofcont,'a') as f:
					f.write(cont+"\n")
				parser.lock.release()


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

	def readDictionary(self):
		self.dictMap = {}
		if self.args.ifdict == "": return
		for line in open(self.args.ifdict,'r'):
			if line.find(";;;")>=0: continue
			words = line.split("\t")
			if words[0] == words[2]: continue
			self.dictMap[words[0].rstrip()] = words[2].rstrip()

	def parseText(self,text):
		dictBofw=defaultdict(int) # python >= 2.5
		for word in text.split():
			word = word.lower()
			if len(word)==1: continue
			if word in self.stopwords: continue
			if re.search("^[a-z][a-z0-9'-]*[a-z0-9]$",word) is None: continue
			dictBofw[self.dictMap.get(word,word)] += 1 # defaultdict initializes the fist value of a key
		return dictBofw


def parseArgs(argv):

	parser = argparse.ArgumentParser(description="Parse WikipediaXML to make bag-of-words.")
	parser.add_argument('-i','--ifwiki',required=True)
	parser.add_argument('-d','--ifdict',default="")
	parser.add_argument('-s','--ofcont',required=True)
	parser.add_argument('-t','--oftitle')
	parser.add_argument('-m','--minw',default=1,type=int)
	parser.add_argument('-x','--maxw',default=65535,type=int)
	parser.add_argument('-c','--minc',default=2,type=int)
	parser.add_argument('-g','--recateg',default=".*")
	parser.add_argument('-j','--isjapanese',action="store_const",const=True,default=False)

	return parser.parse_args(argv[1:])


if __name__ == "__main__":
	args = parseArgs(sys.argv)
	if args.isjapanese:
		parser = JapParser(args)
	else:
		parser = EngParser(args)
		parser.readDictionary()
	parser.startParse()

