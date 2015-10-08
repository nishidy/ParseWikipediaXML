import sys
import argparse
import re
import threading
import Queue
from collections import defaultdict
from igo.Tagger import Tagger

lock = threading.Lock()

class bofwThread(threading.Thread):

	def __init__(self, parser, queue):
		super(bofwThread,self).__init__()
		self.parser = parser
		self.queue = queue

	def run(self):

		args = self.parser.args
		stopwords = self.parser.stopwords
		dictMap = self.parser.dictMap

		while True:

			page = self.queue.get()
			if page == "Finished": break

			# Do not use re.match for this purpose. Instead, we can use re.search as well.
			if re.search("<category[^<>]*>%s</category>"%args.recateg,page) is None: continue

			match = re.search("<text[^<>]*>([^<>]+)</text>",page)
			text = match.group(1)

			dictBofw=defaultdict(int) # python >= 2.5
			for word in text.split():
				word = word.lower()
				if len(word)==1: continue
				if word in stopwords: continue
				if re.search("^[a-z][a-z0-9'-]*[a-z0-9]$",word) is None: continue
				dictBofw[dictMap.get(word,word)] += 1 # defaultdict initializes the fist value of a key

			docCount=sum(dictBofw.values())
			listTupleBofw = sorted(dictBofw.items(), key=lambda x:x[1], reverse=True)
			if docCount >= args.minw and docCount <= args.maxw:
				cont = reduce(lambda i,t: i+t[0]+" "+str(t[1])+" " if t[1] >= args.minc else i+"", listTupleBofw, "").rstrip()
				if len(cont) > 1:
					lock.acquire()
					with open(args.ofcont,'a') as f:
						f.write(cont+"\n")
					lock.release()

			# put() counts up and task_done() counts down
			self.queue.task_done()

class AbstParser:

	def __init__(self):
		pass

	def startParse(self,argv):
		return NotImplementedError


class JapParser(AbstParser):

	def __init__(self,args):
		self.args = args

		stopwords = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"
		self.stopwords=stopwords.split(",")

class EngParser(AbstParser):

	def __init__(self,args):
		self.args = args

		stopwords="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
		self.stopwords=stopwords.split(",")

	def readDictionary(self):
		self.dictMap = {}
		if self.args.notcarebaseform: return
		for line in open(self.args.ifdict,'r'):
			if line.find(";;;")>=0: continue
			words = line.split("\t")
			if words[0] == words[2]: continue
			self.dictMap[words[0].rstrip()] = words[2].rstrip()

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


def parseArgs(argv):

	parser = argparse.ArgumentParser(description="Parse WikipediaXML to make bag-of-words.")
	parser.add_argument('-i','--ifwiki',required=True)
	parser.add_argument('-d','--ifdict',required=True)
	parser.add_argument('-s','--ofcont',required=True)
	parser.add_argument('-t','--oftitle')
	parser.add_argument('-m','--minw',default=1,type=int)
	parser.add_argument('-x','--maxw',default=65535,type=int)
	parser.add_argument('-c','--minc',default=2,type=int)
	parser.add_argument('-g','--recateg',default=".*")
	parser.add_argument('-b','--notcarebaseform',action="store_const",const=True,default=False)
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

