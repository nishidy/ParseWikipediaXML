import sys
import argparse
import re
import threading
import Queue

parser = argparse.ArgumentParser(description="Parse WikipediaXML to make bag-of-words.")
parser.add_argument('-i','--ifwiki',required=True)
parser.add_argument('-d','--ifdict',required=True)
parser.add_argument('-s','--ofcont',required=True)
parser.add_argument('-t','--oftitle')
parser.add_argument('-m','--minw',default=1)
parser.add_argument('-x','--maxw',default=65535)
parser.add_argument('-c','--minc',default=2)
parser.add_argument('-g','--recateg',default=".*")

args = parser.parse_args(sys.argv[1:])

stopwords="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
stopwords=stopwords.split(",")

lock = threading.Lock()

class bowThread(threading.Thread):

	def __init__(self):
		super(bowThread,self).__init__()

	def run(self):

		while True:
			page = queue.get()
			if page == "Finished": break

			# Do not use match for this purpose. Instead, we can use search as well.
			if re.search("<title[^<>]*>%s</title>"%args.recateg,page) is None: continue
		
			match = re.search("<text[^<>]*>([^<>]+)</text>",page)
			text = match.group(1)
		
			docDict={}
			docCount=0
		
			for word in text.split():
				if len(word)==1: continue
				if word in stopwords: continue
				if re.search("^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$",word) is None: continue
				if word in dictMap: word = dictMap[word]
		
				if word in docDict:
					docDict[word] += 1
				else:
					docDict[word] = 1
				docCount+=1
		
			if docCount < args.minw or docCount > args.maxw:
				pass
			else:
				f = open(args.ofcont,'a')
				cont = reduce(lambda i,t: i+t[0]+" "+str(t[1])+" " if t[1] >= args.minc else i+"", docDict.iteritems(), "").rstrip()
				if len(cont) > 1:
					lock.acquire()
					f.write(cont+"\n")
					lock.release()

			# put() counts up and task_done() counts down
			queue.task_done()

dictMap = {}
for line in open(args.ifdict,'r'):
	if line.find(";;;")>=0: continue
	words = line.split("\t")
	dictMap[words[0].rstrip()] = words[2].rstrip()

queue = Queue.Queue()
bowthread = bowThread()
bowthread.start()

page=""
startFlag=endFlag=False
for line in open(args.ifwiki,'r'):
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

