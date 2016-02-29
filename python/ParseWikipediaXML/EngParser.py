# -*- coding: utf-8 -*-

from __future__ import print_function

from ParseWikipediaXML.AbstParser import AbstParser
from ParseWikipediaXML.funcs import *

import sys
import re
from collections import defaultdict

if sys.version_info[0] == 2:
    import msgpack

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
            dictBofw[self.dictMap.get(word,word)] += 1
            totalNumOfWords +=1

        if totalNumOfWords > 0:
            self.saveWordCountsToRedis(totalNumOfWords, len(dictBofw.keys()))

        return dictBofw

