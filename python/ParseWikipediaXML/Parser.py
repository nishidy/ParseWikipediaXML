# -*- coding: utf-8 -*-

from __future__ import print_function
import argparse
from ParseWikipediaXML.EngParser import EngParser
from ParseWikipediaXML.JapParser import JapParser

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

