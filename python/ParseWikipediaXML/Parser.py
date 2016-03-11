# -*- coding: utf-8 -*-

from __future__ import print_function

from ParseWikipediaXML.EngParser import EngParser
from ParseWikipediaXML.JapParser import JapParser

import argparse
import sys

#if sys.version_info[:3] == 3.6:
#    import pyyaml
#else:
#    import yaml
import yaml

class Parser():

    def __init__(self,argv):
        self.exclusives = []
        self.parseArgs(argv)

    def make_parser(self):
        parser=argparse.ArgumentParser(description="Parse WikipediaXML to make bag-of-words.")
        parser.add_argument('-i','--ifwiki','--input_file_wikipedia')
        parser.add_argument('-y','--yaml')
        parser.add_argument('-d','--ifdict','--input_file_dictionary')
        parser.add_argument('-s','--ofcont','--output_file_bagofwords')
        parser.add_argument('-t','--oftitle','--output_file_title')
        parser.add_argument('-f','--oftfidf','--output_file_tfidf')
        parser.add_argument('-m','--minw','--minimum_count_per_doc',default=1,type=int)
        parser.add_argument('-x','--maxw','--maximum_count_per_doc',default=65535,type=int)
        parser.add_argument('-c','--minc','--minimum_count_per_word',default=1,type=int)
        parser.add_argument('-g','--recateg','--regexp_for_category',default=".*")
        parser.add_argument('-j','--isjapanese','--is_japanese',action="store_const",const=True,default=False)
        parser.add_argument('-w','--workers','--number_of_workers',default=1,type=int)
        return parser

    def parseArgs(self,argv):
        parser = self.make_parser()
        self.args = parser.parse_args(argv[1:])
        self.checkArgs()
        self.yamlLoad(parser)

    def yamlLoad(self,parser):
        if not self.args.yaml:
            return

        new_args = []
        for key, val in yaml.load(open(self.args.yaml,"r")).items():
            if not isinstance(val,bool):
                new_args.append("--"+key)
                new_args.append(str(val))
            else:
                if val:
                    new_args.append("--"+key)

        self.args = parser.parse_args(new_args)

    def checkExclusiveArgs(self):
        for [arg1,arg2] in self.exclusives:
            if arg1 and arg2:
                return False
            elif arg1 and not arg2:
                return True
            elif not arg1 and arg2:
                return True
            else:
                return False

    def checkArgs(self):
        self.exclusives.append([self.args.ifwiki,self.args.yaml])

        if self.checkExclusiveArgs():
            pass
        else:
            print("Exclusive argument check failed.")
            raise IOError

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

