import pyximport; pyximport.install()

from ParseWikipediaXML import Parser
import sys

parser = Parser(sys.argv).new()
parser.readDictionary()
parser.setRegexp(separator=r"[.,;|\n]")
parser.startParse()

