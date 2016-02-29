import pyximport; pyximport.install()

from ParseWikipediaXML.Parser import Parser
import sys

parser = Parser(sys.argv).new()
parser.readDictionary()
parser.setRegexp(separator=r"[.,;|\n]")
parser.startParse()

