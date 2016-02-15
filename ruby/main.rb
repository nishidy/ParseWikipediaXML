require './ParseWikipediaXML.rb'

parser = ParseWikipediaXML.new(:parser, ARGV)

parser.read_dictionary
parser.register_tags("page","text","title")
parser.set_regexps("[,.;|\n]", "^[a-z][0-9a-z'-]*[0-9a-z]$")
parser.run_parser
parser.run_tfidf

