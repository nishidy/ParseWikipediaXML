require './ParseWikipediaXML.rb'

parser = ParseWikipediaXML.new(:parser, ARGV)

parser.read_dictionary
parser.register_tags("page","text","title")
parser.run_parser
parser.run_tfidf

