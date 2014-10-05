#Purpose
Extract each page content and title from Wikipedia database provided in xml under some conditions in order to make bug-of-words.
Words and their counts how many times they appear in the page contents are extracted together and titles are also saved in separated output file.
You can specify a name of category to be extracted, maximum/minimum number of words and so on.

#Requirement
You need to install boost-{thread/regex}.
and you need a dictionary for parsing English pages available at http://www.cis.upenn.edu/~xtag/swrelease.html.
Specifically, please download and uncompress ftp://ftp.cis.upenn.edu/pub/xtag/morph-1.5/morph-1.5.tar.gz and then you'll find data/morph_english.flat.
It will let this program convert words to original form.
You also need to put stopwords.text which represents words to be ignored on the same level as this program.
This tool currently supports parsing Japanese pages, which needs to install R and RMeCab with mecab-ipadic.

#Compilation
To compile on Fedora 20 with g++ 4.8.3 and boost-1.54.0:  
$g++ extWikipediaXml.cpp -std=c++11 -lboost_thread -lboost_regex -o extWikipediaXml

#Usage example
./extWikipediaXml -i jawiki-20140930-pages-articles1.xml -d morph_english.flat -s page.out -t title.out -m 1000 -x 5000 -c 2 -g ".*people$" -v 1 -l EN

Please simply run ./extWikipediaXml to see option usage.

#Note
This uses cpu_set_t but currently I do not know if Max OS provides it.

