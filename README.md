#Purpose
Extract content and title from wikipedia database provided in xml into two files respectively.
You can specify category name to be extracted, maximum/minimum number of words and so on.

#Requirement
You need a dictionary available at http://www.cis.upenn.edu/~xtag/swrelease.html.
Specifically, please download and uncompress ftp://ftp.cis.upenn.edu/pub/xtag/morph-1.5/morph-1.5.tar.gz and then you'll find data/morph_english.flat.
It will let this program convert words to original form.

#To compile on Fedora 20 with g++ 4.8.3 and boost-1.54.0
g++ extWikipediaXml.cpp -std=c++0x -lboost_thread -lboost_regex -o extWikipediaXml

#Note
This uses cpu_set_t but currently I do not know if Max OS provides it.

test
