#Purpose
Extract content and title from wikipedia database provided in xml into two files respectively.
You can specify category name to be extracted, maximum/minimum number of words and so on.

#Requirement
You need a dictionary available at http://www.cis.upenn.edu/~xtag/swrelease.html.
Specifically, download and uncompress ftp://ftp.cis.upenn.edu/pub/xtag/morph-1.5/morph-1.5.tar.gz and then you'll find data/morph_english.flat.

#To compile
g++ extract_wikipedia_xml.cpp -std=c++0x -lboost_thread-mt -lboost_regex -o extract_wikipedia_xml

#Note
This uses cpu_set_t but I do not know if Max OS provides it.

