#Purpose
Extract each page content and title from Wikipedia database provided in xml under some conditions in order to make bug-of-words.
Words and their counts how many times they appear in the page contents are extracted together and titles are also saved in separated output file.
You can specify a name of category to be extracted, maximum/minimum number of words and so on.

#Requirement
You need a dictionary available at http://www.cis.upenn.edu/~xtag/swrelease.html.
Specifically, please download and uncompress ftp://ftp.cis.upenn.edu/pub/xtag/morph-1.5/morph-1.5.tar.gz and then you'll find data/morph_english.flat.
It will let this program convert words to original form.
  
  
You also need to put stopwords.text which represents words to be ignored on the same level as this program.

#Compilation
To compile on Fedora 20 with g++ 4.8.3 and boost-1.54.0:  
$g++ extWikipediaXml.cpp -std=c++11 -lboost_thread -lboost_regex -o extWikipediaXml

#Usage example
./extWikipediaXml enwiki-latest-pages-articles1.xml ./morph_english.flat page.out title.out 1000 5000 2 ".*people$" 1

#Note
This uses cpu_set_t but currently I do not know if Max OS provides it.

