#Purpose
Extract each page content and title from Wikipedia database provided in xml under some conditions in order to make [bug-of-words](http://en.wikipedia.org/wiki/Bag-of-words_model).
Words and their counts how many times they appear in the page contents are extracted together and titles are also saved in separated output file.
You can specify a name of category to be extracted, maximum/minimum number of words and so on.

#Requirement
You need to install boost-{thread/regex}.
and you need a dictionary for parsing English pages available at http://www.cis.upenn.edu/~xtag/swrelease.html.
Specifically, please download and uncompress ftp://ftp.cis.upenn.edu/pub/xtag/morph-1.5/morph-1.5.tar.gz and then you'll find data/morph_english.flat.
It will let this program convert words to original form.
This tool currently supports parsing Japanese pages, which needs to install R and RMeCab with mecab-ipadic.

Now, you don't need to put stopwords.text which represents words to be ignored.
They are included in code.

#Compilation
To compile on Fedora 20 with g++ 4.8.3 and boost-1.54.0:  
$g++ ParseWikipediaXML.cpp -std=c++11 -lboost_thread -lboost_regex -o ParseWikipediaXML

#Usage example

```cpp:cpp
$ ./ParseWikipediaXML
Usage:./ParseWikipediaXML -i File(Wikipedia) [-d File(dictionary)] -s File(Sentence) -t File(Title) -m min_words -x max_words -c min_word_count -g category [-v debug] -l [JP|EN] 
Note:
 - category is regular expression.
  - debug message is shown by setting -v.
```

```go:go
$ ./ParseWikipediaXML_go --help
Usage of ./ParseWikipediaXML_go:
  -c=2: min_word_count
  -d="": Input File(dictionary)
  -g=".*": Category(regular expression)
  -i="": Input File(Wikipedia)
  -m=2: min_word_length
  -s="": Output File(Contents)
  -t="": Output File(Title)
  -x=256: max_word_length
```

#Note
This uses cpu_set_t but currently I do not know if Max OS provides it.

