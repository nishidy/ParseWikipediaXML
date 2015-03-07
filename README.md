#Purpose
This is to extract the content of pages from Wikipedia database provided in XML and make [bag-of-words](http://en.wikipedia.org/wiki/Bag-of-words_model).

You can give your own conditions, such as the number of words that a page should have and/or the searching regular expression that should be found in a category tag of a page, etc.

#Compilation

```
$ g++ ParseWikipediaXML.cpp -std=c++11 -lboost_thread-mt -lboost_regex -o ParseWikipediaXML
```

```
$ javac -cp "/usr/share/java/commons-cli-1.2.jar" ParseWikipediaXML.java
```

```
$ go get github.com/PuerkitoBio/goquery
$ go build -o ParseWikipediaXML_go ParseWikipediaXML.go
```

```
$ sbt compile
```

#Usage
Note that only C++ can handle Japanese database.

```
$ ./ParseWikipediaXML
Usage:./ParseWikipediaXML -i File(Wikipedia) [-d File(dictionary)] -s File(Sentence) -t File(Title) -m min_words -x max_words -c min_word_count -g category [-v debug] -l [JP|EN] 
Note:
 - category is regular expression.
 - debug message is shown by setting -v.
```

```
$ ./ParseWikipediaXML_go -h
Usage of ./ParseWikipediaXML_go:
  -c=2: Minimum number that a word should have
  -d="": Input File(dictionary)
  -g=".*": Category(regular expression)
  -i="": Input File(Wikipedia)
  -j=false: Generate bug-of-words in JSON format
  -m=1: Minimum number of words that a page should have
  -s="": Output File(Contents)
  -t="": Output File(Title)
  -x=65535: Maximum number of words that a page should have
```

