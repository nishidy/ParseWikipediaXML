#Purpose
This is to extract the content of pages from Wikipedia database provided in XML and make [bag-of-words](http://en.wikipedia.org/wiki/Bag-of-words_model).

You can give your own conditions, such as the number of words that a page should have and/or the searching regular expression that should be found in a category tag of a page, etc.

This can work on multiple CPUs effectively.

#Installation & Compilation

## C++
```
$ g++ ParseWikipediaXML.cpp -std=c++11 -lboost_thread-mt -lboost_regex -lmecab -o ParseWikipediaXML
```

## Java
```
$ javac -cp "/usr/share/java/commons-cli-1.2.jar" ParseWikipediaXML.java
```

## Go
```
$ go get github.com/PuerkitoBio/goquery
$ go build -o ParseWikipediaXML_go ParseWikipediaXML.go
```

## Scala
```
$ sbt compile
```

## Perl
```
$ sudo cpan install Getopt::ArgParse
```

## Erlang
```
$ erlc parseWikipediaXML.erl 
```

#Usage
Note that only C++ can handle Japanese database as of now.

##C++
```
$ ./ParseWikipediaXML
Usage:./ParseWikipediaXML -i File(Wikipedia) [-d File(dictionary)] -s File(Sentence) -t File(Title) -m min_words -x max_words -c min_word_count -g category [-v debug] -l [JP|EN] 
Note:
 - category is regular expression.
 - debug message is shown by setting -v.
```

##Go
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

###Perl
```
$ perl ParseWikipediaXML.pl --help
usage: ParseWikipediaXML --ofcont|-s --ifdict|-d --ifwiki|-i [--help|-h]
[--ngram|-n] [--recateg|-g] [--minc|-c] [--maxw|-x] [--minw|-m] [--oftitle|-t]

required named arguments:
  --ofcont, -s OFCONT      Output file with bag-of-words of each page
  --ifdict, -d IFDICT      Dictionary file as input 
  --ifwiki, -i IFWIKI      Wikipedia XML file as input

optional named arguments:
  --help, -h               ? show this help message and exit
  --ngram, -n NGRAM        ? The N number for N-gram
                               Default: 1
  --recateg, -g RECATEG    ? Regular expresion which each page should match with its
                               category title
                               Default: .*
  --minc, -c MINC          ? Minimum number which each word should have in each page
                               Default: 2
  --maxw, -x MAXW          ? Maximum number of words which each page should contain
                               Default: 65535
  --minw, -m MINW          ? Minimum number of words which each page should contain
                               Default: 1
  --oftitle, -t OFTITLE    ? Output file with title of each page
```

##Erlang
```
$ erl -noshell -s parseWikipediaXML main -s init stop
```
