# Purpose

This repository contains programs which create bag-of-words from Wikipedia database which is a large content in XML syntax in various programming languages to support (not completely as of now, but in future):
 
* Various conditions to bag-of-words such as the minimum number of each word in a document, etc.
* Documents in English and in Japanese
* Parsing in concurrent way employing threads
* Web interface to check the intermediate report e.g. throughput
* Test program to check if the program produces correct results with or without threads (under share directory)

Hoping to provide as a module in these programming languages for general purpose, not specifically for Wikipedia database. In terms of it, this repository is like an ecosystem to try and learn implementations in a better way.

```

<page>
	<title> ... </title>
	<text ...>
		HERE IS THE CONTENTS
	</text>
</page>
...

```
