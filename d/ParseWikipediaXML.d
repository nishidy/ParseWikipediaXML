import std.stdio, std.string, std.conv, std.regex, std.algorithm;

void main() {

	auto file = File("../share/enwiki-test-5000");

	bool beginpage = false, endpage = false;
	char[] page;

	foreach(line; file.byLine() ) {
		if(line.indexOf("<page>")>-1) beginpage = true;
		if(line.indexOf("</page>")>-1) endpage = true;

		if(beginpage) page ~= line;
		if(endpage){
			auto hashDict = parsePage(page);
			writeln(hashDict);
			beginpage=endpage=false;
		}
	}

}

int[string] parsePage(char[] page){

	int[string] hashDict;

	foreach(word; page.split().map!(to!string).filter!matchWord()){
		if(word in hashDict){
			hashDict[word] += 1;
		}else{
			hashDict[word] = 1;
		}
	}

	return hashDict;

}

bool matchWord(string word){
	return match(word, r"^[a-z][a-z0-9'-]*[a-z0-9]$").to!bool;
}
