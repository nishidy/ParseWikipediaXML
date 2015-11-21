import std.stdio, std.string, std.conv, std.regex, std.algorithm, std.c.stdlib;

void main(string argv[]) {

	if(argv.length!=3){
		writef("%s\n inFile outFile",argv[0]);
		exit(-1);
	}

	auto infile = File(argv[1]);
	auto outfile = File(argv[2],"w");

	bool beginpage = false, endpage = false;
	char[] page;

	foreach(char[] line; infile.byLine() ) {
		if(line.indexOf("<page>")>-1) beginpage = true;
		if(line.indexOf("</page>")>-1) endpage = true;

		if(beginpage) page ~= line;
		if(endpage){
			auto hashBofw= parsePage(page);
			outfile.writef("%s\n",hashToString(hashBofw));
			beginpage=endpage=false;
		}
	}

}

string hashToString(int[string] hashBofw){
	char[] sliceBofw;
	foreach(k,v;hashBofw) {
		sliceBofw ~= k ~ " " ~ to!string(v) ~ " ";
	}
	--sliceBofw.length;

	return to!string(sliceBofw);
}

int[string] parsePage(char[] page){

	int[string] hashDict;

	foreach(string word; page.split().map!(to!string).filter!matchWord()){
		if(word in hashDict){
			++hashDict[word];
		}else{
			hashDict[word] = 1;
		}
	}

	return hashDict;

}

bool matchWord(string word){
	return match(word, r"^[a-z][a-z0-9'-]*[a-z0-9]$").to!bool;
}

