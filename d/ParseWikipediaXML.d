import std.stdio, std.string, std.conv, std.regex, std.algorithm, std.c.stdlib;

struct Bofw {
	string term;
	uint freq;
};

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
			auto arrBofw= parsePage(page);
			outfile.writef("%s\n",arrayToString(arrBofw));
			beginpage=endpage=false;
		}
	}

}

string arrayToString(Bofw[] arrBofw){
	char[] sliceBofw;
	foreach(bofw;arrBofw) {
		sliceBofw ~= bofw.term ~ " " ~ to!string(bofw.freq) ~ " ";
	}
	--sliceBofw.length;

	return to!string(sliceBofw);
}

Bofw[] parsePage(char[] page){

	Bofw[] bofw;
	ulong[string] bofwIndex;

	foreach(string word; page.split().map!(to!string).filter!matchWord()){
		if(word in bofwIndex){
			++bofw[bofwIndex[word]].freq;
		}else{
			auto cnt=bofwIndex.keys.length;
			bofwIndex[word]=cnt;
			bofw ~= Bofw(word,1);
		}
	}

	multiSort!("a.freq > b.freq","a.term < b.term",SwapStrategy.unstable)(bofw);
	return bofw;

}

bool matchWord(string word){
	return match(word, r"^[a-z][a-z0-9'-]*[a-z0-9]$").to!bool;
}


