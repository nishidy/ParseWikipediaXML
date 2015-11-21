import std.stdio, std.string, std.conv, std.regex, std.algorithm, std.c.stdlib;

struct Bofw {
	string term;
	uint freq;
};

auto stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",");

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

	foreach(string word;
			page.split()
			.map!(to!string)
			.filter!matchWord()
			.filter!(a=>!stopwords.canFind(a))
	){
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


