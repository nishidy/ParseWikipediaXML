#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <math.h>

using namespace std;

#define ASCII_DIFF_UPPER_AND_LOWER 32

string stopwords= "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

string makeDocFromGroupOfBofw(string line, vector<string> *vecStopwords){

	stringstream ss(line);
	string term;
	int freq;

	vector<string> vecTerms;

	string tmpCurrentTerm;
	while(ss >> term >> freq){
		vector<string>::iterator it = find(vecTerms.begin(),vecTerms.end(),term);
		if( it == vecTerms.end() ){
			tmpCurrentTerm = term;
		}else{
			cerr << "Term is duplicated in a document! " << term << endl;
			exit(1);
		}
		if(tmpCurrentTerm.size()>1){
			tmpCurrentTerm[0] = (char)((int)tmpCurrentTerm[0]-ASCII_DIFF_UPPER_AND_LOWER);
		}
		for(int i=0;i<freq;i++){
			vecTerms.push_back(tmpCurrentTerm);
		}
	}
	// Append one of stopwords
	vecTerms.push_back(vecStopwords->at(line.length()%vecStopwords->size()));

	for(int i=0;i<log(vecTerms.size())+1;i++){
		random_shuffle(vecTerms.begin(),vecTerms.end());
	}

	string document;
	for(int i=0;i<vecTerms.size();i++){
		document += vecTerms[i];
		if(i<vecTerms.size()-1) document += " ";
	}

	return document;
}

int main(int argc, char *argv[]){

	string inputFile;
	if(argc==2){
		inputFile = argv[1];
	}else{
		cout << argv[0] << " inputFile"<< endl;
		return -1;
	}

	ifstream ifs(inputFile);
	if(ifs.fail()){
		cerr << "Failed to open file." << endl;
		return -1;
	}

	stringstream ss(stopwords);
	string word;
	vector<string> vecStopwords;
	while(getline(ss,word,',')){
		vecStopwords.push_back(word);
	}

	string line;
	int cnt=0;
	while(getline(ifs, line)){
		cout << "<page>" << endl << "<title>title" << cnt << "</title><text>";
		cout << makeDocFromGroupOfBofw(line,&vecStopwords) << endl;
		cout << "</text>"<< endl << "</page>" << endl;
		cnt++;
	}

	return 0;
}
