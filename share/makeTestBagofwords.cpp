#include <iostream>
#include <random>
#include <vector>
#include <sstream>
#include <algorithm>

using namespace std;

#define ASCII_LOWER_START 97
#define ASCII_LOWER_END 122
#define CHAR_SET_NUM (ASCII_LOWER_END-ASCII_LOWER_START)

#define ASCII_APOSTROPHE 39
#define ASCII_DASH 45

string stopwords= "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

uniform_int_distribution<int> gGenCharAlphabet(0,CHAR_SET_NUM);
uniform_int_distribution<int> gGenCharAlphabetDashApostrophe(0,CHAR_SET_NUM+2);

char selectAlphabet(mt19937* mt){

	int ichar = gGenCharAlphabet(*mt);
	return (char)(ichar+ASCII_LOWER_START);

}

char selectAlphabetDashApostrophe(mt19937* mt){

	int ichar = gGenCharAlphabetDashApostrophe(*mt);

	if(ichar < CHAR_SET_NUM){
		return (char)(ichar+ASCII_LOWER_START);
	}else if(ichar < CHAR_SET_NUM+1){
		return (char)(ASCII_APOSTROPHE);
	}else{
		return (char)(ASCII_DASH);
	}

}

string makeRandomTerm(mt19937* mt, int maxNumCharOfTerm){

	uniform_int_distribution<int> genNumCharOfTerm(2,maxNumCharOfTerm);
	int length = genNumCharOfTerm(*mt);

	string term;
	for(int i=0;i<length;i++){
		char a;
		if(i==0||i==length-1){
			a = selectAlphabet(mt);
		}else{
			a = selectAlphabetDashApostrophe(mt);
		}
		term += a;
	}
	return term;
}

int makeRandomFreq(mt19937* mt, int limit){
	uniform_int_distribution<int> genFreqOfTerm(1,limit);
	return genFreqOfTerm(*mt);
}

bool findStrFromVec(vector<string>* vecStrs, string str){
	vector<string>::iterator itVecStrs =
		find(vecStrs->begin(),vecStrs->end(),str);

	if( itVecStrs == vecStrs->end() )
		return false;
	else
		return true;

}

int main(int argc, char *argv[]){

	int numDocs;
	int maxNumTermsInDocs;
	int maxNumCharOfTerm;
	int maxFreq;

	if(argc==5){
		numDocs=atoi(argv[1]);
		maxNumTermsInDocs=atoi(argv[2]);
		maxNumCharOfTerm=atoi(argv[3])<2?2:atoi(argv[3]);
		maxFreq=atoi(argv[4]);
	}else{
		cout << argv[0] << " numDocs maxNumTermsInDocs maxNumCharOfTerm maxFreq"<< endl;
		return 1;
	}

	random_device rnd;
	mt19937 mt(rnd());

	stringstream ss(stopwords);
	string word;
	vector<string> vecStopwords;
	while(getline(ss,word,',')){
		vecStopwords.push_back(word);
	}

	for(int i=0;i<numDocs;i++){
		uniform_int_distribution<int> genNumTermsInDoc(1,maxNumTermsInDocs);
		int numTermsInDoc = genNumTermsInDoc(mt);
	
		int limit=maxFreq;
		vector<string> vecTerms;

		for(int j=0;j<numTermsInDoc;j++){
			string term;
			while(true){
				term = makeRandomTerm(&mt,maxNumCharOfTerm);

				if(findStrFromVec(&vecStopwords, term)) continue;
				if(findStrFromVec(&vecTerms, term)) continue;

				vecTerms.push_back(term);
				break;
			}
		}
		sort(vecTerms.begin(), vecTerms.end());

		for(int j=0;j<numTermsInDoc;j++){
			int freq  = makeRandomFreq(&mt,limit);
			cout << vecTerms[j] << " " << freq;
			if(j<numTermsInDoc-1) cout << " ";
			limit = freq;
		}
		cout << endl;
	}

	return 0;
}

