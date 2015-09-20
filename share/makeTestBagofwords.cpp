#include <iostream>
#include <random>
#include <vector>

using namespace std;

#define ASCII_LOWER_START 97
#define ASCII_LOWER_END 122
#define ASCII_DASH 45

#define CHAR_SET_NUM ((ASCII_LOWER_END-ASCII_LOWER_START+1)-1+1)

uniform_int_distribution<int> gGenCharAlphabet(0,CHAR_SET_NUM-1);
uniform_int_distribution<int> gGenCharAlphabetDash(0,CHAR_SET_NUM);

char selectAlphabet(mt19937* mt){

	int ichar = gGenCharAlphabet(*mt);
	return (char)(ichar+ASCII_LOWER_START);

}

char selectAlphabetDash(mt19937* mt){

	int ichar = gGenCharAlphabetDash(*mt);

	if(ichar < 26){
		return (char)(ichar+ASCII_LOWER_START);
	}else{
		return (char)(ASCII_DASH);
	}

}

string makeRandomTerm(mt19937* mt, int maxNumCharOfTerm){

	uniform_int_distribution<int> genNumCharOfTerm(1,maxNumCharOfTerm);
	int length = genNumCharOfTerm(*mt);

	string term;
	for(int i=0;i<length;i++){
		char a;
		if(i==0||i==length-1){
			a = selectAlphabet(mt);
		}else{
			a = selectAlphabetDash(mt);
		}
		term += a;
	}
	return term;
}

int makeRandomFreq(mt19937* mt, int limit){
	uniform_int_distribution<int> genFreqOfTerm(1,limit);
	return genFreqOfTerm(*mt);
}

int main(int argc, char *argv[]){

	int numDocs;
	int maxNumTermsInDocs;
	int maxNumCharOfTerm;
	int maxFreq;
	if(argc==5){
		numDocs=atoi(argv[1]);
		maxNumTermsInDocs=atoi(argv[2]);
		maxNumCharOfTerm=atoi(argv[3]);
		maxFreq=atoi(argv[4]);
	}else{
		cout << argv[0] << " numDocs maxNumTermsInDocs maxNumCharOfTerm maxFreq"<< endl;
		return 1;
	}

	random_device rnd;
	mt19937 mt(rnd());

	for(int i=0;i<numDocs;i++){
		uniform_int_distribution<int> genNumTermsInDoc(1,maxNumTermsInDocs);
		int numTermsInDoc = genNumTermsInDoc(mt);
	
		int limit=maxFreq;
		vector<string> vecTerms;

		for(int j=0;j<numTermsInDoc;j++){
			string term;
			while(true){
				term = makeRandomTerm(&mt,maxNumCharOfTerm);
				vector<string>::iterator itVecTerms = find(vecTerms.begin(),vecTerms.end(),term);
				if( itVecTerms == vecTerms.end() ){
					vecTerms.push_back(term);
					break;
				}
			}
			int freq  = makeRandomFreq(&mt,limit);
			cout << term << " " << freq;
			if(j<numTermsInDoc-1) cout << " ";
			limit = freq;
		}
		cout << endl;
	}

	return 0;
}


