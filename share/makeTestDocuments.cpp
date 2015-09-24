#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <math.h>

using namespace std;

#define ASCII_DIFF_UPPER_AND_LOWER 32

string makeDocFromGroupOfBofw(string line){

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

	string line;
	int cnt=0;
	while(getline(ifs, line)){
		cout << "<page>" << endl << "<title>title" << cnt << "</title><text>";
		cout << makeDocFromGroupOfBofw(line) << endl;
		cout << "</text>"<< endl << "<category>category" << cnt << "</category>" << "</page>" << endl;
		cnt++;
	}

	return 0;
}
