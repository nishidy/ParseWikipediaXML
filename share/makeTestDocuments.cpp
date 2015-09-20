#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <math.h>

using namespace std;

string makeDocFromGroupOfBofw(string line){

	stringstream ss(line);
	string termOrFreq;

	vector<string> vecTerms;

	bool c=false;
	string tmpCurrentTerm;
	while(ss >> termOrFreq){
		if((c=!c)){ // term
			vector<string>::iterator it = find(vecTerms.begin(),vecTerms.end(),termOrFreq);
			if( it == vecTerms.end() ){
				tmpCurrentTerm = termOrFreq;
			}else{
				cerr << "Term is duplicated in a document! " << termOrFreq << endl;
				exit(1);
			}
		}else{ // freq
			for(int i=0;i<stoi(termOrFreq);i++){
				vecTerms.push_back(tmpCurrentTerm);
			}
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
	while(getline(ifs, line)){
		cout << makeDocFromGroupOfBofw(line) << endl;
	}

	return 0;
}
