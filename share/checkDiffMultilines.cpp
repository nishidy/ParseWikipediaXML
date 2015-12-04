#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;

class Checker {
	public:
	string filename;
	vector<int> line_hash;
	vector<int> diff_hash;

	void push_hashes();
	void sort_hashes();
	void show_hashes(string);
	void show_diff_line(string);
};

void Checker::push_hashes(){

	ifstream ifs(filename);
	if(ifs.fail()){
		cerr << "Failed to open " << filename << "." << endl;
		exit(-2);
	}

	string line;
	while(getline(ifs, line)){
		line_hash.push_back(hash<string>()(line));
	}
	ifs.close();

}

void Checker::sort_hashes(){
	sort(line_hash.begin(), line_hash.end());
}

void Checker::show_hashes(string f){
	cout << f << ":" << endl;
	cout << "line_hash" << endl;

	// auto needs -std=c++11
	for(auto it=line_hash.begin(); it!=line_hash.end(); it++){
		cout << *it << endl;
	}
	cout << "diff_hash" << endl;
	for(auto it=diff_hash.begin(); it!=diff_hash.end(); it++){
		cout << *it << endl;
	}
}

void Checker::show_diff_line(string f){

	cout << endl << f << ": " << filename << endl << "--------------------" << endl;

	ifstream ifs(filename);
	if(ifs.fail()){
		cerr << "Failed to open " << filename << "." << endl;
		exit(-2);
	}

	string line;
	int n=1, h;
	vector<int>::iterator it;
	while(getline(ifs, line) && diff_hash.size()>0){
		h = hash<string>()(line);
		it = diff_hash.begin();
		while(it!=diff_hash.end() && *it<=h){
			if(h==*it){
				cout << n << ": " << line << endl;
				diff_hash.erase(it);
				break;
			}
			it++;
		}
		n++;
	}
	ifs.close();

}


void print_green(int a, int b){
	cout << "\x1b[34m[# of lines] A:" << a << " == B:" << b <<"\x1b[0m" << endl;
}

void print_red(int a, int b){
	cout << "\x1b[33m[# of lines] A:" << a << " != B:" << b <<"\x1b[0m" << endl;
}


bool equal(Checker* c1, Checker* c2){
	return c1->line_hash == c2->line_hash;
}

void size_check(Checker* c1, Checker* c2){
	if( c1->line_hash.size() == c2->line_hash.size() ){
		print_green(c1->line_hash.size(), c2->line_hash.size());
	} else {
		print_red(c1->line_hash.size(), c2->line_hash.size());
	}
}

void diff_check(Checker* c1, Checker* c2){

	vector<int>::iterator ai;
	vector<int>::iterator bi;

	ai=c1->line_hash.begin();
	bi=c2->line_hash.begin();

	while(ai!=c1->line_hash.end() && bi!=c2->line_hash.end()){
		if(*ai==*bi){
			ai++; bi++;
		}else{
			if(*ai<*bi){
				c1->diff_hash.push_back(*ai);
				ai++;
			}else if(*ai>*bi){
				c2->diff_hash.push_back(*bi);
				bi++;
			}
		}
	}

	while(ai!=c1->line_hash.end()){
		c1->diff_hash.push_back(*ai);
		ai++;
	}

	while(bi!=c2->line_hash.end()){
		c2->diff_hash.push_back(*bi);
		bi++;
	}

}

int main(int argc, char* argv[])
{

	Checker c1;
	Checker c2;

	bool verbose=false;
	bool v_verbose=false;

	switch(argc){
		case 3:
			c1.filename = argv[1];
			c2.filename = argv[2];
			break;
		case 4:
			c1.filename = argv[1];
			c2.filename = argv[2];
			if(string(argv[3])=="-v"){
				verbose=true;
			}else if(string(argv[3])=="-vv"){
				verbose=true;
				v_verbose=true;
			}else{
				return -1;
			}
			break;
		default:
			return -1;
	}

	c1.push_hashes();
	c2.push_hashes();

	c1.sort_hashes();
	c2.sort_hashes();

	if(verbose){
		size_check(&c1,&c2);
		diff_check(&c1,&c2);
	}

	if(v_verbose){
		c1.show_hashes("A");
		c2.show_hashes("B");
	}

	if( !equal(&c1,&c2) ){
		if(verbose){
			c1.show_diff_line("A");
			c2.show_diff_line("B");
		}
		return 1;
	}

	return 0;
}

