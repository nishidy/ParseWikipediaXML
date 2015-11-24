#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

int main(int argc, char* argv[])
{
	string inputFile1,inputFile2;
	if(argc==3){
		inputFile1 = argv[1];
		inputFile2 = argv[2];
	}else{
		cout << argv[0] << " inputFile1 inputFile2"<< endl;
		return -1;
	}

	ifstream ifs1(inputFile1);
	if(ifs1.fail()){
		cerr << "Failed to open " << inputFile1 << "." << endl;
		return -2;
	}

	ifstream ifs2(inputFile2);
	if(ifs2.fail()){
		cerr << "Failed to open " << inputFile2 << "." << endl;
		return -3;
	}

	vector<int> vec_int_file1;
	vector<int> vec_int_file2;
	string line;

	while(getline(ifs1, line)){
		if(line.size()>0)
			vec_int_file1.push_back(hash<string>()(line));
	}

	while(getline(ifs2, line)){
		if(line.size()>0)
			vec_int_file2.push_back(hash<string>()(line));
	}

	sort(vec_int_file1.begin(),vec_int_file1.end());
	sort(vec_int_file2.begin(),vec_int_file2.end());

	if( vec_int_file1 != vec_int_file2 ){
		return 1;
	}

	return 0;
}
