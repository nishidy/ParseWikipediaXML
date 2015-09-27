#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <queue>
#include <memory>
#include <utility>
#include <algorithm>

//#include <mecab.h>

#include <boost/thread.hpp> // For std::thread::hardware_concurrency()
#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include <boost/program_options.hpp>
#include <boost/interprocess/sync/named_semaphore.hpp>
#include <boost/regex.hpp>

using namespace std;

namespace bt = boost;
namespace ip = boost::interprocess;
namespace po = boost::program_options;

#define semaphore_name "worker_number_control"

class Worker {

	private:
		vector<string> stopwords;

		po::variables_map args;

		bt::mutex *lockOutBofwFile;
		ofstream *osOutBofwFile;
		unordered_map<string,string> *map_dict;

		string page;
		string bofw;
		int num_terms_in_doc;

	public:
		Worker(){};
		Worker(po::variables_map args,string page,ofstream *osOutBofwFile, bt::mutex *lockOutBofwFile, unordered_map<string,string> *map_dict):
			args(args),page(page),osOutBofwFile(osOutBofwFile),lockOutBofwFile(lockOutBofwFile),map_dict(map_dict){ set_stopwords(); };
		~Worker(){};
		void parse();
		void save_to_file();
		void set_stopwords();
		string get_page(){ return page; }

};

void Worker::set_stopwords(){

	string stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

	replace(stopwords.begin(),stopwords.end(),',',' ');
	stringstream ss(stopwords);
	string stopword;
	while(!ss.eof()){
		ss>>stopword;
		this->stopwords.push_back(stopword);
	}
}

struct comparator{
	bool operator()(const pair<string,int>& a, const pair<string,int>& b) const {
		int af = a.second;
		int bf = b.second;
		return af<bf? true: af>bf? false: a.first>b.first;
	}
};

void Worker::parse(){

	bt::regex text_reg("<text[^>]*>([\\s\\S]*)</text>");
	bt::smatch match_text_reg;
	string text;

	if(bt::regex_search(page,match_text_reg,text_reg))
		text=match_text_reg.str(1);

	unordered_map<string,int> map_term_freq;
	stringstream ss_text(text);
	int num_terms_in_doc=0;
	string term;

	bt::regex term_reg("^[a-z][0-9a-z'-]*[0-9a-z]$");
	bt::smatch _smatch;

	while(!ss_text.eof()){
		ss_text>>term;
		transform(term.begin(),term.end(),term.begin(),::tolower);

		if(!bt::regex_search(term,_smatch,term_reg)) continue;
		auto itr_stopwords = find(stopwords.begin(),stopwords.end(),term);
		if(itr_stopwords!=stopwords.end()) continue;

		if(map_dict->find(term)!=map_dict->end()) term=(*map_dict)[term];

		if(map_term_freq.find(term)==map_term_freq.end()){
			map_term_freq[term]=1;
		}else{
			map_term_freq[term]++;
		}
		num_terms_in_doc++;
	}
	this->num_terms_in_doc=num_terms_in_doc;

	priority_queue<pair<string,int>,vector<pair<string,int> >, comparator> queue_term_freq;
	for(auto it=map_term_freq.begin();it!=map_term_freq.end();++it){
		queue_term_freq.push(*it);
	}

	bofw = "";
	stringstream ss_freq;
	pair<string,int> pair_term_freq;
	while(!queue_term_freq.empty()){

		if(bofw.length()>0) bofw+=" ";

		pair_term_freq = queue_term_freq.top();
		queue_term_freq.pop();
		
		ss_freq.str("");
		ss_freq.clear();
		ss_freq<<pair_term_freq.second;
		bofw+=pair_term_freq.first+" "+ss_freq.str();

	}

	this->bofw = bofw;
}

void Worker::save_to_file(){
	if(num_terms_in_doc>0)
	{
		bt::mutex::scoped_lock lk(*lockOutBofwFile);
		*osOutBofwFile<<bofw<<endl;
	}
}

void run_worker(shared_ptr<Worker> worker){
	worker->parse();
	worker->save_to_file();
	ip::named_semaphore semaphore(ip::open_only_t(), semaphore_name);
	semaphore.post();
}

void read_dictionary(string inDictFile, unordered_map<string,string> *map_dict){

	ifstream hInDictFile(inDictFile);
	string line;
	stringstream ss;
	string baseform, transform;
	while(hInDictFile && getline(hInDictFile,line)){
		ss.str("");
		ss.clear();
		ss<<line;
		ss>>transform>>baseform;
		(*map_dict)[transform]=baseform;
	}

}

int main(int argc, char *argv[]){

	string inWikiFile,inDictFile,outBofwFile;
	int minFreqOfTerm;

	po::options_description option("ParseWikipediaXML:");
	option.add_options()
		("inWikiFile,i",po::value<string>(&inWikiFile),"Input WikipediaXML file.")
		("inDictFile,d",po::value<string>(&inDictFile),"Input Dictionary file.")
		("outBofwFile,s",po::value<string>(&outBofwFile),"Output bag-of-words file.")
		("minFreqOfTerm,c",po::value<int>(&minFreqOfTerm),"How many times a term should appear in a document.")
	;

	po::variables_map args;

	try {
		po::store(po::parse_command_line(argc,argv,option),args);
	} catch (exception &e) {
		cout << e.what() << endl;
	}
	po::notify(args);

	bt::thread_group workers;
	// Number of concurrent threads supported.
	int max_running_workers = boost::thread::hardware_concurrency();

	ip::named_semaphore::remove(semaphore_name);
	ip::named_semaphore(ip::create_only_t(), semaphore_name, max_running_workers);
	ip::named_semaphore semaphore(ip::open_only_t(), semaphore_name);

	ifstream hInWikiFile(inWikiFile.c_str());
	if(!hInWikiFile) return 1;

	ofstream osOutBofwFile(outBofwFile);
	bt::mutex lockOutBofwFile;

	unordered_map<string,string> map_dict;
	read_dictionary(args["inDictFile"].as<string>(),&map_dict);

	string line="", page="";
	int insidePage=0,outsidePage=0;
	int c=0;
	while(hInWikiFile && getline(hInWikiFile,line)){
		if(string::npos!=line.find("<page>",0)) insidePage=1;
		if(string::npos!=line.find("</page>",0)) outsidePage=1;
		if(insidePage) page += line;
		if(outsidePage){
			c++;
			//cout<<c<<endl;
			semaphore.wait();
			shared_ptr<Worker> worker =
				make_shared<Worker>(args,page,&osOutBofwFile,&lockOutBofwFile,&map_dict);
			workers.create_thread(bt::bind(&run_worker,worker));
			page = "";
			insidePage=outsidePage=0;
		}
	}
	workers.join_all();

	return 0;
}

