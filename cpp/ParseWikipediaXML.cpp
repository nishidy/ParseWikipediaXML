#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <queue>
#include <memory>
#include <utility>
#include <algorithm>

#include <mecab.h>

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

struct comparator{
	bool operator()(const pair<string,int>& a, const pair<string,int>& b) const {
		int af = a.second;
		int bf = b.second;
		return af<bf? true: af>bf? false: a.first>b.first;
	}
};

class AbstParser {

	protected:

		po::variables_map args;
		string page;
		ofstream *hdlr_out_bofw_file;
		bt::mutex *lock_out_bofw_file;
		unordered_map<string,string> *map_dict;

		bool is_japanese;
		vector<string> vec_stopwords;

		string bofw;
		int num_terms_in_doc = 0;

	public:

		// Factory needs this to initialize its AbstParser member
		AbstParser(){};

		AbstParser(
			po::variables_map args,
			string page,
			ofstream *hdlr_out_bofw_file,
			bt::mutex *lock_out_bofw_file,
			unordered_map<string,string> *map_dict,
			bool is_japanese
		) :
		// XXX: The order to initialize these properties
		//      is not following this way. It depends on
		//      what order they are defined in the class
		//      actually.
		args(args),
		page(page),
		hdlr_out_bofw_file(hdlr_out_bofw_file),
		lock_out_bofw_file(lock_out_bofw_file),
		map_dict(map_dict),
		is_japanese(is_japanese)
		{};

		virtual ~AbstParser(){};

		virtual void set_stopwords(){};
		virtual void parse(){};
		virtual string get_page() const { return ""; };

		void save_to_file();

};

void AbstParser::save_to_file(){
	if(bofw.length()>0)
	{
		bt::mutex::scoped_lock lk(*lock_out_bofw_file);
		*hdlr_out_bofw_file<<bofw<<endl;
	}
}

// singleton
class JapParserMecab {

	private:
		JapParserMecab(){
			model = MeCab::createModel(0,NULL);
			tagger = model->createTagger();
		};
		static JapParserMecab* singleton;

	public:

		MeCab::Model *model;
		MeCab::Tagger *tagger;

		static JapParserMecab* get_instance(){
			return singleton;
		}

		~JapParserMecab(){};
};
JapParserMecab* JapParserMecab::singleton = new JapParserMecab();

class JapParser : public AbstParser {

	private:
		JapParserMecab* mecab;
		MeCab::Lattice *lattice;

	public:

		JapParser(
			po::variables_map args,
			string page,
			ofstream *hdlr_out_bofw_file,
			bt::mutex *lock_out_bofw_file,
			unordered_map<string,string> *map_dict,
			bool is_japanese = true
		) :
		AbstParser(
			args,
			page,
			hdlr_out_bofw_file,
			lock_out_bofw_file,
			map_dict,
			is_japanese
		)
		{
			set_stopwords();
			mecab = JapParserMecab::get_instance();
			lattice = mecab->model->createLattice();
		};

		~JapParser(){};

		void parse();
		void set_stopwords();
		string get_page(){ return page; };

};

void JapParser::set_stopwords(){

	string stopwords = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ";

	replace(stopwords.begin(),stopwords.end(),',',' ');
	stringstream ss(stopwords);
	string stopword;
	while(!ss.eof()){
		ss>>stopword;
		vec_stopwords.push_back(stopword);
	}
}

void JapParser::parse(){

	bt::regex text_reg("<text[^>]*>([\\s\\S]*)</text>");
	bt::smatch match_text_reg;
	string text;

	if(bt::regex_search(page,match_text_reg,text_reg))
		text=match_text_reg.str(1);

	lattice->set_sentence(text.c_str());
	if(!mecab->tagger->parse(lattice)){
		cout << "targger->parse failed." << endl;
		return ;
	}

	unordered_map<string,int> map_term_freq;

	MeCab::Node* node = lattice->bos_node();
	for(; node; node=node->next){
		istringstream iss(node->feature);
		string feature;
		vector<string> vec_feature;
		while(getline(iss,feature,',')){
			vec_feature.push_back(feature);
		}

		string term;
		if( (vec_feature[0] == "名詞" && vec_feature[1] == "サ変接続") ||
			(vec_feature[0] == "動詞" || vec_feature[0] == "形容詞" || vec_feature[0] == "副詞") )
		{
			if("*" == vec_feature[6]){
				// XXX: node->surface is NOT terminated by 0
				//term = ((string)node->surface).substr(0,node->length);
				continue;
			}else{
				term = vec_feature[6];
			}

			auto itr_stopwords = find(vec_stopwords.begin(),vec_stopwords.end(),term);
			if(itr_stopwords!=vec_stopwords.end()) continue;

			if(map_term_freq.find(term)==map_term_freq.end()){
				map_term_freq[term]=1;
			}else{
				map_term_freq[term]++;
			}
			num_terms_in_doc++;
		}
	}

	// To sort unordered_map, use priority_queue is good practice
	priority_queue<pair<string,int>,vector<pair<string,int> >, comparator> queue_term_freq;
	for(auto it=map_term_freq.begin();it!=map_term_freq.end();++it){
		queue_term_freq.push(*it);
	}

	bofw = "";
	stringstream ss_freq;
	pair<string,int> pair_term_freq;
	while(!queue_term_freq.empty()){

		pair_term_freq = queue_term_freq.top();
		queue_term_freq.pop();

		if(pair_term_freq.second>=args["min_freq_of_term"].as<int>()){
			ss_freq.str("");
			ss_freq.clear();
			ss_freq<<pair_term_freq.second;
			if(bofw.length()>0) bofw+=" ";
			bofw+=pair_term_freq.first+" "+ss_freq.str();
		}
	}

}

class EngParser : public AbstParser {

	public:

		EngParser(
			po::variables_map args,
			string page,
			ofstream *hdlr_out_bofw_file,
			bt::mutex *lock_out_bofw_file,
			unordered_map<string,string> *map_dict,
			bool is_japanese = false
		) :
		AbstParser(
			args,
			page,
			hdlr_out_bofw_file,
			lock_out_bofw_file,
			map_dict,
			is_japanese
		)
		{
			set_stopwords();
		};

		~EngParser(){};

		void parse();
		void set_stopwords();
		string get_page(){ return page; };

};

void EngParser::set_stopwords(){

	string stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

	replace(stopwords.begin(),stopwords.end(),',',' ');
	stringstream ss(stopwords);
	string stopword;
	while(!ss.eof()){
		ss>>stopword;
		vec_stopwords.push_back(stopword);
	}
}

void EngParser::parse(){

	bt::regex text_reg("<text[^>]*>([\\s\\S]*)</text>");
	bt::smatch match_text_reg;
	string text;

	if(bt::regex_search(page,match_text_reg,text_reg))
		text=match_text_reg.str(1);

	unordered_map<string,int> map_term_freq;
	stringstream ss_text(text);
	string term;

	bt::regex term_reg("^[a-z][0-9a-z'-]*[0-9a-z]$");
	bt::smatch _smatch;

	while(!ss_text.eof()){
		ss_text>>term;
		transform(term.begin(),term.end(),term.begin(),::tolower);

		if(!bt::regex_search(term,_smatch,term_reg)) continue;
		auto itr_stopwords = find(vec_stopwords.begin(),vec_stopwords.end(),term);
		if(itr_stopwords!=vec_stopwords.end()) continue;

		if(map_dict->find(term)!=map_dict->end()) term=(*map_dict)[term];

		if(map_term_freq.find(term)==map_term_freq.end()){
			map_term_freq[term]=1;
		}else{
			map_term_freq[term]++;
		}
		num_terms_in_doc++;
	}

	if(num_terms_in_doc<args["min_terms_in_doc"].as<int>() ||
	   num_terms_in_doc>args["max_terms_in_doc"].as<int>() )
		return;

	// To sort unordered_map, use priority_queue is good practice
	priority_queue<pair<string,int>,vector<pair<string,int> >, comparator> queue_term_freq;
	for(auto it=map_term_freq.begin();it!=map_term_freq.end();++it){
		queue_term_freq.push(*it);
	}

	bofw = "";
	stringstream ss_freq;
	pair<string,int> pair_term_freq;
	while(!queue_term_freq.empty()){

		pair_term_freq = queue_term_freq.top();
		queue_term_freq.pop();

		if(pair_term_freq.second>=args["min_freq_of_term"].as<int>()){
			ss_freq.str("");
			ss_freq.clear();
			ss_freq<<pair_term_freq.second;
			if(bofw.length()>0) bofw+=" ";
			bofw+=pair_term_freq.first+" "+ss_freq.str();
		}
	}

}


class Factory {

	public:
		AbstParser* parser;

		Factory(
			po::variables_map args,
			string page,
			ofstream *hdlr_out_bofw_file,
			bt::mutex *lock_out_bofw_file,
			unordered_map<string,string> *map_dict,
			bool is_japanese
		){

			if(is_japanese){

				parser = new JapParser(
					args,
					page,
					hdlr_out_bofw_file,
					lock_out_bofw_file,
					map_dict
				);

			}else{

				parser = new EngParser(
					args,
					page,
					hdlr_out_bofw_file,
					lock_out_bofw_file,
					map_dict
				);
			}
		};

		~Factory(){};

};

void run_worker(shared_ptr<Factory> worker){
	worker->parser->parse();
	worker->parser->save_to_file();
	ip::named_semaphore semaphore(ip::open_only_t(), semaphore_name);
	semaphore.post();
}

void read_dictionary(string in_dict_file, unordered_map<string,string> *map_dict){

	ifstream hInDictFile(in_dict_file);
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

	string out_bofw_file;
	bool is_japanese;

	po::options_description option("ParseWikipediaXML:");
	option.add_options()
		("help,h","Show help message.")
		("in_wiki_file,i",po::value<string>(),"Input WikipediaXML file.")
		("in_dict_file,d",po::value<string>(),"Input Dictionary file.")
		("out_bofw_file,s",po::value<string>(&out_bofw_file),"Output bag-of-words file.")
		("min_freq_of_term,c",po::value<int>()->default_value(2), "How many times a term "
		 "should appear in a document.")
		("min_terms_in_doc,m",po::value<int>()->default_value(1), "How many terms a document "
		 "should contain at least.")
		("max_terms_in_doc,x",po::value<int>()->default_value(65535), "How many terms a document "
		 "should contain at most.")
		("is_japanese,j","If the document is in Japanese.")
	;

	po::variables_map args;

	try {
		po::store(po::parse_command_line(argc,argv,option),args);
	} catch (exception &e) {
		cout << e.what() << endl;
	}
	po::notify(args);

	if(args.count("help")){
		cout << option << endl;
		return 0;
	}

	if(args.count("is_japanese")){
		 is_japanese = true;
	}else{
		 is_japanese = false;
	}

	bt::thread_group workers;
	// Number of concurrent threads supported.
	int num_max_workers = boost::thread::hardware_concurrency();

	ip::named_semaphore::remove(semaphore_name);
	ip::named_semaphore(ip::create_only_t(), semaphore_name, num_max_workers);
	ip::named_semaphore semaphore(ip::open_only_t(), semaphore_name);

	ifstream hdlr_in_wiki_file(args["in_wiki_file"].as<string>().c_str());
	if(!hdlr_in_wiki_file) return 1;

	ofstream hdlr_out_bofw_file(out_bofw_file);
	bt::mutex lock_out_bofw_file;

	unordered_map<string,string> map_dict;
	if(!is_japanese)
		read_dictionary(args["in_dict_file"].as<string>(),&map_dict);

	string line="", page="";
	bool is_inside_page=false,is_outside_page=false;
	while(hdlr_in_wiki_file && getline(hdlr_in_wiki_file,line)){
		if(string::npos!=line.find("<page>",0)) is_inside_page=true;
		if(string::npos!=line.find("</page>",0)) is_outside_page=true;
		if(is_inside_page) page += line;
		if(is_inside_page && is_outside_page){
			semaphore.wait();
			shared_ptr<Factory> worker =
				make_shared<Factory>(args,page,&hdlr_out_bofw_file,&lock_out_bofw_file,&map_dict,is_japanese);
			workers.create_thread(bt::bind(&run_worker,worker));
			page = "";
			is_inside_page=is_outside_page=false;
		}
	}
	workers.join_all();

	return 0;
}

