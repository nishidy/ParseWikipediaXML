#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include <stdlib.h>
#include <string.h>

#include <deque>
#include <algorithm>
#include <unordered_map>
#include <vector>
using namespace std;

#include <boost/thread.hpp>
#include <boost/thread/condition.hpp>
#include <boost/bind.hpp>
#include <boost/regex.hpp>
#include <boost/algorithm/string.hpp>
using namespace boost;

#include <sched.h>
#include <error.h>
#include <sys/types.h> // gettid() (they do not provide wrapper func)

#include <unistd.h>
#include <sys/syscall.h>
#include <sys/types.h>


pid_t gettid(void)
{
    return syscall(SYS_gettid);
}

/* Fix thread with a certain CPU. */
int set_cpu_id(int id)
{
	cpu_set_t mask;
	CPU_ZERO(&mask);
	CPU_SET(id,&mask);

	pid_t p=gettid();
	if(sched_setaffinity(p,sizeof(mask),&mask)==-1){
		cout<<"Failed to set affinity:"<<id<<endl;
		if(errno==ESRCH)
			cout<<"No process or thread with the given ID found."<<endl;
		else if(errno==EFAULT)
			cout<<"The pointer cpuset is does not point to a valid object."<<endl;
		else if(errno==EINVAL)
			cout<<"The bitset is not valid."
				  "This might mean that the affinity set might not leave a processor"
				  " for the process or thread to run on."<<endl;

		return 0;
	}
	return 1;
}


string stopwords="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";


/*********************
 *
 * Params class
 *
 ********************/

class Params
{
	public:
	static bool debug;
	static string lang;
	static int min_words_doc;
	static int max_words_doc;
	static int min_word_cnt;
	static string category;
	static unsigned int cpus;
};

bool Params::debug;
string Params::lang;
int Params::min_words_doc;
int Params::max_words_doc;
int Params::min_word_cnt;
string Params::category;
unsigned int Params::cpus;


/*********************
 *
 * Common class
 *
 ********************/

class Common
{
	public:
	static condition* c;
	static bool finished;
};

condition* Common::c;
bool Common::finished;


/*********************
 *
 * Queue class
 *
 ********************/

class Queue : public Common, public Params
{
	private:
	deque<string> q;
	mutex mq; // mutex for queue push and pop

	public:
	static ifstream *ifs;

	Queue(){};
	Queue(condition* c){this->c=c;};
	void set_condition(condition* c){this->c=c;};

	void run();
	void push(string page);
	string pop();
	void finish();

	void set_final_flag(){finished=true;};

};

ifstream* Queue::ifs;

void Queue::run(){

	string line="", page="";
	int sflag=0,eflag=0,lnum=0;

	while(*ifs && getline(*ifs,line)){
		if(string::npos!=line.find("<page>",0)) sflag=1;
		if(string::npos!=line.find("</page>",0)) eflag=1;
		if(sflag) page+=line;
		if(eflag){
			push(page);
			page="";
			sflag=eflag=0;
		}
		lnum++;
	}
}

void Queue::push(string page)
{
	{
		mutex::scoped_lock lk(mq);

		// To avoid consuming much memory
		if(q.size()>100*cpus) c->wait(lk);
		q.push_back(page);
	}
	c->notify_one();
}

string Queue::pop(){
	string page;

	{
		mutex::scoped_lock lk(mq);
		if(q.size()>0){
			page=q.front();
			q.pop_front();
		}else{
			page="";
		}
		if(page=="") c->wait(lk);
	}
	c->notify_one();

	return page;
}

void Queue::finish(){

	if(Params::debug)
		cout<<"[Info: Finish reading.]"<<endl;

	while(q.size()>0){}

	if(Params::debug)
		cout<<"[Info: Queue is empty.]"<<endl;

	// FIXME: This notifies before thread goes into wait state
	// on this condition variable.
	c->notify_all();

	set_final_flag();
}


/*********************
 *
 * Threads class
 *
 ********************/

class Threads : public Common, public Params
{
	private:
	int id;

	public:
	static Queue* qobj;
	static mutex mf; //mutex for output to ostreams

	static vector<string> st; // stopwords
	static unordered_map<string,string> dict;

	static struct ostreams f;

	Threads(){id=0;};
	Threads(int id){this->id=id;};
	void set_id(int id){this->id=id;};

	void pop_n_parse();
	void parse_n_save(string);
	string parse_title(string);
	string parse_text(string,string);
	void save(string,string);

	/* Read stopwords and dictionary to convert
	 * to original form */
	static void read_stopwords();
	static void read_dict(string);

	string convert_text(string,string);
	string convert_text_en(string);
	string convert_text_jp(string,string);
	string count_words(string,int*);
	string count_words_en(string,int*);
	string count_words_jp(string,int*);

	bool category_check(string);
	bool title_check(string);

	// R script may return the sequence of only symbols extracted by "サ変接続" noun.
	bool check_all_symbols(string);

};

Queue* Threads::qobj;
mutex Threads::mf;
vector<string> Threads::st;
unordered_map<string,string> Threads::dict;

struct ostreams {
	ostream* s; // -s:sentence
	ostream* t; // -t:title
} Threads::f;


void Threads::pop_n_parse(){

	string page;

	for(;;){

		if((page=qobj->pop())!=""){
			parse_n_save(page);
		}

		if(finished) break;
	}
}

void Threads::parse_n_save(string page){
	string title=parse_title(page);
	string text=parse_text(title,page);

	if(text!="")
		save(title,text);
}

string Threads::parse_title(string page){

	regex t_reg("<title>(.*)</title>");
	smatch match;
	string title;

	if(regex_search(page,match,t_reg))
		title=match.str(1);
	if(!title_check(title)) return "";

	algorithm::replace_all(title, " ", "_");

	if(title.size()==0){
		cerr<<"No title found"<<endl;
		return "";
	}

	return title;
}

string Threads::parse_text(string title, string page){

	if(title.size()==0) return "";

	regex txt_reg("<text[^>]*>(.*)</text>");
	smatch match;
	string text;

	if(regex_search(page,match,txt_reg))
		text=match.str(1);

	if(text.size()>0){
		if(!category_check(text))
			return "";
	}else{
		if(Params::debug)
			cerr<<"[Info : No content in <text>. "<<title<<" ]"<<endl;
		return "";
	}

	text=convert_text(text,title);
	if(text.size()==0){
		if(Params::debug)
			cerr<<"[Info: No content after convert. "<<title<<" ]"<<endl;
		return "";
	}

	int n=0;
	text=count_words(text,&n);
	if(text.size()==0){
		if(Params::debug)
			cerr<<"[Info: No content after count. "<<title<<" ]"<<endl;
		return "";
	}

	if(n<min_words_doc){
		if(Params::debug){
			cout<<"[Info: "<<min_words_doc<<" > "<<
			title<<": "<<n<<"]"<<endl;
		}
		return "";
	}else if(n>max_words_doc){
		if(Params::debug){
			cout<<"[Info: "<<max_words_doc<<" < "<<
			title<<": "<<n<<"]"<<endl;
		}
		return "";
	}else{
		cout<<title<<": "<<n<<endl;
	}

	return text;
}

void Threads::save(string title, string text){
	{
		mutex::scoped_lock lk(mf);
		*f.t<<title<<endl;
		*f.s<<text<<endl;
	}
}

void Threads::read_stopwords(){
	string w;
	stringstream ss;

	algorithm::replace_all(stopwords, ",", " ");
	ss<<stopwords;
	while(!ss.eof()){
		ss>>w;
		if(find(st.begin(),st.end(),w)==st.end()){
			st.push_back(w);
		}
		ss.ignore();
	}

}

void Threads::read_dict(string dictionary){

	read_stopwords();

	ifstream ifs(dictionary);
	if(!ifs) return;

	string line,w1,w2,w3;
	stringstream ss;

	while(ifs&&getline(ifs,line)){
		if(line.at(0)==';') continue;
		ss.str(line);
		w1=w2=w3="";
		ss>>w1>>w2>>w3;
		ss.clear();

		if(find(st.begin(),st.end(),w1)!=st.end())
			continue;
		if(find(st.begin(),st.end(),w2)!=st.end())
			continue;
		if(w3=="Punct") continue;

		if(dict.find(w1)==dict.end()){
			dict[w1]=w2;
		}else{
			break;
		}
	}
}

string Threads::convert_text(string text,string title){
	if(Params::lang=="EN"){
		return convert_text_en(text);
	}else if(Params::lang=="JP"){
		return convert_text_jp(text,title);
	}else{
		return  "";
	}
}

string Threads::convert_text_en(string text){

	stringstream ss(text);
	string w,res="";

	regex t_reg("^[a-zA-Z0-9]+$");
	smatch match;

	while(!ss.eof()){
		ss>>w;
		if(w.size()==0) continue;
		if(w.at(w.size()-1)=='\n') w.erase(--w.end());

		if(find(st.begin(),st.end(),w)!=st.end()) continue;

		// Books -> books => book
		// Australian -> australian -> Australian => Asutralia
		w.at(0)=tolower(w.at(0));
		if(dict.find(w)!=dict.end()){
			res+=dict[w]+" ";
		}else{
			w.at(0)=toupper(w.at(0));
			if(dict.find(w)!=dict.end()){
				res+=dict[w]+" ";
				if(regex_search(w,match,t_reg)){
					res+=w+" ";
				}
			}
		}
	}
	if(res.size()>0) res.erase(--res.end());

	return res;
}

string Threads::convert_text_jp(string text,string title){

	string res="";

	char tmp[64];
	sprintf(tmp,"/tmp/.%s%02d",__func__,(int)gettid());
	ofstream ofs(tmp);
	ofs<<text;
	
	string command = "Rscript rmecabfreq.r "+(string)tmp+" \""+title+"\"";
	FILE *fp = popen((const char*)command.c_str(),"r");
	char buf[64];
	string w,cnt;

	stringstream ss;
	while(fgets(buf,sizeof(buf),fp) != NULL){

		if(strstr(buf,"=")!=0) continue;
		ss<<(string)buf;
		ss>>w>>cnt;
		
		//if(check_all_symbols(w)) continue;

		if(!isdigit(cnt.c_str()[0])) continue;
		if(find(st.begin(),st.end(),w)!=st.end()) continue;

		res+=w+" "+cnt+" ";
	}

	pclose(fp);
	return res;
}

string Threads::count_words(string text, int* n){
	if(Params::lang=="EN"){
		return count_words_en(text,n);
	}else if(Params::lang=="JP"){
		return count_words_jp(text,n);
	}else{
		return  "";
	}
}

string Threads::count_words_en(string text,int* n){

	unordered_map<string,int> words;
	stringstream ss(text);
	string w,res="";

	while(!ss.eof()){
		ss>>w;
		if(words.find(w)==words.end()){
			words[w]=1;
		}else{
			words[w]++;
		}
	}

	for(unordered_map<string,int>::iterator it=words.begin();\
		it!=words.end();++it){
		if(it->second<min_word_cnt) continue;
		ss.str("");
		ss.clear();
		ss<<it->second;
		res+=it->first+" "+ss.str()+" ";
		(*n)+=it->second;
	}
	if(res.size()>0) res.erase(--res.end());
 
	return res;
}

string Threads::count_words_jp(string text,int* n){

	unordered_map<string,int> words;
	stringstream ss(text);
	string w,res="";
	int cnt;

	while(!ss.eof()){
		ss>>w>>cnt;
		if(words.find(w)==words.end()){
			words[w]=cnt;
		}else{
			words[w]+=cnt;
		}
	}

	for(unordered_map<string,int>::iterator it=words.begin();\
		it!=words.end();++it){
		if(it->second<min_word_cnt) continue;
		ss.str("");
		ss.clear();
		ss<<it->second;
		res+=it->first+" "+ss.str()+" ";
		(*n)+=it->second;
	}
	if(res.size()>0) res.erase(--res.end());
 
	return res;
}

bool Threads::category_check(string text){

	if(Params::category=="") return 1;

	regex txt_reg("\\[\\[\\:*Category:([^\\[\\]]+)\\|*[^\\[\\]]*\\]\\]");
	smatch match;

	string::const_iterator start=text.begin();
	string::const_iterator end=text.end();

	regex cate_reg(Params::category);

	while(regex_search(start,end,match,txt_reg)){
		smatch cmatch;
		if(regex_search(match.str(1),cmatch,cate_reg)){
			if(Params::debug)
				cout<<"[Info: Category matched is "<<match.str(1)<<".]"<<endl;
			return 1;
		}
		start=match[0].second;
	}

	return 0;
}

bool Threads::title_check(string text){

	regex txt_reg("Wikipedia:|Portal:|Template:|Category:");
	smatch match;
	if(regex_search(text,match,txt_reg)) return 0;

	return 1;
}

// FIXME
bool Threads::check_all_symbols(string w){

	setlocale(LC_CTYPE,"ja_JP.utf8");

	int wcsn;
	wchar_t *wc = new wchar_t[w.size()+1];
	wcsn=mbstowcs(wc,w.c_str(),w.size()+1);
	if(wcsn<0){
		cout<<"[Warning: Failed to convert from multibyte string to wide string: "<<w<<endl;
		return 0;
	}

	wregex wsym_reg(L"[｛｝［］（）＼「」〜！＠＃＄％＾＆＊ーｰ”’：；｜※。、＿？ ]+");
	wsmatch wmatch;

	wstring::const_iterator wstart=((wstring)wc).begin();
	wstring::const_iterator wend=((wstring)wc).end();

	int len=0;

	// FIXME: Not sure about the reason why regex_search fails...
	// According to the backtrace, L\000 should not be passed to this func.
	// I tried Boost-1.54.0 and 1.56.0 but neither of them worked.
	while(regex_search(wstart,wend,wmatch,wsym_reg)){
		len+=wmatch.str(0).size();

		// Some of the wide character string may be converted
		// to one of wsym_reg!! Abort check.
		if(len>wcsn) return 1;

		wstart=wmatch[0].second;
	}

	regex sym_reg("[\\{\\}\\[\\]\\(\\)\\~\\!\\@\\#\\$\\%\\^\\&\\*\\-\"\'\\:\\;\\|\\.,_\\/\? ]+");
	smatch match;

	string::const_iterator start=w.begin();
	string::const_iterator end=w.end();

	while(regex_search(start,end,match,sym_reg)){
		len+=match.str(0).size();
		// XXX
		if(len>wcsn) return 1;
		start=match[0].second;
	}

	if(len==wcsn){
		if(Params::debug){
			cout<<"[Info: "<<w<<" is omitted since the length is same as "
			<<wcsn<<".]"<<endl;
		}
		return 1;
	}
	
	return 0;
}


void run(int id){
	Threads thread(id);
	if(!set_cpu_id(id)){return;}
	thread.pop_n_parse();
}


int main(int argc, char* argv[])
{
	/* Read inputs and outputs */
	if(argc<9){
		cout<<"Usage:"<<argv[0]<<" -i File(Wikipedia) [-d File(dictionary)] "
			"-s File(Sentence) -t File(Title) -m min_words -x max_words "
			"-c min_word_count -g category [-v debug] -l [JP|EN] \n"
			"Note:\n"
			" - category is regular expression.\n"
			" - debug message is shown by setting -v."<<endl;
		return 0;
	}


	ifstream ifs;
	ofstream ofs,oft;
	int result;
	string dictfile;

	while((result=getopt(argc,argv,"i:d:s:t:m:x:c:g:vl:"))!=-1){

		switch(result){
		case 'i':
			ifs.open((string)optarg);
			if(!ifs){
				cerr<<"Invalid file "<<(string)optarg<<endl;
				return 99;
			}
			break;

		case 'd':
			dictfile=(string)optarg;
			break;

		case 's':
			ofs.open((string)optarg);
			if(!ofs){
				cerr<<"Invalid files "<<(string)optarg<<endl;
				return 99;
			}
			break;

		case 't':
			oft.open((string)optarg);
			if(!oft){
				cerr<<"Invalid files "<<(string)optarg<<endl;
				return 99;
			}
			break;

		case 'm':
			Params::min_words_doc = atoi(optarg);
			break;

		case 'x':
			Params::max_words_doc = atoi(optarg);
			break;

		case 'c':
			Params::min_word_cnt = atoi(optarg);
			break;

		case 'g':
			Params::category=(string)optarg;
			break;

		case 'v':
			Params::debug=true;
			break;

		case 'l':
			Params::lang=(string)optarg;
			if(!(Params::lang=="EN"||Params::lang=="JP")){
				cerr<<"No such language supported."<<endl;
			}
			break;

		default:
			cerr<<"No such option: "<<result<<"."<<endl;
			exit(10);
			break;
			
		}
	}

	Common::finished=false;

	condition c;

	Queue::ifs=&ifs;
	Queue queue(&c);

	Threads::f.s=&ofs;
	Threads::f.t=&oft;
	Threads::read_dict(dictfile);
	Threads::qobj=&queue;
	Threads::c=&c;

	unsigned int cpus;
	cpus=thread::hardware_concurrency();
	Params::cpus=cpus;

	thread_group g;
	if(cpus==1){
		g.create_thread(bind(&run,0));
	}else{
		for(unsigned int i=0;i<cpus-1;i++){
			g.create_thread(bind(&run,i));
		}
	}

	if(Params::debug)
		cout<<"[Info: "<<cpus<<" threads spawned.]"<<endl;

	queue.run();

	queue.finish();
	g.join_all();

	return 0;
}

