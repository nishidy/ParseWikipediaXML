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

boost::mutex mq;
boost::mutex mo;
condition c;
deque<string> q;
int f=0;

struct smisc{
	ostream* ofs;
	ostream* tofs;
};

unordered_map<string,string> dict;
vector<string>st;

void read_dict(string inFile2)
{
	ifstream ifs2("stopwords.txt");
	if(!ifs2){cout<<"no stopwords"<<endl;exit(1);}

	string line,w1,w2,w3;
	stringstream ss;

	getline(ifs2,line);
	algorithm::replace_all(line, ",", " ");
	ss<<line;
	while(!ss.eof()){
		ss>>w1;
		if(find(st.begin(),st.end(),w1)==st.end()){
			st.push_back(w1);
		}
		ss.ignore();
	}


	ifstream ifs(inFile2);
	if(!ifs) return;

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

bool DEBUG=false;

// R script may return the sequence of only symbols extracted by "サ変接続" noun.
bool check_all_symbols(string w){

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

	// XXX Not sure about the reason why regex_search fails...
	// According to the backtrace, L\000 should not be passed to this func.
	// I tried Boost-1.54.0 and 1.56.0 but neither of them worked.
	while(regex_search(wstart,wend,wmatch,wsym_reg)){
		len+=wmatch.str(0).size();
		// XXX Some of the wide character string may be converted
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
		if(DEBUG){
			cout<<"[Info: "<<w<<" is omitted since the length is same as "
			<<wcsn<<".]"<<endl;
		}
		return 1;
	}
	return 0;

}

string LANGUAGE="EN";
string convert_text_jp(string text,string title,int id){
	string res="";

	char tmp[64];
	sprintf(tmp,"/tmp/.%s%02d",__func__,id);
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

string convert_text(string text){
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

int MIN_WORDS;
int MAX_WORDS;
int MIN_WORD_CNT;
string count_words(string text,int* n){
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
		// XXX: bug!
		//*n++;
	}

	for(unordered_map<string,int>::iterator it=words.begin();\
		it!=words.end();++it){
		if(it->second<MIN_WORD_CNT) continue;
		ss.str("");
		ss.clear();
		ss<<it->second;
		res+=it->first+" "+ss.str()+" ";
		(*n)+=it->second;
	}
	if(res.size()>0) res.erase(--res.end());
 
	return res;
}

string count_words_jp(string text,int* n){
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
		if(it->second<MIN_WORD_CNT) continue;
		ss.str("");
		ss.clear();
		ss<<it->second;
		res+=it->first+" "+ss.str()+" ";
		(*n)+=it->second;
	}
	if(res.size()>0) res.erase(--res.end());
 
	return res;
}


string trim_text(string text){

	string text_wo_http;
	boost::regex http_reg("http://[^ ]+");
	text_wo_http = boost::regex_replace(text,http_reg," ");

	string rep;
	string wikisym_reps[16] = {"&lt","&quot","&gt","&amp"};
	unsigned int array_num = 4;
	for(unsigned int n=0;n<array_num;n++){
		rep = wikisym_reps[n];
		algorithm::replace_all(text_wo_http, rep, " ");
	}

	string nonchar_reps = "{}[](),.;:=_-+!?|/*`~^%#@/\\\"'<>";
	for(unsigned int n=0;n<nonchar_reps.length();n++){
		rep = nonchar_reps.at(n);
		algorithm::replace_all(text_wo_http, rep, " ");
	}

	string text_wo_spaces;
	boost::regex spaces_reg("[ ]{2,}");
	text_wo_spaces = boost::regex_replace(text_wo_http,spaces_reg," ");

	return text_wo_spaces;
}

/* Fix thread with a certain CPU. The argument id is just incremented. */
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

string CATEGORY;
int category_check(string text)
{
	if(CATEGORY=="") return 1;

	regex txt_reg("\\[\\[\\:*Category:([^\\[\\]]+)\\|*[^\\[\\]]*\\]\\]");
	smatch match;
	string::const_iterator start=text.begin();
	string::const_iterator end=text.end();
	regex cate_reg(CATEGORY);
	while(regex_search(start,end,match,txt_reg)){
		smatch cmatch;
		if(regex_search(match.str(1),cmatch,cate_reg)){
			if(DEBUG)
				cout<<"[Info: Category matched is "<<match.str(1)<<".]"<<endl;
			return 1;
		}
		start=match[0].second;
	}

	return 0;
}

int title_check(string text)
{
	regex txt_reg("Wikipedia:|Portal:|Template:|Category:");
	smatch match;
	if(regex_search(text,match,txt_reg)) return 0;

	return 1;
}

/* To filter text out */
void do_write(string page,struct smisc *misc,int id)
{
	string title;
	regex t_reg("<title>(.*)</title>");
	smatch match;
	if(regex_search(page,match,t_reg))
		title=match.str(1);
	if(!title_check(title)) return;

	algorithm::replace_all(title, " ", "_");

	if(title.size()==0){cout<<"No title"<<endl; return;}

	string text;
	regex txt_reg("<text[^>]*>(.*)</text>");
	if(regex_search(page,match,txt_reg))
		text=match.str(1);

	if(text.size()>0){
		if(!category_check(text)) return;

		if(LANGUAGE=="EN"){
			text=convert_text(text);
		}else{
			text=convert_text_jp(text,title,id);
		}
	}

	int n=0;
	if(text.size()>0){
		/* Show debug prints */
		if(LANGUAGE=="EN"){
			text=count_words(text,&n);
		}else{
			text=count_words_jp(text,&n);
		}
	}

	if(n<MIN_WORDS){
		if(DEBUG)
			cout<<"[Info: "<<MIN_WORDS<<" > "<<title<<": "<<n<<"]"<<endl;
		return ;
	}else if(n>MAX_WORDS){
		if(DEBUG)
			cout<<"[Info: "<<MAX_WORDS<<" < "<<title<<": "<<n<<"]"<<endl;
		return ;
	}else{
		cout<<title<<": "<<n<<endl;

	}

	{
		boost::mutex::scoped_lock lk(mo);
		*misc->tofs<<title<<endl;
		*misc->ofs<<text<<endl;
	}

}

/* This is the top function of functions to write the result into output files */
void write(struct smisc *misc,int id)
{

	if(!set_cpu_id(id)){return;}

	string page;
	while(1){
		page="";
		{
			boost::mutex::scoped_lock lk(mq);
			if(q.size()>0){
				page=q[0];
				q.pop_front();
				c.notify_one();
			}
		}
		if(page!="") do_write(page,misc,id);
		if(f) break;
	}

}

void enqueue(string page)
{
	{
		boost::mutex::scoped_lock lk(mq);
		if(q.size()>10000)
			c.wait(lk);
		q.push_back(page);
	}
	c.notify_one();
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
			" - debug message is shown by setting debug 1."<<endl;
		return 0;
	}

	string inFile,inFile2,outFile,outFile2;

	int argcnt=1;
	char arg;
	while(argcnt<argc){
		arg=argv[argcnt][1];
		switch(arg){
		case 'i':
			inFile=(string)argv[++argcnt];
			break;
		case 'd':
			inFile2=(string)argv[++argcnt];
			break;
		case 's':
			outFile=(string)argv[++argcnt];
			break;
		case 't':
			outFile2=(string)argv[++argcnt];
			break;
		case 'm':
			MIN_WORDS = atoi(argv[++argcnt]);
			break;
		case 'x':
			MAX_WORDS = atoi(argv[++argcnt]);
			break;
		case 'c':
			MIN_WORD_CNT = atoi(argv[++argcnt]);
			break;
		case 'g':
			CATEGORY=(string)argv[++argcnt];
			break;
		case 'v':
			++argcnt;
			DEBUG=true;
			break;
		case 'l':
			LANGUAGE=(string)argv[++argcnt];
			if(!(LANGUAGE=="EN"||LANGUAGE=="JP")){
				cerr<<"No such language supported."<<endl;
			}
			break;
		default:
			cerr<<"No such option: "<<argcnt<<" ("<<argcnt<<")"<<endl;
			exit(10);
			//break;
		}
		++argcnt;
	}


	ofstream ofs(outFile);
	ofstream tofs(outFile2);

	/* Read stopwords and dictionary to convert to original form */
	read_dict(inFile2);

	/* Create threads fetching text within <page> from queue */
	int cpus;
	cpus=thread::hardware_concurrency();
	//cpus--;

	struct smisc misc;
	misc.ofs=&ofs;
	misc.tofs=&tofs;

	thread_group g;
	if(cpus==1){
		g.create_thread(bind(&write,&misc,0));
	}else{
		for(int i=1;i<cpus;i++){
			g.create_thread(bind(&write,&misc,i));
		}
	}

	if(DEBUG)
		cout<<"[Info: "<<cpus<<" threads spawned.]"<<endl;

	string line="";
	string page="";
	int sflag=0,eflag=0,lnum=0;

	ifstream ifs(inFile);
	if(!ifs) return 0;

	/* Display three decimal places */
	cout<<setprecision(3);

	/* Put text within <page> onto queue */
	while(ifs&&getline(ifs,line)){
		if(string::npos!=line.find("<page>",0)) sflag=1;
		if(string::npos!=line.find("</page>",0)) eflag=1;
		if(sflag) page+=line;
		if(eflag){
			enqueue(page);
			page="";
			sflag=eflag=0;
		}
		lnum++;
	}

	if(DEBUG)
		cout<<"[Info: Finish reading "<<inFile<<".]"<<endl;

	while(q.size()>0){}

	if(DEBUG)
		cout<<"[Info: Queue is empty.]"<<endl;

	f=1;
	c.notify_all();

	g.join_all();

	return 1;
}

