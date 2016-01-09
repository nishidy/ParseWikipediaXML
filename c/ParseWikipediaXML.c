#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <pthread.h>
#include "ParseWikipediaXML.h"

static char verbose;
static ui queue_first= 0;
static ui queue_last = 0;
static char *queue[QSIZE];

static pthread_mutex_t qmutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t fmutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t omutex = PTHREAD_MUTEX_INITIALIZER;

static pthread_cond_t push_cond = PTHREAD_COND_INITIALIZER;
static pthread_cond_t pop_cond  = PTHREAD_COND_INITIALIZER;

static ui page_saved = 0;
static ui page_parsed= 0;


void registTags(prog_args *args, const char *target, const char *content, const char *title){
    strncpy(args->tags.target,target,32);
    strncpy(args->tags.content,content,32);
    strncpy(args->tags.title,title,32);
}

void startJoinThreads(prog_args *args, Stopwords *stopwords, dict_info *dict){

    thread_args *targs = setThreadArgs(args, stopwords, dict);

    allocMemQueue();

    pthread_t *threads[QSIZE];

    for(ui i=0;i<args->workers;i++){
        pthread_t *th = (pthread_t*)malloc(sizeof(pthread_t));
        if(th==NULL) fprintf(stderr,"malloc failed\n");
        pthread_create(th, NULL, parse_thread, targs);
        threads[i] = th;
    }

    struct timeval s,f;
    gettimeofday(&s,NULL);

    readDatabase(targs->fpi, args->tags.target);

    for(ui i=0;i<args->workers;i++){
        queue_push("::FINISHED::");
    }

    for(ui i=0;i<args->workers;i++){
        pthread_join(*threads[i], NULL);
    }

    gettimeofday(&f,NULL);
    printf(" > Read database in %.2f sec.\n",((float)(f.tv_sec-s.tv_sec)+(f.tv_usec-s.tv_usec)/1000000.0));

    fclose(targs->fpi);
    fclose(targs->fpo);
}

thread_args* setThreadArgs(prog_args *args, Stopwords *stopwords, dict_info *dict){

    thread_args *targs = (thread_args*)malloc(sizeof(thread_args));

    FILE *fpi = (FILE*)malloc(sizeof(FILE));
    if(fpi==NULL) fprintf(stderr,"malloc failed\n");

    FILE *fpo = (FILE*)malloc(sizeof(FILE));
    if(fpo==NULL) fprintf(stderr,"malloc failed\n");

    fpi = fopen(args->inDbFile,"r");

    if(strlen(args->outBofwFile)>0){
        fpo = fopen(args->outBofwFile,"w");
    }

    targs->fpi = fpi;
    targs->fpo = fpo;
    targs->stopwords = (char*)stopwords->s;
    targs->stopwords_num = stopwords->n;
    targs->dictionary = (Dictionary*)dict->dictionary;
    targs->dictionary_num = (ui*)dict->dictionary_num;
	targs->content = args->tags.content;
	targs->title = args->tags.title;

    return targs;

}


prog_args* set_args(int argc, char* argv[]){

    prog_args *args = (prog_args*)malloc(sizeof(prog_args));
    if(args==NULL) fprintf(stderr,"malloc failed\n");
    memset(args,'\0',sizeof(prog_args));

    argc--;
    argv++;

    char r = 0;

    for(int i=0;i<argc;i++){
        if(i%2==r){
            if(strlen(argv[i])>2){
                printf("Wrong option (%s).\n",argv[i]);
                exit(0);
            }
            char c = argv[i][1];
            switch (c) {
                case 'i':
                    strncpy(args->inDbFile, argv[i+1], FNAMELEN);
                    break;
                case 'd':
                    strncpy(args->inDictFile, argv[i+1], FNAMELEN);
                    break;
                case 's':
                    strncpy(args->outBofwFile, argv[i+1], FNAMELEN);
                    break;
                case 'w':
                    args->workers = atoi(argv[i+1]);
                    break;
                case 'v':
                    verbose = 1;
                    r^=1;
                    break;
                default:
                    break;
            }
        } else {
        }
    }

    if(strlen(args->inDbFile)==0){
        printf("Need to give input file.\n");
        exit(1);
    }

    return args;
}

int getIndexOfTerm(Bofw *bofw, ui cnt_bofw, char *term){
    for(ui i=0;i<cnt_bofw;i++){
        if(strncmp(bofw[i].term,term,strlen(term))==0){
            return i;
        }
    }
    return -1;
}

void save_file(Bofw *bofw, ui cnt_bofw, FILE* fpo){
    for(ui i=0;i<cnt_bofw;i++){
        if(i>0)  fprintf(fpo," ");
        fprintf(fpo, "%s %d",bofw[i].term,bofw[i].freq);
    }
    fprintf(fpo,"\n");
}

void disp_stdio(Bofw *bofw, ui cnt_bofw){
    for(ui i=0;i<cnt_bofw;i++){
        if(i>0)  printf(" ");
        printf("%s %d",bofw[i].term,bofw[i].freq);
    }
    printf("\n");
}

void save(Bofw *bofw, ui cnt_bofw, FILE* fpo){
    pthread_mutex_lock(&fmutex);
    if(fpo==NULL){
        disp_stdio(bofw,cnt_bofw);
        fflush(stdout);
    }else{
        save_file(bofw,cnt_bofw,fpo);
        fflush(fpo);
    }
    pthread_mutex_unlock(&fmutex);
}

int sort_bofw(const void *a, const void *b){
    if(((Bofw*)a)->freq > ((Bofw*)b)->freq){
        return -1;
    }else if(((Bofw*)a)->freq < ((Bofw*)b)->freq){
        return 1;
    }else{

        ui alen = strlen(((Bofw*)a)->term);
        ui blen = strlen(((Bofw*)b)->term);
        ui minlen=0;
        minlen = alen<blen ? alen : blen;

        for(ui i=0;i<minlen;i++){
            if(((Bofw*)a)->term[i] > ((Bofw*)b)->term[i]){
                return 1;
            }else if(((Bofw*)a)->term[i] < ((Bofw*)b)->term[i]){
                return -1;
            }
        }

        return alen<blen ? 1 : -1;
    }
}

void strcaterr(){
    fprintf(stderr,"strcat does not return the valid pointer.\n");
    exit(1);
}

void reallocerr(char *p){
    if(p==NULL){
        fprintf(stderr,"Realloc could not allocate memory.");
        exit(15);
    }else{
        if(verbose==1) printf("Realloc'd. The size is %lu.\n",strlen(p));
    }
}

char* getElementText(char *page, char *tag){
    char open_tag[32] = {0};
    char close_tag[32] = {0};

    sprintf(open_tag,"<%s",tag);
    sprintf(close_tag,"</%s",tag);

    if(strstr(page, open_tag)==NULL){
        fprintf(stderr,"Not found : %s\n",page);
        return NULL;
    }
    char *open_pos = strstr(strstr(page, open_tag),">")+1;
    char *close_pos= strstr(page, close_tag)-1;

    ui text_len = (close_pos-open_pos)/sizeof(char);

    ui need_size = sizeof(char) * (LSIZE * (text_len/LSIZE+1) + 1);
    char *text = (char*)malloc(need_size);
    if(text==NULL) exit(21);
    memset(text,'\0',need_size);

    if(open_pos!=NULL && close_pos!=NULL){
        strncpy(text, open_pos, text_len);
    }else{
        fprintf(stderr, "Open tag : %p / Close tag : %p\n", open_pos, close_pos);
        text = NULL;
    }

    return text;
}

void append_text(char **text, char *l){
    if((strlen(*text)%LSIZE)+strlen(l)>=LSIZE){
        ui need_size = sizeof(char) * LSIZE * (strlen(*text)/LSIZE+2);
        *text = (char*)realloc(*text, need_size);
        reallocerr(*text);
        memset(*text+(need_size-LSIZE),'\0',LSIZE);
    }
    if(strcat(*text,l)!=*text) strcaterr();
}

char* cbElementTextRaw(FILE* fp, char *tag){

    char l[LSIZE] = {0};
    char open_tag[32] = {0};
    char close_tag[32] = {0};

    sprintf(open_tag,"<%s",tag);
    sprintf(close_tag,"</%s",tag);

    char tag_flag = 0;
    char *text = (char*)malloc(sizeof(char)*LSIZE);
    if(text==NULL) exit(21);
    memset(text,'\0',sizeof(char)*LSIZE);

    while(fgets(l,LSIZE,fp)){
        if(strstr(l,open_tag)!=NULL){
            tag_flag = 1;
        }
        if(tag_flag == 1){
            append_text(&text,l);
        }
        if(strstr(l,close_tag)!=NULL){
            tag_flag = 2;
            break;
        }
    }

    if(tag_flag==2){
        return text;
    }else{
        return NULL;
    }
}

char* cbElementText(char *tag){
    return "";
}

void toLower(char *term){
    for(ui i=0;i<strlen(term);i++){
        term[i] = tolower(term[i]);
    }
}

void allocMemDictionary(ui n, Dictionary **d){

    if(n==0){
        *d = (Dictionary*)malloc(sizeof(Dictionary)*SSIZE);
        if(*d==NULL) exit(25);
        memset(*d,'\0',sizeof(Dictionary)*SSIZE);
    }
    if(SSIZE==n){
        *d = (Dictionary*)realloc(*d,sizeof(Dictionary)*MSIZE);
        if(*d==NULL) exit(26);
        memset(*d+SSIZE,'\0',sizeof(Dictionary)*(MSIZE-SSIZE));
    }
    if(MSIZE==n){
        *d = (Dictionary*)realloc(*d,sizeof(Dictionary)*LSIZE);
        if(*d==NULL) exit(27);
        memset(*d+MSIZE,'\0',sizeof(Dictionary)*(LSIZE-MSIZE));
    }
    if(LSIZE==n){
        *d = (Dictionary*)realloc(*d,sizeof(Dictionary)*LSIZE*2);
        if(*d==NULL) exit(27);
        memset(*d+LSIZE,'\0',sizeof(Dictionary)*(LSIZE));
    }

}

dict_info* readDictionary(prog_args *args){

    dict_info *dict = (dict_info*)malloc(sizeof(dict_info));
    if(dict==NULL) fprintf(stderr,"malloc failed\n");
    memset(dict,'\0',sizeof(dict_info));

    FILE *fpd = NULL;
    if(strlen(args->inDictFile)>0){
        fpd = fopen(args->inDictFile,"r");
    }

    struct timeval s,f;
    gettimeofday(&s,NULL);
    getDictionary(fpd,dict);
    gettimeofday(&f,NULL);
    printf(" > Read dictionary in %.2f sec.\n",((float)(f.tv_sec-s.tv_sec)+(f.tv_usec-s.tv_usec)/1000000.0));

    fclose(fpd);

    return dict;
}

void getDictionary(FILE *fp, dict_info *dict){

    char l[256] = {0};
    int cnt_dict = 0;
    char m[256] = "Read dictionary";
    ui lc=0,pc=0;

    while(fgets(l,256,fp)){
        if(strlen(l)>0 && l[0]==';') continue;

        char infl[TERMLEN] = {0};
        char base[TERMLEN] = {0};

        Dictionary *d = NULL;
        ui *n = NULL;
        ui i=0;

        char *c = strtok(l,"\t");
        while(c!=NULL && i<2){
            if(i==0){
                if(strlen(c)==1) break;
                ui t0 = (ui)c[0];
                ui t1 = (ui)c[1];
                if(ASCII<=t0 && t0<ASCII+26){
                    if(ASCII<=t1 && t1<ASCII+26){
                        n = &dict->dictionary_num[t0-ASCII][t1-ASCII];
                        allocMemDictionary(*n, &dict->dictionary[t0-ASCII][t1-ASCII]);
                        d = &dict->dictionary[t0-ASCII][t1-ASCII][(*n)];
                        strncpy(infl,c,strlen(c)-1); // remove the last space
                    }
                }
            }
            if(i==1){
                if(d!=NULL && n!=NULL){
                    strcpy(base,c);
                    if(strchr(base,' ')==NULL){
                        if(strcmp(infl,base)!=0){
                            strcpy(d->infl,infl);
                            strcpy(d->base,base);
                            (*n)++;
                            printf(" > %s [#word(loaded/parsed) %d/%d]\r",m,++lc,++pc);
                        }else{
                            printf(" > %s [#word(loaded/parsed) %d/%d]\r",m,lc,++pc);
                        }
                        fflush(stdout);
                        d = NULL;
                        n = NULL;
                    }
                }
            }

            c = strtok(NULL,"\t");
            i++;
        }
        cnt_dict++;
    }
    printf(" > %s [#word(loaded/parsed) %d/%d]\n",m,lc,++pc);
}

void toBaseform(char *term, Dictionary *dictionary[27][27], ui dictionary_num[27][27]){
    ui t0 = (ui)term[0];
    ui t1 = (ui)term[1];

    if(ASCII<=t0 && t0<ASCII+26){
        if(ASCII<=t1 && t1<ASCII+26){
            Dictionary *d = dictionary[t0-ASCII][t1-ASCII];
            for(ui i=0;i<dictionary_num[t0-ASCII][t1-ASCII];i++){
                if(strcmp(term,d[i].infl)==0){
                    strcpy(term,d[i].base);
                    break;
                }
            }
        }
    }
}

Stopwords* getStopwords(){

    Stopwords *stopwords = (Stopwords*)malloc(sizeof(Stopwords));

    char stopwords_base[1024] = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

    char *c = strtok(stopwords_base,",");
    while(c != NULL){
        strcpy(*(stopwords->s+stopwords->n),c);
        ++stopwords->n;
        c = strtok(NULL,",");
    }

    return stopwords;
}

int isStopword(char *term, char stopwords[][16], ui stopwords_num){
    for(ui i=0;i<stopwords_num;i++){
        if(strcmp(term,stopwords[i])==0) return 1;
    }
    return 0;
}

int isValidTerm(char *term){

    if(strlen(term)<2) return 0;

    for(ui i=0;i<strlen(term);i++){
        if(i==0){
            if(!( isalpha(term[i]) )) return 0;
        }else if(i==strlen(term)-1){
            if(!( isalnum(term[i]) )) return 0;
        }else{
            if(!( isalnum(term[i]) ||
                  term[i]=='\''    ||
                  term[i]=='-'
            )) return 0;
        }
    }

    return 1;
}

void allocMemQueue(){
    for(ui i=0;i<QSIZE;i++){
        queue[i] = (char*)malloc(sizeof(char)*QDATASIZE);
        if(queue[i]==NULL) exit(21);
        memset(queue[i],'\0',sizeof(char)*QDATASIZE);
    }
}

char* queue_pop(){

    pthread_mutex_lock(&qmutex);
    while(queue_first==queue_last){
        pthread_cond_wait(&push_cond, &qmutex);
    }
    pthread_mutex_unlock(&qmutex);


    char *page;
    pthread_mutex_lock(&qmutex);
    {
        page = (char*)malloc(sizeof(char) * (strlen(queue[queue_first]) + 1));
        if(page==NULL) exit(21);

        if(verbose==1) printf("[POP] Queue prev pos: %d->%d\n",queue_first,queue_last);

        // TODO: Remove strcpy to use memory address assgined into queue
        //       in order to avoid copying data as much as possible
        strcpy(page, queue[queue_first++]);
        if(queue_first>QSIZE-1) queue_first=0;

        if(verbose==1) printf("[POP] Queue post pos: %d->%d\n",queue_first,queue_last);

        pthread_cond_signal(&pop_cond);
    }
    pthread_mutex_unlock(&qmutex);

    return page;
}

void stretch_qdatasize(ui block_size, char **queue_pos){
    ui need_size = sizeof(char) * QDATASIZE * block_size;
    *queue_pos = (char*)realloc(*queue_pos, need_size);
    reallocerr(*queue_pos);
    memset(*queue_pos,'\0',need_size);
}

void copy_to_queue(char **queue_pos, char *page){
    ui new_block_size = strlen(page)/QDATASIZE + 1;
    ui old_block_size = strlen(*queue_pos)/QDATASIZE + 1;

    if(old_block_size < new_block_size){
        if(verbose==1)
            printf("Realloc'd with block size %d to %d.\n",old_block_size,new_block_size);
        stretch_qdatasize(new_block_size, queue_pos);
    }else if(old_block_size > new_block_size){
        if(verbose==1)
            printf("Realloc'd with block size %d to %d.\n",old_block_size,new_block_size);
        stretch_qdatasize(old_block_size, queue_pos);
    }

    strcpy(*queue_pos, page);
}

void queue_push(char* page){

    pthread_mutex_lock(&qmutex);
    while(queue_first==0 && queue_last==QSIZE-1){
        pthread_cond_wait(&pop_cond, &qmutex);
    }
    pthread_mutex_unlock(&qmutex);

    pthread_mutex_lock(&qmutex);
    while(queue_first>0 && queue_last==queue_first-1){
        pthread_cond_wait(&pop_cond, &qmutex);
    }
    pthread_mutex_unlock(&qmutex);

    pthread_mutex_lock(&qmutex);
    {
        if(verbose==1) printf("[PUSH] Queue prev pos: %d->%d\n",queue_first,queue_last);

        copy_to_queue(&queue[queue_last++],page);
        if(queue_last>QSIZE-1) queue_last = 0;

        if(verbose==1) printf("[PUSH] Queue post pos: %d->%d\n",queue_first,queue_last);

        pthread_cond_signal(&push_cond);
    }
    pthread_mutex_unlock(&qmutex);

}

void readDatabase(FILE *fpi, char *target){
    char *page_raw;
    while( (page_raw=cbElementTextRaw(fpi,target))!=NULL ){
        queue_push(page_raw);
        free(page_raw);
    }
}

void run_parse(char* page_raw, thread_args *targs){

    char *text_raw = getElementText(page_raw, targs->content);
    if(text_raw==NULL){
        printf("\ntext is NULL. %s\n",page_raw);
        return;
    }
    if(verbose==1) printf("%d %p\n",(int)pthread_self(),text_raw);

    if(verbose==1){
        printf("[%d]page:%p-%p\n",(int)pthread_self(),page_raw,page_raw+strlen(page_raw)+1);
        printf("[%d]text:%p-%p\n",(int)pthread_self(),text_raw,text_raw+strlen(text_raw)+1);
    }

    Bofw *bofw;
    char *term, *savep;
    term = strtok_r(text_raw, " .,;\n", &savep);

    bofw = (Bofw*)malloc(sizeof(Bofw)*LSIZE);
    if(bofw==NULL) return;
    memset(bofw,'\0',sizeof(Bofw)*LSIZE);
    ui cnt_bofw = 0;

    char m[256] = "Read database";

    while(term != NULL){
        toLower(term);
        if( isValidTerm(term) > 0 &&
            isStopword(term, (char(*)[16])targs->stopwords, targs->stopwords_num) == 0
        ){
            toBaseform(
                term,
                (Dictionary*(*)[27])targs->dictionary,
                (ui(*)[27])targs->dictionary_num
            );

            int idx_bofw = getIndexOfTerm(bofw, cnt_bofw, term);
            if(idx_bofw>-1){
                bofw[idx_bofw].freq++;
            }else{
                strncpy(bofw[cnt_bofw].term, term, strlen(term));
                bofw[cnt_bofw].freq = 1;
                cnt_bofw++;
            }
        }
        if(cnt_bofw>LSIZE){ fprintf(stderr,"Bofw size over."); exit(11); }
        term = strtok_r(NULL, " .,;\n", &savep);
    }

    if(cnt_bofw>0){
        qsort((void *)bofw, cnt_bofw, sizeof(Bofw), sort_bofw);
        save(bofw, cnt_bofw, targs->fpo);
        pthread_mutex_lock(&omutex);
        {
            printf(" > %s [#page(saved/parsed) %d/%d]\r",m,++page_saved,++page_parsed);
        }
        pthread_mutex_unlock(&omutex);
    }else{
        pthread_mutex_lock(&omutex);
        {
            printf(" > %s [#page(saved/parsed) %d/%d]\r",m,page_saved,++page_parsed);
        }
        pthread_mutex_unlock(&omutex);
    }

    free(text_raw);
    free(bofw);

}

void* parse_thread(void* args){
    thread_args *targs = args;

    for(;;){
        char *page_raw = queue_pop();
        if(strcmp(page_raw,"::FINISHED::")==0) break;
        run_parse(page_raw, targs);
        free(page_raw);
    }

    return NULL;
}

