#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <pthread.h>

#define LSIZE 65535
#define QDATASIZE 65535
#define QSIZE 32
#define ASCII 97
#define TERMLEN 48

typedef unsigned int ui;

typedef struct {
    char term[48];
    int  freq;
} Bofw;

typedef struct {
    char infl[TERMLEN]; // inflected
    char base[TERMLEN]; // baseform
} Dictionary;

char verbose = 0;

ui queue_first= 0;
ui queue_last = 0;
char *queue[QSIZE];

typedef struct {
    FILE* fpi;
    FILE* fpo;
    char *stopwords;
    ui stopwords_num;
    Dictionary *dictionary;
    ui *dictionary_num;
} thread_args;

pthread_mutex_t qmutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t fmutex = PTHREAD_MUTEX_INITIALIZER;

pthread_cond_t push_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t pop_cond  = PTHREAD_COND_INITIALIZER;

///////////////////////////////////////////////////////

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
        if(verbose==1) printf("Realloc'd.\n");
    }
}

char* getElementText(char *page, char *tag){
    char open_tag[32] = {0};
    char close_tag[32] = {0};

    sprintf(open_tag,"<%s",tag);
    sprintf(close_tag,"</%s",tag);

    if(strstr(page, open_tag)==NULL){
        fprintf(stderr,"Not found : %s\n",page);
        exit(20);
    }
    char *open_pos = strstr(strstr(page, open_tag),">")+1;
    char *close_pos= strstr(page, close_tag)-1;

    ui text_len = (close_pos-open_pos)/sizeof(char);

    ui need_size = sizeof(char) * LSIZE * (text_len/LSIZE+1);
    char *text = (char*)malloc(need_size+1);
    if(text==NULL) exit(21);
    memset(text,'\0',need_size+1);

    if(open_pos!=NULL && close_pos!=NULL){
        strncpy(text, open_pos, text_len);
    }else{
        text = NULL;
    }

    return text;
}

void append_text(char *text, char *l){
    if((strlen(text)%LSIZE)+strlen(l)>LSIZE){
        ui need_size = sizeof(char) * LSIZE * (strlen(text)/LSIZE+2);
        text = (char*)realloc(text, need_size);
        reallocerr(text);
        memset(&text[need_size-LSIZE],'\0',LSIZE);
    }
    if(strcat(text,l)!=text) strcaterr();
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
            append_text(text,l);
        }
        if(strstr(l,close_tag)!=NULL){
            append_text(text,l);
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

void readDictionary(FILE *fp, Dictionary *dictionary[27][27], ui dictionary_num[27][27]){

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
                if(ASCII<=(ui)c[0] && (ui)c[0]<ASCII+26){
                    if(ASCII<=(ui)c[1] && (ui)c[1]<ASCII+26){
                        n = &dictionary_num[(ui)c[0]-ASCII][(ui)c[1]-ASCII];
                        d = &dictionary[(ui)c[0]-ASCII][(ui)c[1]-ASCII][(*n)+1];
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

void getStopwords(char stopwords[][16], ui* stopwords_num){

    char stopwords_base[1024] = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";

    char *c = strtok(stopwords_base,",");
    while(c != NULL){
        strcpy(*stopwords,c);
        stopwords++;
        (*stopwords_num)++;
        c = strtok(NULL,",");
    }
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

    char *page = (char*)malloc(sizeof(char) * strlen(queue[queue_first]));
    if(page==NULL) exit(21);

    pthread_mutex_lock(&qmutex);
    {
        if(verbose==1) printf("[POP] Queue prev pos: %d->%d\n",queue_first,queue_last);

        // TODO: Remove strcpy to use memory address assgined into queue
        //       in order to reduce times of copying data
        strcpy(page, queue[queue_first++]);
        if(queue_first>QSIZE-1) queue_first=0;

        if(verbose==1) printf("[POP] Queue post pos: %d->%d\n",queue_first,queue_last);

        pthread_cond_signal(&pop_cond);
    }
    pthread_mutex_unlock(&qmutex);

    return page;
}

void stretch_qdatasize(ui block_size, char *queue_pos){
    ui need_size = sizeof(char) * QDATASIZE * block_size;
    queue_pos = (char*)realloc(queue_pos, need_size);
    reallocerr(queue_pos);
    memset(queue_pos,'\0',need_size);
}

void copy_to_queue(char *page, char *queue_pos){
    ui new_block_size = strlen(page)/QDATASIZE + 1;
    ui old_block_size = strlen(queue_pos)/QDATASIZE + 1;

    if(old_block_size < new_block_size){
        stretch_qdatasize(new_block_size, queue_pos);
    }else if(old_block_size > new_block_size){
        stretch_qdatasize(old_block_size, queue_pos);
    }

    strcpy(queue_pos, page);
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

        copy_to_queue(page,queue[queue_last]);
        if(++queue_last>QSIZE-1) queue_last = 0;

        if(verbose==1) printf("[PUSH] Queue post pos: %d->%d\n",queue_first,queue_last);

        pthread_cond_signal(&push_cond);
    }
    pthread_mutex_unlock(&qmutex);

}

void readDatabase(FILE *fpi){
    char *page_raw;
    while( (page_raw=cbElementTextRaw(fpi,"page"))!=NULL ){
        queue_push(page_raw);
        free(page_raw);
    }
}

void run_parse(char* page_raw, thread_args *targs){

    char *text_raw = getElementText(page_raw, "text");
    if(text_raw==NULL){
        printf("text is NULL\n");
        return;
    }
    if(verbose==1) printf("%d %p\n",(int)pthread_self(),text_raw);

    char *term;
    term = strtok(text_raw, " .,;\n");

    Bofw *bofw;
    bofw = (Bofw*)malloc(sizeof(Bofw)*LSIZE);
    if(bofw==NULL) return;
    memset(bofw,'\0',sizeof(Bofw)*LSIZE);
    ui cnt_bofw = 0;

    char m[256] = "Read database";
    ui lc=0,pc=0;

    while(term != NULL){
        toLower(term);
        if( isValidTerm(term) > 0 &&
            isStopword( term, (char(*)[16])targs->stopwords, targs->stopwords_num ) == 0 )
        {
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
        term = strtok(NULL," .,;\n");
    }

    if(cnt_bofw>0){
        qsort((void *)bofw, cnt_bofw, sizeof(Bofw), sort_bofw);
        save(bofw, cnt_bofw, targs->fpo);
        printf(" > %s [#page(saved/parsed) %d/%d]\r",m,++lc,++pc);
    }else{
        printf(" > %s [#page(saved/parsed) %d/%d]\r",m,lc,++pc);
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


void allocMemDictionary(Dictionary *dictionary[27][27]){
    for(ui i=0;i<27;i++){
        for(ui j=0;j<27;j++){
            dictionary[i][j] = (Dictionary*)malloc(sizeof(Dictionary)*LSIZE);
            if(dictionary[i][j]==NULL) exit(22);
            memset(dictionary[i][j],'\0',sizeof(Dictionary)*LSIZE);
            if(dictionary[i][j]==NULL) exit(10);
        }
    }
}

int main(int argc, char* argv[]){

    char inDbFile[64] = {0};
    char inDictFile[64] = {0};
    char outBofwFile[64] = {0};
    ui workers = 1;
    char r = 0;

    argc--;
    argv++;

    for(int i=0;i<argc;i++){
        if(i%2==r){
            if(strlen(argv[i])>2){
                printf("Wrong option (%s).\n",argv[i]);
                exit(0);
            }
            char c = argv[i][1];
            switch (c) {
                case 'i':
                    strncpy(inDbFile, argv[i+1], strlen(argv[i+1]));
                    break;
                case 'd':
                    strncpy(inDictFile, argv[i+1], strlen(argv[i+1]));
                    break;
                case 's':
                    strncpy(outBofwFile, argv[i+1], strlen(argv[i+1]));
                    break;
                case 'w':
                    workers = atoi(argv[i+1]);
                    break;
                case 'v':
                    verbose = 1;
                    r^=r;
                    break;
                default:
                    break;
            }
        } else {
        }
    }

    if(strlen(inDbFile)==0){
        printf("Need to give input file.\n");
        exit(1);
    }

    FILE *fpi, *fpd = NULL, *fpo = NULL;
    fpi = fopen(inDbFile,"r");
    if(strlen(inDictFile)>0){
        fpd = fopen(inDictFile,"r");
    }
    if(strlen(outBofwFile)>0){
        fpo = fopen(outBofwFile,"w");
    }

    char stopwords[1024][16] = {{0}};
    ui stopwords_num = 0;
    getStopwords(stopwords, &stopwords_num);

    Dictionary *dictionary[27][27];
    allocMemDictionary(dictionary);
    ui dictionary_num[27][27] = {{0}};

    clock_t s, f;

    s = clock();
    readDictionary(fpd, dictionary, dictionary_num);
    f = clock();
    printf(" > Read dictionary in %.2f sec.\n",((float)(f-s))/1000000.0);

    allocMemQueue();

    thread_args targs;
    targs.fpi = fpi;
    targs.fpo = fpo;
    targs.stopwords = (char*)stopwords;
    targs.stopwords_num = stopwords_num;
    targs.dictionary = (Dictionary*)dictionary;
    targs.dictionary_num = (ui*)dictionary_num;

    pthread_t *threads;
    threads = (pthread_t*)malloc(sizeof(pthread_t)*workers);
    if(threads==NULL) return -1;

    for(ui i=0;i<workers;i++){
        pthread_t th;
        pthread_create(&th, NULL, parse_thread, &targs);
        threads[i] = th;
    }

    s = clock();
    readDatabase(fpi);

    for(ui i=0;i<workers;i++){
        queue_push("::FINISHED::");
    }

    for(ui i=0;i<workers;i++){
        pthread_join(threads[i], NULL);
    }

    f = clock();
    printf(" > Read database in %.2f sec.\n",((float)(f-s))/1000000.0);

    fclose(fpi);
    fclose(fpo);

}

