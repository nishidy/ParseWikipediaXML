#ifndef PARSEWIKIPEDIAXML_H
#define PARSEWIKIPEDIAXML_H

#include <stdio.h>

#define SSIZE 256
#define MSIZE 4096
#define LSIZE 65536
#define QDATASIZE 65536
#define QSIZE 32
#define ASCII 97
#define TERMLEN 48
#define FNAMELEN 64

typedef unsigned int ui;

typedef struct {
    char term[48];
    int  freq;
} Bofw;

typedef struct {
    char infl[TERMLEN]; // inflected
    char base[TERMLEN]; // baseform
} Dictionary;

typedef struct {
    char s[1024][16];
    ui n;
} Stopwords;

typedef struct {
    Dictionary *dictionary[27][27];
    ui dictionary_num[27][27];
} dict_info;

typedef struct {
    char inDbFile[FNAMELEN];
    char inDictFile[FNAMELEN];
    char outBofwFile[FNAMELEN];
    ui workers;
} prog_args;

typedef struct {
    FILE* fpi;
    FILE* fpo;
    char *stopwords;
    ui stopwords_num;
    Dictionary *dictionary;
    ui *dictionary_num;
} thread_args;


void startJoinThreads(prog_args *args, Stopwords *stopwords, dict_info *dict);

thread_args* setThreadArgs(prog_args *args, Stopwords *stopwords, dict_info *dict);

prog_args* set_args(int argc, char* argv[]);

int getIndexOfTerm(Bofw *bofw, ui cnt_bofw, char *term);

void save_file(Bofw *bofw, ui cnt_bofw, FILE* fpo);

void disp_stdio(Bofw *bofw, ui cnt_bofw);

void save(Bofw *bofw, ui cnt_bofw, FILE* fpo);

int sort_bofw(const void *a, const void *b);

void strcaterr();

void reallocerr(char *p);

char* getElementText(char *page, char *tag);

void append_text(char **text, char *l);

char* cbElementTextRaw(FILE* fp, char *tag);

char* cbElementText(char *tag);

void toLower(char *term);

void allocMemDictionary(ui n, Dictionary **d);

void getDictionary(FILE *fp, dict_info *dict);

dict_info* readDictionary(prog_args *args);

void toBaseform(char *term, Dictionary *dictionary[27][27], ui dictionary_num[27][27]);

Stopwords* getStopwords();

int isStopword(char *term, char stopwords[][16], ui stopwords_num);

int isValidTerm(char *term);

void allocMemQueue();

char* queue_pop();

void stretch_qdatasize(ui block_size, char **queue_pos);

void copy_to_queue(char **queue_pos, char *page);

void queue_push(char* page);

void readDatabase(FILE *fpi);

void run_parse(char* page_raw, thread_args *targs);

void* parse_thread(void* args);

#endif
