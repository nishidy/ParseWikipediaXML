#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <pthread.h>
#include "ParseWikipediaXML.h"

char verbose = 0;

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
                    r^=1;
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
    ui dictionary_num[27][27] = {{0}};

    struct timeval s,f;

    gettimeofday(&s,NULL);
    readDictionary(fpd, dictionary, dictionary_num);
    gettimeofday(&f,NULL);
    printf(" > Read dictionary in %.2f sec.\n",((float)(f.tv_sec-s.tv_sec)+(f.tv_usec-s.tv_usec)/1000000.0));

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

    gettimeofday(&s,NULL);
    readDatabase(fpi);

    for(ui i=0;i<workers;i++){
        queue_push("::FINISHED::");
    }

    for(ui i=0;i<workers;i++){
        pthread_join(threads[i], NULL);
    }

    gettimeofday(&f,NULL);
    printf(" > Read database in %.2f sec.\n",((float)(f.tv_sec-s.tv_sec)+(f.tv_usec-s.tv_usec)/1000000.0));

    fclose(fpi);
    fclose(fpo);

}

