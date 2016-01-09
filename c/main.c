#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <pthread.h>
#include "ParseWikipediaXML.h"

int main(int argc, char* argv[]){

    prog_args *args = set_args(argc, argv);

    dict_info *dict = readDictionary(args);

    Stopwords *stopwords = getStopwords();

    startJoinThreads(args, stopwords, dict);

    free(stopwords);
    free(dict);

    // Run TF-IDF

    free(args);
}

