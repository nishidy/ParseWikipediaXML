#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned int ui;

typedef struct {
    char term[64];
    int  freq;

} Bofw;

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
    if(fpo==NULL){
        disp_stdio(bofw,cnt_bofw);
    }else{
        save_file(bofw,cnt_bofw,fpo);
    }
}

int sort_bofw(const void *a, const void *b){
    if(((Bofw*)a)->freq > ((Bofw*)b)->freq){
        return -1;
    }else if(((Bofw*)a)->freq < ((Bofw*)b)->freq){
        return 1;
    }else{
        if(((Bofw*)a)->term > ((Bofw*)b)->term){
            return 1;
        }else{
            return -1;
        }
    }
}

int main(int argc, char* argv[]){

    char inDbFile[64] = {0};
    char outBofwFile[64] = {0};

    argc--;
    argv++;

    for(int i=0;i<argc;i++){
        if(i%2==0){
            if(strlen(argv[i])>2){
                printf("Wrong option (%s).\n",argv[i]);
                exit(0);
            }
            char c = argv[i][1];
            switch (c) {
                case 'i':
                    strncpy(inDbFile, argv[i+1], strlen(argv[i+1]));
                    break;
                case 's':
                    strncpy(outBofwFile, argv[i+1], strlen(argv[i+1]));
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

    FILE *fpi, *fpo = NULL;
    fpi = fopen(inDbFile,"r");
    if(strlen(outBofwFile)>0){
        fpo = fopen(outBofwFile,"w");
    }
    char l[65535] = {0};

    while(fgets(l,65535,fpi)){
        Bofw bofw[65535] = {0};
        char *c;
        ui cnt_bofw = 0;
        c = strtok(l, " .,;\n");
        while(c != NULL){
            int idx_bofw = getIndexOfTerm(bofw, cnt_bofw, c);
            if(idx_bofw>-1){
                bofw[idx_bofw].freq++;
            }else{
                strncpy(bofw[cnt_bofw].term, c, strlen(c));
                bofw[cnt_bofw].freq = 1;
                cnt_bofw++;
            }
            c=strtok(NULL," .,;\n");
        }
        qsort((void *)bofw, cnt_bofw, sizeof(Bofw), sort_bofw);
        save(bofw, cnt_bofw, fpo);
    }

    fclose(fpi);
    fclose(fpo);

}

