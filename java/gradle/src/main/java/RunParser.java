package com.github.nishidy.ParseWikipediaXML;

import java.io.*;
import java.util.regex.*;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.*;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;

import org.apache.commons.lang.*;
import org.apache.commons.cli.*;

class RunParser implements Runnable {

    String page;
    BufferedWriter bw;
    AbstParser parser;

    //Collection<String> listNgrams; /* list can manipulate set with sort */
    List<String> listNgrams = new ArrayList<>();

    private final static Object lock = new Object();

    public RunParser(String page, BufferedWriter bw, AbstParser parser){
        this.page= page;
        this.bw= bw;
        this.parser= parser;
    }

    boolean isTooShortWord(String word){
        if(word.length()==1) return true;
        return false;
    }

    boolean isDupInNgram(String word){
        if(listNgrams.size()==0) return false;

        //if( listNgrams instanceof Set<String> )
        if( parser.args.preferListNgram ){
            if(listNgrams.get(listNgrams.size()-1).equals(word)){
                if(parser.args.isVerb) System.out.printf("%s is duplicated.\n",word);
                return true;
            }
        }else{
            if(listNgrams.contains(word)){
                if(parser.args.isVerb) System.out.printf("%s is duplicated.\n",word);
                return true;
            }
        }

        return false;
    }

    void bowCreator(String text, String title){

        Map<String,Integer> mapbow = new HashMap<String,Integer>();

        // To remove the last one in listNgrams in case preferListNgram is not set.
        List<String> listSaveNgramsOrder = new ArrayList<>();

        int wordcnt= 0;

        int ngramcountinit;
        if(parser.args.ngramsCollection){
            ngramcountinit=1;
        }else{
            ngramcountinit=parser.args.ngram;
        }

        for(int ngramcnt=(parser.args.ngramsCollection?1:parser.args.ngram);
                ngramcnt<=parser.args.ngram;
                ngramcnt++){

            for(String word: parser.getWordList(text)){

                if(isTooShortWord(word)) continue;
                if(!parser.isWord(word)) continue;
                if(parser.isCommonWord(word)) continue;

                if(isDupInNgram(word)) continue;

                listNgrams.add(word);
                listSaveNgramsOrder.add(word);
                if(!parser.args.preferListNgram) Collections.sort(listNgrams);

                if(listNgrams.size()<ngramcnt) continue;
                if(listNgrams.size()>ngramcnt){
                    listNgrams.remove(listSaveNgramsOrder.get(0));
                    listSaveNgramsOrder.remove(0);
                }

                String ngramstr= StringUtils.join(listNgrams,":");
                String bowWord= parser.convertToBaseWord(ngramstr);

                if(mapbow.containsKey(bowWord)){
                    mapbow.put(bowWord,mapbow.get(bowWord)+1);
                }else{
                    mapbow.put(bowWord,1);
                }
                wordcnt+=1;

                if(parser.args.isVerb && !parser.isJap()){
                    System.out.printf("%s -> %s.\n",ngramstr,bowWord);
                }

                if(wordcnt>parser.args.maxl) return;
            }
            if(wordcnt<parser.args.minl) return;
        }

        List<Map.Entry<String,Integer>> entries= new ArrayList<>(mapbow.entrySet());
        Collections.sort(entries, new Comparator<Map.Entry>(){
            @Override
            public int compare(Map.Entry o1, Map.Entry o2){
                return ((Integer)o2.getValue()).compareTo((Integer)o1.getValue());
            }
        });

        int cnt=0;
        StringBuffer bowBuf = new StringBuffer("");
        for(Map.Entry<String,Integer> entry: entries){
            if(entry.getValue()<parser.args.minc) continue;
            if(cnt>0) bowBuf.append(" ");
            bowBuf.append(String.format("%s %d",entry.getKey(),entry.getValue()));
            cnt+=1;
        }

        if(bowBuf.length()>1){

            bowBuf.append("\n");
            try{
                synchronized(lock){
                    bw.write(bowBuf.toString());
                    bw.flush();
                }
            } catch (IOException e){
                System.err.println("BufferedWriter error.");
                System.exit(12);
            }
            parser.incrSavedPages();

        } else {
            parser.incrParsedPages();

        }

        parser.showProgress();
    }

    public void run(){
        Pattern categoryTagPattern =
            Pattern.compile("\\[\\[:*Category:([^\\[\\]\\|]+)\\|*[^\\[\\]]*\\]\\]");
        Matcher categoryTagMatcher= categoryTagPattern.matcher(page);

        boolean cmatflag= true; // no category, no check
        while(categoryTagMatcher.find()){
            cmatflag= false;
            String category= categoryTagMatcher.group(1);
            Pattern categoryPattern= Pattern.compile(parser.args.recateg);
            Matcher categoryMatcher= categoryPattern.matcher(category);
            if(categoryMatcher.find()){ cmatflag= true; break; }
        }
        if(!cmatflag) return;

        Pattern p = Pattern.compile("<title[^<>]*>([^<>]+)</title>");
        Matcher m = p.matcher(page);

        String title= "";
        if(m.find()){
            title= m.group(1);
        }

        p = Pattern.compile("<text[^<>]*>([^<>]+)</text>");
        m = p.matcher(page);

        String text= "";
        if(m.find()){
            text= m.group(1);
            bowCreator(text,title);
        }
    }
}

