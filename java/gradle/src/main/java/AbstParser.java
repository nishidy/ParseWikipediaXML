package com.github.nishidy;

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

import java.lang.Runtime;

abstract class AbstParser {

    List<String> stopwords;
    ArgStore args;

    abstract void createBaseformsMap(String file) throws Exception;
    abstract String convertToBaseWord(String line);
    abstract boolean isJap();
    abstract List<String> getWordList(String text);
    abstract boolean isWord(String word);

    private ParseTfIdf parseTfIdf;

    boolean isCommonWord(String word){
        if(stopwords.contains(word)) return true;
        return false;
    }

    boolean ifPageStart(String line){
        return line.indexOf("<page")>=0;
    }

    boolean ifPageEnd(String line){
        return line.indexOf("</page>")>=0;
    }

    void takeArgs(ArgStore args) {
        this.args = args;
    }

    private void initTfIdf() {
        parseTfIdf = new ParseTfIdf(args.ofcont, args.oftfidf);
    }

    void ParseBofwForTfIdf() {

        if(args.oftfidf.equals("")) return;

        initTfIdf();
        parseTfIdf.getDocFreq();
        parseTfIdf.getTfIdf();

    }

    BufferedWriter getBufferedWriter() {
        BufferedWriter bw = null;
        try{
            if(args.ofcont==null){
                // stdout
                bw = new BufferedWriter(new OutputStreamWriter(System.out));
            }else{
                bw = new BufferedWriter(new FileWriter(args.ofcont));
            }
        } catch (IOException e){
            System.exit(13);
        }
        return bw;
    }

    int getRuntimeCpuNum() {
        int numofcpus = Runtime.getRuntime().availableProcessors();
        if(args.isVerb) System.out.printf("# of CPU is %d.\n",numofcpus);
        return numofcpus;
    }

    void readDictionary() {
        try{
            createBaseformsMap(args.ifdict);
        } catch (Exception e){
            System.exit(211);
        }
    }

    void ParseTextToBofw() {

        BufferedWriter bw = getBufferedWriter();

        int numofcpus = getRuntimeCpuNum();
        ExecutorService ex = Executors.newFixedThreadPool(numofcpus);

        readDictionary();

        try( BufferedReader br = new BufferedReader(new FileReader(args.ifwiki)) ){

            StringBuffer buf = new StringBuffer("");
            String line;
            boolean sflag=false, eflag=false;

            while((line=br.readLine())!=null){

                if(ifPageStart(line)) sflag = true;
                if(ifPageEnd(line))   eflag = true;

                if(sflag) buf.append(line);

                if(sflag && eflag){
                    ex.execute(new RunParser(buf.toString(),bw,this));
                    sflag=eflag=false;
                    buf.delete(0, buf.length());
                }
            }

        } catch (IOException e){
            System.err.println("BufferedReader error "+e);
            System.exit(10);
        } finally {
            // shutdown() waits for currently running tasks to finish
            ex.shutdown();
        }

    }
}

