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

import org.apache.commons.lang.*;
import org.apache.commons.cli.*;

class ParseTfIdf {

    private String inFile;
    private String outFile;

    private List<Integer> numTermsInDoc;
    private Map<String,Integer> numDocForTerms;
    private int numDocInFile;

    ParseTfIdf(String inFile, String outFile) {
        this.inFile = inFile;
        this.outFile = outFile;
        numTermsInDoc = new ArrayList<>();
        numDocForTerms = new HashMap<String,Integer>();
        numDocInFile = 0;
    }

    private BufferedWriter getTfIdfBufferedWriter() {
        BufferedWriter bw = null;
        try{
            bw = new BufferedWriter(new FileWriter(outFile));
        } catch (IOException e){
            System.err.println("BufferedWriter error "+e);
            System.exit(13);
        }
        return bw;
    }

    void getDocFreq() {

        String line;

        try( BufferedReader br = new BufferedReader(new FileReader(inFile)) ){

            while((line=br.readLine())!=null){
                String terms[] = line.split(" ");
                int num=0;
                boolean b=false;
                for( String term : terms ){
                    if(b=!b){
                        numDocForTerms.putIfAbsent(term,1);
                        numDocForTerms.compute(term,(k,v) -> v+1); // Require Java 8
                    }else{
                        num += Integer.parseInt(term);
                    }
                }
                numTermsInDoc.add(num);
                numDocInFile++;
            }

        } catch (IOException e){
            System.err.println("BufferedReader error "+e);
            System.exit(111);
        }

    }

    void getTfIdf() {

        BufferedWriter bw = getTfIdfBufferedWriter();

        String line;
        int cntDoc = 0;

        try( BufferedReader br = new BufferedReader(new FileReader(inFile)) ){

            while((line=br.readLine())!=null){

                String terms[] = line.split(" ");
                StringBuffer bowBuf = new StringBuffer("");
                boolean isTerm=false;
                String currentTerm="";

                for( String term : terms ){

                    if( isTerm = !isTerm ) {
                        // term
                        if(!term.equals(terms[0])) bowBuf.append(" ");
                        bowBuf.append(term);
                        bowBuf.append(" ");
                        currentTerm = term;

                    }else{
                        // freq
                        int numTermInDoc = Integer.parseInt(term);
                        int numTermsInCurrentDoc = numTermsInDoc.get(cntDoc);
                        int tf = numTermInDoc / numTermsInCurrentDoc;

                        int numDocForTerm = numDocForTerms.get(currentTerm);
                        int idf = (int)Math.log10(numDocInFile/numDocForTerm)+1;

                        bowBuf.append(String.format("%d",tf*idf));

                    }
                }

                cntDoc++;
                bowBuf.append("\n");

                try{
                    bw.write(bowBuf.toString());
                    bw.flush();
                } catch (IOException e){
                    System.err.println("BufferedWriter error.");
                    System.exit(12);
                }
            }

        } catch (IOException e){
            System.err.println("BufferedReader error "+e);
            System.exit(12);
        }

    }

}

