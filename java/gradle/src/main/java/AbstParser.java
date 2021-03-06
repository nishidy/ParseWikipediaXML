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

import java.lang.Runtime;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.SAXException;

import javax.xml.parsers.*;

public abstract class AbstParser {

    List<String> stopwords;
    ArgStore args;

    abstract void createBaseformsMap(String file) throws Exception;
    abstract String convertToBaseWord(String line);
    abstract boolean isJap();
    abstract List<String> getWordList(String text);
    abstract boolean isWord(String word);

    private String pageStartTag="", pageEndTag="";

    public void registPageTag(String tag) {
        pageStartTag = "<"+tag;
        pageEndTag = "</"+tag;
    }

    private ParseTfIdf parseTfIdf;

    boolean isCommonWord(String word){
        if(stopwords.contains(word)) return true;
        return false;
    }

    boolean ifPageStart(String line){
        return line.indexOf(pageStartTag)>=0;
    }

    boolean ifPageEnd(String line){
        return line.indexOf(pageEndTag)>=0;
    }

    void takeArgs(String... args) {
        this.args = new ArgStore(args);
    }

    private void initTfIdf() {
        parseTfIdf = new ParseTfIdf(args.ofcont, args.oftfidf);
    }

    private boolean canRunParseBofwForTfIdf() {
        if( args.oftfidf.equals("") ){
            System.err.println("You did not give the file path to save the result of TF-IDF.");
            return false;
        }
        return true;
    }

    public void ParseBofwForTfIdf() {

        if(!canRunParseBofwForTfIdf()) return;

        initTfIdf();
        parseTfIdf.getDocFreq();
        parseTfIdf.getTfIdf();

    }

    BufferedWriter getBufferedWriter(String file) {
        BufferedWriter bw = null;
        try{
            if(file==null){
                // stdout
                bw = new BufferedWriter(new OutputStreamWriter(System.out));
            }else{
                bw = new BufferedWriter(new FileWriter(file));
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

    void parseText() {

        ExecutorService ex;
        if ( args.workers == 0 ) {
            int numofcpus = getRuntimeCpuNum();
            ex = Executors.newFixedThreadPool(numofcpus);
        } else {
            ex = Executors.newFixedThreadPool(args.workers);
        }

        BufferedWriter bwBofw = getBufferedWriter(args.ofcont);
        BufferedWriter bwTitle = getBufferedWriter(args.oftitle);

        long s = System.currentTimeMillis();
        try( BufferedReader br = new BufferedReader(new FileReader(args.ifwiki)) ){

            StringBuffer buf = new StringBuffer("");
            String line;
            boolean sflag=false, eflag=false;

            while((line=br.readLine())!=null){

                if(ifPageStart(line)) sflag = true;
                if(ifPageEnd(line))   eflag = true;

                if(sflag) buf.append(line);

                if(sflag && eflag){
                    ex.execute(new RunParserText(buf.toString(),bwBofw,bwTitle,this));
                    sflag=eflag=false;
                    buf.delete(0, buf.length());
                }

            }

        } catch (IOException e){
            System.err.println("BufferedReader error "+e);
            System.exit(10);
        } finally {
            ex.shutdown();
        }

        try{
            ex.awaitTermination(1, TimeUnit.HOURS);
        } catch (InterruptedException e){
            System.err.println("Executor awaitTermination error "+e);
            System.exit(11);
        }

        long f = System.currentTimeMillis();
        System.out.printf(" %s in %.2f sec.\n", message, (f-s)/1000.0);

    }

    private boolean canRunParseTextToBofw() {
        if( pageStartTag.equals("") || pageEndTag.equals("") ){
            System.err.println("You did not register a tag to parse pages.");
            return false;
        }
        return true;
    }

    public void ParseTextToBofw() {
        if(!canRunParseTextToBofw()) return;
        readDictionary();
        parseText();
    }

    public void ParseXMLToBofw() {
        if(!canRunParseTextToBofw()) return;
        readDictionary();
        parseXML();
    }

    private int savedPages = 0;
    private int parsedPages = 0;
    protected String message = "> Read database";

    void incrSavedPages () {
        savedPages ++;
        incrParsedPages();
    }

    void incrParsedPages () {
        parsedPages ++;
    }

    synchronized void showProgress () {
        if ( parsedPages > 1 ){
            System.out.print("\033[1A");
            System.out.flush();
        }

        System.out.printf(" %s [ # page (saved/parsed) %d / %d ]\n",
                message, savedPages, parsedPages);
    }

    private void writeOutput (BufferedWriter bw, String output) {
        try{
            bw.write(output);
            bw.flush();
        } catch (IOException e){
            System.err.println("BufferedWriter error.");
            System.exit(12);
        }
    }

    synchronized void postParse (BufferedWriter bwBofw, String bofw, BufferedWriter bwTitle, String title) {
        writeOutput(bwBofw, bofw+"\n");
        writeOutput(bwTitle, title+"\n");

        incrSavedPages();
    }

    synchronized void postParse () {
        incrParsedPages();
    }

    private void parseXML() {

        ExecutorService ex;
        if ( args.workers == 0 ) {
            int numofcpus = getRuntimeCpuNum();
            ex = Executors.newFixedThreadPool(numofcpus);
        } else {
            ex = Executors.newFixedThreadPool(args.workers);
        }

        BufferedWriter bwBofw = getBufferedWriter(args.ofcont);
        BufferedWriter bwTitle = getBufferedWriter(args.oftitle);

        long s = System.currentTimeMillis();
        try {
            SAXParserFactory spfactory = SAXParserFactory.newInstance();
            SAXParser parser = spfactory.newSAXParser();
            parser.parse(new File(args.ifwiki), new ParseXML(ex,bwBofw,bwTitle,this));
        } catch (IOException | ParserConfigurationException e) {
            System.err.println("testSax error "+e);
        } catch (SAXException e) {
            System.err.println("*** The structure of XML sseems broken ***\n");
        } finally {
            ex.shutdown();

            try{
                ex.awaitTermination(1, TimeUnit.HOURS);
            } catch (InterruptedException e){
                System.err.println("Executor awaitTermination error "+e);
                System.exit(11);
            }

            long f = System.currentTimeMillis();
            System.out.printf(" %s in %.2f sec.\n", message, (f-s)/1000.0);
        }
    }

}

class ParseXML extends DefaultHandler {

    private ExecutorService ex;

    BufferedWriter bwBofw;
    BufferedWriter bwTitle;

    AbstParser parser;

    private StringBuffer bufferText;
    private StringBuffer bufferTitle;

    private boolean flagText = false;
    private boolean flagTitle = false;

    ParseXML(
        ExecutorService ex,
        BufferedWriter bwBofw,
        BufferedWriter bwTitle,
        AbstParser parser)
    {
        this.ex = ex;
        this.bwBofw = bwBofw;
        this.bwTitle = bwTitle;
        this.parser = parser;
    }

    public void startDocument() {
        bufferText = new StringBuffer();
        bufferTitle = new StringBuffer();
    }

    public void startElement(
            String uri,
            String localName,
            String qName,
            Attributes attr ) {

        if(qName.equals("text")) {
            flagText = true;
        }

        if(qName.equals("title")) {
            flagTitle = true;
        }
    }

    public void characters(char []ch, int offset, int length) {

        if(flagText) {
            bufferText.append(new String(ch, offset, length));
        }

        if(flagTitle) {
            bufferTitle.append(new String(ch, offset, length));
        }
    }

    public void endElement(String uri, String localName, String qName){

        if(qName.equals("text")){

            ex.execute(
                new RunParserXML(
                    bufferText.toString(),
                    bufferTitle.toString(),
                    bwBofw,
                    bwTitle,
                    parser
                )
            );

            bufferText.setLength(0);
            bufferTitle.setLength(0);
            flagText = false;
        }

        if(qName.equals("title")){
            flagTitle = false;
        }
    }

    public void endDocument() {
    }

}
