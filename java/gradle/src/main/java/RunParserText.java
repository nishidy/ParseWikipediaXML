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

class RunParserText extends RunParser implements Runnable {

    private String page;

    //Collection<String> listNgrams; /* list can manipulate set with sort */
    List<String> listNgrams = new ArrayList<>();

    private final static Object lock = new Object();

    public RunParserText(String page, BufferedWriter bwBofw, BufferedWriter bwTitle, AbstParser parser){
        this.page= page;
        this.category= parser.args.recateg;
        this.bwBofw= bwBofw;
        this.bwTitle= bwTitle;
        this.parser= parser;
    }

    public void run(){
       if(!matchCategory()) return;

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

