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

import org.apache.commons.cli.*;

public class ParseWikipediaXML {

    public static void main(String... args){

        BasicParser basicparser = new BasicParser();
        Options options = new Options();

        options.addOption("i","input-file",true,"Input File(Wikipedia)");
        options.addOption("d","dict-file",true,"Input File(Dictionary)");
        options.addOption("s","cont-file",true,"Output File(Contents)");
        options.addOption("t","title-file",true,"Output File(Title)");
        options.addOption("m","min-page-words",true,"Minimum number of words that a page should have");
        options.addOption("x","max-page-words",true,"Maxmum number of words that a page should have");
        options.addOption("c","min-word-count",true,"Minimum number that a word should have");
        options.addOption("g","category-regex",true,"Category in regular expression");
        options.addOption("n","ngram",true,"Enables Ngram on N>1");

        options.addOption("j","japanese",false,"Flag for Japanese");
        options.addOption("l","ngram-list",false,"Flag for Ngram as list");
        options.addOption("e","ngram-collection",false,"Flag for Ngram collection");
        options.addOption("v","verbose",false,"Verbose");
        options.addOption("f","tf-idf",true,"TF-IDF");

        HelpFormatter help = new HelpFormatter();

        try{

            CommandLine cl = basicparser.parse(options, args);
            ArgStore argstore = new ArgStore(cl);

            AbstParser parser;
            if(argstore.isJap){
                parser = JapParser.getInstance(argstore);
            }else{
                parser = EngParser.getInstance(argstore);
            }

            parser.ParseTextToBofw();
            parser.ParseBofwForTfIdf();

        } catch (ParseException e){
            help.printHelp("ParseWikipediaXML",options);
            System.exit(1);
        }

    }

}
