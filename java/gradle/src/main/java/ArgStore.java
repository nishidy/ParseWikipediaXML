package com.github.nishidy;

import org.apache.commons.cli.*;

class ArgStore {

    String ifwiki;
    String ifdict;
    String ofcont;
    String oftitle;
    String recateg;
    int minl;
    int maxl;
    int minc;
    int ngram;
    boolean isJap;
    boolean isVerb;
    boolean preferListNgram; // make ngram like list or set
    boolean ngramsCollection;
    String oftfidf;

    ArgStore(String... args) {

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

        try {
            init( basicparser.parse(options, args) );
        } catch (ParseException e){
            help.printHelp("ParseWikipediaXML",options);
            System.exit(1);
        }


    };

    void init(CommandLine cl) throws ParseException {

        if(cl.hasOption("j")) isJap= true;
        else isJap= false;

        /* If set, option c (ofcont) will be read-only value */
        if(cl.hasOption("f")) oftfidf = cl.getOptionValue("f");
        else oftfidf = "";

        if(cl.hasOption("i")) ifwiki = cl.getOptionValue("i");
        else if(oftfidf.equals("")) throw new ParseException("i is not specified.");

        if(cl.hasOption("d")) ifdict = cl.getOptionValue("d");
        else if(oftfidf.equals("") && !isJap) throw new ParseException("d is not specified.");

        if(cl.hasOption("s")) ofcont = cl.getOptionValue("s");
        else ofcont = null;

        if(cl.hasOption("t")) oftitle= cl.getOptionValue("t");
        //else throw new ParseException("t is not specified.");

        if(cl.hasOption("m")) minl = Integer.parseInt(cl.getOptionValue("m"));
        else minl=2;

        if(cl.hasOption("x")) maxl = Integer.parseInt(cl.getOptionValue("x"));
        else maxl=65535;

        if(cl.hasOption("c")) minc = Integer.parseInt(cl.getOptionValue("c"));
        else minc=1;

        if(cl.hasOption("g")) recateg = cl.getOptionValue("g");
        else recateg = ".*";

        if(cl.hasOption("n")) ngram = Integer.parseInt(cl.getOptionValue("n"));
        else ngram=1;

        /* If set, the parameter ngram is regarded as maximum number.
         * For example, if ngram=3, all the words with ngram=1,2,3 will be collected.
         * If not set, it will collect words only with ngram=3.
         */
        if(cl.hasOption("e")) ngramsCollection= true;
        else ngramsCollection= false;

        if(cl.hasOption("v")) isVerb = true;
        else isVerb = false;

        /* Note for preferListNgram.
         * true : Ngrams are managed like List.
         * false: Ngrams are managed like Set.
         *
         * How to deal with duplicated words when the word is
         * about to be added to ngrams?
         * List: Ignored if a word appears twice in a row.
         * Set : Ignored if a word appears twice regardless of the place.
         *
         * When is the number of Ngrams counted?
         * List: Words in Ngram and the emergence order are same.
         * Set : Words in Ngram is same. (Represented by sorted list)
         *
         */
        if(cl.hasOption("l")) preferListNgram = true;
        else preferListNgram = false;

    }
}

