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

class RunParserXML extends RunParser implements Runnable {

    String text;
    String title;

    public RunParserXML(
            String text,
            String title,
            BufferedWriter bwBofw,
            BufferedWriter bwTitle,
            AbstParser parser)
    {
        this.text= text;
        this.title= title;
        this.category= parser.args.recateg;
        this.bwBofw= bwBofw;
        this.bwTitle= bwTitle;
        this.parser= parser;
    }

    public void run(){
        if(matchCategory()) bowCreator(text,title);
    }

}

