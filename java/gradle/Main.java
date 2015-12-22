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

        ArgStore argstore = new ArgStore(args);

        AbstParser parser;
        if(argstore.isJap){
            parser = JapParser.getInstance(argstore);
        }else{
            parser = EngParser.getInstance(argstore);
        }

        parser.ParseTextToBofw();
        parser.ParseBofwForTfIdf();

    }

}
