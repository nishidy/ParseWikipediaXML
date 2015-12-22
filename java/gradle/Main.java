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

import com.github.nishidy.ParseWikipediaXML.*;

public class Main {

    public static void main(String... args){

        AbstParser parser;
        parser = EngParser.getInstance(args);
        //parser = JapParser.getInstance(args);

        parser.ParseTextToBofw();
        parser.ParseBofwForTfIdf();

    }

}
