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

import org.msgpack.MessagePack;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import org.msgpack.template.Template;
import org.msgpack.MessageTypeException;
import static org.msgpack.template.Templates.tMap;
import static org.msgpack.template.Templates.TString;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

// Singleton
public class EngParser extends AbstParser {

    private EngParser(){
        stopwords= Arrays.asList( "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",") );
    }

    private static final EngParser instance = new EngParser();
    public static EngParser getInstance(String... args){
        instance.takeArgs(args);
        return instance;
    }

    Map<String,String> mapDict = new HashMap<String,String>();

    @Override
    boolean isJap(){ return false; }

    private String message = "> Read dictionary";
    private int loadedWords = 0;
    private int parsedWords = 0;

    private void incrLoadedWords() {
        loadedWords ++;
    }

    private void incrParsedWords() {
        parsedWords ++;
    }

    private void showReadDictProgress() {
        if ( parsedWords > 1 ){
            System.out.print("\033[1A");
            System.out.flush();
        }

        System.out.printf(" %s [ # word (loaded/parsed)  %d / %d ]\n",
                message,loadedWords,parsedWords);
    }

    void readAsMsgpack(String file) throws Exception {
        MessagePack msgpack = new MessagePack();

        File f = new File(file);
        byte [] bytes = new byte[(int) f.length() ];;

        InputStream inputStream = new FileInputStream(f);
        inputStream.read(bytes);

        ByteArrayInputStream in = new ByteArrayInputStream(bytes);
        Unpacker unpacker = msgpack.createUnpacker(in);

        Template<Map<String,String>> mapTmpl = tMap(TString,TString);
        mapDict = unpacker.read(mapTmpl);

        final Iterator<String> mapIter = mapDict.keySet().iterator();
        while(mapIter.hasNext()){
            String key = mapIter.next();
            if(key.equals(mapDict.get(key))){
                mapIter.remove();
            }
            incrLoadedWords();
            showReadDictProgress();
        }
    }

    void readAsFile(String file) throws IOException {
        String line;
        try( BufferedReader br = new BufferedReader(new FileReader(file)) ){
            while((line=br.readLine())!=null){
                showReadDictProgress();
                incrParsedWords();

                if(line.indexOf(";;;")>=0) continue;
                String cols[] = line.split("[ \\t]");
                if(cols[0].equals(cols[3])) continue;
                mapDict.put(cols[0],cols[3]);

                incrLoadedWords();
            }
        } catch (IOException io){
            throw io;
        }
        showReadDictProgress();
    }

    @Override
    void createBaseformsMap(String file) throws Exception {

        long s = System.currentTimeMillis();

        try{ // try MessagePack format first
            readAsMsgpack(file);
        } catch (MessageTypeException e) {
            readAsFile(file);
        } catch (Exception io){
            throw io;
        }

        long f = System.currentTimeMillis();
        System.out.printf(" %s in %.2f sec.\n",message,(f-s)/1000.0);

    }

    @Override
    String convertToBaseWord(String word) {
        if(mapDict.containsKey(word)){
            return mapDict.get(word);
        }else{
            return word;
        }
    }

    @Override
    List<String> getWordList(String text){
        return Lists.transform( Arrays.asList(text.split(" ")), new Function<String, String>() {
            @Override
            public String apply(String arg0) {
                return arg0.toLowerCase();
            }
        });
    }

    @Override
    boolean isWord(String word){
        Pattern wpat= Pattern.compile("^[a-z][a-z0-9'-]*[a-z0-9]$");
        Matcher wmat= wpat.matcher(word);
        if(wmat.find()){
            return true;
        }else{
            return false;
        }
    }

}

