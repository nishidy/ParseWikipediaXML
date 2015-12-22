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

import org.atilika.kuromoji.Token;
import org.atilika.kuromoji.Tokenizer;

// Singleton
public class JapParser extends AbstParser {

    Tokenizer tokenizer;

    private JapParser(){
        stopwords= Arrays.asList( "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ".split(",") );

        tokenizer=Tokenizer.builder().build();

    }

    private static final JapParser instance = new JapParser();
    public static JapParser getInstance(String... args){
        instance.takeArgs(args);
        return instance;
    }

    @Override
    boolean isJap(){ return true; }

    @Override
    String convertToBaseWord(String word) {
        return word;
    }

    @Override
    void createBaseformsMap(String file) { }

    @Override
    List<String> getWordList(String text){
        List<Token> tokens = tokenizer.tokenize(text);
        List<String> words = new ArrayList<>();

        for (Token token : tokens) {
            String baseword= token.getBaseForm();
            if(baseword!=null){
                words.add(baseword);
            }else{
                baseword=token.getSurfaceForm();
                String[] features= token.getAllFeaturesArray();
                if(features[0].equals("動詞") ||
                   ( features[0].equals("名詞") &&
                     !features[1].equals("サ変接続") ) ||
                   features[0].equals("形容詞") ||
                   features[0].equals("副詞")){
                    words.add(baseword);
                }
            }
            if(args.isVerb && isJap()){
                System.out.printf("%s: %s\n",token.getSurfaceForm(),token.getAllFeatures());
            }
        }
        return words;
    }

    @Override
    boolean isWord(String word){
        return true;
    }

}

