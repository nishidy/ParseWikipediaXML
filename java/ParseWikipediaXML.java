package com.github.nishidy8;

import java.io.*;
import java.util.regex.*;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.*;
import org.apache.commons.cli.*;
import java.lang.Runtime;

import org.atilika.kuromoji.Token;
import org.atilika.kuromoji.Tokenizer;

class BOW implements Runnable {

	static Map<String,String> mapDict = new HashMap<String,String>();
	static BufferedWriter bw;
	static List<String> stopwords = new ArrayList<>();
	private final static Object lock = new Object();

	String pageStr;
	String categoryRe;
	Integer minl = 2;
	Integer maxl = 64;
	Integer minc = 1;

	public BOW(String pageStr, String categoryRe, Integer... margs){
		this.pageStr = pageStr;
		this.categoryRe = categoryRe;
		this.minl = margs[0];
		this.maxl = margs[1];
		this.minc = margs[2];
	}

	void bowCreater(String text){

		Map<String,Integer> mapBow = new HashMap<String,Integer>();

		Pattern pattern = Pattern.compile("^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$");
		List<String> notWord = new ArrayList<String>();

		int wordcount = 0;
		for(String word: text.split(" ")){

			if(stopwords.contains(word)) continue;

			if(notWord.contains(word)) continue;

			Matcher matcher = pattern.matcher(word);
			if(!matcher.find()){
				notWord.add(word);
				continue;
			}

			if(word.length()==1) continue;

			String bowWord;
			if(mapDict.containsKey(word)){
				bowWord=mapDict.get(word);
			}else{
				bowWord = word;
			}

			if(mapBow.containsKey(bowWord)){
				mapBow.put(bowWord,mapBow.get(bowWord)+1);
			}else{
				mapBow.put(bowWord,1);
			}
			wordcount+=1;

			if(wordcount>maxl) return;
		}
		if(wordcount<minl) return;

		int count=0;
		StringBuffer bowBuf = new StringBuffer("");
		for(Map.Entry<String,Integer> entry: mapBow.entrySet()){
			if(entry.getValue()<minc) continue;
			if(count>0) bowBuf.append(" ");
			bowBuf.append(String.format("%s %d",entry.getKey(),entry.getValue()));
			count+=1;
		}
		bowBuf.append("\n");

		if(bowBuf.length()>1){
			try{
				synchronized(lock){ bw.write(bowBuf.toString()); bw.flush(); }
			} catch (IOException e){
				System.exit(12);
			}
		}

	}

	public void run(){
		Pattern cpattern = Pattern.compile("\\[\\[:*Category:([^\\[\\]\\|]+)\\|*[^\\[\\]]*\\]\\]");
		Matcher cmatcher = cpattern.matcher(pageStr);

		Boolean categoryMatch = false;
		while(cmatcher.find()){
			String categoryStr = cmatcher.group(1);
			Pattern patternCategory = Pattern.compile(categoryRe);
			Matcher matcherCategory = patternCategory.matcher(categoryStr);
			if(matcherCategory.find()){
				categoryMatch = true;
				break;
			}
		}
		if(!categoryMatch) return;

		Pattern pattern = Pattern.compile("<text[^<>]*>([^<>]+)</text>");
		Matcher matcher = pattern.matcher(pageStr);

		String text = "";
		if(matcher.find()){
			text = matcher.group(1);
			bowCreater(text);
		}
	}
}

public class ParseWikipediaXML {

	public static void main(String... args){

		BasicParser parser = new BasicParser();
		Options options = new Options();

		options.addOption("i","input-file",true,"Input File(Wikipedia)");
		options.addOption("d","dict-file",true,"Input File(Dictionary)");
		options.addOption("s","cont-file",true,"Output File(Contents)");
		options.addOption("t","title-file",true,"Output File(Title)");
		options.addOption("m","min-page-words",true,"Minimum number of words that a page should have");
		options.addOption("x","max-page-words",true,"Maxmum number of words that a page should have");
		options.addOption("c","min-word-count",true,"Minimum number that a word should have");
		options.addOption("g","category-regex",true,"Category in regular expression");

		HelpFormatter help = new HelpFormatter();

		String ifwiki,ifdict,ofcont,oftitle,categoryRe;
		ifwiki=ifdict=ofcont=oftitle=categoryRe="";

		Integer minl,maxl,minc;
		minl=maxl=minc=0;

		try{

			CommandLine cl = parser.parse(options, args);

			if(cl.hasOption("i")) ifwiki = cl.getOptionValue("i");
			else throw new ParseException("i is not specified.");

			if(cl.hasOption("d")) ifdict = cl.getOptionValue("d");
			else throw new ParseException("d is not specified.");

			if(cl.hasOption("s")) ofcont = cl.getOptionValue("s");
			else throw new ParseException("s is not specified.");

			if(cl.hasOption("t")) oftitle= cl.getOptionValue("t");
			//else throw new ParseException("t is not specified.");

			if(cl.hasOption("m")) minl = Integer.parseInt(cl.getOptionValue("m"));
			if(cl.hasOption("x")) maxl = Integer.parseInt(cl.getOptionValue("x"));
			if(cl.hasOption("c")) minc = Integer.parseInt(cl.getOptionValue("c"));

			if(cl.hasOption("g")) categoryRe = cl.getOptionValue("g");
			else categoryRe = ".*";

		} catch (ParseException e){
			help.printHelp("ParseWikipediaXML",options);
			System.exit(1);
		}

		try{
			BOW.bw = new BufferedWriter(new FileWriter(ofcont));
		} catch (IOException e){
			System.exit(13);
		}

		int cpu = Runtime.getRuntime().availableProcessors();
		System.out.printf("# of CPU is %d.\n",cpu);
		ExecutorService ex = Executors.newFixedThreadPool(cpu);

		String line;
		try( BufferedReader br = new BufferedReader(new FileReader(ifdict)) ){
			while((line=br.readLine())!=null){
				if(line.indexOf(";;;")>=0) continue;
				String cols[] = line.split("\\t\\t");
				BOW.mapDict.put(cols[0],cols[1]);
			}
		} catch (IOException e){
			System.exit(11);
		}

		String stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
		BOW.stopwords = Arrays.asList(stopwords.split(","));

		try( BufferedReader br = new BufferedReader(new FileReader(ifwiki)) ){

			StringBuffer pageBuf = new StringBuffer("");
			Boolean startFlag, endFlag;

			startFlag=endFlag=false;
			while((line=br.readLine())!=null){

				if(line.indexOf("<page")>=0){
					startFlag = true;
				}
				if(line.indexOf("</page>")>=0){
					endFlag= true;
				}

				if(startFlag){
					pageBuf.append(line);
					if(endFlag){
						ex.execute(new BOW(pageBuf.toString(),categoryRe,minl,maxl,minc));
						startFlag=endFlag=false;
						pageBuf.delete(0, pageBuf.length());
					}
				}
			}

		} catch (IOException e){
			System.exit(10);
		} finally {
			ex.shutdown();
		}

	}

}
