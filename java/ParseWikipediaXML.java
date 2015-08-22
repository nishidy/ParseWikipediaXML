//package com.github.nishidy8;

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
import org.apache.commons.cli.*;
import java.lang.Runtime;

import org.atilika.kuromoji.Token;
import org.atilika.kuromoji.Tokenizer;

class ArgStore {

	static String ifwiki;
	static String ifdict;
	static String ofcont;
	static String oftitle;
	static String recateg;
	static int minl;
	static int maxl;
	static int minc;
	static boolean isJap;
	static boolean isVerb;

	static void init(CommandLine cl) throws ParseException {

		if(cl.hasOption("i")) ifwiki = cl.getOptionValue("i");
		else throw new ParseException("i is not specified.");

		if(cl.hasOption("d")) ifdict = cl.getOptionValue("d");
		else throw new ParseException("d is not specified.");

		if(cl.hasOption("s")) ofcont = cl.getOptionValue("s");
		else ofcont = null;

		if(cl.hasOption("t")) oftitle= cl.getOptionValue("t");
		//else throw new ParseException("t is not specified.");

		if(cl.hasOption("m")) minl = Integer.parseInt(cl.getOptionValue("m"));
		else minl=2;

		if(cl.hasOption("x")) maxl = Integer.parseInt(cl.getOptionValue("x"));
		else maxl=64;

		if(cl.hasOption("c")) minc = Integer.parseInt(cl.getOptionValue("c"));
		else minc=1;

		if(cl.hasOption("g")) recateg = cl.getOptionValue("g");
		else recateg = ".*";

		if(cl.hasOption("j")) isJap= true;
		else isJap= false;

		if(cl.hasOption("v")) isVerb = true;
		else isVerb = false;

	}
}

abstract class AbstParse {

	abstract void createBaseWords(String file) throws IOException;
	abstract String convertToBaseWord(String line);
	abstract boolean isJap();
	abstract boolean isCommonWord(String word);

	boolean ifPageStart(String line){
		return line.indexOf("<page")>=0;
	}

	boolean ifPageEnd(String line){
		return line.indexOf("</page>")>=0;
	}

}

// Singleton
class EngParse extends AbstParse {

	private EngParse(){
		stopwords= Arrays.asList( "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",") );
	}

	private static final EngParse instance = new EngParse();
	public static EngParse getInstance(){ return instance; }

	List<String> stopwords = null;
	Map<String,String> mapDict = new HashMap<String,String>();

	@Override
	boolean isJap(){ return false; }

	@Override
	boolean isCommonWord(String word){
		if(stopwords.contains(word)) return true;
		return false;
	}

	@Override
	void createBaseWords(String file) throws IOException {

		String line;
		try( BufferedReader br = new BufferedReader(new FileReader(file)) ){
			while((line=br.readLine())!=null){
				if(line.indexOf(";;;")>=0) continue;
				String cols[] = line.split("\\t\\t");
				mapDict.put(cols[0],cols[1]);
			}
		} catch (IOException e){
			throw e;
		}

	}

	@Override
	String convertToBaseWord(String word) {
		if(mapDict.containsKey(word)){
			return mapDict.get(word);
		}else{
			return word;
		}
	}

}

// Singleton
class JapParse extends AbstParse {

	private JapParse(){}

	private static final JapParse instance = new JapParse();
	public static JapParse getInstance(){ return instance; }

	@Override
	boolean isJap(){ return true; }

	@Override
	String convertToBaseWord(String word) {
		return word;
	}

	@Override
	boolean isCommonWord(String word){ return false; }

	@Override
	void createBaseWords(String file) { }

}

//class ChiParse extends AbstParse {

class RunParse implements Runnable {

	String page;
	BufferedWriter bw;
	AbstParse parse;

	List<String> notwords= new ArrayList<String>();

	private final static Object lock = new Object();

	public RunParse(String page, BufferedWriter bw, AbstParse parse){
		this.page= page;
		this.bw= bw;
		this.parse= parse;
	}

	boolean isNotWord(String word){
		if(word.length()==1) return true;
		return false;
	}

	boolean isInNotWordList(String word){
		if(notwords.contains(word)) return true;
		return false;
	}

	boolean isWordAndUpdateList(String word){
		Pattern wpat= Pattern.compile("^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$");
		Matcher wmat= wpat.matcher(word);
		if(wmat.find()){
			return true;
		}else{
			notwords.add(word);
			return false;
		}
	}

	void bowCreator(String text){

		Map<String,Integer> mapbow = new HashMap<String,Integer>();

		int wordcnt= 0;
		for(String word: text.split(" ")){

			if(isNotWord(word)) continue;
			if(isInNotWordList(word)) continue;
			if(!isWordAndUpdateList(word)) continue;
			if(parse.isCommonWord(word)) continue;

			String bowWord= parse.convertToBaseWord(word);

			if(mapbow.containsKey(bowWord)){
				mapbow.put(bowWord,mapbow.get(bowWord)+1);
			}else{
				mapbow.put(bowWord,1);
			}
			wordcnt+=1;

			if(ArgStore.isVerb){
				System.out.printf("%s -> %s.\n",word,bowWord);
			}

			if(wordcnt>ArgStore.maxl) return;
		}
		if(wordcnt<ArgStore.minl) return;

		//List<Map.Entry<String,Integer>> listmapbow = new ArrayList<Map.Entry<String,Integer>>(mapbow.entrySet())
		List<Map.Entry<String,Integer>> entries= new ArrayList<>(mapbow.entrySet());
		Collections.sort(entries, new Comparator<Map.Entry>(){
			@Override
			public int compare(Map.Entry o1, Map.Entry o2){
				return ((Integer)o2.getValue()).compareTo((Integer)o1.getValue());
			}
		});

		int cnt=0;
		StringBuffer bowBuf = new StringBuffer("");
		for(Map.Entry<String,Integer> entry: entries){
			if(entry.getValue()<ArgStore.minc) continue;
			if(cnt>0) bowBuf.append(" ");
			bowBuf.append(String.format("%s %d",entry.getKey(),entry.getValue()));
			cnt+=1;
		}
		bowBuf.append("\n");

		if(bowBuf.length()>1){
			try{
				synchronized(lock){ bw.write(bowBuf.toString()); bw.flush(); }
			} catch (IOException e){
				System.err.println("BufferedWriter error.");
				System.exit(12);
			}
		}

	}

	public void run(){
		Pattern cpat= Pattern.compile("\\[\\[:*Category:([^\\[\\]\\|]+)\\|*[^\\[\\]]*\\]\\]");
		Matcher cmat= cpat.matcher(page);

		boolean cmatflag= false;
		while(cmat.find()){
			String category= cmat.group(1);
			Pattern rcpat= Pattern.compile(ArgStore.recateg);
			Matcher rcmat= rcpat.matcher(category);
			if(rcmat.find()){ cmatflag= true; break; }
		}
		if(!cmatflag) return;

		Pattern tpat= Pattern.compile("<text[^<>]*>([^<>]+)</text>");
		Matcher tmat= tpat.matcher(page);

		String text= "";
		if(tmat.find()){
			text= tmat.group(1);
			bowCreator(text);
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
		options.addOption("j","japanese",false,"Flag for Japanese");
		options.addOption("v","verbose",false,"Verbose");

		HelpFormatter help = new HelpFormatter();

		try{
			CommandLine cl = parser.parse(options, args);
			ArgStore.init(cl);

		} catch (ParseException e){
			help.printHelp("ParseWikipediaXML",options);
			System.exit(1);
		}


		AbstParse parse;
		if(ArgStore.isJap){
			parse = JapParse.getInstance();
		}else{
			parse = EngParse.getInstance();
		}

		BufferedWriter bw = null;
		try{
			if(ArgStore.ofcont!=null){
				bw = new BufferedWriter(new FileWriter(ArgStore.ofcont));
			}else{
				bw = new BufferedWriter(new OutputStreamWriter(System.out));
			}
		} catch (IOException e){
			System.exit(13);
		}

		int numofcpus = Runtime.getRuntime().availableProcessors();
		if(ArgStore.isVerb) System.out.printf("# of CPU is %d.\n",numofcpus);

		ExecutorService ex = Executors.newFixedThreadPool(numofcpus);

		try{
			parse.createBaseWords(ArgStore.ifdict);
		} catch (IOException e){
			System.exit(11);
		}

		try( BufferedReader br = new BufferedReader(new FileReader(ArgStore.ifwiki)) ){

			StringBuffer buf = new StringBuffer("");
			String line;
			boolean sflag=false, eflag=false;

			while((line=br.readLine())!=null){

				if(parse.ifPageStart(line)) sflag = true;
				if(parse.ifPageEnd(line))   eflag = true;

				if(sflag){
					buf.append(line);
					if(eflag){
						ex.execute(new RunParse(buf.toString(),bw,parse));
						sflag=eflag=false;
						buf.delete(0, buf.length());
					}
				}
			}

		} catch (IOException e){
			System.err.println("BufferedReader error "+e);
			System.exit(10);
		} finally {
			ex.shutdown();
		}

	}

}
