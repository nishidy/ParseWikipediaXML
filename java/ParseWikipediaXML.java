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

	String ifwiki;
	String ifdict;
	String ofcont;
	String oftitle;
	String recateg;
	int minl;
	int maxl;
	int minc;
	boolean ifjap;
	boolean ifverb;

	ArgStore(CommandLine cl) throws ParseException {

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

		if(cl.hasOption("j")) ifjap = true;
		else ifjap = false;

		if(cl.hasOption("v")) ifverb = true;
		else ifverb = false;

	}
}

class Env {
	int numofcpus;
}

abstract class AbstParse {

	List<String> stopwords;

	boolean ifPageStart(String line){
		return line.indexOf("<page")>=0;
	}

	boolean ifPageEnd(String line){
		return line.indexOf("</page>")>=0;
	}

	abstract boolean ifjap();
	abstract void holdStopwords(String str);

}

// Singleton
class EngParse extends AbstParse {

	Map<String,String> mapDict = new HashMap<String,String>();

	private static final EngParse instance = new EngParse();

	private EngParse(){
		holdStopwords(
			"a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
		);
	}

	@Override
	void holdStopwords(String str){
		stopwords= Arrays.asList( str.split(",") );
	}

	@Override
	boolean ifjap(){ return false; }

	public static EngParse getInstance(){
		return instance;
	}

	void getEngMapDict(String file) throws IOException {

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
}

// Singleton
class JapParse extends AbstParse {

	private static final JapParse instance = new JapParse();

	private JapParse(){}

	public static JapParse getInstance(){
		return instance;
	}

	@Override
	void holdStopwords(String str){
		stopwords= Arrays.asList( str.split(",") );
	}

	@Override
	boolean ifjap(){ return true; }
}

class RunParse implements Runnable {

	String page;
	ArgStore argstore;
	BufferedWriter bw;
	AbstParse parse;

	private final static Object lock = new Object();

	public RunParse(String page, ArgStore argstore, BufferedWriter bw, AbstParse parse){
		this.page= page;
		this.argstore= argstore;
		this.bw= bw;
		this.parse= parse;
	}

	void bowCreator(String text){

		Map<String,Integer> mapbow = new HashMap<String,Integer>();

		Pattern wpat= Pattern.compile("^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$");
		List<String> notwords= new ArrayList<String>();

		int wordcnt= 0;
		for(String word: text.split(" ")){

			if(parse.stopwords.contains(word)) continue;
			if(notwords.contains(word))  continue;

			Matcher wmat= wpat.matcher(word);
			if(!wmat.find()){
				notwords.add(word);
				continue;
			}

			if(word.length()==1) continue;

			String bowWord;
			if(parse.ifjap()){
				bowWord = word;
			}else{
				EngParse engparse = (EngParse)parse;
				if(engparse.mapDict.containsKey(word)){
					bowWord= engparse.mapDict.get(word);
				}else{
					bowWord = word;
				}
			}

			if(mapbow.containsKey(bowWord)){
				mapbow.put(bowWord,mapbow.get(bowWord)+1);
			}else{
				mapbow.put(bowWord,1);
			}
			wordcnt+=1;

			if(wordcnt>argstore.maxl) return;
		}
		if(wordcnt<argstore.minl) return;

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
			if(entry.getValue()<argstore.minc) continue;
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
			Pattern rcpat= Pattern.compile(argstore.recateg);
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

		ArgStore argstore= null;
		try{
			CommandLine cl = parser.parse(options, args);
			argstore= new ArgStore(cl);

		} catch (ParseException e){
			help.printHelp("ParseWikipediaXML",options);
			System.exit(1);
		}


		AbstParse parse;
		if(argstore.ifjap){
			parse = JapParse.getInstance();
		}else{
			parse = EngParse.getInstance();
		}

		BufferedWriter bw = null;
		try{
			if(argstore.ofcont!=null){
				bw = new BufferedWriter(new FileWriter(argstore.ofcont));
			}else{
				bw = new BufferedWriter(new OutputStreamWriter(System.out));
			}
		} catch (IOException e){
			System.exit(13);
		}

		Env env = new Env();
		env.numofcpus = Runtime.getRuntime().availableProcessors();
		if(argstore.ifverb) System.out.printf("# of CPU is %d.\n",env.numofcpus);

		ExecutorService ex = Executors.newFixedThreadPool(env.numofcpus);

		if(!argstore.ifjap){
			EngParse engparse = (EngParse)parse;
			try{
				engparse.getEngMapDict(argstore.ifdict);
			} catch (IOException e){
				System.exit(11);
			}
		}

		try( BufferedReader br = new BufferedReader(new FileReader(argstore.ifwiki)) ){

			StringBuffer buf = new StringBuffer("");
			String line;
			boolean sflag=false, eflag=false;

			while((line=br.readLine())!=null){

				if(parse.ifPageStart(line)) sflag = true;
				if(parse.ifPageEnd(line))   eflag = true;

				if(sflag){
					buf.append(line);
					if(eflag){
						ex.execute(new RunParse(buf.toString(),argstore,bw,parse));
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
