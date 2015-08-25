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

import org.apache.commons.lang.*;

class ArgStore {

	static String ifwiki;
	static String ifdict;
	static String ofcont;
	static String oftitle;
	static String recateg;
	static int minl;
	static int maxl;
	static int minc;
	static int ngram;
	static boolean isJap;
	static boolean isVerb;
	static boolean preferListNgram; // make ngram like list or set

	static void init(CommandLine cl) throws ParseException {

		if(cl.hasOption("j")) isJap= true;
		else isJap= false;

		if(cl.hasOption("i")) ifwiki = cl.getOptionValue("i");
		else throw new ParseException("i is not specified.");

		if(cl.hasOption("d")) ifdict = cl.getOptionValue("d");
		else if(!isJap) throw new ParseException("d is not specified.");

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

abstract class AbstParse {

	List<String> stopwords= null;

	abstract void createBaseWords(String file) throws IOException;
	abstract String convertToBaseWord(String line);
	abstract boolean isJap();
	abstract String[] getWordList(String text);
	abstract boolean isWord(String word);

	boolean isCommonWord(String word){
		if(stopwords.contains(word)) return true;
		return false;
	}

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

	Map<String,String> mapDict = new HashMap<String,String>();

	@Override
	boolean isJap(){ return false; }

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

	@Override
	String[] getWordList(String text){
		return text.split(" ");
	}

	@Override
	boolean isWord(String word){
		Pattern wpat= Pattern.compile("^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]$");
		Matcher wmat= wpat.matcher(word);
		if(wmat.find()){
			return true;
		}else{
			return false;
		}
	}

}

// Singleton
class JapParse extends AbstParse {

	Tokenizer tokenizer;

	private JapParse(){
		stopwords= Arrays.asList( "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ".split(",") );

		tokenizer=Tokenizer.builder().build();

	}

	private static final JapParse instance = new JapParse();
	public static JapParse getInstance(){ return instance; }

	@Override
	boolean isJap(){ return true; }

	@Override
	String convertToBaseWord(String word) {
		return word;
	}

	@Override
	void createBaseWords(String file) { }

	@Override
	String[] getWordList(String text){
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
			if(ArgStore.isVerb && isJap()){
				System.out.printf("%s: %s\n",token.getSurfaceForm(),token.getAllFeatures());
			}
		}
		return words.toArray(new String[words.size()]);
	}

	@Override
	boolean isWord(String word){
		return true;
	}

}

//class ChiParse extends AbstParse {

class RunParse implements Runnable {

	String page;
	BufferedWriter bw;
	AbstParse parse;

	//Collection<String> ngrams; /* list can manipulate set with sort */
	List<String> ngrams = new ArrayList<>();

	private final static Object lock = new Object();

	public RunParse(String page, BufferedWriter bw, AbstParse parse){
		this.page= page;
		this.bw= bw;
		this.parse= parse;
	}

	boolean isTooShortWord(String word){
		if(word.length()==1) return true;
		return false;
	}

	boolean isDupInNgram(String word){
		if(ngrams.size()==0) return false;

		//if( ngrams instanceof Set<String> )
		if( ArgStore.preferListNgram ){
			if(ngrams.get(ngrams.size()-1).equals(word)) return true;
		}else{
			if(ngrams.contains(word)) return true;
		}

		return false;
	}

	void bowCreator(String text){

		Map<String,Integer> mapbow = new HashMap<String,Integer>();
		List<String> ngramsorder = new ArrayList<>();

		int wordcnt= 0;
		for(String word: parse.getWordList(text)){

			if(isTooShortWord(word)) continue;
			if(!parse.isWord(word)) continue;
			if(parse.isCommonWord(word)) continue;

			if(isDupInNgram(word)) continue;

			ngrams.add(word);
			ngramsorder.add(word);
			if(!ArgStore.preferListNgram) Collections.sort(ngrams);

			if(ngrams.size()<ArgStore.ngram) continue;
			if(ngrams.size()>ArgStore.ngram){
				ngrams.remove(ngramsorder.get(0));
				ngramsorder.remove(0);
			}

			String ngramstr= StringUtils.join(ngrams,":");
			String bowWord= parse.convertToBaseWord(ngramstr);

			if(mapbow.containsKey(bowWord)){
				mapbow.put(bowWord,mapbow.get(bowWord)+1);
			}else{
				mapbow.put(bowWord,1);
			}
			wordcnt+=1;

			if(ArgStore.isVerb && !parse.isJap()){
				System.out.printf("%s -> %s.\n",ngramstr,bowWord);
			}

			if(wordcnt>ArgStore.maxl) return;
		}
		if(wordcnt<ArgStore.minl) return;

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
		options.addOption("n","n-gram",true,"Enables N-gram on N>1");

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
