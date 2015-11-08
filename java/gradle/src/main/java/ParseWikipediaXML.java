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
import java.lang.Runtime;
import org.atilika.kuromoji.Token;
import org.atilika.kuromoji.Tokenizer;
import org.apache.commons.lang.*;

import org.msgpack.MessagePack;
import org.msgpack.packer.Packer;
import org.msgpack.unpacker.Unpacker;
import org.msgpack.template.Template;
import org.msgpack.MessageTypeException;
import static org.msgpack.template.Templates.tMap;
import static org.msgpack.template.Templates.TString;
import com.google.common.base.Function;
import com.google.common.collect.Lists;

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
	static boolean ngramsCollection;
	static String oftfidf;

	static void init(CommandLine cl) throws ParseException {

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

abstract class AbstParser {

	List<String> stopwords= null;

	abstract void createMapDictionary(String file) throws IOException;
	abstract String convertToBaseWord(String line);
	abstract boolean isJap();
	abstract List<String> getWordList(String text);
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
class EngParser extends AbstParser {

	private EngParser(){
		stopwords= Arrays.asList( "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",") );
	}

	private static final EngParser instance = new EngParser();
	public static EngParser getInstance(){ return instance; }

	Map<String,String> mapDict = new HashMap<String,String>();

	@Override
	boolean isJap(){ return false; }

	@Override
	void createMapDictionary(String file) throws IOException {

		try{ // try MessagePack format first

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
			}

		} catch (MessageTypeException e) {

			String line;
			try( BufferedReader br = new BufferedReader(new FileReader(file)) ){
				while((line=br.readLine())!=null){
					if(line.indexOf(";;;")>=0) continue;
					String cols[] = line.split("[ \\t]");
					if(cols[0].equals(cols[3])) continue;
					mapDict.put(cols[0],cols[3]);
				}
			} catch (IOException io){
				throw io;
			}

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

// Singleton
class JapParser extends AbstParser {

	Tokenizer tokenizer;

	private JapParser(){
		stopwords= Arrays.asList( "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ".split(",") );

		tokenizer=Tokenizer.builder().build();

	}

	private static final JapParser instance = new JapParser();
	public static JapParser getInstance(){ return instance; }

	@Override
	boolean isJap(){ return true; }

	@Override
	String convertToBaseWord(String word) {
		return word;
	}

	@Override
	void createMapDictionary(String file) { }

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
			if(ArgStore.isVerb && isJap()){
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

//class ChiParser extends AbstParser {}

class RunParser implements Runnable {

	String page;
	BufferedWriter bw;
	AbstParser parser;

	//Collection<String> listNgrams; /* list can manipulate set with sort */
	List<String> listNgrams = new ArrayList<>();

	private final static Object lock = new Object();

	public RunParser(String page, BufferedWriter bw, AbstParser parser){
		this.page= page;
		this.bw= bw;
		this.parser= parser;
	}

	boolean isTooShortWord(String word){
		if(word.length()==1) return true;
		return false;
	}

	boolean isDupInNgram(String word){
		if(listNgrams.size()==0) return false;

		//if( listNgrams instanceof Set<String> )
		if( ArgStore.preferListNgram ){
			if(listNgrams.get(listNgrams.size()-1).equals(word)){
				if(ArgStore.isVerb) System.out.printf("%s is duplicated.\n",word);
				return true;
			}
		}else{
			if(listNgrams.contains(word)){
				if(ArgStore.isVerb) System.out.printf("%s is duplicated.\n",word);
				return true;
			}
		}

		return false;
	}

	void bowCreator(String text){

		Map<String,Integer> mapbow = new HashMap<String,Integer>();

		// To remove the last one in listNgrams in case preferListNgram is not set.
		List<String> listSaveNgramsOrder = new ArrayList<>();

		int wordcnt= 0;

		int ngramcountinit;
		if(ArgStore.ngramsCollection){
			ngramcountinit=1;
		}else{
			ngramcountinit=ArgStore.ngram;
		}

		for(int ngramcnt=(ArgStore.ngramsCollection?1:ArgStore.ngram);
				ngramcnt<=ArgStore.ngram;
				ngramcnt++){

			for(String word: parser.getWordList(text)){

				if(isTooShortWord(word)) continue;
				if(!parser.isWord(word)) continue;
				if(parser.isCommonWord(word)) continue;

				if(isDupInNgram(word)) continue;

				listNgrams.add(word);
				listSaveNgramsOrder.add(word);
				if(!ArgStore.preferListNgram) Collections.sort(listNgrams);

				if(listNgrams.size()<ngramcnt) continue;
				if(listNgrams.size()>ngramcnt){
					listNgrams.remove(listSaveNgramsOrder.get(0));
					listSaveNgramsOrder.remove(0);
				}

				String ngramstr= StringUtils.join(listNgrams,":");
				String bowWord= parser.convertToBaseWord(ngramstr);

				if(mapbow.containsKey(bowWord)){
					mapbow.put(bowWord,mapbow.get(bowWord)+1);
				}else{
					mapbow.put(bowWord,1);
				}
				wordcnt+=1;

				if(ArgStore.isVerb && !parser.isJap()){
					System.out.printf("%s -> %s.\n",ngramstr,bowWord);
				}

				if(wordcnt>ArgStore.maxl) return;
			}
			if(wordcnt<ArgStore.minl) return;
		}

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
		Pattern categoryTagPattern =
			Pattern.compile("\\[\\[:*Category:([^\\[\\]\\|]+)\\|*[^\\[\\]]*\\]\\]");
		Matcher categoryTagMatcher= categoryTagPattern.matcher(page);

		boolean cmatflag= true; // no category, no check
		while(categoryTagMatcher.find()){
			cmatflag= false;
			String category= categoryTagMatcher.group(1);
			Pattern categoryPattern= Pattern.compile(ArgStore.recateg);
			Matcher categoryMatcher= categoryPattern.matcher(category);
			if(categoryMatcher.find()){ cmatflag= true; break; }
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

		try{
			CommandLine cl = basicparser.parse(options, args);
			ArgStore.init(cl);

		} catch (ParseException e){
			help.printHelp("ParseWikipediaXML",options);
			System.exit(1);
		}


		AbstParser parser;
		if(ArgStore.isJap){
			parser = JapParser.getInstance();
		}else{
			parser = EngParser.getInstance();
		}

		if(ArgStore.oftfidf.equals("")){
			outputBofw(parser);
		}else{
			inputBofwForTfidf(parser);
		}

	}

	private static void inputBofwForTfidf(AbstParser parser) {

		BufferedWriter bw = null;
		try{
			bw = new BufferedWriter(new FileWriter(ArgStore.oftfidf));
		} catch (IOException e){
			System.err.println("BufferedWriter error "+e);
			System.exit(13);
		}

		List<Integer> numTermsInDoc = new ArrayList<>();
		Map<String,Integer> numDocForTerms = new HashMap<String,Integer>();
		String line;
		int numDocInFile = 0;

		try( BufferedReader br = new BufferedReader(new FileReader(ArgStore.ofcont)) ){

			while((line=br.readLine())!=null){
				String terms[] = line.split(" ");
				int num=0;
				boolean b=false;
				for( String term : terms ){
					if(b=!b){
						numDocForTerms.putIfAbsent(term,1);
						numDocForTerms.compute(term,(k,v) -> v+1); // Require Java 8
					}else{
						num += Integer.parseInt(term);
					}
				}
				numTermsInDoc.add(num);
				numDocInFile++;
			}

		} catch (IOException e){
			System.err.println("BufferedReader error "+e);
			System.exit(111);
		}

		int cntDoc = 0;
		try( BufferedReader br = new BufferedReader(new FileReader(ArgStore.ofcont)) ){

			while((line=br.readLine())!=null){

				String terms[] = line.split(" ");
				StringBuffer bowBuf = new StringBuffer("");
				boolean isTerm=false;
				String currentTerm="";

				for( String term : terms ){

					// term
					if(isTerm=!isTerm){

						if(!term.equals(terms[0])) bowBuf.append(" ");
						bowBuf.append(term);
						bowBuf.append(" ");
						currentTerm = term;

					// number of the term
					}else{

						int numTermInDoc = Integer.parseInt(term);
						int numTermsInCurrentDoc = numTermsInDoc.get(cntDoc);
						int tf = numTermInDoc / numTermsInCurrentDoc;

						int numDocForTerm = numDocForTerms.get(currentTerm);
						int idf = (int)Math.log10(numDocInFile/numDocForTerm)+1;

						bowBuf.append(String.format("%d",tf*idf));

					}
				}
				cntDoc++;
				bowBuf.append("\n");

				try{
					bw.write(bowBuf.toString());
					bw.flush();
				} catch (IOException e){
					System.err.println("BufferedWriter error.");
					System.exit(12);
				}

			}
		} catch (IOException e){
			System.err.println("BufferedReader error "+e);
			System.exit(12);
		}

	}

	private static void outputBofw(AbstParser parser) {

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
			parser.createMapDictionary(ArgStore.ifdict);
		} catch (IOException e){
			System.exit(211);
		}

		try( BufferedReader br = new BufferedReader(new FileReader(ArgStore.ifwiki)) ){

			StringBuffer buf = new StringBuffer("");
			String line;
			boolean sflag=false, eflag=false;

			while((line=br.readLine())!=null){

				if(parser.ifPageStart(line)) sflag = true;
				if(parser.ifPageEnd(line))   eflag = true;

				if(sflag){
					buf.append(line);
					if(eflag){
						ex.execute(new RunParser(buf.toString(),bw,parser));
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
