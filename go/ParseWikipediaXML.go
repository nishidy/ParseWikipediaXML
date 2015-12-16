//package ParseWikipediaXML
package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"math"
	"net/http"
	"os"
	"regexp"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/PuerkitoBio/goquery"
	"github.com/ikawaha/kagome/tokenizer"
	"gopkg.in/redis.v3"
)

var (
	stopWordsEn string = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

	stopWordsJp string = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"
)

type Entry struct {
	word string
	freq int
}

type List []Entry

func (l List) Len() int {
	return len(l)
}

func (l List) Less(i, j int) bool {
	if l[i].freq != l[j].freq {
		return l[i].freq > l[j].freq
	} else {
		return l[i].word < l[j].word
	}
}

func (l List) Swap(i, j int) {
	l[i], l[j] = l[j], l[i]
}

func (l List) FilterByCnt(args Args) List {
	nlist := List{}
	for _, e := range l {
		if e.freq < args.minWord {
			continue
		}
		nlist = append(nlist, e)
	}
	return nlist
}

func (l List) ToString() string {
	var strs []string
	for i, entry := range l {
		if i > 0 {
			strs = append(strs, fmt.Sprintf(" "))
		}
		strs = append(strs, fmt.Sprintf("%s %d", entry.word, entry.freq))
	}
	return strings.Join(strs, "")
}

func Any(w string, list []string) bool {
	for _, sp := range list {
		if sp == w {
			return true
		}
	}
	return false
}

type countType struct {
	text        string
	baseforms   map[string]string
	stopWords   []string
	MapWordFreq map[string]int
}

func (ctype *countType) countWordJp() int {

	tkn := tokenizer.New()
	morphs := tkn.Tokenize(ctype.text)

	var wc = 0

	re1 := regexp.MustCompile("^[" + regexp.QuoteMeta("-[]{}()|") + ",.*+=_:;~!@#$%^&?`'/]+$")
	re2 := regexp.MustCompile("^[（）｛｝「」『』［］、。]+$")

	for _, m := range morphs {

		word := m.Surface
		if word == "EOS" || word == "BOS" || re1.MatchString(word) || re2.MatchString(word) {
			continue
		}

		//fmt.Printf("%s %v\n", m.Surface, m.Features())
		f := m.Features()
		class := f[0]
		if strings.Contains(class, "名詞") ||
			strings.Contains(class, "形容詞") ||
			strings.Contains(class, "動詞") ||
			strings.Contains(class, "副詞") {
		} else {
			continue
		}

		if Any(word, ctype.stopWords) {
			continue
		}

		if _, err := ctype.MapWordFreq[word]; err {
			ctype.MapWordFreq[word]++
		} else {
			ctype.MapWordFreq[word] = 1
		}

		wc++
	}

	return wc
}

func (ctype *countType) countWordEn() int {

	var re *regexp.Regexp

	re, _ = regexp.Compile("^[a-z][a-z0-9'-]*[a-z0-9]$")

	var wc = 0
	for _, word := range strings.Split(ctype.text, " ") {
		word = strings.ToLower(word)

		if !re.MatchString(word) {
			continue
		}

		if Any(word, ctype.stopWords) {
			continue
		}

		if _, err := ctype.baseforms[word]; err {
			word = ctype.baseforms[word]
		}

		if _, err := ctype.MapWordFreq[word]; err {
			ctype.MapWordFreq[word]++
		} else {
			ctype.MapWordFreq[word] = 1
		}

		wc++
	}
	return wc
}

func GetMatchWord(str, regstr string) string {
	re, err := regexp.Compile(regstr)
	if err != nil {
		panic(err)
	}
	ret := re.FindStringSubmatch(str)
	var res string
	switch len(ret) {
	case 0:
		res = ""
	case 1:
		res = ret[0]
	case 2:
		res = ret[1]
	}
	return res
}

func categoryCheck(catreg, text string) bool {

	var re *regexp.Regexp

	re, _ = regexp.Compile("\\[\\[\\:*Category:([^\\[\\]]+)\\|*[^\\[\\]]*\\]\\]")
	ret := re.FindStringSubmatch(text)
	var cat string
	switch len(ret) {
	case 2:
		cat = ret[1]
	default:
		// no category, no check
		return true
	}

	re, _ = regexp.Compile(catreg)
	if re.MatchString(cat) {
		return true
	} else {
		return false
	}

}

func readDictionary(inDictFile string, baseforms map[string]string) {

	file, err := os.Open(inDictFile)
	if err != nil {
		os.Exit(10)
	}
	defer file.Close()

	m := " > Read dictionary"
	c := 0

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		splitline := strings.Split(line, " \t\t")
		if len(splitline) != 2 {
			continue
		}

		inflect := splitline[0]

		words := strings.Split(splitline[1], "\t")
		baseform := strings.Trim(words[0], " ")

		if strings.Index(baseform, " ") > -1 {
			continue
		}

		baseforms[inflect] = baseform

		c++
		fmt.Printf("%s [ # word %d ]\r", m, c)
	}

	fmt.Printf("%s [ # word %d ]\n", m, c)

}

func downloadXml() {

	fmt.Println("Dowloading the latest list of Wikipedia DB of articles...")

	re, _ := regexp.Compile("^enwiki-latest-pages-articles[^-].*bz2$")

	doc, err := goquery.NewDocument("http://dumps.wikimedia.org/enwiki/latest/")
	if err != nil {
		fmt.Println(err)
		os.Exit(10)
	}

	urls := make([]string, 0, 100)
	doc.Find("a").Each(func(_ int, s *goquery.Selection) {
		url, _ := s.Attr("href")
		if re.MatchString(url) {
			fmt.Printf("%d: %s\n", len(urls)+1, url)
			urls = append(urls, url)
		}

	})

	var num int
	fmt.Print("Please select one of them. : ")
	fmt.Scanf("%d", &num)
	fmt.Println(urls[num-1])

	resp, _ := http.Get("http://dumps.wikimedia.org/enwiki/latest/" + urls[num-1])
	defer resp.Body.Close()

	file, _ := os.Create(urls[num-1])
	io.Copy(file, resp.Body)

	fmt.Println("Download finished.")

}

type parseType struct {
	wrMtx      chan int
	stdioMtx   chan int
	goSemaph   chan int
	baseforms  map[string]string
	stopWords  []string
	rdHdrWiki  *os.File
	wrHdrTitle *os.File
	wrHdrBofw  *os.File
	wait       *sync.WaitGroup
	client     *redis.Client
	pages      int
}

func (p *parseType) goParse(args Args, data []string) {

	defer func() {
		<-p.goSemaph
		p.wait.Done()
	}()

	// strings.Join is fast enough to concat strings
	str := strings.Join(data, "")

	title := GetMatchWord(str, "<title>(.*)</title>")
	text := GetMatchWord(str, "<text[^>]*>(.*)</text>")

	if !categoryCheck(args.matchCategory, text) {
		return
	}

	ctype := countType{
		text,
		p.baseforms,
		p.stopWords,
		make(map[string]int),
	}

	var wordCount int
	if args.isJapanese {
		wordCount = ctype.countWordJp()
		if wordCount == 0 ||
			wordCount > args.maxWordsInDoc ||
			wordCount < args.minWordsInDoc {
			return
		}
	} else {
		wordCount = ctype.countWordEn()
		if wordCount == 0 ||
			wordCount > args.maxWordsInDoc ||
			wordCount < args.minWordsInDoc {
			return
		}
	}

	var strText string
	if args.outFormatJson {
		bs, err := json.Marshal(ctype.MapWordFreq)
		if err != nil {
			panic(err)
		}
		strText = string(bs)
	} else {
		// Put map into List{} to sort them by value
		structWordFreq := List{}
		for k, v := range ctype.MapWordFreq {
			e := Entry{k, v}
			structWordFreq = append(structWordFreq, e)
		}
		sort.Sort(structWordFreq)
		strText = structWordFreq.FilterByCnt(args).ToString()
	}

	if len(strText) > 0 {
		strText += "\n"

		/* Only one thread can write the result into file at the same time */
		p.wrMtx <- 1
		p.wrHdrBofw.WriteString(strText)
		p.wrHdrTitle.WriteString(fmt.Sprintf("%s\n", title))
		<-p.wrMtx

	}

	{
		p.stdioMtx <- 1
		fmt.Printf(" > Parsed text [ # page %d ]\r", p.pages)
		p.pages++
		<-p.stdioMtx
	}

	setTotalNum(p.client, wordCount)
	setNum(p.client, len(ctype.MapWordFreq))

}

func setTotalNum(client *redis.Client, total_num int) {
	if client != nil {
		err := client.ZIncrBy("total_num", 1, strconv.Itoa(total_num/100*100)).Err()
		if err != nil {
			panic(err)
		}
	}
}

func setNum(client *redis.Client, num int) {
	if client != nil {
		err := client.ZIncrBy("num", 1, strconv.Itoa(num/100*100)).Err()
		if err != nil {
			panic(err)
		}
	}
}

type Args struct {
	inWikiFile    string
	inDictFile    string
	outBofwFile   string
	outTitleFile  string
	outTfIdfFile  string
	minWordsInDoc int
	maxWordsInDoc int
	minWord       int
	matchCategory string
	outFormatJson bool
	isJapanese    bool
	workers       int
}

func getOpts() *Args {

	args := new(Args)

	flag.StringVar(&args.inWikiFile, "i", "", "Input File(Wikipedia)")
	flag.StringVar(&args.inDictFile, "d", "", "Input File(dictionary)")
	flag.StringVar(&args.outBofwFile, "s", "", "Output File(Contents)")
	flag.StringVar(&args.outTitleFile, "t", "", "Output File(Title)")
	flag.StringVar(&args.outTfIdfFile, "f", "", "Output File(TF-IDF)")
	flag.IntVar(&args.minWordsInDoc, "m", 1, "Minimum number of words that a page should have")
	flag.IntVar(&args.maxWordsInDoc, "x", 65535, "Maximum number of words that a page should have")
	flag.IntVar(&args.minWord, "c", 1, "Minimum number that a word should have")
	flag.StringVar(&args.matchCategory, "g", ".*", "Category(regular expression)")
	flag.BoolVar(&args.outFormatJson, "n", false, "Generate bug-of-words in JSON format")
	flag.BoolVar(&args.isJapanese, "j", false, "If this is for Japanese text")
	flag.IntVar(&args.workers, "w", 1, "# of workers")

	flag.Parse()

	if args.inWikiFile == "" {
		downloadXml()
		os.Exit(0)
	}

	runtime.GOMAXPROCS(args.workers)

	return args

}

func (args *Args) getBaseforms() map[string]string {

	baseforms := make(map[string]string)
	if !args.isJapanese && args.inDictFile != "" {
		s := time.Now().UnixNano() / int64(time.Millisecond)
		readDictionary(args.inDictFile, baseforms)
		f := time.Now().UnixNano() / int64(time.Millisecond)
		fmt.Printf(" > Read dictionary in %.2f sec.\n", float64(f-s)/1000.0)
	}

	return baseforms

}

func (args *Args) getStopWords() []string {

	stopWords := make([]string, 0, 256)
	if args.isJapanese {
		for _, word := range strings.Split(stopWordsJp, ",") {
			stopWords = append(stopWords, word)
		}
	} else {
		for _, word := range strings.Split(stopWordsEn, ",") {
			stopWords = append(stopWords, word)
		}
	}

	return stopWords
}

func getClientRedis() *redis.Client {

	client := redis.NewClient(&redis.Options{
		Addr:     "localhost:6379",
		Password: "",
		DB:       0,
	})

	_, err := client.Ping().Result()
	if err != nil {
		client = nil
	}

	return client
}

func setStartTime(client *redis.Client) {
	if client != nil {
		err := client.Set("start_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}
}

func setFinishTime(client *redis.Client) {
	if client != nil {
		err := client.Set("finish_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}
}

func (t *tfidfType) runTfIdf(args *Args) {

	df := make(map[string]int)

	m := " > Read Bag-of-words"
	c := 0

	scanner := bufio.NewScanner(t.rdHdrBofw)
	for scanner.Scan() {
		line := scanner.Text()

		splitline := strings.Split(line, " ")
		if len(splitline)%2 == 1 {
			fmt.Fprintln(os.Stderr, "Wrong Bag-Of-Word format.")
			os.Exit(10)
		}

		terms := func(ss []string) []string {
			var lss []string
			for i, s := range ss {
				if i%2 == 0 {
					lss = append(lss, s)
				}
			}
			return lss
		}(splitline)

		for _, t := range terms {
			if _, err := df[t]; err {
				df[t]++
			} else {
				df[t] = 1
			}
		}

		c++
		fmt.Printf("%s [ # page %d ]\r", m, c)
	}
	fmt.Printf("%s [ # page %d ]\n", m, c)

	t.rdHdrBofw.Close()

	docs := c
	t.rdHdrBofw, _ = os.Open(args.outBofwFile)

	c = 0
	scanner = bufio.NewScanner(t.rdHdrBofw)
	for scanner.Scan() {
		line := scanner.Text()

		splitline := strings.Split(line, " ")
		if len(splitline)%2 == 1 {
			fmt.Fprintln(os.Stderr, "Wrong Bag-Of-Word format.")
			os.Exit(10)
		}

		terms := func(ss []string) []string {
			var lss []string
			for i, s := range ss {
				if i%2 == 0 {
					lss = append(lss, s)
				}
			}
			return lss
		}(splitline)

		freqs, total := func(ss []string) ([]int, int) {
			var lss []int
			var sum int = 0
			for i, s := range ss {
				if i%2 == 1 {
					si, _ := strconv.Atoi(s)
					lss = append(lss, si)
					sum += si
				}
			}
			return lss, sum
		}(splitline)

		var strs []string
		for i, t := range terms {
			tf := float64(freqs[i]) / float64(total)
			idf := math.Log10(float64(docs)/float64(df[t])) + 1
			strs = append(strs, fmt.Sprintf("%s %.3f", t, tf*idf))
		}

		t.wrHdrTfIdf.WriteString(strings.Join(strs, " ") + "\n")

		c++
		fmt.Printf("%s [ # page %d ]\r", m, c)
	}
	fmt.Printf("%s [ # page %d ]\n", m, c)
}

func (p *parseType) runParse(args *Args) {

	defer p.closeHdrs()

	var page = make([]string, 0, 65535)
	var beginAppend, finishAppend = false, false
	var str string

	setStartTime(p.client)
	s := time.Now().UnixNano() / int64(time.Millisecond)

	scanner := bufio.NewScanner(p.rdHdrWiki)
	for scanner.Scan() {
		str = scanner.Text()

		if strings.Contains(str, "<page>") {
			beginAppend = true
		}
		if strings.Contains(str, "</page>") {
			finishAppend = true
		}
		if beginAppend {
			page = append(page, str)
		}
		if finishAppend {
			p.goSemaph <- 1
			//fmt.Printf("+:%d\n", len(goSemaph))
			p.wait.Add(1)
			go p.goParse(*args, page)
			page = make([]string, 0, 65535)
			beginAppend = false
			finishAppend = false
		}
	}

	p.wait.Wait()

	f := time.Now().UnixNano() / int64(time.Millisecond)
	fmt.Printf(" > Parsed text [ # page %d ] in %.2f sec.\n", p.pages, float64(f-s)/1000.0)

	setFinishTime(p.client)
}

func NewParseType(args *Args) *parseType {

	stopWords := args.getStopWords()
	baseforms := args.getBaseforms()

	rdHdrWiki, err := os.Open(args.inWikiFile)
	if err != nil {
		os.Exit(1)
	}

	wrHdrTitle, _ := os.Create(args.outTitleFile)
	wrHdrBofw, _ := os.Create(args.outBofwFile)

	wrMtx := make(chan int, 1)
	stdioMtx := make(chan int, 1)
	goSemaph := make(chan int, args.workers)

	client := getClientRedis()

	var wait sync.WaitGroup

	return &parseType{
		wrMtx,
		stdioMtx,
		goSemaph,
		baseforms,
		stopWords,
		rdHdrWiki,
		wrHdrTitle,
		wrHdrBofw,
		&wait,
		client,
		0,
	}
}

type tfidfType struct {
	rdHdrBofw  *os.File
	wrHdrTfIdf *os.File
}

func NewTfIdfType(args *Args) *tfidfType {
	rdHdrBofw, err := os.Open(args.outBofwFile)
	if err != nil {
		os.Exit(1)
	}

	wrHdrTfIdf, _ := os.Create(args.outTfIdfFile)

	return &tfidfType{
		rdHdrBofw,
		wrHdrTfIdf,
	}
}

func (p *parseType) closeHdrs() {
	p.rdHdrWiki.Close()
	p.wrHdrTitle.Close()
	p.wrHdrBofw.Close()
	close(p.wrMtx)
	close(p.goSemaph)
}

func main() {

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	args := getOpts()

	p := NewParseType(args)
	p.runParse(args)

	t := NewTfIdfType(args)
	t.runTfIdf(args)

}
