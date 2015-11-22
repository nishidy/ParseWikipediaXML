//package ParseWikipediaXML
package main

import (
	"bufio"
	"fmt"
	"io"
	//"io/ioutil"
	//"compress/bzip2"
	"encoding/json"
	"flag"
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

type countWordType struct {
	text        string
	mapDict     map[string]string
	stopWords   []string
	MapWordFreq map[string]int
}

func CreateCountWordType(text string) countWordType {
	return countWordType{
		text,
		nil,
		nil,
		make(map[string]int),
	}
}

func (ctype *countWordType) CountWordJp() int {

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

func (ctype *countWordType) CountWordEn() int {

	var re *regexp.Regexp

	re, _ = regexp.Compile("^[a-z][a-z0-9'-]*[a-z0-9]$")

	var wc = 0
	for _, word := range strings.Split(ctype.text, " ") {
		word = strings.ToLower(word)
		if re.MatchString(word) {

			if Any(word, ctype.stopWords) {
				continue
			}

			if _, err := ctype.mapDict[word]; err {
				word = ctype.mapDict[word]
			}

			if _, err := ctype.MapWordFreq[word]; err {
				ctype.MapWordFreq[word]++
			} else {
				ctype.MapWordFreq[word] = 1
			}

			wc++
		}
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

func CategoryCheck(catreg, text string) bool {

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

func ReadDictionary(inDictFile string, mapDict map[string]string) {

	file, err := os.Open(inDictFile)
	if err != nil {
		os.Exit(10)
	}
	defer file.Close()

	var line, from, trans string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line = scanner.Text()
		splitline := strings.Split(line, " \t\t")
		if len(splitline) != 2 {
			continue
		}
		words := strings.Split(splitline[1], "\t")

		from = splitline[0]
		trans = words[0]
		mapDict[from] = trans
	}

}

func DownloadXml() {

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

type routineType struct {
	chanMutex     chan int
	chanSem       chan int
	mapDict       map[string]string
	stopWords     []string
	hOutTitleFile *os.File
	hOutBofwFile  *os.File
	wait          *sync.WaitGroup
	client        *redis.Client
}

func (rtype *routineType) ParseAndWriteRoutine(args Args, data []string) {

	defer func() {
		<-rtype.chanSem
		rtype.wait.Done()
	}()

	// strings.Join is fast enough to concat strings
	str := strings.Join(data, "")

	var regstr string
	regstr = "<title>(.*)</title>"
	title := GetMatchWord(str, regstr)

	regstr = "<text[^>]*>(.*)</text>"
	text := GetMatchWord(str, regstr)

	if !CategoryCheck(args.matchCategory, text) {
		return
	}

	ctype := countWordType{
		text,
		rtype.mapDict,
		rtype.stopWords,
		make(map[string]int),
	}

	var wordCount int
	if args.isJapanese {
		wordCount = ctype.CountWordJp()
		if wordCount == 0 ||
			wordCount > args.maxWordsInDoc ||
			wordCount < args.minWordsInDoc {
			return
		}
	} else {
		wordCount = ctype.CountWordEn()
		if wordCount == 0 ||
			wordCount > args.maxWordsInDoc ||
			wordCount < args.minWordsInDoc {
			return
		}
	}

	structWordFreq := List{}

	// Put map into List{} to sort them by value
	for k, v := range ctype.MapWordFreq {
		e := Entry{k, v}
		structWordFreq = append(structWordFreq, e)
	}

	sort.Sort(structWordFreq)

	var strText string
	if args.outFormatJson {
		byteText, _ := json.Marshal(structWordFreq)
		strText = string(byteText)
	} else {
		strText = structWordFreq.FilterByCnt(args).ToString()
	}
	strText += "\n"

	/* Only one thread can write the result into file at the same time */
	rtype.chanMutex <- 1
	rtype.hOutBofwFile.WriteString(strText)
	rtype.hOutTitleFile.WriteString(fmt.Sprintf("%s\n", title))
	<-rtype.chanMutex

	if rtype.client != nil {
		err := rtype.client.ZIncrBy("total_num", 1, strconv.Itoa(wordCount/100*100)).Err()
		if err != nil {
			panic(err)
		}
	}

	if rtype.client != nil {
		err := rtype.client.ZIncrBy("num", 1, strconv.Itoa(len(structWordFreq)/100*100)).Err()
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
	minWordsInDoc int
	maxWordsInDoc int
	minWord       int
	matchCategory string
	outFormatJson bool
	isJapanese    bool
	workers       int
}

func main() {

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	args := new(Args)

	flag.StringVar(&args.inWikiFile, "i", "", "Input File(Wikipedia)")
	flag.StringVar(&args.inDictFile, "d", "", "Input File(dictionary)")
	flag.StringVar(&args.outBofwFile, "s", "", "Output File(Contents)")
	flag.StringVar(&args.outTitleFile, "t", "", "Output File(Title)")
	flag.IntVar(&args.minWordsInDoc, "m", 1, "Minimum number of words that a page should have")
	flag.IntVar(&args.maxWordsInDoc, "x", 65535, "Maximum number of words that a page should have")
	flag.IntVar(&args.minWord, "c", 2, "Minimum number that a word should have")
	flag.StringVar(&args.matchCategory, "g", ".*", "Category(regular expression)")
	flag.BoolVar(&args.outFormatJson, "n", false, "Generate bug-of-words in JSON format")
	flag.BoolVar(&args.isJapanese, "j", false, "If this is for Japanese text")
	flag.IntVar(&args.workers, "w", 1, "# of workers")

	flag.Parse()

	if args.inWikiFile == "" {
		DownloadXml()
		os.Exit(0)
	}

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

	mapDict := make(map[string]string)
	if !args.isJapanese && args.inDictFile != "" {
		fmt.Println("Begin reading the dictionary file...")
		ReadDictionary(args.inDictFile, mapDict)
		fmt.Println("Finished.")
	}

	//cpus := runtime.NumCPU()
	//runtime.GOMAXPROCS(cpus)
	runtime.GOMAXPROCS(args.workers)

	// Mutex for write
	chanMutex := make(chan int, 1)
	defer close(chanMutex)

	hOutTitleFile, _ := os.Create(args.outTitleFile)
	hOutBofwFile, _ := os.Create(args.outBofwFile)
	defer hOutTitleFile.Close()
	defer hOutBofwFile.Close()

	var page = make([]string, 0, 65535)
	var beginAppend, finishAppend = false, false
	var str string

	hInWikiFile, err := os.Open(args.inWikiFile)
	if err != nil {
		os.Exit(1)
	}
	defer hInWikiFile.Close()

	// Semaphore for limiting # of goroutines
	chanSem := make(chan int, args.workers)
	defer close(chanSem)

	var wait sync.WaitGroup

	client := redis.NewClient(&redis.Options{
		Addr:     "localhost:6379",
		Password: "",
		DB:       0,
	})

	_, err = client.Ping().Result()
	if err != nil {
		client = nil
	}

	rtype := routineType{
		chanMutex,
		chanSem,
		mapDict,
		stopWords,
		hOutTitleFile,
		hOutBofwFile,
		&wait,
		client,
	}

	if client != nil {
		err = client.Set("start_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}

	scanner := bufio.NewScanner(hInWikiFile)
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
			chanSem <- 1
			//fmt.Printf("+:%d\n", len(chanSem))
			go rtype.ParseAndWriteRoutine(*args, page)
			wait.Add(1)
			page = make([]string, 0, 65535)
			beginAppend = false
			finishAppend = false
		}
	}

	wait.Wait()

	if client != nil {
		err = client.Set("finish_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}

}
