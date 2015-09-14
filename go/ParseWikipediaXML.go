package main

import (
	"bufio"
	"fmt"
	"io"
	//"io/ioutil"
	//"compress/bzip2"
	"flag"
	"net/http"
	"os"
	"regexp"
	"runtime"
	"sort"
	"strings"
	//"sync"

	"github.com/PuerkitoBio/goquery"
	"github.com/ikawaha/kagome"
)

var stopwords string = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

type countWordType struct {
	text        string
	mapDict     map[string]string
	stopWords   []string
	mapWordFreq map[string]int
}

type Entry struct {
	word string
	freq int
}

type List []Entry

func (l List) Len() int {
	return len(l)
}

func (l List) Less(i, j int) bool {
	return l[i].freq > l[j].freq
}

func (l List) Swap(i, j int) {
	l[i], l[j] = l[j], l[i]
}

func (ctype *countWordType) CountWordJp() int {

	tkn := kagome.NewTokenizer()
	morphs := tkn.Tokenize(ctype.text)

	var wc = 0

	for _, m := range morphs {
		f := m.Features()
		if strings.Contains(f[0], "名詞") {
			ctype.mapWordFreq[m.Surface] = 1
		}

	}

	return wc
}

func Any(w string, list []string) bool {
	for _, sp := range list {
		if sp == w {
			return true
		}
	}
	return false
}

func (ctype *countWordType) CountWord() int {

	var re *regexp.Regexp

	re = regexp.MustCompile("[" + regexp.QuoteMeta("[[]]();|") + "(, )(. )( -)]")
	text := re.ReplaceAllString(ctype.text, " ")

	re, _ = regexp.Compile("^[0-9a-z][-|0-9a-z]+$")
	words := strings.Split(text, " ")

	var wc = 0
	for _, word := range words {
		word = strings.ToLower(word)
		if re.MatchString(word) {

			if len(word) == 1 {
				continue
			}

			if Any(word, ctype.stopWords) {
				continue
			}

			if _, err := ctype.mapDict[word]; err {
				word = ctype.mapDict[word]
			}

			if _, err := ctype.mapWordFreq[word]; err {
				ctype.mapWordFreq[word]++
			} else {
				ctype.mapWordFreq[word] = 1
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
		return false
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
}

func (rtype *routineType) ParseAndWriteRoutine(args Args, data []string) {

	// strings.Join is fast enough to concat strings
	str := strings.Join(data, "")
	rtype.RoutineRun(args, str)

	<-rtype.chanSem

}

func (rtype *routineType) RoutineRun(args Args, str string) {

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

	if args.isJapanese {
		if wc := ctype.CountWordJp(); wc == 0 {
			return
		}
	} else {
		if wc := ctype.CountWord(); wc == 0 {
			return
		}
	}

	structWordFreq := List{}
	for k, v := range ctype.mapWordFreq {
		e := Entry{k, v}
		structWordFreq = append(structWordFreq, e)
	}
	sort.Sort(structWordFreq)

	var listText []string

	listText = append(listText, fmt.Sprintf("%s\n", title))

	if args.outFormatJson {
		st, err := rtype.hOutBofwFile.Stat()
		if err != nil {
			fmt.Println(err)
			os.Exit(11)
		}
		if st.Size() == 0 {
			listText = append(listText, "[\n    { ")
		} else {
			listText = append(listText, (",\n    { "))
		}
	}

	k := 0
	for _, entry := range structWordFreq {

		word := entry.word
		freq := entry.freq

		if freq < args.minWord {
			continue
		}

		if k > 0 {
			if args.outFormatJson {
				listText = append(listText, (", "))
			} else {
				listText = append(listText, (" "))
			}
		}
		if args.outFormatJson {
			listText = append(listText, fmt.Sprintf("%s:%d", word, freq))
		} else {
			listText = append(listText, fmt.Sprintf("%s %d", word, freq))
		}
		k++
	}

	if args.outFormatJson {
		listText = append(listText, (" }"))
	} else {
		listText = append(listText, ("\n"))
	}

	strText := strings.Join(listText, "")

	/* Only one thread can write the result into file at the same time */
	rtype.chanMutex <- 1
	rtype.hOutBofwFile.WriteString(strText)
	<-rtype.chanMutex
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
	flag.BoolVar(&args.outFormatJson, "j", false, "Generate bug-of-words in JSON format")
	flag.BoolVar(&args.isJapanese, "p", false, "If this is for Japanese text")

	flag.Parse()

	if args.inWikiFile == "" {
		DownloadXml()
		os.Exit(0)
	}

	stopWords := make([]string, 0, 256)
	for _, word := range strings.Split(stopwords, ",") {
		stopWords = append(stopWords, word)
	}

	mapDict := make(map[string]string)
	ReadDictionary(args.inDictFile, mapDict)

	cpus := runtime.NumCPU()
	//fmt.Printf("# of CPU is %d\n", cpus)
	runtime.GOMAXPROCS(cpus)

	// Mutex for write
	chanMutex := make(chan int, 1)
	defer close(chanMutex)

	hOutTitleFile, _ := os.Create(args.outTitleFile)
	hOutBofwFile, _ := os.Create(args.outBofwFile)
	defer hOutTitleFile.Close()
	defer hOutBofwFile.Close()

	if args.outFormatJson {
		defer func() {
			hOutBofwFile.WriteString("\n]")
		}()
	}

	var page = make([]string, 0, 65535)
	var beginAppend, finishAppend = false, false
	var str string

	hInWikiFile, err := os.Open(args.inWikiFile)
	if err != nil {
		os.Exit(1)
	}
	defer hInWikiFile.Close()

	// Semaphore for limiting # of goroutines
	chanSem := make(chan int, cpus)
	defer close(chanSem)

	rtype := routineType{
		chanMutex,
		chanSem,
		mapDict,
		stopWords,
		hOutTitleFile,
		hOutBofwFile,
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
			go rtype.ParseAndWriteRoutine(*args, page)
			page = make([]string, 0, 65535)
			beginAppend = false
			finishAppend = false
		}
	}

	fmt.Println("Finished reading from the input file.")

	for len(chanSem) > 0 {
		// Spinlock
	}

	fmt.Println("Finished writing to the output file.")
}
