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
	"strings"
	//"sync"

	"github.com/PuerkitoBio/goquery"
	"github.com/ikawaha/kagome"
)

var stopwords string = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

type countWordType struct {
	text      string
	mapDict   map[string]string
	stopWords []string
}

func (ctype *countWordType) CountWordJp() (int, map[string]int) {

	tkn := kagome.NewTokenizer()
	morphs := tkn.Tokenize(ctype.text)

	var wc = 0
	var mapWordCnt = make(map[string]int)

	for _, m := range morphs {
		f := m.Features()
		if strings.Contains(f[0], "名詞") {
			mapWordCnt[m.Surface] = 1
		}

	}

	return wc, mapWordCnt
}

func (ctype *countWordType) CountWord() (int, map[string]int) {

	var re *regexp.Regexp

	re = regexp.MustCompile("[" + regexp.QuoteMeta("[[]]();|") + "(, )(. )( -)]")
	text := re.ReplaceAllString(ctype.text, " ")

	var words_cnt = make(map[string]int)
	re, _ = regexp.Compile("^[0-9a-z][-|0-9a-z]+$")
	words := strings.Split(text, " ")

	var wc = 0
	for _, word := range words {
		word = strings.ToLower(word)
		if re.MatchString(word) {

			if len(word) == 1 {
				continue
			}

			// Should create Any function
			flag := false
			for _, sp := range ctype.stopWords {
				if sp == word {
					flag = true
					break
				}
			}
			if flag {
				continue
			}

			if _, err := ctype.mapDict[word]; err {
				word = ctype.mapDict[word]
			}

			if _, err := words_cnt[word]; err {
				words_cnt[word]++
			} else {
				words_cnt[word] = 1
			}

			wc++
		}
	}
	return wc, words_cnt
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

func ReadDictionary(inWifiFile string, mapDict map[string]string) {

	file, err := os.Open(inWifiFile)
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

	fmt.Println("Dowload the latest list of wikipedia databases for articles.")

	re, _ := regexp.Compile("^enwiki-latest-pages-articles[^-].*bz2$")

	doc, err := goquery.NewDocument("http://dumps.wikimedia.org/enwiki/latest/")
	if err != nil {
		fmt.Println(err)
		os.Exit(10)
	}

	urls := make([]string, 0, 100)
	doc.Find("tr").Each(func(_ int, s *goquery.Selection) {
		url, _ := s.Find("a").Attr("href")
		var size string
		if re.MatchString(url) {
			//text := s.Find("a").Text()
			s.Find("td").Each(func(_ int, st *goquery.Selection) {
				class, _ := st.Attr("class")
				if class == "s" {
					size = st.Text()
					fmt.Printf("%d %s, %s\n", len(urls)+1, url, size)
					urls = append(urls, url)
				}
			})
		}

	})

	var num int
	fmt.Print("Please select one of them. : ")
	fmt.Scanf("%d", &num)
	fmt.Println(urls[num-1])

	resp, _ := http.Get("http://dumps.wikimedia.org/enwiki/latest/" + urls[num-1])
	defer resp.Body.Close()

	//cont, _ := ioutil.ReadAll(resp.Body)
	//file.Write(cont)

	//file, _ := os.OpenFile(urls[num-1], os.O_CREATE|os.O_WRONLY, 0666)

	file, _ := os.Create(urls[num-1])
	io.Copy(file, resp.Body)

	// XXX: Tried but could not read them as scanner without uncompressing the bzip2 file
	// FIXME: Uncompress and give it the arg to keep running

	// This should be automated
	fmt.Println("Please uncompress it and give it the uncompressed file to thig program for -i option.")

}

type routineType struct {
	chanData      chan []string
	chanMutex     chan int
	mapDict       map[string]string
	stopWords     []string
	hOutTitleFile *os.File
	hOutBofwFile  *os.File
}

func (rtype *routineType) ParseAndWriteRoutine(args Args) {

	for {

		str := strings.Join(<-rtype.chanData, "")
		if str == "" {
			break
		}

		var regstr string

		regstr = "<title>(.*)</title>"
		title := GetMatchWord(str, regstr)

		regstr = "<text[^>]*>(.*)</text>"
		text := GetMatchWord(str, regstr)

		if !CategoryCheck(args.matchCategory, text) {
			continue
		}

		var mapWordFreq = make(map[string]int)
		var wc = 0

		ctype := countWordType{
			text,
			rtype.mapDict,
			rtype.stopWords,
		}

		if args.isJapanese {
			if wc, mapWordFreq = ctype.CountWordJp(); wc == 0 {
				continue
			}
		} else {
			if wc, mapWordFreq = ctype.CountWord(); wc == 0 {
				continue
			}
		}

		/* Only one thread can write the result into file at once */
		rtype.chanMutex <- 1

		rtype.hOutTitleFile.WriteString(fmt.Sprintf("%s\n", title))

		if args.outFormatJson {
			st, err := rtype.hOutBofwFile.Stat()
			if err != nil {
				fmt.Println(err)
				os.Exit(11)
			}
			if st.Size() == 0 {
				rtype.hOutBofwFile.WriteString("[\n    { ")
			} else {
				rtype.hOutBofwFile.WriteString(",\n    { ")
			}
		}

		k := 0
		for word, freq := range mapWordFreq {
			if freq < args.minWord {
				continue
			}

			if k > 0 {
				if args.outFormatJson {
					rtype.hOutBofwFile.WriteString(", ")
				} else {
					rtype.hOutBofwFile.WriteString(" ")
				}
			}
			if args.outFormatJson {
				rtype.hOutBofwFile.WriteString(fmt.Sprintf("%s:%d", word, freq))
			} else {
				rtype.hOutBofwFile.WriteString(fmt.Sprintf("%s %d", word, freq))
			}
			k++
		}

		if args.outFormatJson {
			rtype.hOutBofwFile.WriteString(" }")
		} else {
			rtype.hOutBofwFile.WriteString("\n")
		}

		_ = <-rtype.chanMutex

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
}

func main() {

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	args := new(Args)

	args.inWikiFile = *(flag.String("i", "", "Input File(Wikipedia)"))
	args.inDictFile = *(flag.String("d", "", "Input File(dictionary)"))
	args.outBofwFile = *(flag.String("s", "", "Output File(Contents)"))
	args.outTitleFile = *(flag.String("t", "", "Output File(Title)"))
	args.minWordsInDoc = *(flag.Int("m", 1, "Minimum number of words that a page should have"))
	args.maxWordsInDoc = *(flag.Int("x", 65535, "Maximum number of words that a page should have"))
	args.minWord = *(flag.Int("c", 2, "Minimum number that a word should have"))
	args.matchCategory = *(flag.String("g", ".*", "Category(regular expression)"))
	args.outFormatJson = *(flag.Bool("j", false, "Generate bug-of-words in JSON format"))
	args.isJapanese = *(flag.Bool("p", false, "If this is for Japanese text"))

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
	ReadDictionary(args.inWikiFile, mapDict)

	var numgor = runtime.NumGoroutine()

	cpu := runtime.NumCPU()
	fmt.Printf("# of CPU is %d\n", cpu)
	if cpu <= numgor {
		cpu = numgor + 1
	}
	runtime.GOMAXPROCS(cpu)

	chanData := make(chan []string)
	//defer close(cp)

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

	rtype := routineType{
		chanData,
		chanMutex,
		mapDict,
		stopWords,
		hOutTitleFile,
		hOutBofwFile,
	}

	for i := 0; i < cpu; i++ {
		go rtype.ParseAndWriteRoutine(*args)
	}

	var page = make([]string, 0, 65535)
	var beginAppend, finishAppend = false, false
	var str string

	hInWikiFile, err := os.Open(args.inWikiFile)
	if err != nil {
		os.Exit(1)
	}
	defer hInWikiFile.Close()

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
			chanData <- page
			page = make([]string, 0, 65535)
			beginAppend = false
			finishAppend = false
		}
	}

	fmt.Println("Finished reading from the input file.")

	for len(chanData) > 0 {
	}

	close(chanData)

	for runtime.NumGoroutine() > numgor {
	}

	fmt.Println("Finished writing to the output file.")
}
