package ParseWikipediaXML

import (
	"bufio"
	"encoding/json"
	"encoding/xml"
	"flag"
	"fmt"
	"gopkg.in/yaml.v2"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/PuerkitoBio/goquery"
	//"github.com/lestrrat/go-libxml2"
	"gopkg.in/redis.v3"
)

var (
	stopWordsEn string = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

	stopWordsJp string = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"
)

type entry struct {
	word string
	freq int
}

type list []entry

func (l list) Len() int {
	return len(l)
}

func (l list) Less(i, j int) bool {
	if l[i].freq != l[j].freq {
		return l[i].freq > l[j].freq
	} else {
		return l[i].word < l[j].word
	}
}

func (l list) Swap(i, j int) {
	l[i], l[j] = l[j], l[i]
}

func (l list) FilterByCnt(args Args) list {
	nlist := list{}
	for _, e := range l {
		if e.freq < args.MinWord {
			continue
		}
		nlist = append(nlist, e)
	}
	return nlist
}

func (l list) toString() string {
	var strs []string
	for i, e := range l {
		if i > 0 {
			strs = append(strs, fmt.Sprintf(" "))
		}
		strs = append(strs, fmt.Sprintf("%s %d", e.word, e.freq))
	}
	return strings.Join(strs, "")
}

type parseType struct {
	wrMtx      chan int
	stdoutMtx  chan int
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

type Args struct {
	InWikiFile    string `yaml:"input_file_wikipedia"`
	InDictFile    string `yaml:"input_file_dictionary"`
	OutBofwFile   string `yaml:"output_file_bagofwords"`
	OutTitleFile  string `yaml:"output_file_title"`
	OutTfIdfFile  string `yaml:"output_file_tfidf"`
	MinWordsInDoc int    `yaml:"minimum_count_per_doc"`
	MaxWordsInDoc int    `yaml:"maximum_count_per_doc"`
	MinWord       int    `yaml:"minimum_count_per_word"`
	MatchCategory string `yaml:"regexp_for_category"`
	OutFormatJson bool
	IsJapanese    bool `yaml:"is_japanese"`
	Workers       int  `yaml:"number_of_workers"`
}

func readDictionary(InDictFile string, baseforms map[string]string) {

	file, err := os.Open(InDictFile)
	if err != nil {
		os.Exit(10)
	}
	defer file.Close()

	m := " > Read dictionary"
	lc := 0
	pc := 0

	s := time.Now().UnixNano() / int64(time.Millisecond)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {

		fmt.Printf("%s [ # word (loaded/parsed) %d / %d ]\r", m, lc, pc)

		line := scanner.Text()
		pc++

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

		if inflect == baseform {
			continue
		}

		baseforms[inflect] = baseform
		lc++
	}

	f := time.Now().UnixNano() / int64(time.Millisecond)
	fmt.Printf("%s [ # word (loaded/parsed) %d / %d ] in %.2f sec.\n", m, lc, pc, float64(f-s)/1000.0)

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

func findIndex(arr []string, e string) (int, bool) {
	index := 0
	for _, a := range arr {
		if a == e {
			return index, true
		}
		index += 1
	}
	return -1, false
}

func GetOpts() *Args {

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	if index, flag := findIndex(os.Args, "-y"); flag {
		return GetOptsYaml(os.Args[index+1])
	} else {
		return GetOptsStdio()
	}
}

func GetOptsYaml(yaml_file string) *Args {
	buf, err := ioutil.ReadFile(yaml_file)
	if err != nil {
		panic(err)
	}

	args := new(Args)
	err = yaml.Unmarshal(buf, args)
	//args.OutFormatJson = false
	return args
}

func GetOptsStdio() *Args {

	args := new(Args)

	flag.StringVar(&args.InWikiFile, "i", "", "Input File(Wikipedia)")
	flag.StringVar(&args.InDictFile, "d", "", "Input File(dictionary)")
	flag.StringVar(&args.OutBofwFile, "s", "", "Output File(Contents)")
	flag.StringVar(&args.OutTitleFile, "t", "", "Output File(Title)")
	flag.StringVar(&args.OutTfIdfFile, "f", "", "Output File(TF-IDF)")
	flag.IntVar(&args.MinWordsInDoc, "m", 1, "Minimum number of words that a page should have")
	flag.IntVar(&args.MaxWordsInDoc, "x", 65535, "Maximum number of words that a page should have")
	flag.IntVar(&args.MinWord, "c", 1, "Minimum number that a word should have")
	flag.StringVar(&args.MatchCategory, "g", ".*", "Category(regular expression)")
	flag.BoolVar(&args.OutFormatJson, "n", false, "Generate bug-of-words in JSON format")
	flag.BoolVar(&args.IsJapanese, "j", false, "If this is for Japanese text")
	flag.IntVar(&args.Workers, "w", 1, "# of Workers")

	flag.Parse()

	if args.InWikiFile == "" {
		downloadXml()
		os.Exit(0)
	}

	runtime.GOMAXPROCS(args.Workers)

	return args

}

func (args *Args) getBaseforms() map[string]string {

	baseforms := make(map[string]string)
	if !args.IsJapanese && args.InDictFile != "" {
		readDictionary(args.InDictFile, baseforms)
	}

	return baseforms

}

func (args *Args) getStopWords() []string {

	stopWords := make([]string, 0, 256)
	if args.IsJapanese {
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

func (p *parseType) RunParseXML(args *Args) {

	defer p.closeHdrs()
	d := xml.NewDecoder(p.rdHdrWiki)

	var titleBuf []string = make([]string, 0, 65535)
	var titleFlag bool = false

	var textBuf []string = make([]string, 0, 65535)
	var textFlag bool = false

	s := time.Now().UnixNano() / int64(time.Millisecond)

	for {

		token, err := d.Token()
		if err == io.EOF {
			err = nil
			break
		}
		if err != nil {
			panic(err)
		}

		switch token.(type) {
		case xml.StartElement:
			switch token.(xml.StartElement).Name.Local {
			case "title":
				titleFlag = true
			case "text":
				textFlag = true
			}
		case xml.CharData:
			if titleFlag {
				titleBuf = append(titleBuf, string(token.(xml.CharData)))
			}
			if textFlag {
				textBuf = append(textBuf, string(token.(xml.CharData)))
			}
		case xml.EndElement:
			switch token.(xml.EndElement).Name.Local {
			case "title":
				titleFlag = false
			case "text":
				p.goSemaph <- 1
				p.wait.Add(1)
				go p.goParseXML(*args, titleBuf, textBuf)

				titleBuf = make([]string, 0, 65535)
				textBuf = make([]string, 0, 65535)
				textFlag = false
			}
		}
	}

	p.wait.Wait()

	f := time.Now().UnixNano() / int64(time.Millisecond)
	fmt.Printf(" > Parsed text [ # page %d ] in %.2f sec.\n", p.pages, float64(f-s)/1000.0)
}

func (p *parseType) RunParseText(args *Args) {

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
			go p.goParseText(*args, page)
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

func (p *parseType) goParseXML(args Args, title []string, text []string) {
	p.goParse(args, strings.Join(title, ""), strings.Join(text, ""))
}

func (p *parseType) goParseText(args Args, page []string) {
	str := strings.Join(page, "")

	title := getMatchWord(str, "<title>(.*)</title>")
	text := getMatchWord(str, "<text[^>]*>(.*)</text>")

	p.goParse(args, title, text)
}

func (p *parseType) goParse(args Args, title string, text string) {

	defer func() {
		<-p.goSemaph
		p.wait.Done()
	}()

	if !categoryCheck(args.MatchCategory, text) {
		return
	}

	ctype := countType{
		text,
		p.baseforms,
		p.stopWords,
		make(map[string]int),
	}

	var wordCount int
	if args.IsJapanese {
		wordCount = ctype.countWordJp()
		if wordCount == 0 ||
			wordCount > args.MaxWordsInDoc ||
			wordCount < args.MinWordsInDoc {
			return
		}
	} else {
		wordCount = ctype.countWordEn()
		if wordCount == 0 ||
			wordCount > args.MaxWordsInDoc ||
			wordCount < args.MinWordsInDoc {
			return
		}
	}

	var strText string
	if args.OutFormatJson {
		bs, err := json.Marshal(ctype.MapWordFreq)
		if err != nil {
			panic(err)
		}
		strText = string(bs)
	} else {
		// Put map into list{} to sort them by value
		structWordFreq := list{}
		for k, v := range ctype.MapWordFreq {
			structWordFreq = append(structWordFreq, entry{k, v})
		}
		sort.Sort(structWordFreq)
		strText = structWordFreq.FilterByCnt(args).toString()
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
		p.stdoutMtx <- 1
		fmt.Printf(" > Parsed text [ # page %d ]\r", p.pages)
		p.pages++
		<-p.stdoutMtx
	}

	setTotalNum(p.client, wordCount)
	setNum(p.client, len(ctype.MapWordFreq))

}

func NewParseType(args *Args) *parseType {

	stopWords := args.getStopWords()
	baseforms := args.getBaseforms()

	rdHdrWiki, err := os.Open(args.InWikiFile)
	if err != nil {
		fmt.Printf("The input file %s is not available.\n", args.InWikiFile)
		os.Exit(1)
	}

	wrHdrTitle, _ := os.Create(args.OutTitleFile)
	wrHdrBofw, _ := os.Create(args.OutBofwFile)

	wrMtx := make(chan int, 1)
	stdoutMtx := make(chan int, 1)
	goSemaph := make(chan int, args.Workers)

	client := getClientRedis()

	var wait sync.WaitGroup

	return &parseType{
		wrMtx,
		stdoutMtx,
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

func (p *parseType) closeHdrs() {
	p.rdHdrWiki.Close()
	p.wrHdrTitle.Close()
	p.wrHdrBofw.Close()
	close(p.wrMtx)
	close(p.goSemaph)
}
