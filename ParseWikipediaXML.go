package main

import (
	"bufio"
	"fmt"
	"github.com/PuerkitoBio/goquery"
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
)

var stopwords string = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

func make_map(text string, dict map[string]string, stop []string) (int, map[string]int) {

	var re *regexp.Regexp

	re = regexp.MustCompile("[" + regexp.QuoteMeta("[[]]();|") + "(, )(. )( -)]")
	text = re.ReplaceAllString(text, " ")

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

			flag := false
			for _, sp := range stop {
				if sp == word {
					flag = true
					break
				}
			}
			if flag {
				continue
			}

			if _, err := dict[word]; err {
				word = dict[word]
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

func parse(str, regstr string) string {
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

func category_check(catreg, text string) bool {

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

func read_dictionary(ifdict string, dict map[string]string) {
	file, err := os.Open(ifdict)
	if err != nil {
		os.Exit(10)
	}
	defer file.Close()

	var line string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line = scanner.Text()
		splitline := strings.Split(line, " \t\t")
		if len(splitline) != 2 {
			continue
		}
		//fmt.Println(splitline[0])
		words := strings.Split(splitline[1], "\t")
		_, _err := dict[splitline[0]]
		if _err == false {
			dict[splitline[0]] = words[0]
		}
	}

}

func main() {

	var ifwiki = flag.String("i", "", "Input File(Wikipedia)")
	var ifdict = flag.String("d", "", "Input File(dictionary)")
	var ofcont = flag.String("s", "", "Output File(Contents)")
	var oftitle = flag.String("t", "", "Output File(Title)")
	var minl = flag.Int("m", 1, "Minimum number of words that a page should have")
	var maxl = flag.Int("x", 65535, "Maximum number of words that a page should have")
	var minc = flag.Int("c", 2, "Minimum number that a word should have")
	var cat = flag.String("g", ".*", "Category(regular expression)")
	var json = flag.Bool("j", false, "Generate bug-of-words in JSON format")

	flag.Parse()

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	if *ifwiki == "" {

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

		fmt.Println("Please uncompress it and give it the uncompressed file to thig program for -i option.")
		os.Exit(0)
	}

	dict := make(map[string]string)
	read_dictionary(*ifdict, dict)

	stop := make([]string, 0, 256)
	for _, word := range strings.Split(stopwords, ",") {
		stop = append(stop, word)
	}

	cpu := runtime.NumCPU()
	fmt.Printf("# of CPU is %d\n", cpu)
	if cpu > 1 {
		cpu--
	}
	runtime.GOMAXPROCS(cpu)

	cp := make(chan []string, cpu)
	//defer close(cp)

	cf := make(chan int, 1) // Use this as mutex to lock writting
	defer close(cf)

	//var m sync.Mutex // No need to lock for channel

	ft, _ := os.Create(*oftitle)
	fc, _ := os.Create(*ofcont)
	defer ft.Close()
	defer fc.Close()

	if *json {
		defer func() {
			fc.WriteString("\n]")
		}()
	}

	var numgor = runtime.NumGoroutine
	for i := 0; i < cpu; i++ {
		go func() {
			for {

				str := strings.Join(<-cp, "")
				if str == "" {
					break
				}

				var regstr string

				regstr = "<title>(.*)</title>"
				title := parse(str, regstr)

				regstr = "<text[^>]*>(.*)</text>"
				text := parse(str, regstr)

				if !category_check(*cat, text) {
					continue
				}

				var text_map = make(map[string]int)
				var wc = 0
				if wc, text_map = make_map(text, dict, stop); wc < *minl || wc > *maxl {
					continue
				}

				/* Only one thread can write the result into file at once */
				cf <- 1

				ft.WriteString(fmt.Sprintf("%s\n", title))

				if *json {
					st, err := fc.Stat()
					if err != nil {
						fmt.Println(err)
						os.Exit(11)
					}
					if st.Size() == 0 {
						fc.WriteString("[\n    { ")
					} else {
						fc.WriteString(",\n    { ")
					}
				}

				k := 0
				for key, val := range text_map {
					if val < *minc {
						continue
					}

					if k > 0 {
						if *json {
							fc.WriteString(", ")
						} else {
							fc.WriteString(" ")
						}
					}
					if *json {
						fc.WriteString(fmt.Sprintf("%s:%d", key, val))
					} else {
						fc.WriteString(fmt.Sprintf("%s %d", key, val))
					}
					k++
				}

				if *json {
					fc.WriteString(" }")
				} else {
					fc.WriteString("\n")
				}
				_ = <-cf

			}
		}()
	}

	var page []string
	page = make([]string, 0, 65535)

	var sflag, eflag bool
	var str string

	sflag = false
	eflag = false

	filein, err := os.Open(*ifwiki)
	if err != nil {
		os.Exit(1)
	}
	defer filein.Close()

	scanner := bufio.NewScanner(filein)
	for scanner.Scan() {
		str = scanner.Text()

		if strings.Contains(str, "<page>") {
			sflag = true
		}
		if strings.Contains(str, "</page>") {
			eflag = true
		}
		if sflag {
			page = append(page, str)
		}
		if eflag {
			cp <- page
			page = make([]string, 0, 65535)
			sflag = false
			eflag = false
		}
	}

	fmt.Println("Finished reading from the input file.")

	for len(cp) > 0 {
	}

	close(cp)

	for runtime.NumGoroutine() > numgor {
	}

	fmt.Println("Finished writing to the output file.")
}
