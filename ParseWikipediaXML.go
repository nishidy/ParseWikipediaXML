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
)

func make_map(text string, minl, maxl int, dict *map[string]string) map[string]int {

	var re *regexp.Regexp

	re = regexp.MustCompile("[" + regexp.QuoteMeta("[[]]();|") + "(, )(. )( -)]")
	text = re.ReplaceAllString(text, " ")

	var words_cnt = make(map[string]int)
	re, _ = regexp.Compile("^[0-9a-z][-|0-9a-z]+$")
	words := strings.Split(text, " ")

	for _, word := range words {
		word = strings.ToLower(word)
		if re.MatchString(word) {

			if len(word) < minl || len(word) > maxl {
				continue
			}

			_, _err := (*dict)[word]
			if _err == true {
				word = (*dict)[word]
			}

			_, err := words_cnt[word]
			if err == false {
				words_cnt[word] = 1
			} else {
				words_cnt[word]++
			}
		}
	}
	return words_cnt
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

func read_dictionary(ifdict string, dict *map[string]string) {
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
		_, _err := (*dict)[splitline[0]]
		if _err == false {
			(*dict)[splitline[0]] = words[0]
		}
	}

}

func main() {

	var ifwiki = flag.String("i", "", "Input File(Wikipedia)")
	var ifdict = flag.String("d", "", "Input File(dictionary)")
	var ofcont = flag.String("s", "", "Output File(Contents)")
	var oftitle = flag.String("t", "", "Output File(Title)")
	var minl = flag.Int("m", 2, "min_word_length")
	var maxl = flag.Int("x", 256, "max_word_length")
	var minc = flag.Int("c", 2, "min_word_count")
	var cat = flag.String("g", ".*", "Category(regular expression)")

	flag.Parse()

	if len(os.Args[1:]) == 0 {

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

	cpu := runtime.NumCPU()
	//fmt.Printf("# of CPU is %d\n", cpu)
	if cpu > 1 {
		cpu--
	}

	cp := make(chan []string, cpu)
	cf := make(chan int, 1)

	ft, _ := os.Create(*oftitle)
	fc, _ := os.Create(*ofcont)
	defer ft.Close()
	defer fc.Close()

	dict := make(map[string]string)
	read_dictionary(*ifdict, &dict)

	for i := 0; i < cpu; i++ {
		go func(cp chan []string, cf chan int, minl, maxl, mic *int, ft, fc *os.File, cat *string, dict *map[string]string) {
			for {
				str := strings.Join(<-cp, "")
				var regstr string

				regstr = "<title>(.*)</title>"
				title := parse(str, regstr)

				regstr = "<text[^>]*>(.*)</text>"
				text := parse(str, regstr)

				if !category_check(*cat, text) {
					continue
				}

				cf <- 1

				ft.WriteString(fmt.Sprintf("%s\n", title))

				k := 0
				for key, val := range make_map(text, *minl, *maxl, dict) {
					if val < *minc {
						continue
					}

					if k > 0 {
						fc.WriteString(" ")
					}
					fc.WriteString(fmt.Sprintf("%s %d", key, val))
					k++
				}
				fc.WriteString("\n")

				_ = <-cf
			}
		}(cp, cf, minl, maxl, minc, ft, fc, cat, &dict)
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
}
