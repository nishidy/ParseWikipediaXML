package ParseWikipediaXML

import (
	"bufio"
	"errors"
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
)

type fentry struct {
	word  string
	tfidf float64
}

type flist []fentry

func (l flist) Len() int {
	return len(l)
}

func (l flist) Less(i, j int) bool {
	return l[i].tfidf > l[j].tfidf
}

func (l flist) Swap(i, j int) {
	l[i], l[j] = l[j], l[i]
}

type tfidfType struct {
	rdHdrBofw  *os.File
	wrHdrTfIdf *os.File
}

func (t *tfidfType) getDfCorpus() (map[string]int, int) {

	df := make(map[string]int)

	m := " > Read Bag-of-words"
	c := 0

	s := time.Now().UnixNano() / int64(time.Millisecond)

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

	f := time.Now().UnixNano() / int64(time.Millisecond)
	fmt.Printf("%s [ # page %d ] in %.2f sec.\n", m, c, float64(f-s)/1000.0)

	t.rdHdrBofw.Close()

	docs := c
	return df, docs
}

func (t *tfidfType) getTfIdf(df map[string]int, docs int) {

	m := " > Read Bag-of-words"
	c := 0

	s := time.Now().UnixNano() / int64(time.Millisecond)

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

		tfidf := flist{}
		for i, t := range terms {
			tf := float64(freqs[i]) / float64(total)
			idf := math.Log10(float64(docs)/float64(df[t])) + 1
			tfidf = append(tfidf, fentry{t, tf * idf})
		}

		sort.Sort(tfidf)
		ntfidf := t.normalize(tfidf)

		var strs []string
		for k, v := range ntfidf {
			strs = append(strs, fmt.Sprintf("%s %d", k, v))
		}
		t.wrHdrTfIdf.WriteString(strings.Join(strs, " ") + "\n")

		c++
		fmt.Printf("%s [ # page %d ]\r", m, c)
	}

	f := time.Now().UnixNano() / int64(time.Millisecond)
	fmt.Printf("%s [ # page %d ] in %.2f sec.\n", m, c, float64(f-s)/1000.0)

}

func (t *tfidfType) normalize(tfidf flist) map[string]int {

	ntfidf := make(map[string]int)
	norm := 1.0 / tfidf[len(tfidf)-1].tfidf

	for _, e := range tfidf {
		ntfidf[e.word] = int(math.Trunc(e.tfidf*norm + 0.5))
	}
	return ntfidf
}

func (t *tfidfType) RunTfIdf(args *Args) {

	df, docs := t.getDfCorpus()

	t.rdHdrBofw, _ = os.Open(args.OutBofwFile)
	t.getTfIdf(df, docs)

}

func NewTfIdfType(args *Args) (*tfidfType, error) {
	if args.OutTfIdfFile == "" {
		return nil, errors.New("File to save not specified for TF-IDF.")
	}

	rdHdrBofw, err := os.Open(args.OutBofwFile)
	if err != nil {
		os.Exit(1)
	}

	wrHdrTfIdf, _ := os.Create(args.OutTfIdfFile)

	return &tfidfType{
		rdHdrBofw,
		wrHdrTfIdf,
	}, nil
}
