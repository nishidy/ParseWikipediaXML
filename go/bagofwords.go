package ParseWikipediaXML

import (
	"regexp"
	"strings"

	"github.com/ikawaha/kagome/tokenizer"
)

type countType struct {
	text        string
	baseforms   map[string]string
	stopWords   []string
	MapWordFreq map[string]int
}

func any(w string, list []string) bool {
	for _, sp := range list {
		if sp == w {
			return true
		}
	}
	return false
}

func TestCountType(text string) countType {
	return countType{
		text,
		nil,
		nil,
		make(map[string]int),
	}
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

		if any(word, ctype.stopWords) {
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

		if any(word, ctype.stopWords) {
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

func getMatchWord(str, regstr string) string {
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
