package ParseWikipediaXML

import (
	pxml "github.com/nishidy/ParseWikipediaXML/go"
	"reflect"
	"testing"
)

func TestCountWordEn(t *testing.T) {

	c := pxml.TestCountType(
		"aaa bbb ccc aaa bbb aaa",
	)

	ret := c.countWordEn()
	if 6 != ret {
		t.Errorf("Wrong number of words: %d.\n", ret)
	}

	expected := map[string]int{"aaa": 3, "bbb": 2, "ccc": 1}
	if reflect.DeepEqual(expected, c.MapWordFreq) {
		t.Errorf("Not equal %s.\n", c.MapWordFreq)
	}

}
