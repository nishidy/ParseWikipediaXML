package ParseWikipediaXML

import (
	"ParseWikipediaXML"
	"reflect"
	"testing"
)

func TestCountWordEn(t *testing.T) {

	ctype := ParseWikipediaXML.CreateCountWordType(
		"aaa bbb ccc aaa bbb aaa",
	)

	ret := ctype.CountWordEn()
	if 6 != ret {
		t.Errorf("Wrong number of words: %d.\n", ret)
	}

	expected := map[string]int{"aaa": 3, "bbb": 2, "ccc": 1}
	if reflect.DeepEqual(expected, ctype.MapWordFreq) {
		t.Errorf("Not equal %s.\n", ctype.MapWordFreq)
	}

}
