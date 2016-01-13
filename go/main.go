package main

import (
	"fmt"
	//pxml "github.com/nishidy/ParseWikipediaXML/go"
	pxml "ParseWikipediaXML"
)

func main() {

	a := pxml.GetOpts()
	p := pxml.NewParseType(a)

	//p.RunParseText(a)
	p.RunParseXML(a)

	t, e := pxml.NewTfIdfType(a)
	if e == nil {
		t.RunTfIdf(a)
	} else {
		fmt.Println(e)
	}

}
