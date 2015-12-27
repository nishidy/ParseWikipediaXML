package main

import (
	"fmt"
	//pxml "github.com/nishidy/ParseWikipediaXML/go"
	pxml "ParseWikipediaXML"
	"os"
)

func main() {

	if len(os.Args[1:]) == 0 {
		fmt.Println("Run with -h to show help.")
		os.Exit(1)
	}

	args := pxml.GetOpts()

	p := pxml.NewParseType(args)
	p.RunParse(args)

	t, err := pxml.NewTfIdfType(args)
	if err == nil {
		t.RunTfIdf(args)
	}

}
