package main

import (
	"fmt"
	pxml "github.com/nishidy/ParseWikipediaXML/go"
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

	t := pxml.NewTfIdfType(args)
	t.RunTfIdf(args)

}
