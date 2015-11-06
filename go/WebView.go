package main

import (
	"fmt"
	"gopkg.in/redis.v3"
	"net/http"
	"text/template"
)

type Report struct {
	Pages      int64
	Duration   float64
	Throughput float64
	Total_num  []redis.Z
	Num        []redis.Z
}

func handler(w http.ResponseWriter, r *http.Request) {
	client := redis.NewClient(&redis.Options{
		Addr:     "localhost:6379",
		Password: "",
		DB:       0,
	})

	pong, err := client.Ping().Result()
	if err != nil {
		fmt.Println(pong)
		panic(err)
	}

	start_time, err := client.Get("start_time").Int64()
	if err != nil {
		start_time = 0
	}

	finish_time, err := client.Get("finish_time").Int64()
	if err != nil {
		finish_time = 0
	}

	total_num := client.ZRevRangeWithScores("total_num", 0, -1)
	num := client.ZRevRangeWithScores("num", 0, -1)

	var pages int64 = 0
	for _, z := range total_num.Val() {
		pages += int64(z.Score)
	}

	duration := float64(finish_time-start_time) / 1000.0

	report := Report{
		Pages:      pages,
		Duration:   duration,
		Throughput: float64(pages) / duration,
		Total_num:  total_num.Val(),
		Num:        num.Val(),
	}

	tmpl := template.Must(template.ParseFiles("views/index.tpl"))
	err = tmpl.Execute(w, report)
	if err != nil {
		panic(err)
	}
}

func main() {
	http.HandleFunc("/", handler)
	http.ListenAndServe(":8080", nil)
}
