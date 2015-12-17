package ParseWikipediaXML

import (
	"strconv"
	"time"

	"gopkg.in/redis.v3"
)

func getClientRedis() *redis.Client {

	client := redis.NewClient(&redis.Options{
		Addr:     "localhost:6379",
		Password: "",
		DB:       0,
	})

	_, err := client.Ping().Result()
	if err != nil {
		client = nil
	}

	return client
}

func setTotalNum(client *redis.Client, total_num int) {
	if client != nil {
		err := client.ZIncrBy("total_num", 1, strconv.Itoa(total_num/100*100)).Err()
		if err != nil {
			panic(err)
		}
	}
}

func setNum(client *redis.Client, num int) {
	if client != nil {
		err := client.ZIncrBy("num", 1, strconv.Itoa(num/100*100)).Err()
		if err != nil {
			panic(err)
		}
	}
}

func setStartTime(client *redis.Client) {
	if client != nil {
		err := client.Set("start_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}
}

func setFinishTime(client *redis.Client) {
	if client != nil {
		err := client.Set("finish_time", time.Now().UnixNano()/int64(time.Millisecond), 0).Err()
		if err != nil {
			panic(err)
		}
	}
}
