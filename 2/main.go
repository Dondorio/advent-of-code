package main

import (
	"os"
	"strconv"
	"strings"
)

func check(str string) bool {
	l := len(str)
	h := l / 2

	if l%2 != 0 {
		return false
	}

	left := str[0:h]
	right := str[h:l]

	return left == right
}

func main() {
	buf, err := os.ReadFile("2/input.txt")

	if err != nil {
		return
	}

	str := string(buf)
	arr := strings.SplitSeq(str, ",")

	res := 0

	for s := range arr {
		v := strings.Split(s, "-")
		begin, _ := strconv.Atoi(v[0])
		end, _ := strconv.Atoi(v[1])

		for i := begin; i <= end; i++ {
			if check(strconv.Itoa(i)) {
				res += i
			}
		}
	}

	println(res)
}
