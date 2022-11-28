package main

import (
	"strings"
	"testing"
)

func TestByte(t *testing.T) {
	high, low := splitByte('w')
	if high != 7 || low != 7 {
		t.Fatal("w -> 7,7")
	}
	w := mergeByte(7, 7)
	if w != 119 {
		t.Fatal("7,7 -> 119(w)")
	}
}

func TestSuccessCode(t *testing.T) {
	ss := []string{
		"what", "alice", "bob",
		"开发", "测试", "欢迎你来到这里",
		"hello world!!!\nwelcome here",
	}
	for _, s := range ss {
		en := strings.Builder{}
		de := strings.Builder{}
		encode(strings.NewReader(s), &en)
		decode(strings.NewReader(en.String()), &de)
		if s != de.String() {
			t.Fatal(s, "==", de.String())
		}
	}
}
