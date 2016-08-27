package main

import (
	"reflect"
	"sort"
	"testing"
)

func TestCountLetters(t *testing.T) {
	var cases = []struct {
		input  string
		output AnagramBase
	}{
		{
			"",
			AnagramBase{},
		},
		{
			"The quick brown fox jumps over the lazy dog!",
			AnagramBase{
				'a': 1, 'b': 1, 'c': 1, 'd': 1,
				'e': 3, 'f': 1, 'g': 1, 'h': 2,
				'i': 1, 'j': 1, 'k': 1, 'l': 1,
				'm': 1, 'n': 1, 'o': 4, 'p': 1,
				'q': 1, 'r': 2, 's': 1, 't': 2,
				'u': 2, 'v': 1, 'w': 1, 'x': 1,
				'y': 1, 'z': 1,
			},
		},
	}

	for _, c := range cases {
		output := CountLetters(c.input)
		if !reflect.DeepEqual(output, c.output) {
			t.Errorf("%q...", c.input)
			t.Errorf("\tExpected: %v", c.output)
			t.Errorf("\tGot:      %v", output)
		}
	}
}

func TestAnagramBase_Hash(t *testing.T) {
	var cases = []struct {
		a    AnagramBase
		hash string
	}{
		{
			nil,
			"",
		},
		{
			AnagramBase{},
			"",
		},
		{
			AnagramBase{'a': 1, 'b': 2, 'c': 3},
			"a1b2c3",
		},
		{
			AnagramBase{'a': 1, 'b': 2, 'c': 3, 'd': 0},
			"a1b2c3",
		},
	}

	for _, c := range cases {
		hash := c.a.Hash()
		if hash != c.hash {
			t.Errorf("%v...", c.a)
			t.Errorf("\tExpected: %q", c.hash)
			t.Errorf("\tGot:      %q", hash)
		}
	}
}

func TestAddAnagrams(t *testing.T) {
	var cases = []struct {
		a, b, r AnagramBase
	}{
		{
			nil, nil, AnagramBase{},
		},
		{
			nil, AnagramBase{}, AnagramBase{},
		},
		{
			AnagramBase{}, AnagramBase{}, AnagramBase{},
		},
		{
			AnagramBase{'a': 2, 'b': 3},
			AnagramBase{'a': 1, 'c': 4},
			AnagramBase{'a': 3, 'b': 3, 'c': 4},
		},
	}

	for _, c := range cases {
		var r AnagramBase

		r = AddAnagrams(c.a, c.b)
		if !reflect.DeepEqual(r, c.r) {
			t.Errorf("%v + %v...", c.a, c.b)
			t.Errorf("\tExpected: %v", c.r)
			t.Errorf("\tGot:      %v", r)
		}

		r = AddAnagrams(c.b, c.a)
		if !reflect.DeepEqual(r, c.r) {
			t.Errorf("%v + %v...", c.b, c.a)
			t.Errorf("\tExpected: %v", c.r)
			t.Errorf("\tGot:      %v", r)
		}
	}
}

func TestAnagramEquals(t *testing.T) {
	var cases = []struct {
		a, b AnagramBase
		eq   bool
	}{
		{
			nil, nil, true,
		},
		{
			nil, AnagramBase{}, true,
		},
		{
			AnagramBase{}, AnagramBase{}, true,
		},
		{
			AnagramBase{'a': 2, 'b': 3},
			AnagramBase{'a': 1, 'c': 4},
			false,
		},
		{
			AnagramBase{'a': 2, 'b': 3},
			AnagramBase{'a': 1, 'b': 4},
			false,
		},
		{
			AnagramBase{'a': 1, 'b': 3},
			AnagramBase{'a': 1, 'b': 3},
			true,
		},
	}

	for _, c := range cases {
		var eq bool

		eq = AnagramEqual(c.a, c.b)
		if eq != c.eq {
			t.Errorf("%v == %v...", c.a, c.b)
			t.Errorf("\tExpected: %v", c.eq)
			t.Errorf("\tGot:      %v", eq)
		}

		eq = AnagramEqual(c.b, c.a)
		if eq != c.eq {
			t.Errorf("%v == %v...", c.b, c.a)
			t.Errorf("\tExpected: %v", c.eq)
			t.Errorf("\tGot:      %v", eq)
		}
	}
}

func TestAnagramListEquals(t *testing.T) {
	var cases = []struct {
		as []AnagramBase
		b  AnagramBase
		eq bool
	}{
		{
			nil, nil, true,
		},
		{
			nil, AnagramBase{}, true,
		},
		{
			[]AnagramBase{
				AnagramBase{'b': 3},
				AnagramBase{'a': 2},
			},
			AnagramBase{'a': 2, 'b': 3},
			true,
		},
		{
			[]AnagramBase{
				AnagramBase{'c': 1},
				AnagramBase{'a': 1, 'c': 1},
				AnagramBase{'c': 1},
			},
			AnagramBase{'a': 1, 'c': 4},
			false,
		},
		{
			[]AnagramBase{
				AnagramBase{'a': 1, 'b': 2},
				AnagramBase{'d': 1},
				AnagramBase{'b': 1},
			},
			AnagramBase{'a': 1, 'b': 3, 'd': 1},
			true,
		},
		{
			[]AnagramBase{
				AnagramBase{'w': 1, 'd': 1},
			},
			AnagramBase{'w': 1, 'o': 1, 'r': 1, 'd': 1},
			false,
		},
	}

	for _, c := range cases {
		var eq bool

		eq = AnagramSumEqual(c.as, c.b)
		if eq != c.eq {
			t.Errorf("%v == %v...", c.as, c.b)
			t.Errorf("\tExpected: %v", c.eq)
			t.Errorf("\tGot:      %v", eq)
		}
	}
}

func TestAnagramExceedsLimit(t *testing.T) {
	var cases = []struct {
		a, limit AnagramBase
		ex       bool
	}{
		{
			nil, nil, false,
		},
		{
			nil, AnagramBase{}, false,
		},
		{
			AnagramBase{}, AnagramBase{}, false,
		},
		{
			AnagramBase{'a': 2, 'b': 3},
			AnagramBase{'a': 1, 'c': 4},
			true,
		},
		{
			AnagramBase{'a': 1, 'c': 3},
			AnagramBase{'a': 1, 'c': 4},
			false,
		},
		{
			AnagramBase{'a': 1, 'b': 3, 'd': 1},
			AnagramBase{'a': 1, 'c': 4},
			true,
		},
	}

	for _, c := range cases {
		var ex bool

		ex = AnagramExceedsLimit(c.a, c.limit)
		if ex != c.ex {
			t.Errorf("%v > %v...", c.a, c.limit)
			t.Errorf("\tExpected: %v", c.ex)
			t.Errorf("\tGot:      %v", ex)
		}
	}
}

func TestAnagramListExceedsLimit(t *testing.T) {
	var cases = []struct {
		as    []AnagramBase
		limit AnagramBase
		ex    bool
	}{
		{
			nil, nil, false,
		},
		{
			nil, AnagramBase{}, false,
		},
		{
			[]AnagramBase{
				AnagramBase{'b': 3},
				AnagramBase{'a': 2},
			},
			AnagramBase{'a': 1, 'c': 4},
			true,
		},
		{
			[]AnagramBase{
				AnagramBase{'c': 1},
				AnagramBase{'a': 1, 'c': 1},
				AnagramBase{'c': 1},
			},
			AnagramBase{'a': 1, 'c': 4},
			false,
		},
		{
			[]AnagramBase{
				AnagramBase{'a': 1, 'b': 2},
				AnagramBase{'d': 1},
				AnagramBase{'b': 1},
			},
			AnagramBase{'a': 1, 'c': 4},
			true,
		},
	}

	for _, c := range cases {
		var ex bool

		ex = AnagramListExceedsLimit(c.as, c.limit)
		if ex != c.ex {
			t.Errorf("%v > %v...", c.as, c.limit)
			t.Errorf("\tExpected: %v", c.ex)
			t.Errorf("\tGot:      %v", ex)
		}
	}
}

func TestDictionary_Add(t *testing.T) {
	var cases = []struct {
		words []string
		dict  Dictionary
	}{
		{
			[]string{"a", "b", "c"},
			Dictionary{
				"a1": DictItem{
					Anagram: AnagramBase{'a': 1},
					Words:   []string{"a"},
				},
				"b1": DictItem{
					Anagram: AnagramBase{'b': 1},
					Words:   []string{"b"},
				},
				"c1": DictItem{
					Anagram: AnagramBase{'c': 1},
					Words:   []string{"c"},
				},
			},
		},

		{
			[]string{"HOOT", "Tho", "thoo"},
			Dictionary{
				"h1o2t1": DictItem{
					Anagram: AnagramBase{'h': 1, 'o': 2, 't': 1},
					Words:   []string{"HOOT", "thoo"},
				},
				"h1o1t1": DictItem{
					Anagram: AnagramBase{'h': 1, 'o': 1, 't': 1},
					Words:   []string{"Tho"},
				},
			},
		},
	}

	for _, c := range cases {
		dict := Dictionary{}

		for _, word := range c.words {
			dict.Add(word)
		}

		if !reflect.DeepEqual(dict, c.dict) {
			t.Errorf("%v...", c.words)
			t.Errorf("\tExpected: %v", c.dict)
			t.Errorf("\tGot:      %v", dict)
		}
	}
}

func TestFindAllAnagramsForLine(t *testing.T) {
	var cases = []struct {
		l        string
		dict     Dictionary
		anagrams []string
	}{
		{
			"", Dictionary{}, []string{},
		},

		{
			"word",
			Dictionary{
				"d1o1r1w1": DictItem{
					Anagram: AnagramBase{'d': 1, 'o': 1, 'r': 1, 'w': 1},
					Words:   []string{"dorw", "oWtd", "WORD"},
				},
				"d1w1": DictItem{
					Anagram: AnagramBase{'d': 1, 'w': 1},
					Words:   []string{"WD", "dw"},
				},
				"o1r1": DictItem{
					Anagram: AnagramBase{'o': 1, 'r': 1},
					Words:   []string{"or"},
				},
				"a2o1r1": DictItem{
					Anagram: AnagramBase{'a': 2, 'o': 1, 'r': 1},
					Words:   []string{"Aora"},
				},
			},
			[]string{
				"Dorw", "Owtd",
				"Or Wd", "Dw Or",
			},
		},
	}

	for _, c := range cases {
		anagrams := FindAllAnagramsForLine(c.l, c.dict)

		sort.Strings(anagrams)
		sort.Strings(c.anagrams)

		if !reflect.DeepEqual(anagrams, c.anagrams) {
			t.Errorf("%q...", c.l)
			t.Errorf("\tExpected: %q", c.anagrams)
			t.Errorf("\tGot:      %q", anagrams)
		}
	}
}

func BenchmarkFindAllAnagramsForLine_100words(b *testing.B) {
	line := "Field of dreams"
	dict := Dictionary{}

	// 100 words
	words := []string{
		"angry", "appear", "at", "bankings", "beaters", "beautiful", "bedded", "bending", "bored", "bothered",
		"breathings", "buy", "catcher", "choosing", "cleanest", "confuses", "controllers", "covered", "cried", "dreamed",
		"dressing", "eastest", "empty", "engine", "entire", "fail", "father", "fees", "followings", "foods",
		"gates", "greener", "grew", "guess", "gunned", "hand", "heaters", "heats", "if", "information",
		"keeps", "killer", "languages", "larger", "lining", "marking", "my", "narrowest", "needed", "newest",
		"oh", "or", "over", "parted", "perfecting", "piled", "piles", "plays", "possible", "pulled",
		"pushes", "recognizers", "repeat", "replier", "riders", "rounders", "sanded", "scaring", "sell", "shitting",
		"sign", "softer", "soldiering", "somebodies", "sorting", "sounding", "staying", "steps", "story", "stuffed",
		"stuffing", "talked", "telling", "thanks", "three", "throwing", "told", "understandings", "until", "viewed",
		"visitors", "watched", "waving", "wed", "west", "whatever", "whether", "whispered", "wild", "wings",
	}
	for _, w := range words {
		dict.Add(w)
	}

	for i := 0; i < b.N; i++ {
		ans := FindAllAnagramsForLine(line, dict)
		b.Logf("%v anagrams found", len(ans))
	}
}
