package main

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"runtime/pprof"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

// AnagramBase contains amount of each English letter in a word or sentence.
// All the letters in the map should be lowercase.
type AnagramBase map[rune]int

func (a AnagramBase) String() string {
	// Using `int` instead of `rune`, to sort with `sort.Ints`
	keys := []int{}
	for rn := range a {
		keys = append(keys, int(rn))
	}
	sort.Ints(keys)

	items := []string{}
	for _, key := range keys {
		rn := rune(key)
		it := fmt.Sprintf("%c:%d", rn, a[rn])
		items = append(items, it)
	}
	return fmt.Sprintf("[%s]", strings.Join(items, " "))
}

// Hash returns a string representation of an anagrams, which can be used
// for comparison
func (a AnagramBase) Hash() string {
	// Using `int` instead of `rune`, to sort with `sort.Ints`
	keys := []int{}
	for rn := range a {
		keys = append(keys, int(rn))
	}
	sort.Ints(keys)

	items := []string{}
	for _, key := range keys {
		rn := rune(key)
		if a[rn] == 0 {
			continue
		}

		it := fmt.Sprintf("%c%d", rn, a[rn])
		items = append(items, it)
	}
	return fmt.Sprintf("%s", strings.Join(items, ""))
}

// CountLetters constructs an AnagramBase for a string
func CountLetters(s string) (a AnagramBase) {
	a = make(AnagramBase)
	s = strings.ToLower(s)

	for _, rn := range s {
		if !unicode.IsLetter(rn) {
			continue
		}
		a[rn]++
	}

	return
}

// AddAnagrams returns an AnagramBase, which has counts for every letter summed
// from the addition operands.
func AddAnagrams(a, b AnagramBase) (r AnagramBase) {
	r = make(AnagramBase)

	copyAnagram := func(src, dst AnagramBase) {
		for rn, cnt := range src {
			dst[rn] += cnt
		}
	}

	copyAnagram(a, r)
	copyAnagram(b, r)
	return
}

// AnagramEqual returns true if both anagrams contains same numbers of
// same characters
func AnagramEqual(a, b AnagramBase) bool {
	for rn := range a {
		if a[rn] != b[rn] {
			return false
		}
	}

	for rn := range b {
		if a[rn] != b[rn] {
			return false
		}
	}

	return true
}

// AnagramExceedsLimit returns true, is `a` has bigger number of at least
// one character type.
func AnagramExceedsLimit(a, limit AnagramBase) bool {
	for rn := range a {
		if a[rn] > limit[rn] {
			return true
		}
	}
	return false
}

// DictItem - type of a Dictionary item
type DictItem struct {
	Anagram AnagramBase
	Words   []string
}

// Dictionary maps AnagramBase's onto slices of words, using AnagramBase.Hash()
type Dictionary map[string]DictItem

// Add puts a new word into the dictionary
func (d Dictionary) Add(word string) {
	a := CountLetters(word)
	h := a.Hash()

	item := d[h]
	item.Anagram = a
	item.Words = append(item.Words, word)
	d[h] = item
}

func readInputFile(filename string) (lines []string, err error) {
	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	if !scanner.Scan() {
		if err = scanner.Err(); err != nil {
			return nil, err
		}
		return nil, errors.New("Unexpected end of input file")
	}
	countLine := scanner.Text()

	count, err := strconv.ParseInt(countLine, 10, 64)
	if err != nil {
		return nil, err
	}

	for scanner.Scan() {
		if len(lines) >= int(count) {
			return
		}

		l := scanner.Text()
		lines = append(lines, l)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}

	if len(lines) < int(count) {
		return nil, fmt.Errorf("Input file promised %v lines, but gave %v", count, len(lines))
	}

	return
}

func readDictFile(filename string) (Dictionary, error) {
	dict := Dictionary{}

	file, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		dict.Add(line)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return dict, nil
}

// FindAllAnagramsForLine collects all the possible anagrams for `line`.
// Every result will have words sorted in alphabetical order and title-cased.
// If there appers a result, equivalent to the `line` itself, it will be omitted.
func FindAllAnagramsForLine(line string, dict Dictionary) []string {
	original := CountLetters(line)

	// Collect keys of the dictionary
	var keys []string
	for key := range dict {
		keys = append(keys, key)
	}

	type itemList []DictItem
	var passDict func(int, AnagramBase, itemList) []itemList

	// passDict is a recursive function, which traverses through `dict` by
	// `keys[startInd:]` to find item lists, whose letter count matches `original`.
	//
	// `list` is a list if items, selected on previous recursion frames.
	// `sum` contains their letter counts summed.
	passDict = func(startInd int, sum AnagramBase, items itemList) (results []itemList) {
		for keyInd := startInd; keyInd < len(keys); keyInd++ {
			item := dict[keys[keyInd]]

			newSum := AddAnagrams(sum, item.Anagram)

			if AnagramExceedsLimit(newSum, original) {
				continue
			}

			if AnagramEqual(newSum, original) {
				res := itemList{}
				res = append(res, items...)
				res = append(res, item)

				results = append(results, res)
				continue
			}

			newItems := append(items, item)
			results = append(results, passDict(keyInd+1, newSum, newItems)...)
		}

		return
	}

	// Collect search results as item lists
	itemResults := passDict(0, AnagramBase{}, nil)

	type wordList []string
	var toWordLists func(wordList, itemList) []wordList

	// toWordLists is a recursive function, which collects all the word
	// combinations, made by items in the `list`.
	//
	// If `list` contains items with these words:
	// [A B], [C], [D E]
	// toWordLists will return:
	// [A C D], [A C E], [B C D], [B C E]
	//
	// `prefix` - a list of words, selected on previous recursion frames.
	toWordLists = func(prefix wordList, items itemList) (ws []wordList) {
		if len(items) == 0 {
			return []wordList{prefix}
		}

		item := items[0]
		for _, word := range item.Words {
			newPrefix := append(prefix, word)
			newItems := items[1:]
			ws = append(ws, toWordLists(newPrefix, newItems)...)
		}

		return
	}

	// Collect search results as word lists
	var resultsAsWords []wordList
	for _, ilist := range itemResults {
		resultsAsWords = append(resultsAsWords, toWordLists(nil, ilist)...)
	}

	originalTitled := strings.Title(line)

	// Join word lists into strings
	resultsAsStrings := []string{}
	for _, list := range resultsAsWords {
		for i := range list {
			list[i] = strings.ToLower(list[i])
		}

		sort.Strings(list)
		str := strings.Title(strings.Join(list, " "))

		if str == originalTitled {
			continue
		}

		resultsAsStrings = append(resultsAsStrings, str)
	}

	return resultsAsStrings
}

func main() {
	if len(os.Args) < 2 {
		fmt.Printf("Usage: %v input.txt [dict.txt] [profile.prof]\n", os.Args[0])
		os.Exit(1)
	}

	// Profiler
	if len(os.Args) >= 4 {
		pfilename := os.Args[3]
		pfile, err := os.Create(pfilename)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}

		pprof.StartCPUProfile(pfile)
		defer pprof.StopCPUProfile()
	}

	var lines []string
	{
		var err error

		inputFilename := os.Args[1]
		lines, err = readInputFile(inputFilename)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
	}

	var dict Dictionary
	{
		var err error

		dictFilename := "dict.txt"
		if len(os.Args) >= 3 {
			dictFilename = os.Args[2]
		}

		dict, err = readDictFile(dictFilename)
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
	}

	for _, l := range lines {
		anagrams := FindAllAnagramsForLine(l, dict)
		for _, a := range anagrams {
			fmt.Printf("%v -> %v\n", l, a)
		}
	}
}
