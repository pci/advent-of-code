package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
)

func main() {
	var (
		fileName = flag.String("f", "./input.txt", "file to read in as input to this day")
	)
	flag.Parse()
	if *fileName == "" {
		log.Fatal("filename must be given")
	}
	b, err := ioutil.ReadFile(*fileName)
	if err != nil {
		log.Fatal(err)
	}
	s := string(b)
	if err := runDay(s); err != nil {
		log.Fatal(err)
	}
}

// gping to use a linked list so that
// you don't have to do any memory copying
// and you can store pointer references
// to each "cup" and do O(1) lookups to find the
// cup with the given value
type linked struct {
	val  int
	next *linked
}

func (l *linked) GetList() []int {
	ret := []int{l.val}
	for curr := l.next; curr != l; curr = curr.next {
		ret = append(ret, curr.val)
	}
	return ret
}
func (l *linked) String() string {
	return fmt.Sprintf("%v", l.GetList())
}

func runDay(s string) error {
	in := parseInput(s)
	fmt.Printf("input: %v\n", in)

	list := &linked{
		val: in[0],
	}
	n := 1
	quickFind := map[int]*linked{}
	quickFind[in[0]] = list
	curr := list
	for _, d := range in[1:] {
		next := &linked{
			val: d,
		}
		quickFind[d] = next
		curr.next = next
		curr = next
		n++
	}
	// partII - bulk out:
	for d := len(in) + 1; d <= 1e6; d++ {
		next := &linked{
			val: d,
		}
		quickFind[d] = next
		curr.next = next
		curr = next
		n++
	}
	// make it a ring:
	curr.next = list

	var err error
	next := list
	// run 10 million times
	for i := 0; i < 1e7; i++ {
		next, err = step(next, quickFind, n)
		if err != nil {
			log.Fatalf("Could not iterate number %d: %v\n", i, err)
		}
	}
	a, b := partTwoOutput(quickFind)
	fmt.Println(a * b)
	return nil
}

func parseInput(s string) []int {
	ret := []int{}
	for _, c := range s {
		if c >= '0' && c <= '9' {
			ret = append(ret, int(c-'0'))
		}
	}
	return ret
}

func step(l *linked, m map[int]*linked, n int) (*linked, error) {
	curr := l.val
	x := l.next
	y := x.next
	z := y.next

	destination := curr - 1
	if destination == 0 {
		destination = n
	}
	for destination == x.val || destination == y.val || destination == z.val {
		destination--
		if destination == 0 {
			destination = n
		}
	}
	// get destination
	dest, ok := m[destination]
	if !ok {
		return nil, fmt.Errorf("could not find destination %d", destination)
	}
	// re-wire:
	l.next = z.next
	z.next = dest.next
	dest.next = x
	// more us on one:
	return l.next, nil
}

func partTwoOutput(m map[int]*linked) (int, int) {
	curr := m[1]
	// next two values:
	return curr.next.val, curr.next.next.val
}
