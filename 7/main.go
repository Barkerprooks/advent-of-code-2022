package main 

import (
	"strconv"
	"strings"
	"bufio"
	"sort"
	"fmt"
	"os"
)

type FsNode struct {
	parent *FsNode
	children []*FsNode
	size int
	name string
}

func NewRootNode() *FsNode {
	return &FsNode{nil, []*FsNode{}, 0, "/"}
}

func (node *FsNode) IsFile() bool {
	return node.children == nil
}

func (node *FsNode) IsDir() bool {
	return ! node.IsFile()
}

func (node *FsNode) Mkdir(name string) {
	if node.children != nil {		
		child := &FsNode{node, []*FsNode{}, 0, name}
		node.children = append(node.children, child)
	}
}

func (node *FsNode) Touch(name string, size int) {
	if node.children != nil {
		child := &FsNode{node, nil, size, name}
		node.children = append(node.children, child)
	}
}

func (node *FsNode) Chdir(name string, root *FsNode) *FsNode {
	
	if name == ".." {
		return node.parent
	} else if name == "/" {
		return root
	}

	for _, child := range node.children {
		if child.IsDir() && child.name == name {
			return child
		}
	}

	return nil
}

func (node *FsNode) UpdateDirs() {
	if node.IsDir() && node.size == 0 {
		for _, child := range node.children {
			child.UpdateDirs()
			node.size += child.size
		}
	}
}

func (node *FsNode) WalkDirs() []*FsNode {
	
	dirs := []*FsNode{}

	if node.IsDir() {
		dirs = append(dirs, node)
		for _, child := range node.children {
			if child.IsDir() {
				dirs = append(dirs, child.WalkDirs()...)
			}
		}
	}
	
	return dirs
}

func parseInput(path string) ([]string, error) {
	if file, err := os.Open(path); err == nil {
		lines := []string{}
		scanner := bufio.NewScanner(file)
		for scanner.Scan() {
			lines = append(lines, scanner.Text())
		}
		return lines, nil
	} else {
		return []string{}, err
	}
}

type Pair struct {
	index int
	delta int
}

func main() {
	
	root := NewRootNode()
	cwd := root

	if lines, err := parseInput("input.txt"); err == nil {
		for _, line := range lines {
			tokens := strings.Split(line, " ")
			if tokens[0] == "$" && tokens[1] == "cd" {
				cwd = cwd.Chdir(tokens[2], root)
			} else if tokens[0] != "$" {
				info, name := tokens[0], tokens[1]
				if info == "dir" {
					cwd.Mkdir(name)
				} else if size, err := strconv.Atoi(info); err == nil {		
					cwd.Touch(name, size)
				}
			}
		}
	}
	
	total := 0

	root.UpdateDirs()
	dirs := root.WalkDirs()
	
	needed := 30000000 - (70000000 - dirs[0].size)
	ratings := []*Pair{}

	for index, dir := range dirs {
		if dir.size <= 100000 {
			total += dir.size
		}
		if delta := dir.size - needed; delta >= 0 {
			ratings = append(ratings, &Pair{index, delta})
		}
	}
	
	sort.SliceStable(ratings, func(i, j int) bool {
		return ratings[i].delta < ratings[j].delta
	})

	fmt.Println("part 1:", total)
	fmt.Println("part 2:", dirs[ratings[0].index].size)

}
