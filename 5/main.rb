#!/usr/bin/env ruby

def parsecargo (cargo)

  lines = cargo.lines # extract the lines from the string

  count = lines.last.split.last.to_i
  lines = lines.slice 0, lines.size - 1 # cut out the last line
  
  stacks = Array.new(count) { |stack| [] } # list of stacks

  for line in lines
    line = line.gsub(/\[/, ' ').gsub(/\]/, ' ') # clean up line
    # to preseve space, go by every 4 indicies (how far each letter is spaced)
    for i in (0..(line.size / 4) - 1)
      letter = line[i * 4, 4].strip # save a cleaned up version of the letter
      if letter != ""
        stacks[i].push letter
      end
    end
  end

  return stacks.map { |stack| stack.reverse }
end
    
def parsestep (line)
  tokens = line.split
  return tokens[1].to_i, tokens[3].to_i, tokens[5].to_i
end

def p1move (stacks, n, from, to)
  for _ in (0...n)
    stacks[to - 1].push stacks[from - 1].pop
  end
end

def p2move (stacks, n, from, to)
  stack = []
  for _ in (0...n)
    stack.push stacks[from - 1].pop
  end
  while ! stack.empty?
    stacks[to - 1].push stack.pop
  end
end

# main
if __FILE__ == $0

  text = IO.read "input.txt" 
  cargo, steps = text.split "\n\n"

  p1stacks = parsecargo cargo
  p2stacks = parsecargo cargo

  for line in steps.lines
    n, from, to = parsestep line
    p1move p1stacks, n, from, to
    p2move p2stacks, n, from, to
  end
  
  part1, part2 = '', ''
  for i in (0...p1stacks.size)
    part1 += p1stacks[i].pop
    part2 += p2stacks[i].pop
  end

  puts "part 1: " + part1
  puts "part 2: " + part2

end
