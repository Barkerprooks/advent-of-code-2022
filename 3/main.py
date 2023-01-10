#!/usr/bin/env python3

from string import ascii_lowercase, ascii_uppercase

ALPHA = ascii_lowercase + ascii_uppercase
PRI = dict((l, i + 1) for i, l in enumerate(ALPHA))


def readlines(path):
    with open(path, "rt") as stream:
        return tuple(line.strip() for line in stream.readlines())


def part1(rucksack):
    m = len(rucksack) // 2
    a, b = rucksack[:m], rucksack[m:]
    return PRI[(set(a) & set(b)).pop()]


def part2(group):
    a, b, c = group
    return PRI[(set(a) & set(b) & set(c)).pop()]


if __name__ == "__main__":

    rucksacks = readlines("input.txt")
    groups = [rucksacks[i:i+3] for i in range(0, len(rucksacks), 3)]
    
    p1_total, p2_total = 0, 0

    for group in groups:
        for rucksack in group:
            p1_total += part1(rucksack)
        p2_total += part2(group)

    print(p1_total)
    print(p2_total)
