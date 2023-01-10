use std::collections::HashMap;
use std::fs::read_to_string;

fn subsets(a: &[u32], b: &[u32]) -> bool {
    
    let [n, m] = [a.len(), b.len()];
    let mut freq: HashMap<_, _> = a
        .iter()
        .map(| n | (n, 0))
        .collect();
    
    for i in 0..n {
        let key = &a[i];
        freq.insert(key, freq[key] + 1);
    }

    for i in 0..m {
        let key = &b[i];
        if freq.contains_key(key) && freq[key] > 0 {
            freq.insert(key, freq[key] - 1);
        } else {
            return false;
        }
    }

    true
}

fn overlaps(a: &[u32], b: &[u32]) -> bool {
    for n in a.iter() {
        if b.contains(&n) {
            return true
        }
    }
    false
}

fn parse_elf(elf: &str) -> Option<Vec<u32>> {
    let parts: Vec<_> = elf.split('-').collect();
    match parts[0..2] {
        [l, r] => {
            let lo = l.parse::<u32>().ok()?;
            let hi = r.parse::<u32>().ok()?;
            return Some((lo..=hi).into_iter().collect());
        },
        _ => None
    }
}

fn parse_pair(line: &str) -> Option<(Vec<u32>, Vec<u32>)> {
    let pair: Vec<_> = line.split(',').collect();
    match pair[0..2] {
        [l, r] => Some((parse_elf(l)?, parse_elf(r)?)),
        _ => None
    }
}

fn main() {
    
    let mut p1_total = 0;
    let mut p2_total = 0;

    if let Ok(text) = read_to_string("input.txt") {
        for line in text.lines() {
            
            if let Some((elf1, elf2)) = parse_pair(&line) {
                
                // part 1
                if subsets(&elf1, &elf2) || subsets(&elf2, &elf1) {
                    p1_total += 1;
                }

                // part 2
                if overlaps(&elf1, &elf2) {
                    p2_total += 1;
                }
            }
        }
    }

    println!("total 1: {}", p1_total);
    println!("total 2: {}", p2_total);
}
