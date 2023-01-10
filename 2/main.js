#!/usr/bin/env node

const fs = require('fs')

// rock = 1, beats 3, loses 2
// paper = 2, beats 1, loses 3
// scissors = 3, beats 2, loses 1

// lookup tables
const V = { A: 1, B: 2, C: 3, X: 1, Y: 2, Z: 3 }
// winning moves for choices
// if we choose 1 (X), we would beat 3 (C)
const W = { A: 3, B: 1, C: 2, X: 3, Y: 1, Z: 2 }
// losing moves for choices
// if we choose 1 (X) we would lose to 2 (B)
const L = { A: 2, B: 3, C: 1, X: 2, Y: 3, Z: 1 }

// utilities
const parseGames = (data) => data.trim().split('\n').map(l => l.split(' '))

// main logic
const playP1 = (opponent, player) => {
	
	const [o, p] = [V[opponent], V[player]]

	switch(o) {
		case p: return 3 + p
		case W[player]: return 6 + p
		case L[player]: return p
	}
		
	return 0
}

const playP2 = (opponent, player) => {
	
	switch(player) {
		case 'X': return W[opponent]
		case 'Y': return 3 + V[opponent]
		case 'Z': return 6 + L[opponent]
	}

	return 0
}


fs.readFile("input.txt", "utf-8", (error, data) => {
	
	let [p1Total, p2Total] = [0, 0]
	
	parseGames(data).map(([opponent, player], index) => {
		p1Total += playP1(opponent, player)
		p2Total += playP2(opponent, player)
	})

	console.log("Part 1 total:", p1Total)
	console.log("Part 2 total:", p2Total)
})
