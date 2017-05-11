var a = []
let n = 0
let m = 99

app.post("/api/sort", (req, res) => {
	let result = calc(req.body.numbers)
	res.send(result)
})

const sortNumber = (a,b) => {
    return a - b
}

for (i = 0; i <= m; i++) { 
	a.push(Math.floor(Math.random()*(100-1+1)+1))
  if(i == m) {
  	console.log(a)
    let sum = a.reduce((a, b) => a + b, 0)
    console.log(sum)
  }
}

let a1 = [],
	a2 = [],
	a1p = true,
	a2p = false

let s = a.sort(sortNumber)
console.log(s)

for (i = 0; i <= m; i++) {
	let s1 = a1.reduce((a, b) => a + b, 0)
	let s2 = a2.reduce((a, b) => a + b, 0)
	if (a1p) {
		if (s1 > s2) {
			a2.push(s[i])
			a1p = true			
		} else {
			a1.push(s[i])
			a1p = false
		}
	} else {
		if (s2 > s1) {
			a1.push(s[i])
			a1p = true			
		} else {
			a2.push(s[i])
			a1p = false
		}
	}
	if (a1 == 0) {
		console.log("goot")
		if (a1p) {
			a1.push(s[i])
			a1p = false
		} else {
			a2.push(s[i])
			a1p = true
		}
	}
	if (i == m) {
		console.log("Sum of 1: " + s1)
		console.log("Sum of 2: " + s2)
		console.log("Sum of a: " + a.reduce((a, b) => a + b, 0))
		console.log("Sum of 1 + 2: " + a1.concat(a2).reduce((a, b) => a + b, 0))
	}
}

let sorted1 = a1.sort(sortNumber),
	sorted2 = a2.sort(sortNumber)

console.log(sorted1)