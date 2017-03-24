const readline = require('readline')
const axios = require('axios')

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
})

rl.question('Enter a Subreddit... ', (sub) => {
      axios.get("https://www.reddit.com/r/" + sub + ".json")
      .then((res) => {
        let posts = res.data.data.children
        let lim = posts.length
        let count = 0
        let titles = []
        Object.keys(posts).forEach((x) => {
        	titles.push([parseInt(x) + 1]+": "+posts[x].data.title + ".\n")
        	count++
        	if (count == lim) {
        		console.log(titles.toString().replace(/,/g, ""))
        		rl.close()
        	}
        })
      })
      .catch((err) => {
        console.log(err)
        rl.close()
      })
})