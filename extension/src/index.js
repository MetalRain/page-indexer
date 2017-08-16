const axios = require('axios')
const server = 'http://localhost'

chrome.browserAction.onClicked.addListener(function(tab) {
	axios.post(`${server}/index`, {url: tab.url})
})