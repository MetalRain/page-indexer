/* global chrome */
const axios = require('axios')
const server = 'https://us-central1-ornate-factor-169811.cloudfunctions.net'

// Listen for tags 
chrome.runtime.onMessage.addListener(({type, url, title, image, tags}) => {
    if (type === 'page-indexer-tags') {
        const urlEl = document.querySelector('#url')
        const tagsEl = document.querySelector('#tags')
        const titleEl = document.querySelector('#title')
        const imageEl = document.querySelector('img')
        const resultEl = document.querySelector('#result')

        urlEl.innerText = url
        tagsEl.innerHTML = tags.join(', ')
        titleEl.innerText = title
        imageEl.src = image

        const index = document.querySelector('#index')
        index.onclick = () => {
            axios.post(`${server}/putPage`, {page: {url, tags, image, title}})
                .then(() => {
                    resultEl.innerText = 'Ok!'
                })
                .catch(err => {
                    resultEl.innerText = err.toString()
                    console.error(err)
                })
        }
    }
})

window.onload = () => {
    // Execute page indexer
    chrome.tabs.executeScript(null, {
        file: 'pageTags.js',
    }, () => {
        if (chrome.runtime.lastError) {
            console.error(chrome.runtime.lastError)
        }
    })
}
