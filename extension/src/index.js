/* global chrome */
const axios = require('axios')
const server = 'http://localhost:8000'

// Listen for tags 
chrome.runtime.onMessage.addListener(({type, url, title, tags}) => {
    if (type === 'page-indexer-tags') {
        const urlEl = document.querySelector('#url')
        const tagsEl = document.querySelector('#tags')
        const titleEl = document.querySelector('#title')
        //const imageEl = $('img')

        urlEl.innerText = url
        tagsEl.innerHTML = tags.join(', ')
        titleEl.innerText = title
        //imageEl.src = image

        const index = document.querySelector('#index')
        index.onclick = () => {
            axios.post(`${server}/pages`, {url, tags})
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
