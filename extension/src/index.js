/* global chrome */
const axios = require('axios')
const server = 'https://us-central1-ornate-factor-169811.cloudfunctions.net'

const renderRelated = (page) => {
    const row = document.createElement('li')
    const img = document.createElement('img')
    img.src = page.image
    row.appendChild(img)

    const title = document.createElement('span')
    title.innerText = page.title

    const link = document.createElement('a')
    link.href = page.url
    link.target = '_blank'
    link.appendChild(title)

    row.appendChild(link)
    return row
}

// Listen for tags 
chrome.runtime.onMessage.addListener(({type, url, title, image, tags}) => {
    if (type === 'page-indexer-tags') {
        const tagsEl = document.querySelector('#tags')
        const titleEl = document.querySelector('#title')
        const imageEl = document.querySelector('img')
        const resultEl = document.querySelector('#result')

        tagsEl.innerHTML = tags.join(', ')
        titleEl.innerText = title
        imageEl.src = image

        const index = document.querySelector('#index')
        axios.get(`${server}/getPageInfo`, {params: {url}})
            .then((response) => {
                if (response.data && response.data.related.length > 0) {
                    console.log(response.data.related)

                    resultEl.innerHTML = ''
                    response.data.related.map(page => {
                        const row = renderRelated(page)
                        resultEl.appendChild(row)
                    })
                }
            })
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
