/* global chrome */
const uniq = require('lodash/uniq')
const trim = require('lodash/trim')

// Content extractor functions
const innerText = elem => elem.innerText
const content = elem => elem.content
const imageSource = elem => elem.src
const parselyTags = elem => {
    try {
        const parselyPageRaw = elem.content
        const parselyPage = JSON.parse(parselyPageRaw)
        return parselyPage.tags.join(",")
    } catch(e) {}
    return ''
}

const collectFirst = (selector, fn) => {
    return {selector, fn, limit: 1}
}
const collect = (selector, fn) => {
    return {selector, fn, limit: 30}
}
const runCollectors = collectors => {
    return collectors.reduce((results, {selector, fn, limit}) => {
        const elems = document.querySelectorAll(selector)
        let newResults = []
        elems.forEach(elem => {
            const content = fn(elem) || ''
            newResults = newResults.concat(content)
        })
        return results.concat(newResults.slice(0, limit))
    }, [])
}
const ruleFilter = rules => {
    return item => !rules.find(rule => rule.test(item))
}

const blacklist = [
    /^$/,
    /^[0-9]+$/,
    /^\s+$/,
    /^[^a-z0-9]{1,2}$/i,
    /^(and|is|a|the|an|as|or|for|to|in|on|at|with|that|this|can|do|did|what|out|why|also)$/
]

const commonCollectors = [
    collectFirst('title', innerText),
    collectFirst('h1', innerText),
    collectFirst('[itemprop="name"]', innerText),
    collectFirst('meta[name="og:title"]', content),
    collectFirst('meta[property="og:title"]', content),
    collectFirst('meta[name="og:type"]', content),
    collectFirst('meta[property="og:type"]', content),
]

const tagCollectors = [
    collectFirst('meta[name="tags"]', content),
    collectFirst('meta[property="tags"]', content),
    collectFirst('meta[name="keywords"]', content),
    collectFirst('meta[property="keywords"]', content),
    collectFirst('meta[name="parsely-page"', parselyTags)
].concat(commonCollectors)

const titleCollectors = commonCollectors.concat([
    collectFirst('meta[name="description"]', content),
    collectFirst('meta[property="description"]', content),
    collectFirst('meta[name="og:description"]', content),
    collectFirst('meta[property="og:description"]', content),
])

const imageCollectors = [
    collect('meta[name="og:image"]', content),
    collect('meta[property="og:image"]', content),
    collectFirst('[itemprop="articleBody"] img', imageSource),
    collectFirst('.article-body img', imageSource),
    collectFirst('article img', imageSource),
    collectFirst('#logo img', imageSource),
    collectFirst('img', imageSource)
]

const tags = runCollectors(tagCollectors)
    .reduce((results, tagLine) => {
        return results.concat(tagLine.split(/[,\s]/))
    }, [])
    .map(trim)
    .map(tag => trim(tag, ':-.,;\-/\/_"()[]{}+#%£$€~“”|'))
    .map(tag => tag.toLowerCase())
    .filter(ruleFilter(blacklist))

tags.sort()

const titles = runCollectors(titleCollectors)
const images = runCollectors(imageCollectors)

chrome.runtime.sendMessage({
    type: 'page-indexer-tags',
    url: document.location.href,
    tags: uniq(tags),
    image: images[0] || null,
    title: titles[0] || ''
})