/* global chrome */
const uniq = require('lodash/uniq')
const trim = require('lodash/trim')

const innerText = elem => elem.innerText
const content = elem => elem.content
const imageSource = elem => elem.src
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
    /^(and|is|a|the|an|or|for|to|in|on|at|with|that|this|can|do|did|what|out)$/
]

const commonCollectors = [
    collectFirst('title', innerText),
    collectFirst('h1', innerText),
    collectFirst('[itemprop="name"]', innerText),
    collectFirst('meta[name="og:title"]', content),
    collectFirst('meta[name="og:type"]', content),
]

const tagCollectors = [
    collectFirst('meta[name="tags"]', content)
].concat(commonCollectors)

const titleCollectors = [
    collectFirst('meta[name="description"]', content),
    collectFirst('meta[name="og:description"]', content),
].concat(commonCollectors)

const imageCollectors = [
    collect('meta[name="og:image"]', content),
    collectFirst('article .article-body img', imageSource),
    collectFirst('article img', imageSource),
    collectFirst('#logo img', imageSource),
    collectFirst('img', imageSource)
]

const tags = runCollectors(tagCollectors)
    .reduce((results, tagLine) => {
        return results.concat(tagLine.split(/[,\s]/))
    }, [])
    .map(trim)
    .map(tag => trim(tag, ':-.,;\-/\/_"'))
    .map(tag => tag.toLowerCase())
    .filter(ruleFilter(blacklist))

tags.sort()

const titles = runCollectors(titleCollectors)

titles.sort((a, b) => {
    const aL = a.length || 0
    const bL = b.length || 0
    if (aL > bL) {
        return -1
    } else if (aL < bL) {
        return 1
    }
    return 0
})

const images = runCollectors(imageCollectors)


chrome.runtime.sendMessage({
    type: 'page-indexer-tags',
    url: document.location.href,
    tags: uniq(tags),
    image: images[0] || null,
    title: titles[0] || ''
})