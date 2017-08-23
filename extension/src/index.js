/* global chrome */
const server = 'https://us-central1-ornate-factor-169811.cloudfunctions.net'
const m = require('mithril')
const moment = require('moment')

const oneTwo = (one, two) => {
    return [
        m('.pure-u-1-3', [ m('.container', [].concat(one)) ]), 
        m('.pure-u-2-3', [ m('.container', [].concat(two)) ]) 
    ]
}
const id = a => a

const tagsForPage = (page, changeFn = id, highlight = []) => {
    return m('ul.tags', page.tags.map(tag => {
        if (highlight.indexOf(tag) !== -1){
            return m('li.tag.highlight', {onclick: () => changeFn(page, tag)}, tag)   
        } else {
            return m('li.tag', {onclick: () => changeFn(page, tag)}, tag)
        }
    }))
}

const error = (name, err) => {
    console.error(name, err, err.stack)
}

const App = {
    fetching: false,
    page: { url: '', title: '', image: '', tags: []},
    resultTitle: '',
    related: [],
    loadPage: page => {
        App.page = page
        m.redraw()
        App.getRelated(page, true)
    },
    getRelated: (page, tryBookmark = false) => {
        const url = page.url
        App.related = []
        App.fetching = true

        console.log('getRelated', url)
        return m.request(`${server}/getPageInfo`, {data: {url}})
            .then((response) => {
                console.log('getRelated', url, response)
                if (response && response.related.length > 0) {
                    App.resultTitle = 'Pages like this:'
                    App.related = response.related
                } else {
                    App.resultTitle = 'No pages like this!'
                }
                App.fetching = false
            })
            .catch(err => {
                error('getRelated', err)
                App.resultTitle = ''
                App.related = []
                if (tryBookmark){
                    App.bookmark(page)
                } else {
                    App.fetching = false
                }
            })
    },
    getWithTag: (page, tag) => {
        App.page = page
        App.related = []
        App.fetching = true

        console.log('getWithTag', tag)
        m.request(`${server}/getPages`, {data: {tag, url: page.url}})
            .then((response) => {
                console.log('getWithTag', tag, response)
                App.resultTitle = `Pages with tag: '${tag}'`
                if (response && response.pages.length > 0) {
                    App.related = response.pages
                } else {
                    App.resultTitle = `No pages with tag: '${tag}'`
                }
                App.fetching = false
            })
            .catch(err => {
                error('getWithTag', err)
                App.resultTitle = ''
                App.related = []
                App.fetching = false
            })

    },
    bookmark: (page) => {
        console.log('bookmark', page)
        m.request(`${server}/putPage`, {method: 'POST', data: {page}})
            .then(() => {
                console.log('bookmarked', page)
                App.getRelated(page)
            })
            .catch(err => {
                App.fetching = false
                error('bookmark', err)
            })
    },
    view: vnode => {
        let fetching = vnode.state.fetching
        let page = vnode.state.page
        let related = vnode.state.related
        return m('.pure-g', [
            oneTwo(
                m('img.pure-img', {src: page.image}),
                [
                    m('h1', page.title),
                    tagsForPage(page, App.getWithTag)
                ]
            ),
            oneTwo([], m('h2', vnode.state.resultTitle)),
            m('.pure-u-1-1', [
                fetching ? m('.loader.center') : []
            ]),
            m('.pure-u', [
                related ? 
                    m('ul.related', [
                        related.map(relatedPage => {
                            return m('li', 
                                oneTwo(
                                    m('img.pure-img', {src: relatedPage.image}),
                                    [
                                        m('a', {href: relatedPage.url, target: '_blank'}, m('h2', relatedPage.title)),
                                        m('span.timestamp', moment(relatedPage.updatedAt).fromNow()),
                                        tagsForPage(relatedPage, App.getWithTag, page.tags),
                                    ]
                                )
                            )
                        })
                    ]) : 
                    []
            ])
        ])
    }
}

// Listen for tags 
chrome.runtime.onMessage.addListener(({type, url, title, image, tags}) => {
    if (type === 'page-indexer-tags') {
        App.loadPage({url, title, image, tags})
    }
})

window.onload = () => {
    m.mount(document.body, App)

    // Execute page indexer
    chrome.tabs.executeScript(null, {
        file: 'pageTags.js',
    }, () => {
        if (chrome.runtime.lastError) {
            console.error(chrome.runtime.lastError)
        }
    })
}
