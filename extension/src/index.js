/* global chrome */
const server = 'https://us-central1-ornate-factor-169811.cloudfunctions.net'
const m = require('mithril')

const oneTwo = (one, two) => {
    return [
        m('.pure-u-1-3', [ m('.container', [].concat(one)) ]), 
        m('.pure-u-2-3', [ m('.container', [].concat(two)) ]) 
    ]
}
const id = a => a

const tagsForPage = (page, changeFn = id) => {
    return m('ul.tags', page.tags.map(tag => {
        return m('li.tag', {onclick: () => changeFn(page, tag)}, tag)
    }))
}

const App = {
    sending: false,
    fetching: false,
    page: { url: '', title: '', image: '', tags: [] },
    related: [],
    loadPage: (page) => {
        App.page = page
        m.redraw()
        App.getRelated(page.url)
    },
    getRelated: url => {
        App.related = []
        App.fetching = true
        m.redraw()

        console.log('getRelated', url)
        m.request(`${server}/getPageInfo`, {data: {url}})
            .then((response) => {
                console.log('getRelated', url, response)
                if (response && response.related.length > 0) {
                    App.related = response.related
                    App.fetching = false
                    m.redraw()
                }
            })
            .catch(err => {
                console.error('getRelated', err)
                App.related = []
                App.fetching = false
                m.redraw()
            })
    },
    getWithTag: (page, tag) => {
        App.page = page
        App.related = []
        App.fetching = true
        m.redraw()

        console.log('getWithTag', tag)
        m.request(`${server}/getPages`, {data: {tag, url: page.url}})
            .then((response) => {
                console.log('getWithTag', tag, response)
                if (response && response.pages.length > 0) {
                    App.related = response.pages
                }
                App.fetching = false
                m.redraw()
            })
            .catch(err => {
                console.error('getWithTag', err)
                App.related = []
                App.fetching = false
                m.redraw()
            })

    },
    bookmark: (page) => {
        console.log('bookmark', page)
        App.sending = true
        m.request(`${server}/putPage`, {method: 'POST', data: {page}})
            .then(() => {
                console.log('bookmarked', page)
                App.sending = false
                m.redraw()
            })
            .catch(err => {
                console.error('bookmark', err)
                App.sending = false
                m.redraw()
            })
    },
    view: vnode => {
        let fetching = vnode.state.fetching
        let sending = vnode.state.sending
        let page = vnode.state.page
        let related = vnode.state.related
        return m('.pure-g', [
            oneTwo(
                m('img.pure-img', {src: page.image}),
                [
                    m('h1', page.title),
                    tagsForPage(page, App.getWithTag),
                    m('button', {onclick: () => App.bookmark(page)}, 'Bookmark')
                ]
            ),
            m('.pure-u', [
                (fetching ? m('div', 'Fetching..') : []),
                (sending ? m('div', 'Sending..') : []),
            ]),
            m('.pure-u', [
                related ? 
                    m('ul.related', [
                        related.map(relatedPage => {
                            return m('li', 
                                oneTwo(
                                    m('img.pure-img', {src: relatedPage.image}),
                                    [
                                        m('a', {onclick: () => App.loadPage(relatedPage)}, relatedPage.title),
                                        tagsForPage(relatedPage, App.getWithTag),
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
