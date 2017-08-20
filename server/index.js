const uniqBy = require('lodash/uniqBy')
const {queryPagesWithTag, createPage, getPageWithUrl} = require('./query')
const {errorHandler, validationError} = require('./errors')
const {validateRequest, getPagesSchema, getPageInfoSchema, putPageSchema} = require('./schema')

exports.getPages = (req, res) => {
  validateRequest(req, getPagesSchema)
    .catch(err => validationError(res, err))
    .then(validatedReq => queryPagesWithTag(validatedReq.query.tag))
    .then(pages => res.status(200).json({pages: pages}))
    .catch(errorHandler(res))
}

exports.putPage = (req, res) => {
  validateRequest(req, putPageSchema)
    .catch(err => validationError(res, err))
    .then(validatedReq => createPage(validatedReq.body.page))
    .then(() => res.status(201).end())
    .catch(errorHandler(res))
}

exports.getPageInfo = (req, res) => {
  validateRequest(req, getPageInfoSchema)
    .catch(err => validationError(res, err))
    .then(validatedReq => getPageWithUrl(validatedReq.query.url))
    .then(page => {
        // Get 3 pages for each tag in this page
        return Promise.all([page].concat(
            (page.tags || []).map(tag => queryPagesWithTag(tag, 3))
        ))
    })
    .then(([page, ...groupedByTag]) => {
        // Flatten and filter related pages
        const related = groupedByTag.reduce((result, relatedToTag) => {
            const filtered = relatedToTag.filter(relatedPage => relatedPage.url !== page.url)
            return result.concat(filtered)
        }, [])
        const uniqueRelatedPages = uniqBy(related, page => page.url)

        return [page].concat(uniqueRelatedPages)
    })
    .then(([page, ...related]) => res.status(200).json({page: page, related: related}))
    .catch(errorHandler(res))
}