const {queryPagesWithTag, getPagesWithTags, createPage, getPageWithUrl} = require('./query')
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
    .then(page => Promise.all([page].concat(getPagesWithTags(page.tags, page.url))))
    .then(([page, ...related]) => res.status(200).json({page: page, related: related[0]}))
    .catch(errorHandler(res))
}