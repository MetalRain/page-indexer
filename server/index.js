const Joi = require('joi')
const datastore = require('@google-cloud/datastore')
const db = datastore()

const errorHandler = (res) => {
  return err => {
    console.error(err, err.stack)
    res.status(500).end()
  }
}

const validationError = (res, joiError) => {
  res.status(400).send({errors: joiError.details})
}

const getPagesWithTag = (searchTag, limit = 10) => {
  const query = db.createQuery('Page')
    .filter('tags', '=', searchTag)
    .order('createdAt', {descending: true})
    .limit(limit)

  return db.runQuery(query)
    .then(([pages, info]) => {
      if (pages && pages.length > 0){
        return pages.map(({url, title, image, tags}) => {
          return {url, title, image, tags}
        })
      }
      return []
    })
}

const createPage = ({url, title, image, tags}) => {
  const entity = {
    data: {url, title, image, tags, createdAt: Date.now()},
    key: db.key(['Page', url])
  }
  return db.save(entity)
}

const getPagesSchema = Joi.object().keys({
  query: Joi.object().keys({
    tag: Joi.string().min(1).max(30).required()
  })
})

const putPageSchema = Joi.object().keys({
  body: Joi.object().keys({
    page: Joi.object().keys({
      url: Joi.string().min('http://a.fi'.length).max(2000).required(),
      title: Joi.string().max(300).required(),
      image: Joi.string().max(2000).allow(null),
      tags: Joi.array().items(Joi.string().min(1).max(30))
    })
  })
})

exports.getPages = (req, res) => {
  Joi.validate(req, getPagesSchema, {allowUnknown: true}, (err, validatedReq) => {
    if (err) {
      return validationError(res, err)
    }
    getPagesWithTag(validatedReq.query.tag)
      .then(pages => res.status(200).json({pages: pages}))
      .catch(errorHandler(res))
  })
}

exports.putPage = (req, res) => {
  Joi.validate(req, putPageSchema, {allowUnknown: true}, (err, validatedReq) => {
    if (err) {
      return validationError(res, err)
    }
    createPage(validatedReq.body.page)
      .then(() => res.status(201).end())
      .catch(errorHandler(res))
  })
}