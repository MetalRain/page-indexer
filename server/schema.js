const Joi = require('joi')

const Tag = Joi.string().min(1).max(30)
const URL = Joi.string().min('http://a.fi'.length).max(2000)

exports.getPagesSchema = Joi.object().keys({
  query: Joi.object().keys({
    tag: Tag.required()
  })
})

exports.putPageSchema = Joi.object().keys({
  body: Joi.object().keys({
    page: Joi.object().keys({
      url: URL.required(),
      title: Joi.string().max(300).required(),
      image: URL.allow(null),
      tags: Joi.array().items(Tag)
    })
  })
})

exports.getPageInfoSchema = Joi.object().keys({
  query: Joi.object().keys({
    url: URL.required()
  })
})

exports.validateRequest = (req, schema) => {
  return new Promise((resolve, reject) => {
    Joi.validate(req, schema, {allowUnknown: true}, (err, validatedReq) => {
      if (err) {
        return reject(err)
      }
      resolve(validatedReq)
    })
  })
}