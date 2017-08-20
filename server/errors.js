exports.errorHandler = (res) => {
  return err => {
    console.error(err, err.stack)
    res.status(500).end()
  }
}

exports.validationError = (res, joiError) => {
  res.status(400).send({errors: joiError.details})
}