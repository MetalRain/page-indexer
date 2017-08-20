const datastore = require('@google-cloud/datastore')
const db = datastore()

const outputPage = ({url, title, image, tags}) => {
  return {url, title, image, tags}
}

exports.getPageWithUrl = (url) => {
  const key = db.key(['Page', url])
  return db.get(key)
    .then(([result]) => outputPage(result))
}

exports.queryPagesWithTag = (searchTag, limit = 10) => {
  const query = db.createQuery('Page')
    .filter('tags', '=', searchTag)
    .order('createdAt', {descending: true})
    .limit(limit)

  return db.runQuery(query)
    .then(([pages, info]) => {
      if (pages && pages.length > 0){
        return pages.map(outputPage)
      }
      return []
    })
}

exports.createPage = ({url, title, image, tags}) => {
  const entity = {
    data: {url, title, image, tags, createdAt: Date.now()},
    key: db.key(['Page', url])
  }
  return db.save(entity)
}