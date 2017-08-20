const datastore = require('@google-cloud/datastore')
const db = datastore()
const uniqBy = require('lodash/uniqBy')

const outputPage = ({url, title, image, tags}) => {
  return {url, title, image, tags}
}

exports.getPageWithUrl = (url) => {
  const key = db.key(['Page', url])
  return db.get(key)
    .then(([result]) => outputPage(result))
}

const queryPagesWithTag = (searchTag, limit = 10) => {
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
exports.queryPagesWithTag = queryPagesWithTag

exports.getPagesWithTags = (tags, excludedURL = null, limitPerTag = 3) => {
  // Get 3 pages for each tag
  return Promise.all(
    (tags || []).map(tag => queryPagesWithTag(tag, limitPerTag))
  ).then(groupedByTag => {
      // Flatten and filter pages
      const related = groupedByTag.reduce((result, pageGroup) => {
          const filtered = pageGroup.filter(page => page.url !== excludedURL)
          return result.concat(filtered)
      }, [])
      return uniqBy(related, page => page.url)
  })
}

exports.createPage = ({url, title, image, tags}) => {
  const entity = {
    data: {url, title, image, tags, createdAt: Date.now()},
    key: db.key(['Page', url])
  }
  return db.save(entity)
}