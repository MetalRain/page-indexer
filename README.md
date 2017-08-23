# Smart bookmarks
Chrome extension for storing and viewing rich bookmark information and simple backend service for querying stored bookmarks.

## Why?
I like to bookmark many cool pages, but never visit them afterwards. As this extension shows some related bookmarks there is higher chance to revisit interesting articles or other content. 

Once enough data has been collected, backend can be replaced with more intelligent one.

## Server
Server is implemented as three Google Cloud Functions. See more from [server](server/README.md).

## Frontend

Frontend is [Google Chrome Extension](https://developer.chrome.com/extensions), built with:
- [Mithril.js](https://mithril.js.org/)
- [Pure.css](https://purecss.io/)
- [Webpack](https://webpack.github.io/)
- [Yarn](https://yarnpkg.com/en/)