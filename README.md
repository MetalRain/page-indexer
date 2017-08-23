# Smart bookmarks
Chrome extension for saving bookmarks & backend service for sharing bookmark data with other users.

## Why?
I tend to bookmark many pages, but when I need to find that thing again, I cannot remember what page was or what was the title. Having tags in bookmarks allows me to search for bookmarks having tags.

## Server

Server is implemented as three Google Cloud Functions. See more from [server](server/README.md).

## Frontend

Frontend is [Google Chrome Extension](https://developer.chrome.com/extensions), built with:
- [Mithril.js](https://mithril.js.org/)
- [Pure.css](https://purecss.io/)
- [Webpack](https://webpack.github.io/)
- [Yarn](https://yarnpkg.com/en/)