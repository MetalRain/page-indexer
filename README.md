# Smart bookmarks
Chrome extension for storing and viewing rich bookmark information and simple backend service for querying stored bookmarks.

## Why?
I like to bookmark many cool pages, but never visit them afterwards. As this extension shows some related pages there is higher chance to revisit interesting articles or other content.

Once enough data has been collected, backend can be replaced with more intelligent one.

## Server

Backend consists of three Google Cloud Functions:
- getPages, for querying pages with tag.
- getPageInfo, for querying page and related pages with url.
- putPage, for adding / updating new pages.

Pages are saved in Google Datastore.

### Setting up

1. Create project in Google cloud
2. Enable billing
3. Enable Google Cloud Functions API
4. Enable Datastore
5. Set up Google Cloud SDK
6. Authenticate with Google Cloud SDK

### Deployment

Build Datastore indexes:
```sh
gcloud datastore create-indexes index.yaml
```

Deploy Cloud functions
```
gcloud beta functions deploy getPageInfo --stage-bucket ${BUCKET_NAME} --trigger-http
gcloud beta functions deploy getPages --stage-bucket ${BUCKET_NAME} --trigger-http
gcloud beta functions deploy putPage --stage-bucket ${BUCKET_NAME} --trigger-http
```

### Debugging

View logs from CLI:
```sh
gcloud beta functions logs read getPages
gcloud beta functions logs read putPage
```

### Inserting data manually

1. Go to Google Cloud Console
2. Go to Cloud Functions
3. View putPage function
4. Go to Testing
5. Insert page data as JSON: 

```json
{
  "page": {
    "image": "https://ssl.gstatic.com/pantheon/images/favicon/functions.png",
    "tags": ["google", "dashboard", "api", "functions"],
    "title": "Google Cloud Platform - Cloud Functions",
    "url": "https://console.cloud.google.com"
  }
}
```

## Browser extension

Pages are bookmarked with [Google Chrome Extension](https://developer.chrome.com/extensions), which also shows related pages. Extension uses following technologies:
- [Mithril.js](https://mithril.js.org/)
- [Pure.css](https://purecss.io/)
- [Moment.js](https://momentjs.com/)
- [Webpack](https://webpack.github.io/)
- [Yarn](https://yarnpkg.com/en/)

### Building extension

Make sure you have Yarn installed.

Install dependencies:
```sh
cd extension
yarn install
```

Build production build to `extension/dist` folder
```sh
cd extension
yarn build
```

### Loading extension

1. Open Google Chrome
2. Open page chrome://extensions/
3. Check `Developer mode` checkbox
4. Press button `Load unpacked extension`
5. Choose folder `<repo_path>/extension/dist`

Now extension should have added new "B" button in your toolbar.

### Usage

Press "B" button to bookmark page, extension shows current page details and loads pages related to current page tags. Click tags to load pages related to that tag. Click related page to load that in new browser tab.