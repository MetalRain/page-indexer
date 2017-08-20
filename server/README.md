# Page backend

Backend consists of two Google Cloud Functions:
- getPages, for querying pages with tags
- putPage, for adding new pages

Pages are saved in Google Datastore.

## Setting up

1. Create project in Google cloud
2. Enable billing
3. Enable Google Cloud Functions API
4. Enable Datastore
5. Set up Google Cloud SDK
6. Authenticate

## Deployment

```
gcloud datastore create-indexes index.yaml
gcloud beta functions deploy getPages --stage-bucket BUCKET_NAME --trigger-http
gcloud beta functions deploy putPage --stage-bucket BUCKET_NAME --trigger-http
```

## Debugging

View logs:
```sh
gcloud beta functions logs read getPages
gcloud beta functions logs read putPage
```

Creating pages manually:

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
