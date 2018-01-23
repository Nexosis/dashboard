# Nexosis API Dashboard

Interact with the Nexosis API with a dashboard.

## Get It Running

**1.** Install `elm` and `elm-live`:

> npm install -g elm elm-live

**2.** Set url and api key:

Create a file, config.json.  Provide your api key.

``` JSON
{
    "apiKey": "My Key",
    "url": "https://ml.nexosis.com"
}
```

**3.** Build and run:

> elm-live --output=elm.js src/Main.elm --pushstate --open --debug
