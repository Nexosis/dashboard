# Nexosis API Dashboard

Interact with the [Nexosis API](https://nexosis.com) using this dashboard front-end.

The Nexosis API provides an easy way to build machine learning applications.  It provides a Rest API that allows you to write programs that upload and manage data, run machine learning algorithms to build models, and make predictions on new data.

This dashboard is the [management interface](https://account.nexosis.com) which uses the API under the hood to let you carry out the same tasks.

The source is primarily [Elm](http://elm-lang.org/), and uses the [Nexosis Elm client library](https://github.com/Nexosis/nexosisclient-elm) to call the API.

## Prepare your environment

**1.** Update your configuration settings in VsCode to include elm-format by adding these properties:

Or, if you have elm-format installed globally already you can skip this step.

### OSX/Linux settings

``` json
    "elm.formatCommand": "./node_modules/.bin/elm-format",
    "[elm]": {
        "editor.formatOnSave": true,
    },
```

### Windows settings

``` json
    "elm.formatCommand": ".\\node_modules\\.bin\\elm-format",
    "[elm]": {
        "editor.formatOnSave": true,
    },
```

## Get It Running

**1.** Run Yarn to Install
If you need to install yarn, do so locally either through brew, or downloading the package installer (https://yarnpkg.com/en/docs/install)

> yarn

**2.** Set API Key
Set an API key as an environment variable.  This key will get picked up during the build and added to the web project.

The name of the environment variable is **NEXOSIS_API_KEY**

You can find a key on your [Nexosis Account](https://account.nexosis.com).

Note that your API key should not be deployed with this application.  If you would like to deploy the dashboard to your own server, please contact us.  We can help you get setup with token authentication.

**3.** Build and run:

> yarn open-app