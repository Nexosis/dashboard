// More info on Webpack's Node API here: https://webpack.github.io/docs/node.js-api.html
// Allowing console calls below since this is a build file.
/* eslint-disable no-console */
import webpack from 'webpack';
import prodConfig from '../webpack.config.prod';
import devConfig from '../webpack.config.dev';
import { chalkError, chalkSuccess, chalkWarning, chalkProcessing } from './chalkConfig';

let config;
if (process.env.ENV === 'production') {
    config = prodConfig;
    process.env.NODE_ENV = 'production'; // this assures React is built in prod mode and that the Babel dev config doesn't apply.
    console.log(chalkProcessing('Generating minified bundle. This will take a moment...'));
} else {
    config = devConfig;
    console.log(chalkProcessing('Generating dev bundle. This will take a moment...'));
}

webpack(config).run((error, stats) => {
    if (error) { // so a fatal error occurred. Stop here.
        console.log(chalkError(error.stack || error));
        if (error.details) {
            console.log(chalkError(error.details));
        }
        throw 'Fatal build error.';
    }

    const jsonStats = stats.toJson();

    if (stats.hasErrors()) {
        jsonStats.errors.map(error => console.log(chalkError(error)));
        throw 'Build error occurred.';
    }

    if (stats.hasWarnings()) {
        console.log(chalkWarning('Webpack generated the following warnings: '));
        jsonStats.warnings.map(warning => console.log(chalkWarning(warning)));
    }

    console.log(`Webpack stats: ${stats}`);

    var mode = 'dev';
    if (process.env.ENV === 'production') {
        mode = 'production';
    }
    // if we got this far, the build succeeded.
    console.log(chalkSuccess(`Your app is compiled in ${mode} mode in /js/_webpacked. It\'s ready to roll!`));

    return 0;
});
