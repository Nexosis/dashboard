import Intercept from './js/intercept';
import Prism from 'prismjs';
import 'prismjs/components/prism-json.min.js';
import 'prismjs/themes/prism-okaidia.css';
import 'balloon-css';
import Elm from './Main.elm';
import StackTrace from 'stacktrace-js';
import '../config.json';
import '../tooltips.json';
import { _LTracker } from 'loggly-jslogger';
import { getCookie } from './js/cookies';


if (!Intercept.isWired()) {
    Intercept.wire();
}

fetch('./config.json').then(function (response) {
    response.json().then(function (config) {

        _LTracker.push({
            'logglyKey': config.loggly.key,
            'sendConsoleErrors': false,
            'tag': 'dashboard',
        });

        if (config.loggly.sendConsoleErrors === 'true') {
            window.onerror = function (msg, file, line, col, error) {
                StackTrace.fromError(error).then(stack => {
                    _LTracker.push({
                        msg,
                        file,
                        stack,
                        'Level': 'Error',
                        userAgent: navigator['userAgent']
                    });
                }).catch(console.log);
            }
        }

        const logger = function (logMessage, level = 'Information') {
            console.log(logMessage);
            _LTracker.push({
                'Environment': config.loggly.environment,
                'Message': logMessage,
                'Level': level
            });
        };

        config.token = getCookie('accessToken');

        const mountNode = document.getElementById('main');
        const app = Elm.Main.embed(main, config);

        app.ports.log.subscribe(function (logString) {
            const logMessage = JSON.parse(logString);
            logger(logMessage.message, logMessage.level);
        });

        app.ports.prismHighlight.subscribe(function () {
            requestAnimationFrame(() => {
                Prism.highlightAll();
            });
        });


        app.ports.drawVegaChart.subscribe(function (specObject) {
            requestAnimationFrame(() => {
                for (let name of Object.keys(specObject)) {
                    vegaEmbed(`#histogram_${name}`, specObject[name], {
                        actions: false, logLevel: vega.Warn
                    }).catch(console.warn);
                }
            });
        });

        Intercept.addResponseCallback(function (xhr) {

            if (xhr.url.startsWith(config.url)) {
                let xhrInfo = {
                    status: xhr.status,
                    statusText: xhr.statusText,
                    response: JSON.stringify(JSON.parse(xhr.response), null, 2),
                    method: xhr.method,
                    url: xhr.url,
                    timestamp: new Date().toISOString()
                };

                app.ports.responseReceived.send(xhrInfo);

                if (xhr.status >= 300) {
                    logger(xhr.response, 'Warning');
                }
            }
        });
    });
});