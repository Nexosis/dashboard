import Intercept from './js/intercept';
import Prism from 'prismjs';
import 'prismjs/components/prism-json.min.js';
import 'prismjs/themes/prism-okaidia.css';
import Elm from './Main.elm';
import 'loggly-jslogger';
import '../config.json';

if (!Intercept.isWired()) {
    Intercept.wire();
}

fetch('./config.json').then(function (response) {
    response.json().then(function (config) {

        _LTracker.push({
            'logglyKey': config.loggly.key,
            'sendConsoleErrors': (config.loggly.sendConsoleErrors === 'true'),
            'tag': 'dashboard'
        });

        const logger = function (logMessage, level = 'Information') {
            _LTracker.push({
                'Environment': config.loggly.environment,
                'Message': logMessage,
                'Level': level
            });
        };

        const mountNode = document.getElementById('main');
        const app = Elm.Main.embed(main, config);

        app.ports.prismHighlight.subscribe(function () {
            requestAnimationFrame(() => {
                Prism.highlightAll();
            });
        });

        app.ports.log.subscribe(function (logString) {
            const logMessage = JSON.parse(logString);
            logger(logMessage.message, logMessage.level);
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
            }
        });
    });
});