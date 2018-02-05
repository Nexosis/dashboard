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

        var mountNode = document.getElementById('main');
        var app = Elm.Main.embed(main, config);

        app.ports.prismHighlight.subscribe(function () {
            requestAnimationFrame(() => {
                Prism.highlightAll();
            });
        });

        Intercept.addResponseCallback(function (xhr) {

            if (xhr.url.startsWith(config.url)) {
                var xhrInfo = {
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

