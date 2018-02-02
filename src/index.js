import Intercept from './js/intercept';
import Prism from 'prismjs'
import Elm from './Main.elm';

if (!Intercept.isWired) {
    Intercept.wire();
}

fetch('./config.json').then(function (response) {
    response.json().then(function (result) {
        var mountNode = document.getElementById('main');
        var app = Elm.Main.embed(main, result);

        app.ports.prismHighlight.subscribe(function () {
            requestAnimationFrame(() => {
                Prism.highlightAll();
            });
        });

        Intercept.addResponseCallback(function (xhr) {

            var xhrInfo = {
                status: xhr.status,
                statusText: xhr.statusText,
                response: JSON.stringify(JSON.parse(xhr.response), null, 2),
                method: xhr.method,
                url: xhr.url,
                timestamp: new Date().toISOString()
            };

            app.ports.responseReceived.send(xhrInfo);
        });
    });
});
