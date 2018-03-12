import Intercept from './js/intercept';
import Prism from 'prismjs';
import 'prismjs/components/prism-json.min.js';
import 'prismjs/themes/prism-okaidia.css';
import 'balloon-css';
import Elm from './Main.elm';
import StackTrace from 'stacktrace-js';
import '../config.json';
import * as toolTips from '../tooltips.json';
import { _LTracker } from 'loggly-jslogger';
import { getCookie } from './js/cookies';
import '../interim.css'
import '../elm-datepicker.css'
import '../elm-autocomplete.css'
import '../docs.js'
import { initLocalStoragePort } from './js/localStoragePort';

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
        
        const docsRequests = []
        for(let doc of config.explainers) {
            const tmp = doc;
            docsRequests.push(
                fetch(`./docs/${doc}.md`)
                    .then (resp => {
                        if(resp.ok) {
                            return resp.text().then(text=>{return {name: tmp, content: text}});
                        }
                        return Promise.resolve({err : resp.status});
                    }
                ));
        }

        Promise.all(docsRequests).then(docsContent => {

            config.token = getCookie('accessToken');
            config.toolTips = toolTips;
            
            config.explainerContent = {};
            for(let c of docsContent) {
                if(!c.err)
                {
                    config.explainerContent[c.name] = c.content
                }
                else {
                    console.log(c.err);
                }
                    
            }

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
                        vegaEmbed(`#${name}`, specObject[name], {
                            actions: false, logLevel: vega.Warn
                        }).catch(console.warn);
                    }
                });
            });

            initLocalStoragePort(app);
            
            app.ports.uploadFileSelected.subscribe(function (id) {

                var node = document.getElementById(id);
                if (node === null) {
                    return;
                }

                var file = node.files[0];
                var reader = new FileReader();

                reader.onload = (function (event) {
                    try {

                        var fileContent = event.target.result;

                        var portData = {
                            contents: fileContent,
                            filename: file.name,
                            status: 'Success'
                        };

                        app.ports.fileContentRead.send(portData);
                    }
                    catch (e) {
                        app.ports.fileContentRead.send({ status: 'ReadFail' })
                    }
                });

                reader.onerror = (function (event) {
                    app.ports.fileContentRead.send({ status: 'ReadFail' })
                });

                reader.readAsText(file);
            });


            app.ports.requestSaveFile.subscribe(function (filespec) {
                var a = document.createElement("a");
                var fileUrl = URL.createObjectURL(new Blob([filespec.contents], { type: filespec.contentType }));
                a.href = fileUrl;
                a.download = filespec.name;
                document.body.appendChild(a);
                a.click();
                setTimeout(function(){
                    document.body.removeChild(a);
                    window.URL.revokeObjectURL(fileUrl);  
                }, 100);  
                app.ports.fileSaved.send(true);
            });


            Intercept.addResponseCallback(function (xhr) {
                
                if (xhr.url.startsWith(config.apiUrl)) {

                const getQuotaHeader = (xhr, name) => {
                    return {
                        allotted : parseInt(xhr.getResponseHeader(`Nexosis-Account-${name}-Allotted`) || '0'),
                        current : parseInt(xhr.getResponseHeader(`Nexosis-Account-${name}-Current`) || '0')
                    }
                }

                let quotas = {  
                    dataSets : getQuotaHeader(xhr, "DataSetCount"),
                    predictions : getQuotaHeader(xhr, "PredictionCount"),
                    sessions : getQuotaHeader(xhr, "SessionCount"),
                }


                    let xhrInfo = {
                        status: xhr.status,
                        statusText: xhr.statusText,
                        response: JSON.stringify(JSON.parse(xhr.response), null, 2),
                        method: xhr.method,
                        url: xhr.url,
                    quotas : quotas,
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
});