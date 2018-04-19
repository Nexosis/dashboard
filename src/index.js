import Intercept from './js/intercept';
import Prism from 'prismjs';
import 'prismjs/components/prism-json.min.js';
import 'prismjs/themes/prism-okaidia.css';
import 'balloon-css';
import Elm from './Main.elm';
import StackTrace from 'stacktrace-js';
import '../config.json';
import toolTips from '../tooltips.json';
import { _LTracker } from 'loggly-jslogger';
import { getCookie } from './js/cookies';
import '../elm-datepicker.css'
import '../elm-autocomplete.css'
import '../vega-tooltip.css'
import '../docs.js'
import { initLocalStoragePort } from './js/localStoragePort';
import 'nexosis-styles/bootstrap-custom.css';
import 'nexosis-styles/nexosis.css';
import 'nexosis-styles/api-styles.css';
import 'nexosis-styles/docs-styles.css';
import 'nexosis-styles/hubspot-forms.css';
import './elements/vega-chart';

if (!Intercept.isWired()) {
    Intercept.wire();
}

fetch('./config.json', { cache: 'no-store' }).then(function (response) {
    response.json().then(function (config) {
        let logger;

        if (config.loggly.key) {
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

            logger = function (logMessage, level = 'Information') {
                console.log(logMessage);
                _LTracker.push({
                    'Environment': config.loggly.environment,
                    'Message': logMessage,
                    'Level': level
                });
            };
        } else {
            logger = function (logMessage, level = 'Information') {
                console.log(logMessage);
            }
        }

        const docsRequests = []
        for (let doc of config.explainers) {
            const tmp = doc;
            docsRequests.push(
                fetch(`./docs/${doc}.md`)
                    .then(resp => {
                        if (resp.ok) {
                            return resp.text().then(text => { return { name: tmp, content: text } });
                        }
                        return Promise.resolve({ err: resp.status });
                    }
                    ));
        }

        Promise.all(docsRequests).then(docsContent => {
            config.cookie = getCookie('accessToken');
            config.toolTips = toolTips;

            config.explainerContent = {};
            for (let c of docsContent) {
                if (!c.err) {
                    config.explainerContent[c.name] = c.content
                }
            }

            const mountNode = document.getElementById('main');
            mountNode.innerHTML = '';
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

            initLocalStoragePort(app);

            app.ports.uploadFileSelected.subscribe(function (info) {

                const id = info[0];
                const maxSize = info[1];

                var node = document.getElementById(id);
                if (node === null) {
                    return;
                }

                var file = node.files[0];
                if (file !== undefined) {
                    if (file.size > maxSize) {
                        app.ports.fileContentRead.send({ status: 'FileTooLarge' });
                    } else {

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
                                app.ports.fileContentRead.send({ status: 'UnknownError' });
                            }
                        });

                        reader.onerror = (function (event) {
                            app.ports.fileContentRead.send({ status: 'UnknownError' });
                        });

                        reader.readAsText(file);
                    }
                    //clear the value out so that we can re-select the same file later if we want to
                    node.value = null;
                }
            });


            app.ports.requestSaveFile.subscribe(function (filespec) {
                var a = document.createElement("a");
                var fileUrl = URL.createObjectURL(new Blob([filespec.contents], { type: filespec.contentType }));
                a.href = fileUrl;
                a.download = filespec.name;
                document.body.appendChild(a);
                a.click();
                setTimeout(function () {
                    document.body.removeChild(a);
                    window.URL.revokeObjectURL(fileUrl);
                }, 100);
                app.ports.fileSaved.send(true);
            });

            Intercept.addResponseCallback(function (xhr) {

                if (xhr.url.startsWith(config.apiUrl)) {

                    const getQuotaHeader = (xhr, name, allottedOnly) => {
                        const result = {
                            allotted: parseInt(xhr.getResponseHeader(`Nexosis-Account-${name}-Allotted`) || '0')
                        }
                        if (!allottedOnly) {
                            result.current = parseInt(xhr.getResponseHeader(`Nexosis-Account-${name}-Current`) || '0')
                        }
                        return result;
                    }

                    let quotas = {
                        dataSets: getQuotaHeader(xhr, "DataSetCount"),
                        predictions: getQuotaHeader(xhr, "PredictionCount"),
                        sessions: getQuotaHeader(xhr, "SessionCount"),
                        dataSetSize: getQuotaHeader(xhr, "DataSetSize", true)
                    }

                    let responseText = '';
                    if (xhr.response) {
                        responseText = JSON.stringify(JSON.parse(xhr.response), null, 2);
                    }

                    let xhrInfo = {
                        status: xhr.status,
                        statusText: xhr.statusText,
                        response: responseText,
                        method: xhr.method,
                        url: xhr.url,
                        quotas: quotas,
                        timestamp: new Date().toISOString()
                    };

                    app.ports.responseReceived.send(xhrInfo);

                    if (xhr.status >= 300) {
                        logger(xhr.response, 'Warning');
                    }
                }
            });

            const clipboard = new ClipboardJS('.copyToClipboard');
            clipboard.on('success', function (e) {

                e.trigger.setAttribute("data-balloon", "Copied");
                e.trigger.setAttribute("data-balloon-pos", "right");

                setTimeout(function () {
                    e.trigger.removeAttribute("data-balloon");
                    e.trigger.removeAttribute("data-balloon-pos")
                }, 1500)
            });

            app.ports.setPageTitle.subscribe(function (title) {
                document.title = `${title} - Nexosis API`;
            });

            app.ports.scrollIntoView.subscribe(function (elementId) {
                requestAnimationFrame(() => {
                    let elem = document.getElementById(elementId);
                    if (elem) {
                        elem.scrollIntoView(true);
                    }
                });
            });

            app.ports.highlightIds.subscribe(function (elementIds) {
                requestAnimationFrame(() => {
                    elementIds.forEach(e => {
                        const elem = document.getElementById(e);
                        if (elem) {
                            elem.addEventListener("animationend", () => { elem.classList.remove('save-success'); })
                            elem.classList.add('save-success');
                        }
                    });
                });
            });

            app.ports.setHeaderValues.subscribe(function (headerValues) {
                const userName = document.getElementById('header_username');
                const overview = document.getElementById('header_overview');
                const refer = document.getElementById('header_refer');
                const signout = document.getElementById('header_signout');

                if (userName) {
                    userName.innerHTML = headerValues.userName + '<b class="caret"></b>';
                }
                if (overview) {
                    overview.href = headerValues.overviewLink;
                }
                if (refer) {
                    refer.href = headerValues.referLink;
                }
                if (signout) {
                    signout.href = headerValues.logoutLink;
                }
            });

            if (hbspt && config.hubspot && config.hubspot.portalId) {
                hbspt.forms.create({
                    css: '',
                    portalId: config.hubspot.portalId,
                    formId: config.hubspot.newsletterFormId,
                    target: '#hs-footer'
                });
            }
        });
    });
});
