'use strict';

var COMPLETED_READY_STATE = 4;

var RealXHRSend = XMLHttpRequest.prototype.send;
var RealXHROpen = XMLHttpRequest.prototype.open;

var requestCallbacks = [];
var responseCallbacks = [];


var wired = false;


function arrayRemove(array, item) {
    var index = array.indexOf(item);
    if (index > -1) {
        array.splice(index, 1);
    } else {
        throw new Error("Could not remove " + item + " from array");
    }
}


function fireCallbacks(callbacks, xhr, callArgs) {
    for (var i = 0; i < callbacks.length; i++) {
        callbacks[i](xhr, callArgs);
    }
}

function fireResponseCallbacksIfCompleted(xhr) {
    if (xhr.readyState === COMPLETED_READY_STATE) {
        fireCallbacks(responseCallbacks, xhr);
    }
}

function proxifyOnReadyStateChange(xhr) {
    var realOnReadyStateChange = xhr.onreadystatechange;
    if (realOnReadyStateChange) {
        xhr.onreadystatechange = function () {
            fireResponseCallbacksIfCompleted(xhr);
            realOnReadyStateChange();
        };
    }
}

const Intercept = {
    addRequestCallback: function (callback) {
        requestCallbacks.push(callback);
    },
    removeRequestCallback: function (callback) {
        arrayRemove(requestCallbacks, callback);
    },
    addResponseCallback: function (callback) {
        responseCallbacks.push(callback);
    },
    removeResponseCallback: function (callback) {
        arrayRemove(responseCallbacks, callback);
    },
    isWired: function () {
        return wired;
    },
    wire: function () {
        if (wired) throw new Error("Ajax interceptor already wired");

        XMLHttpRequest.prototype.open = function (method, url) {
            this.method = method;
            this.url = url;
            return RealXHROpen.apply(this, arguments);
        }

        // Override send method of all XHR requests
        XMLHttpRequest.prototype.send = function () {

            // Fire request callbacks before sending the request
            fireCallbacks(requestCallbacks, this, arguments);

            // Wire response callbacks
            if (this.addEventListener) {
                var self = this;
                this.addEventListener("readystatechange", function () {
                    fireResponseCallbacksIfCompleted(self);
                }, false);
            }
            else {
                proxifyOnReadyStateChange(this);
            }

            RealXHRSend.apply(this, arguments);
        };
        wired = true;
    },
    unwire: function () {
        if (!wired) throw new Error("Ajax interceptor not currently wired");
        XMLHttpRequest.prototype.send = RealXHRSend;
        wired = false;
    }
}

export default Intercept;