/**
 * -- From https://raw.githubusercontent.com/flq/elmorse/
 */
const storage = window.localStorage || {
  setItem(k, v) {
    this[k] = v;
  },
  getItem(k) {
    return this[k];
  }
};

export function initLocalStoragePort(elmApp) {
  elmApp.ports.storeObject.subscribe(function ([key, state]) {
    storeObject(key, state);
    elmApp.ports.objectRetrieved.send([key, state]);
  });
  elmApp.ports.retrieveObject.subscribe(function (key) {
    const o = retrieveObject(key);
    elmApp.ports.objectRetrieved.send([key, o]);
  });

}

function storeObject(key, object) {
  storage.setItem(key, JSON.stringify(object));
}

function retrieveObject(key) {
  const value = storage.getItem(key);
  return value ? JSON.parse(value) : null;
}