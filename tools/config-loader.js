import fs from 'fs';

module.exports = function (source) {
    this.addDependency('./config.json');
    this.cacheable();
    const config = JSON.parse(fs.readFileSync('./config.json'));

    const merged = Object.assign({}, JSON.parse(source), {
        'apiKey': process.env.NEXOSIS_API_KEY,
        'apiUrl': process.env.NEXOSIS_API_URL || config.apiUrl
    });

    let indentation = this.minimize ? null : 2
    return JSON.stringify(merged, null, indentation) + '\n';
}