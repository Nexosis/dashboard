class VegaChart extends HTMLElement {
    get spec() {
        return this.getAttribute('spec');
    }

    connectedCallback() {
        var _spec = this.spec;
        vegaEmbed(this, JSON.parse(_spec), { actions: false, logLevel: vega.Warn })
        .then(function(result) {
            vegaTooltip.vegaLite(result.view, result.spec, { colorTheme: 'dark'});
        })
        .catch(console.warn);
        
    }
}

window.addEventListener('WebComponentsReady', function(){
    customElements.define('vega-chart', VegaChart);
});
customElements.define('vega-chart', VegaChart);