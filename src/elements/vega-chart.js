class VegaChart extends HTMLElement {
    get spec() {
        return this.getAttribute('spec');
    }

    connectedCallback() {
        vegaEmbed(this, JSON.parse(this.spec));
    }
}

window.addEventListener('WebComponentsReady', function(){
    customElements.define('vega-chart', VegaChart);
});
customElements.define('vega-chart', VegaChart);