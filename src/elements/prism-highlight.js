class PrismHighlight extends HTMLElement  {
    constructor() {
        super();
    }

    connectedCallback() {
        var pre = document.createElement('pre');
        var code = document.createElement('code');
        code.className = this.className;
        pre.className = this.className;

        code.textContent = this.textContent.trim();
        this.innerText = "";

        pre.appendChild(code);
        this.appendChild(pre);

        Prism.highlightElement(code);

    }
}

customElements.define('prism-highlight', PrismHighlight);