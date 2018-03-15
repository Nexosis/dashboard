##### How to import from a URL
Any publicly accessible CSV or JSON file can be uploaded via import. An import allows you to load any size file as opposed to the 1MB limit on uploaded files. The import will run in the background via the Nexosis API and should complete within a minute or two depending on file size. Our API will attempt to determine the file type but is most reliable when the MIME type is correct. Watch out for file sharing URLs as they often point to HTML pages rather than directly to file contents.

You can read more about imports in our <a href="http://docs.nexosis.com/guides/importing-data#importing-by-url" target="_blank">Import documentation</a>.

**JSON files must match the format <a href="https://developers.nexosis.com/docs/services/98847a3fbbe64f73aa959d3cededb3af/operations/datasets-add-data" target="_blank">specified by the Nexosis API</a>**