##### How to import from a AWS S3
If you have a `CSV` or `JSON` file hosted out on <a href="https://aws.amazon.com/s3">AWS S3</a>, you can tell the Nexosis Platform to import that data into a Dataset. 

An import allows you to load any size file as opposed to the 1MB limit on uploaded files. The import will run in the background via the Nexosis API and should complete within a minute or two depending on file size. Gzipped files are supported.

Our API will attempt to determine the file type based on the extension of the file, and failing that its contents.  It has an easier time of things if the file path ends in one of `.csv`, `.json`, `.csv.gz`, or `.json.gz`, though.

You can read more about imports in our <a href="http://docs.nexosis.com/guides/importing-data#importing-from-aws-s3" target="_blank">Import documentation</a>.

**JSON files must match the format <a href="https://developers.nexosis.com/docs/services/98847a3fbbe64f73aa959d3cededb3af/operations/datasets-add-data" target="_blank">specified by the Nexosis API</a>**