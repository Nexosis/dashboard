##### Uploading a CSV file
Our CSV parser can deal with a few ambiguities but make sure all rows have the same number of commas/values and that your file does not contain a BOM. 

CSV files uploaded through our dashboard *must* to have a header row.  Otherwise, we will skip the first record in your data set.

##### Uploading a JSON file
A JSON file can be useful if you want to set metadata when you create the data source. Your JSON file should at a minimum contain a field called *data* which contains the array of values:

<prism-highlight class="language-json">
{
  "data": [
    {
      "column1": 1,
      "column2": 2
    },
    {
      "column1": 3,
      "column2": 4
    }
  ]
}
</prism-highlight>

If you would like to also provide the metadata add columns definition in line with <a href="http://docs.nexosis.com/guides/column-metadata#example" target="_blank">the documented format</a>

**Any file upload must have a size on disk of less than 1MB.**