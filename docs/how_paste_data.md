##### How to enter data
If you have a small amount of data to use then you may paste it directly into the text box. The format of the data must either be a CSV or adhere to our JSON format ('columns' object is optional):


<prism-highlight class="language-json">
{
  "columns": {
    "Date": { "role" : "timestamp" }
  },
  "data": [
    {
      "Date": "2008-04-08 15:00:00+08:00",
      "Count": 207
    }
  ]
}
</prism-highlight>