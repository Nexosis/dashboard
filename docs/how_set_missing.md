##### How are missing values handled?

The DataSet you are uploading may already have a convention for representing missing data for a particular field.  Some common conventions are "N/A", "null", "missing", "-" etc.  If this is the case, it may be easier to specify how missing values are labeled when uploading the DataSet, than manually removing them from your data.  Any time this data is processed for a machine learning algorithm, values matching these will be treated as a missing value and imputed.  This ensures that the algorithms are able to correctly identify when a value is truly missing.

To specify more than one value, enter the values as a comma separated list, such as `N/A,null,missing,-`.