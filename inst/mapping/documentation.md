
## Fields
The file consists of a list of field translation definitions.

* `spectraKey`: the variable name in the R (`Spectra`) object. In addition to the fields predefined by the `Spectra` object, we maintain a list of `lowerCamelCase` field names.
* `formatKey`: the variable name in the spectrum file / record.
* `formatKeyRead`: One or more variable names in the spectrum file which map to this `spectraKey`. If not set, `formatKey` is used.
* `formatKeyWrite`: The variable name to write to in the spectrum file/record. If not set, `formatKey` is used.
* `dictionary`: A verbatim mapping from values in the record to values in the R object.
  * Contains entries `{value, read, write}` where `read` may have multiple entries mapping to the same R value.
  * Note: if there is a dictionary, it must be exhaustive. Any spectrum entry that doesn't match a dictionary entry is dropped.
  * Todo: is this good?
* `regexRead`: A regular expression to apply for reading the record value to an R value.
  * Contains entries (usually one) `{match, sub}`. Multiple entries are applied sequentially.
* `regexWrite`: A regular expression to apply when writing the R value to a record.
  * Contains entries (usually one) `{match, sub}`. Multiple entries are applied sequentially.
  
## To do

* `order, weight` or such
* `process` in the same way as `regex`, but specifying a function name
  * or just generally have a $processRead and $processWrite named function list in the format?
* `fold, unfold`?
