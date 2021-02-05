# What is `SpectraMapping` good for?

The `Spectra` package provides R with a high-performance infrastructure to handle mass spectra. Importantly, using multiple *backends*, 
mass spectral data from different sources can be accessed. A `Spectra` object provides infrastructure to handle both the mass spectra and their associated metadata. However, `Spectra` handles mostly the technical aspects, storage and access to metadata, and standardizes only the core aspects (precursor, MS level etc), and provides the freedom to handle any additional metadata (chemical identity, peak annotation, authorship etc) without imposing specific semantics; these are relevant mostly for spectral libraries.

Some prominent text formats used for spectra exchange (MGF, MSP) are simple in principle, and allow key-value annotations. However, the details are not specified and many specific usages exist. Specifically, many implementations bypass perceived limitations e.g. by storing additional key-value pairs in `Comment` fields, specifying ion annotations with `/`-separated lists, etc. Default parsers (`MsBackendMgf`) provide a basic way to map key-value annotations to `Spectra` metadata, allowing some flexibility but not handling advanced parsing. `SpectraMapping` is an attempt to provide a tool to handle interconversion of (mainly) metadata between spectra formats using a format specification. This format specification should be a two-way mapping from one to another "style" of writing metadata to address the many small idiosyncrasies of spectrum file formats. While some specifications are supplied with `SpectraMapping`, the goal is to give the user the power to easily (or at least somewhat easily) adapt a specification if a problem arises.

A key goal of `SpectraMapping` is also facilitating the conversion between formats while transferring metadata accurately.

## What is `SpectraMapping` not?

* It is not fast. E.g. `MsBackendMgf` and `MsBackendMsp` are much faster. For routine handling of datasets where metadata is not crucial, consider using those standard backends.
* It is not an attempt to unite all formats and create a standard handling of metadata. While some mappings and a field type definition are included, the goal is rather to let the user define mappings and fields such that they fit into their remaining infrastructure, which is often rigid.
* It also doesn't provide a single way of handling things. For example, it is sometimes useful to handle MassBank tags `CH$LINK: *database* *value*`  with `nesting`, such that they return individual columns (e.g. a column for the InChIKey, a column for the PubChem ID). For other uses, it can be better to handle this with `split`, such that every spectrum has a single `LINK` column containing a table (a `data.frame`) of links.
* It is not omnipotent and some things cannot be handled perfectly. Specifically, fields that need to be split and merged, or that may have duplicated mappings, are a complicated case. 
  * For example, in MSP format, `Name` and `Synon` are both fields for names. There may be just one `Name`, and it may be a "record name" rather than a chemical name.
  * In MassBank format, `CH$NAME[1]` should contain a chemical name; `RECORD_TITLE` should contain a name of the record. So if there is only one MSP `Name`, it should possibly end up in both `CH$NAME` and `RECORD_TITLE`. In this case, reading the MSP should copy the field into two fields, "name" and "title"?  
* It is, honestly, not particularly good and intuitive to use. The YAML format specification file format has already become quite complex, with regex, splits, nesting etc allowed. Probably it would be better to keep the logic but rework the YAML format to be an invertible top-to-bottom/bottom-to-top workflow.


## Workflow: reading

### File parsing
* Text file is read. 
* Text is separated into spectra by separation marker (e.g. two newlines, `//` etc depending on format)
  * note that this is in principle parser-dependent. The parser may split things differently (eg. multiple levels when handling sirius MS files)
* Parsing is applied to every spectrum separately
* Every spectrum is converted into one key-value store (where the value is always of type `character`) and one peak table.
  * The values are stored in a tibble with columns `formatKey` and `value`.
  * Every formatKey may have more than one value (this is realized with multiple entries, NOT with nested tibbles!)
  * Note: currently there is a "special type" where tabular data is stored as "HEADER colname colname ..." for the header,
    and "ENTRY col col ..." for the entries. It is to be discussed whether there should be a different store for tables.

### Entry mapping
This is the real benefit here. The raw key-value store is processed according to a ruleset specified in a mapping YAML file.
Processing order is as follows:

* Unnesting: all `nest` statements are processed. (Note: The `nest` statements do not need to appear in front of the file. It is best to put the `nest` statements in the file position where they should be when writing, unless the export writer does the order by itself.) Note that the unnest statements are processed in the order found in the file. This is relevant if the user desires to multilevel unnest, i.e. an unnested block may be further unnested with another rule.
* Regexes: `regexRead` statements are processed
* Dictionaries: `dictionary` statements are processed
* Splitting/tabling: `split` statements are processed. Note this needs to be *after* regexes and dictionaries because it changes the type of the column! Therefore, after this step no character-based statements can be applied to the `value` column anymore... This is the limit of what this DSL can do.
* Name mapping: Up to here, all entries were identified by the `formatKey`, which is the property name in the source format.
   As a last step, the `formatKey` is mapped to its `spectraKey` (which is its column name when retrieved with `spectraData`).
   
## Workflow: writing
"Writing" is two separate processes. 
 * The first is **entry mapping**, which occurs when the user assigns `spectraData<-`. This translates the `spectraData` DataFrame, which may have list columns, to a long-form `tibble` key-value store. This KVS is supposed to have the same format as when a file is parsed. Therefore, immediately when the user assigns a `spectraData`, the results are prepared for eventual writing to the file format. 
 * The second process is the actual **file export**. This is performed when `export()` is called. This calls the writer, which exports the KVS + ion data to a file (or really, to any other backend.) 
 * TODO: grouping/separation verbs that define how a `Spectra` goes into one or multiple files, folders etc.
  These should act as modifiers upon a `Spectra`. E.g.
  ```R
   export(sp)
   sp %>% export()
   sp %>% 
      group_to_folder("collisionEnergy", name = "ce_{collisionEnergy}") %>%
      group_to_file("InChiKey", name = "{inchikey1}") %>%
      export()
  ```
 * The same kind of grouping should also be applicable when reading, allowing file name components to end up in specific KVS values.
 * Should this be based on `spectraData` variables or on KVS variables? When reading, it is definitely KVS variables, but when writing, the user doesn't know those. Or use dataStorage?


# Entry mapping
This i

## Fields
The file consists of a list of field translation definitions.

* `spectraKey`: the variable name in the R (`Spectra`) object. In addition to the fields predefined by the `Spectra` object, we maintain a list of `lowerCamelCase` field names. TODO: coordinate the naming with e.g. M.Witting
* `formatKey`: the variable name in the spectrum file / record.
* `formatKeyRead`: One or more variable names in the spectrum file which map to this `spectraKey`. If not set, `formatKey` is used.
* `formatKeyWrite`: The variable name to write to in the spectrum file/record. If not set, `formatKey` is used.
* `dictionary`: A verbatim mapping from values in the record to values in the R object.
  * Contains entries `{value, read, write, format}` where `read` may have multiple entries mapping to the same R value.
  * If `format` is given, this is valid for reading and writing.
  * Alternatively, if tolerant reading is desired, `read` can specify a list of values, and `write` should specify a single value. Use only `format` or `read, write` in the same entry (but multiple entries in the same dictionary may be mixed).
  * Note: if there is a dictionary, it must be exhaustive. Any spectrum entry that doesn't match a dictionary entry is dropped.
  * Todo: is this good?
* `regexRead`: A regular expression to apply for reading the record value to an R value.
  * Contains entries (usually one) `{match, sub}`. Multiple entries are applied sequentially.
* `regexWrite`: A regular expression to apply when writing the R value to a record.
  * Contains entries (usually one) `{match, sub}`. Multiple entries are applied sequentially.
* `nest`: An order to nest (while writing) or separate (while reading) an entry. 
    This is performed after regexRead when reading, and before regexWrite when writing.
    (Note: this is because `nest` creates further entries. Those have their own regexRead and regexWrite that may apply.)
    * Contains entries: `prefix`, `separator` (optional), `regexRead`, `write`.
    * `prefix` is the prefix prepended to newly created rows when unnesting, or conversely, the prefix used to gather rows when nesting.
      `prefix` is only mandatory if writing is required. If no prefix is given, the values will be kept in their unnested form for writing.
      TODO/Note: This also means you should typically use `formatKeyRead` when reading from unnested columns - is this true? Probably not. Figure out how this should work!
    * `separator` is optional; it is used to separate single-line entries into multiline entries (e.g. for MSP Comment: / Notes: fields)
      when unnesting, or respectively, combining multiline entries to a single line when nesting.
    * `regexRead` is a regex with two capture groups to get key and value from one entry.
    * `regexMismatch` describes what to do when the regex doesn't match to an entry. 
      By default, the entry is kept as is (`keep`), otherwise, may be `drop`ped. TODO: behaviour when writing is still unclear.
    
* `split`: An order to interpret an entry as a list. 
   * If there is a specification for a header, the result will be a data frame.
   * May this be ragged or not?
  
## To do

* `order, weight` or such
* `process` in the same way as `regex`, but specifying a function name
  * or just generally have a $processRead and $processWrite named function list in the format?
* `fold, unfold`?
