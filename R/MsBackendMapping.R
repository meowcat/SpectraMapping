#' @include hidden_aliases.R
#' @import tidyverse
#' @import tibble
#' @import progress
NULL

#' @title MS data backend for mapped text files
#'
#' @aliases MsBackendMapping-class
#'
#' @description
#'
#' The `MsBackendMapping` class supports import of MS/MS spectra data from
#' files in text format by a defined mapping. 
#' After initial import, the full MS data is kept in
#' memory. `MsBackendMapping` uses tibble/tidyverse stuff in the background.
#'
#' New objects are created with the `MsBackendMapping` function. The
#' `backendInitialize` method has to be subsequently called to
#' initialize the object and import MS/MS data from (one or more) 
#' files.  Optional parameter `nonStop` allows to specify whether the
#' import returns with an error if one of the xml files lacks required
#' data, such as `mz` and `intensity` values (default `nonStop =
#' FALSE`), or whether only affected file(s) is(are) skipped and a
#' warning is shown (`nonStop = TRUE`). Note that any other error
#' (such as xml import error) will abort import regardless of
#' parameter `nonStop`.
#'
#' @param object Instance of `MsBackendMapping` class.
#'
#' @param files `character` with the (full) file name(s) of the  file(s)
#'     from which MS/MS data should be imported.
#'
#' @param nonStop `logical(1)` whether import should be stopped if an
#'     xml file does not contain all required fields. Defaults to
#'     `nonStop = FALSE`.
#'
#' @param BPPARAM Parameter object defining the parallel processing
#'     setup to import data in parallel. Defaults to `BPPARAM =
#'     bpparam()`. See [bpparam()] for more information.
#'
#' @param ... Currently ignored.
#'
#' @author Michele Stravs <stravs@@imsb.biol.ethz.ch>
#'
#' @importClassesFrom Spectra MsBackendDataFrame
#'
#' @exportClass MsBackendMapping
#'
#' @name MsBackendMapping
#'
#' @examples
#'
NULL


dummyFormat <- list()
class(dummyFormat) <- c(class(dummyFormat), "MsFormat")
setOldClass("MsFormat")


setClass("MsBackendMapping",
         contains = "MsBackend",
         slots = c(format = "MsFormat",
                   variables = "data.frame",
                   peaks = "data.frame",
                   fields = "data.frame",
                   spectraVariables = "character",
                   sourceVariables = "character"
                   ),
         prototype = prototype(spectraData = DataFrame(),
                               format = dummyFormat,
                               fields = tibble(),
                               variables = tibble(),
                               peaks = tibble(),
                               spectraVariables = character(),
                               sourceVariables = character(),
                               readonly = FALSE,
                               version = "0.1"))

#' @importMethodsFrom Spectra backendInitialize spectraData<- $<- $
#'
#' @importFrom BiocParallel bpparam
#'
#' @importMethodsFrom BiocParallel bplapply
#'
#' @importFrom methods validObject
#'
#' @exportMethod backendInitialize
#'
#' @rdname MsBackendMapping
setMethod("backendInitialize", signature = "MsBackendMapping",
          function(object, files, data, nonStop = FALSE, parallel = FALSE, ...) {
            
            hasFiles <- !missing(files)
            hasData <- !missing(data)
            
            if(hasData & hasFiles)
              stop("Either 'files' or 'data' (or none) is expected as source, but not both.")
            
            # Initially, initialze empty
            object@variables <- tibble(formatKey = c(), values = c(), spectrum_id = character())
            object@peaks <- tibble(spectrum_id = character(), mz = numeric(), int = numeric(), relint = numeric())
            
            if(hasData) {
              spectraData(object) <- data
              object@spectraVariables <- colnames(object@variables)
              object@sourceVariables <- c()
            }
            else if(hasFiles) {
              if (!is.character(files))
                stop("Parameter 'files' is expected to be a character vector",
                     " with the files names from where data should be",
                     " imported")
              files <- normalizePath(files)
              if (any(!file.exists(files)))
                stop("file(s) ",
                     paste(files[!file.exists(files)], collapse = ", "),
                     " not found")
              message("Start data import from ", length(files), " files ... ",
                      appendLF = FALSE)
              ## Import data and rbind: this produces a long-form key-value store for each spectrum
              res <- map(files, ~ .parse_to_kvs(object, .x)) %>% flatten()
              
              peaks <- map_dfr(res, "ions", .id = "spectrum_id")
              if(nrow(peaks) > 0)
                object@peaks <- peaks
              object@peaks <- object@peaks %>%  dplyr::mutate(spectrum_id = as.integer(spectrum_id))
              
              variables <- map_dfr(res, "variables", .id = "spectrum_id")
              if(nrow(variables) > 0)
                object@variables <- variables
              
              # transform the long-form KVS to a table such as the one from MsBackendDataFrame
              object@variables <- object@variables %>%
                dplyr::mutate(spectrum_id = as.integer(spectrum_id)) %>%
                pivot_wider(names_from = "formatKey", values_from = "value", values_fn = list)
              object@sourceVariables <- colnames(object@variables)
              object@variables$dataStorage <- "<memory>"
              object@variables$centroided <- TRUE
              object@spectraVariables <- union(object@spectraVariables, c("dataStorage", "centroided"))
            }
            
            message("done")
            # Apply mapping transformations
            if(!is.null(object@format$mapping))
              object <- mapVariables(object, object@format$mapping)
            
            
            return(object)
          })


#' @rdname hidden_aliases
setMethod("backendMerge", "MsBackendMapping", function(object, ...) {
  object <- unname(c(object, ...))
  not_empty <- lengths(object) > 0
  if (any(not_empty))
    res <- .combine_backend_mapping(object[not_empty])
  else res <- object[[1L]]
  validObject(res)
  res
})




#' @rdname hidden_aliases
.peaksData.MsBackendMapping <- function(
    object, 
    columns = c("mz", "intensity"), 
    ...) {
  if (!all(columns %in% c("mz", "intensity")))
    stop("'peaksData' for 'MsBackendMapping' does only support",
         " columns \"mz\" and \"intensity\"", .call = FALSE)
  if (!length(object))
    return(list())
  .subset_peaks(object) %>%
    group_by(spectrum_id) %>% 
    group_split() %>%
    map(~ as.matrix(.x[,c("mz", "int")]))
}

#' @rdname hidden_aliases
#' @importMethodsFrom Spectra peaksData
setMethod("peaksData", "MsBackendMapping", .peaksData.MsBackendMapping)

#' @rdname hidden_aliases
setMethod("intensity", "MsBackendMapping", function(object) {
  NumericList(lapply(peaksData(object), "[", , 2), compress = FALSE)
})

#' @rdname hidden_aliases
setMethod("length", "MsBackendMapping", function(x) {
  nrow(x@variables)
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendMapping", function(object) {
  ol <- peaksData(object)
  NumericList(lapply(ol, "[", , 1), compress = FALSE)
})

setMethod("ionCount", "MsBackendMapping", function(object) {
  .subset_peaks(object) %>% 
    group_by(spectrum_id) %>%
    n()
})

#' @rdname hidden_aliases
setMethod("lengths", "MsBackendMapping", function(x, use.names = FALSE) {
  lengths(mz(x))
})


#' @rdname hidden_aliases
setMethod("export", "MsBackendMapping", function(object, x, file = tempfile(), progress = FALSE, ...) {
  
  args <- list(...)
  if("terminate" %in% names(args))
    terminate <- args$terminate
  else
    terminate <- "no"
  if("append" %in% names(args))
    append <- args$append
  else
    append <- FALSE

  d <- spectraData(x)
  d$mz <- mz(x)
  d$intensity <- intensity(x)
  # Conversion of the field happens by assigning to the object,
  # as it would when we setBackend
  spectraData(object) <- d
  #
  mapping <- object@format$mapping
  if(length(mapping) > 0)
    object <- mapVariables(object, mapping, "write")
  
  object@variables <- object@variables %>%
    mutate(file_ = glue(file))
  
  # Process into a nested tibble with columns spectrum_id, file_, variables, peaks,
  # one row per spectrum
  variables <- object@variables %>% 
    group_by(spectrum_id, file_) %>% 
    nest() %>% 
    ungroup() %>% 
    rename(variables=data)
  peaks <- object@peaks %>% 
    group_by(spectrum_id) %>% 
    nest() %>%
    ungroup() %>% 
    rename(peaks=data)
  data <- left_join(variables, peaks, by="spectrum_id")
  
  
  
  data_by_file <- data %>%
    group_by(file_)
  if(terminate == "file_split")
    return(data_by_file)
  
  pb <- NULL
  if(progress) {
    pb <- progress_bar$new(
      "Generating spectra :bar :tick_rate/sec, ETA :eta",
      total = length(object))
  }
  
  export_files <- data_by_file %>%  
    group_split(.keep = FALSE) %>%
    map(~ object@format$writer(.x, backend=object, progress=pb))
  
  names(export_files) <- group_keys(data_by_file)$file_
  
  if(terminate == "generate_spectra")
    return(export_files)
  
  iwalk(export_files, ~ write_lines(.x, file=.y, append = append))
})


#' #' @rdname MsBackendMgf
#' setMethod("export", "MsBackendMgf", function(object, x, file = tempfile(),
#'                                              mapping = spectraVariableMapping(),
#'                                              ...) {
#'   if (missing(x))
#'     stop("Required parameter 'x' is missing. 'x' should be a 'Spectra' ",
#'          "object with the full spectra data.")
#'   if (!inherits(x, "Spectra"))
#'     stop("Parameter 'x' is supposed to be a 'Spectra' object with the full",
#'          " spectra data to be exported.")
#'   .export_mgf(x = x, con = file, mapping = mapping)
#' })


#' @rdname hidden_aliases
setReplaceMethod("spectraData", "MsBackendMapping", function(object, value) {
  
  if(!("spectrum_id" %in% colnames(value)))
    value[,"spectrum_id"] <- seq_len(nrow(value))
  
  df_ <- value[, !(colnames(value) %in% c("mz", "intensity"))]

  object@variables <- as_tibble(df_) # %>% rowid_to_column("spectrum_id")
  object@spectraVariables <- colnames(df_)
  
  #object <- .fill_backend(object)
  if(all(c("mz", "intensity") %in% colnames(value))) {
    object <- .fill_peaks(object, value[,c("spectrum_id", "mz", "intensity")])
  }
  # Check that spectrum ids match
  
  
  return(object)
})
  


#' @rdname hidden_aliases
#'
#' @importFrom methods as
#'
#' @importFrom S4Vectors SimpleList
#'
#' @importMethodsFrom S4Vectors lapply
setMethod("spectraData", "MsBackendMapping",
          function(object, columns = object@spectraVariables) {
            df_columns <- intersect(columns,colnames(object@variables))
            res <- object@variables[, df_columns, drop = FALSE] %>% DataFrame()
            if("mz" %in% columns)
              res$mz <- mz(object)
            if("intensity" %in% columns)
              res$intensity <- intensity(object)
            columns_ <- setdiff(columns, colnames(res))
            for(col in columns_) {
              res[, col] <- Spectra:::.get_column(object@variables, col)
            }
            res[, columns, drop = FALSE]
          })

#' @rdname hidden_aliases
setMethod("spectraVariables", "MsBackendMapping",
          function(object) {
            return(object@spectraVariables)
          })



#' @rdname MsBackendMapping
#'
#' @importFrom methods new
#'
#' @export MsBackendMapping
MsBackendMapping <- function(format, fields = .load_default_fields()) {
    new("MsBackendMapping", format = format, fields = fields)
}




#' @importMethodsFrom S4Vectors [
#'
#' @importFrom MsCoreUtils i2index
#'
#' @rdname hidden_aliases
setMethod("[", "MsBackendMapping", function(x, i, j, ..., drop = FALSE) {
  .subset_backend_mapping(x, i)
})



# Batch assignment for all setters/getters

.spectra_aliases_list <- c(
  #"acquisitionNum",
  "centroided",
  "collisionEnergy",
  "dataOrigin",
  "dataStorage",
  #"ionCount",
  # isCentroided
  "isolationWindowLowerMz",
  "isolationWindowTargetMz",
  "isolationWindowUpperMz",
  "msLevel",
  "polarity",
  "precScanNum",
  "precursorCharge",
  "precursorIntensity",
  "precursorMz",
  "rtime",
  "scanIndex",
  "smoothed"
  # spectraNames
)
.spectra_aliases_ro <- c(
  "precScanNum",
  "scanIndex"
)


#' data types of spectraData columns
#'
#' @noRd
.SPECTRA_DATA_COLUMNS <- c(
  msLevel = "integer",
  rtime = "numeric",
  acquisitionNum = "integer",
  scanIndex = "integer",
  mz = "NumericList",
  intensity = "NumericList",
  dataStorage = "character",
  dataOrigin = "character",
  centroided = "logical",
  smoothed = "logical",
  polarity = "integer",
  precScanNum = "integer",
  precursorMz = "numeric",
  precursorIntensity = "numeric",
  precursorCharge = "integer",
  collisionEnergy = "numeric",
  isolationWindowLowerMz = "numeric",
  isolationWindowTargetMz = "numeric",
  isolationWindowUpperMz = "numeric"
)

.alias_read_fun <- function(.spectra_alias_) {
  spectra_alias_gen <- .SPECTRA_DATA_COLUMNS[.spectra_alias_]
  function(object) {
    spectra_alias_sym <- sym(.spectra_alias_)
    #message(.spectra_alias_)
    if(.spectra_alias_ %in% colnames(object@variables))
      return(object@variables %>% pull(spectra_alias_sym))
    #if(spectra_alias %in% .SPECTRA_DATA_COLUMNS)
    return(do.call(spectra_alias_gen, list(length(object))))
  }
}

.alias_write_fun <- function(.spectra_alias_) {
  spectra_alias_sym <- sym(.spectra_alias_)
  value_type <- .SPECTRA_DATA_COLUMNS[.spectra_alias_]
  function(object, value) {
    if (!is(value, value_type) || length(value) != length(object))
      stop(glue("'value' has to be a '{value_type}' of length {length(object)}"))
    object@variables <- object@variables %>% mutate(!!spectra_alias_sym := as(value, value_type))
    object@spectraVariables <- union(object@spectraVariables, .spectra_alias_)
    validObject(object)
    object
  }
}

for(spectra_alias in .spectra_aliases_list) {

  #' @rdname hidden_aliases
  setMethod(spectra_alias, "MsBackendMapping", .alias_read_fun(spectra_alias))
  
  if(!(spectra_alias %in% .spectra_aliases_ro))
  {
    #' @rdname hidden_aliases
    setReplaceMethod(spectra_alias, "MsBackendMapping", .alias_write_fun(spectra_alias))
  }
}


# very valid object all the time
setValidity("MsBackendMapping", function(object) {
  return(TRUE)
})


#' @rdname hidden_aliases
setMethod("$", "MsBackendMapping", function(x, name) {
  if (!any(spectraVariables(x) == name))
    stop("spectra variable '", name, "' not available")
  spectraData(x, name)[, 1]
})

#' @rdname hidden_aliases
setReplaceMethod("$", "MsBackendMapping", function(x, name, value) {
  if (is.list(value) && any(c("mz", "intensity") == name)) {
    stop("setting peaks data via $ not yet implemented")
  }
  x@variables[[name]] <- value
  validObject(x)
  x
})


#' @rdname hidden_aliases
setReplaceMethod("peaksData", "MsBackendMapping", function(object, value) {
  if (!(is.list(value) || inherits(value, "SimpleList")))
    stop("'value' has to be a list-like object")
  if (length(value) != length(object))
    stop("Length of 'value' has to match length of 'object'")
  names(value) <- object@variables$spectrum_id
  object <- .set_peaks_data(object, value)
  validObject(object)
  object
})

