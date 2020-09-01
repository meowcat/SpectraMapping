#' @include hidden_aliases.R
#' @import tidyverse
#' @import tibble
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
         contains = "MsBackendDataFrame",
         slots = c(format = "MsFormat",
                   variables = "data.frame",
                   peaks = "data.frame",
                   fields = "data.frame"
                   ),
         prototype = prototype(spectraData = DataFrame(),
                               format = dummyFormat,
                               fields = tibble(),
                               variables = tibble(),
                               peaks = tibble(),
                               readonly = FALSE,
                               version = "0.1"))

#' @importMethodsFrom Spectra backendInitialize asDataFrame<- $<- $
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
          function(object, files, nonStop = FALSE, parallel = FALSE, ...) {
              if (missing(files) || !length(files))
                  stop("Parameter 'files' is mandatory for ", class(object))
              if (!is.character(files))
                  stop("Parameter 'files' is expected to be a character vector",
                       " with the files names from where data should be",
                       " imported")
              files <- normalizePath(files)
              if (any(!file.exists(files)))
                  stop("file(s) ",
                       paste(files[!file.exists(files)], collapse = ", "),
                       " not found")
              ## Import data and rbind.
              message("Start data import from ", length(files), " files ... ",
                      appendLF = FALSE)
              res <- .read_mapping(object, files)
              object@peaks <- map_dfr(res, "ions", .id = "spectrum_id") %>% mutate(spectrum_id = as.integer(spectrum_id))
              object@variables <- map_dfr(res, "variables", .id = "spectrum_id") %>% mutate(spectrum_id = as.integer(spectrum_id))
              message("done")
              object <- .fill_variables(object)
              object$dataStorage <- "<memory>"
              object$centroided <- TRUE
              return(object)
              
            
          })


#' @rdname hidden_aliases
as.list.MsBackendMapping <- function(x) {
  if (!length(x))
    return(list())
  .subset_peaks(x) %>%
    group_by(spectrum_id) %>% 
    group_split() %>%
    map(~ as.matrix(.x[,c("mz", "int")]))
}

#' @rdname hidden_aliases
setMethod("as.list", "MsBackendMapping", as.list.MsBackendMapping)

#' @rdname hidden_aliases
setMethod("intensity", "MsBackendMapping", function(object) {
  NumericList(lapply(as.list(object), "[", , 2), compress = FALSE)
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendMapping", function(object) {
  ol <- as.list(object)
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
#'
#' @importFrom methods as
#'
#' @importFrom S4Vectors SimpleList
#'
#' @importMethodsFrom S4Vectors lapply
setMethod("asDataFrame", "MsBackendMapping",
          function(object, columns = spectraVariables(object)) {
            df_columns <- intersect(columns,colnames(object@spectraData))
            res <- object@spectraData[, df_columns, drop = FALSE]
            if("mz" %in% columns)
              res$mz <- mz(object)
            if("intensity" %in% columns)
              res$intensity <- intensity(object)
            columns_ <- setdiff(columns, colnames(res))
            for(col in columns_) {
              res[, col] <- Spectra:::.get_column(object@spectraData, col)
            }
            res[, columns, drop = FALSE]
          })




#' @rdname MsBackendMapping
#'
#' @importFrom methods new
#'
#' @export MsBackendMapping
MsBackendMapping <- function(format, fields = .load_default_fields()) {
    new("MsBackendMapping", format = format, fields = fields)
}
