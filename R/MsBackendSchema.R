library(Spectra)
library(tidyverse)
#' @include hidden_aliases.R
NULL

#' @title MS data backend for Schema files
#'
#' @aliases MsBackendSchema-class
#'
#' @description
#'
#' The `MsBackendSchema` class supports import of MS/MS spectra data from
#' files in Mascot Generic Format
#' ([Schema](http://www.matrixscience.com/help/data_file_help.html))
#' files. After initial import, the full MS data is kept in
#' memory. `MsBackendSchema` extends the [MsBackendDataFrame()] backend
#' directly and supports thus the [applyProcessing()] function to make
#' data manipulations persistent. The backend does however not
#' support export to Schema files yet.
#'
#' New objects are created with the `MsBackendSchema` function. The
#' `backendInitialize` method has to be subsequently called to
#' initialize the object and import MS/MS data from (one or more) Schema
#' files.  Optional parameter `nonStop` allows to specify whether the
#' import returns with an error if one of the xml files lacks required
#' data, such as `mz` and `intensity` values (default `nonStop =
#' FALSE`), or whether only affected file(s) is(are) skipped and a
#' warning is shown (`nonStop = TRUE`). Note that any other error
#' (such as xml import error) will abort import regardless of
#' parameter `nonStop`.
#'
#' @param object Instance of `MsBackendSchema` class.
#'
#' @param files `character` with the (full) file name(s) of the Schema file(s)
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
#' @exportClass MsBackendSchema
#'
#' @name MsBackendSchema
#'
#' @examples
#'
#' ## Create an MsBackendSchema backend and import data from test mgf files
#' fls <- dir(system.file("extdata", package = "MsBackendSchema"),
#'     full.names = TRUE, pattern = "Schema$")
#' be <- backendInitialize(MsBackendSchema(), fls)
#' be
#'
#' be$msLevel
#' be$intensity
#' be$mz
NULL


dummyFormat <- list()
class(dummyFormat) <- c(class(dummyFormat), "MsFormat")
setOldClass("MsFormat")


setClass("MsBackendSchema",
         contains = "MsBackendDataFrame",
         slots = c(format = "MsFormat",
                   variables = "data.frame",
                   peaks = "data.frame"
                   ),
         prototype = prototype(spectraData = DataFrame(),
                               format = dummyFormat,
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
#' @rdname MsBackendSchema
setMethod("backendInitialize", signature = "MsBackendSchema",
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
              res <- .read_schema(object, files)
              object@peaks <- map_dfr(res, "ions", .id = "spectrum_id")
              object@variables <- map_dfr(res, "variables", .id = "spectrum_id")
              message("done")
              object <- .fill_variables(object)
              object$dataStorage <- "<memory>"
              object$centroided <- TRUE
              return(object)
              
              res <- do.call(rbind, res)
              if (nonStop && length(files) > nrow(res))
                      warning("Import failed for ", length(files) - nrow(res),
                              " files")
              asDataFrame(object) <- res

              validObject(object)
              object
          })


#' @rdname hidden_aliases
setMethod("as.list", "MsBackendSchema", function(x) {
  if (!length(x))
    return(list())
  .subset_peaks(x) %>%
    group_by(spectrum_id) %>% 
    group_split() %>%
    map(~ as.matrix(.x[,c("mz", "int")]))
})

#' @rdname hidden_aliases
setMethod("intensity", "MsBackendSchema", function(object) {
  NumericList(lapply(as.list(object), "[", , 2), compress = FALSE)
})

#' @rdname hidden_aliases
setMethod("mz", "MsBackendSchema", function(object) {
  NumericList(lapply(as.list(object), "[", , 1), compress = FALSE)
})

setMethod("ionCount", "MsBackendSchema", function(object) {
  .subset_peaks(object) %>% 
    group_by(spectrum_id) %>%
    n()
})

#' @rdname hidden_aliases
setMethod("lengths", "MsBackendSchema", function(x, use.names = FALSE) {
  lengths(mz(x))
})


#' @rdname MsBackendSchema
#'
#' @importFrom methods new
#'
#' @export MsBackendSchema
MsBackendSchema <- function(format) {
    new("MsBackendSchema", format = format)
}
