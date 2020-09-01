
#' @export
MsFormatMsp <- function(parallel = FALSE, progress = FALSE) {
  mapping <- .msp_mapping()
  format <- list(
    reader = .msp_reader(parallel = parallel, progress = progress),
    writer = function(...) error("not implemented yet"),
    mapping = spectraMapping(mapping),
    dictionary = spectraDictionary(mapping),
    regex = spectraRegex(mapping)
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.msp_mapping <- function() {
  loadSpectraMapping(system.file("mapping/msp-mapping.yaml", package="SpectraMapping"))
}
