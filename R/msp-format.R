
#' @export
MsFormatMsp <- function(parallel = FALSE) {
  format <- list(
    reader = .msp_reader(parallel = parallel),
    mapping = .msp_mapping(),
    writer = function(...) error("not implemented yet")
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.msp_mapping <- function() {
  mapping <- loadSpectraMapping(system.file("mapping/msp-schema.yaml", package="MsBackendSchema"))
  return(spectraMapping(mapping))
}


