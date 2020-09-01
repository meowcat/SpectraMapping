
#' @export
MsFormatMsp <- function(parallel = FALSE, progress = FALSE) {
  format <- list(
    reader = .msp_reader(parallel = parallel, progress = progress),
    mapping = .msp_mapping(),
    writer = function(...) error("not implemented yet")
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.msp_mapping <- function() {
  mapping <- loadSpectraMapping(system.file("mapping/msp-mapping.yaml", package="SpectraMapping"))
  return(spectraMapping(mapping))
}


