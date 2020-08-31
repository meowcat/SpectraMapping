
#' @export
MsFormatMgf <- function(parallel = FALSE) {
  format <- list(
    reader = .mgf_reader(parallel = parallel),
    mapping = .mgf_mapping(),
    writer = function(...) error("not implemented yet")
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.mgf_mapping <- function() {
  mapping <- loadSpectraMapping(system.file("mapping/mgf-mapping.yaml", package="SpectraMapping"))
  return(spectraMapping(mapping))
}

