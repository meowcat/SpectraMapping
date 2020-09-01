
#' @export
MsFormatMgf <- function(parallel = FALSE, progress=FALSE) {
  
  mapping <- .mgf_mapping()
  
  format <- list(
    reader = .mgf_reader(parallel = parallel, progress=progress),
    writer = .mgf_writer(),
    mapping = spectraMapping(mapping),
    dictionary = spectraDictionary(mapping)
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.mgf_mapping <- function() {
  loadSpectraMapping(system.file("mapping/mgf-mapping.yaml", package="SpectraMapping"))
}

