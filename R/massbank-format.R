
#' @export
MsFormatMassbank <- function(parallel = FALSE, progress=FALSE) {
  
  mapping <- .massbank_mapping()
  
  format <- list(
    reader = .massbank_reader(parallel = parallel, progress=progress),
    #writer = .mgf_writer(),
    mapping = spectraMapping(mapping),
    dictionary = spectraDictionary(mapping),
    regex = spectraRegex(mapping),
    nesting = spectraNesting(mapping)
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.massbank_mapping <- function() {
  loadSpectraMapping(system.file("mapping/massbank-mapping.yaml", package="SpectraMapping"))
}

