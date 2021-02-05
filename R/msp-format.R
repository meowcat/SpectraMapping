
#' @export
MsFormatMsp <- function(parallel = FALSE, progress = FALSE) {
  mapping <- .msp_mapping()
  format <- list(
    reader = .msp_reader(parallel = parallel, progress = progress),
    writer = .msp_writer()
  ) %>% load_mapping(mapping)
  class(format) <- c("MsFormat", class(format))
  format
}

.msp_mapping <- function() {
  loadSpectraMapping(system.file("mapping/msp-mapping.yaml", package="SpectraMapping"))
}
