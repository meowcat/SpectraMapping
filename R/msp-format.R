
#' @export
MsFormatMsp <- function(parallel = FALSE, progress = FALSE, mapping = NULL) {
  
  if(is.null(mapping))
    mapping_ <- list()
  else if(is.list(mapping))
    mapping_ <- mapping 
  else if(mapping == "default")
    mapping_ <- .msp_mapping()
  else
    mapping_ <- read_yaml(mapping)
  
  format <- list(
    reader = .msp_reader(parallel = parallel, progress = progress),
    writer = .msp_writer(),
    mapping = mapping_
  ) 
  class(format) <- c("MsFormat", class(format))
  format
}

.msp_mapping <- function() {
  read_yaml(system.file("mapping/nist-msp.yaml", package="SpectraMapping"))
}
