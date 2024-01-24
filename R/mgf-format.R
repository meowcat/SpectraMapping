
#' @export
MsFormatMgf <- function(parallel = FALSE, progress=FALSE, mapping = NULL) {
  
  if(is.null(mapping))
    mapping_ <- list()
  else if(is.list(mapping))
    mapping_ <- mapping 
  else if(mapping == "default")
    mapping_ <- .mgf_mapping()
  else
    mapping_ <- read_yaml(mapping)
  
  format <- list(
    reader = .mgf_reader(parallel = parallel, progress=progress),
    writer = .mgf_writer(),
    mapping = mapping_,
    splitter = .mgf_splitter,
    joiner = .mgf_joiner
  ) 
  class(format) <- c("MsFormat", class(format))
  format
}

.mgf_mapping <- function() {
  read_yaml(system.file("mapping/gnps-mgf.yaml", package="SpectraMapping"))
}

