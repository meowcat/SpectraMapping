
#' @export
MsFormatMassbank <- function(parallel = FALSE, progress=FALSE, mapping = NULL) {
  
  if(is.null(mapping))
    mapping_ <- list()
  else if(is.list(mapping))
    mapping_ <- mapping 
  else if(mapping == "default")
    mapping_ <- .massbank_mapping()
  else
    mapping_ <- read_yaml(mapping)
  
  format <- list(
    reader = .massbank_reader(parallel = parallel, progress=progress),
    mapping = mapping_,
    writer = .massbank_writer()
  )
  
  class(format) <- c("MsFormat", class(format))
  format
}

.massbank_mapping <- function() {
  read_yaml(system.file("mapping/massbank.yaml", package = "SpectraMapping"))
}

