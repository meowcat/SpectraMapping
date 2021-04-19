
#' @export
MsFormatMetfrag <- function(parallel = FALSE, progress=FALSE, mapping = NULL) {
  
  if(is.null(mapping))
    mapping_ <- list()
  else if(is.list(mapping))
    mapping_ <- mapping 
  else if(mapping == "default")
    mapping_ <- .metfrag_mapping()
  else
    mapping_ <- read_yaml(mapping)
  
  format <- list(
    #reader = .massbank_reader(parallel = parallel, progress=progress),
    mapping = mapping_,
    writer = .metfrag_writer()
  )
  
  class(format) <- c("MsFormat", class(format))
  format
}

.metfrag_mapping <- function() {
  read_yaml(system.file("mapping/metfrag-library.yaml", package = "SpectraMapping"))
}

