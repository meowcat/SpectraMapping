
#' @export
MsFormatSirius <- function(parallel = FALSE, progress=FALSE, mapping = NULL) {

  if(is.null(mapping))
    mapping_ <- list()
  else if(is.list(mapping))
    mapping_ <- mapping
  else if(mapping == "default")
    mapping_ <- .mgf_mapping()
  else
    mapping_ <- read_yaml(mapping)

  format <- list(
    reader = .sirius_reader(parallel = parallel, progress=progress),
    writer = .sirius_writer(),
    mapping = mapping_
  )
  class(format) <- c("MsFormat", class(format))
  format
}

.mgf_mapping <- function() {
  read_yaml(system.file("mapping/sirius.yaml", package="SpectraMapping"))
}

