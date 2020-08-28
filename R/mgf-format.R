library(yaml)
library(tidyverse)

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
  mapping <- loadSpectraMapping("../inst/mapping/mgf-schema.yaml")
  return(spectraMapping(mapping))
}


# Reader: returns a 
# list(variables, ions)
# both are tibbles
# variables is key, value
# value may be a list

# Mapping: returns a tibble with columns
# format_key character, spectra_key character, writable logical
spectraMapping <- function(mapping) {
  if(is(mapping, "MsFormatMapping"))
    mapping_ <- mapping
  else
    mapping_ <- yaml.load_file(mapping)
  # Fill up mapping with formatKey where formatReadKey / formatWriteKey are not explicitely set
  mapping_ <- mapping_ %>% map( function(entry) {
    if(is.null(entry$formatReadKey))
      entry$formatReadKey <- entry$formatKey
    if(is.null(entry$formatWriteKey))
      entry$formatWriteKey <- entry$formatKey
    entry
  })
  # Remove empty entries
  mapping_read <- mapping_ %>% keep(~ length(.x$formatReadKey) > 0)
  mapping_write <- mapping_ %>% keep(~ length(.x$formatWriteKey) > 0)
  
  mapping_dfr_read <- function(entry, type)
  {
    tibble(spectraKey = rep(entry$spectraKey, length(entry$formatReadKey)),
           formatKey = entry$formatReadKey,
           type = rep("read", length(entry$formatReadKey)))
  }
  mapping_read_ <- map_dfr(mapping_read, mapping_dfr_read)
  
  
  mapping_dfr_write <- function(entry, type)
  {
    tibble(spectraKey = rep(entry$spectraKey, length(entry$formatWriteKey)),
           formatKey = entry$formatWriteKey,
           type = rep("write", length(entry$formatWriteKey)))
  }
  mapping_write_ <- map_dfr(mapping_write, mapping_dfr_write)
  rbind(mapping_read_, mapping_write_)
}

loadSpectraMapping <- function(f) {
  y <- yaml.load_file(f)
  class(y) <- c(class(y), "MsFormatMapping")
  y
}
