#' @import yaml
#' @import tidyverse
NULL
 

# Mapping: returns a tibble with columns
# format_key character, spectra_key character, writable logical
spectraMapping <- function(mapping) {
  if(is(mapping, "MsFormatMapping"))
    mapping_ <- mapping
  else
    mapping_ <- yaml.load_file(mapping)
  # This is for the final mapping, so find the ones with a spectraKey
  mapping_ <- mapping_ %>% 
      keep(~!is.null(.x$spectraKey))
  # Fill up mapping with formatKey where formatKeyRead / formatKeyWrite are not explicitely set
  mapping_ <- mapping_ %>%
    map( function(entry) {
    if(is.null(entry$formatKeyRead))
      entry$formatKeyRead <- entry$formatKey
    if(is.null(entry$formatKeyWrite))
      entry$formatKeyWrite <- entry$formatKey
    entry
  })
  # Remove empty entries
  mapping_read <- mapping_ %>% keep(~ length(.x$formatKeyRead) > 0)
  mapping_write <- mapping_ %>% keep(~ length(.x$formatKeyWrite) > 0)
  
  mapping_dfr_read <- function(entry, type)
  {
    tibble(spectraKey = rep(entry$spectraKey, length(entry$formatKeyRead)),
           formatKey = entry$formatKeyRead,
           type = rep("read", length(entry$formatKeyRead)))
  }
  mapping_read_ <- map_dfr(mapping_read, mapping_dfr_read)
  
  
  mapping_dfr_write <- function(entry, type)
  {
    tibble(spectraKey = rep(entry$spectraKey, length(entry$formatKeyWrite)),
           formatKey = entry$formatKeyWrite,
           type = rep("write", length(entry$formatKeyWrite)))
  }
  mapping_write_ <- map_dfr(mapping_write, mapping_dfr_write)
  rbind(mapping_read_, mapping_write_)
}


spectraDictionary <- function(mapping) {
  # Find mappings that are defined for this format
  mapping_ <- mapping %>% 
    keep(~ !is.null(.x$formatKey)) %>%
    set_names(map_chr(., "formatKey"))
  
  # Find and extract mappings that have a dictionary
  mapping_ <- mapping_ %>%
    keep(~ !is.null(.x$dictionary)) %>%
    map("dictionary")
  
  # Copy format to write/read if specified,
  # then make into a tibble [formatKey, value, type, format].
  mapping_ <- mapping_ %>%
    map_depth(2, function(x) {
      if(!is.null(x$format)) {
        x$write <- x$format
        x$read <- x$format
      }
      return(tribble(
        ~ value, ~ type, ~ format,
        x$value, "read", x$read,
        x$value, "write", x$write
      )) %>% unnest(format)
    }) %>%
    map_dfr(bind_rows, .id="formatKey")
  if(nrow(mapping_) == 0)
    mapping_ <- tibble(formatKey = character(), value=character(), type=character(), format=character())
  mapping_
}

spectraNesting <- function(mapping) {
  nestings <- mapping %>% 
    keep(~!is.null(.x$nest)) %>%
    map(~.x$nest %>% list_modify(formatKey = .x$formatKey))
}

spectraRegex <- function(mapping, type = NULL) {
  # Collect read, write or both regex types
  if(is.null(type))
    return(bind_rows(list(
      spectraRegex(mapping, "read"),
      spectraRegex(mapping, "write")
    )))
  if(type=="read")
    filterFind <- "regexRead"
  else if(type=="write")
    filterFind <- "regexWrite"
  else
    stop("unknown regex type")
  
  # Find mapping entries that are defined for this format
  mapping_ <- mapping %>% 
    keep(~ !is.null(.x$formatKey)) %>%
    set_names(map_chr(., "formatKey"))
  
  # Find and extract mapping entries that have defined regex
  mapping_ <- mapping_ %>% 
    keep(~ !is.null(.x[[filterFind]])) %>%
    map(~ .x[[filterFind]])
  
  mapping_ <- mapping_ %>%
    map(bind_rows) %>% map_dfr(~ .x %>% mutate(type = type), .id="formatKey")
  
  if(nrow(mapping_) == 0)
    mapping_ <- tibble(formatKey = character(), match=character(), sub=character(), type=character())
  mapping_
  
}

loadSpectraMapping <- function(f) {
  y <- yaml.load_file(f)
  class(y) <- c(class(y), "MsFormatMapping")
  y
}
