




.write_mgf_spectrum <- function(spectrum_id, variables, peaks, backend, progress) {
  
  variables_ <- variables %>% as.list()
  var_factor <- factor(names(variables_), levels = backend@sourceVariables)
  var_order <- order(var_factor, na.last = NA)
  var_print <- variables_[var_order]
  var_tbl_ <- tibble(key = names(var_print), value = var_print)
  var_tbl_ <- var_tbl_ %>% 
    mutate(value = if_else(map_lgl(value, is.list), value, map(value, as.list)))
  var_tbl_ <- var_tbl_ %>% unnest(value)
  var_tbl_ <- var_tbl_ %>% mutate(value = map(value, \(x) map_chr(x, as.character)))
  # Render tables according to MSP specifications
  var_tbl <- var_tbl_ %>% unchop(value, ptype = character())
  var_render <- var_tbl %>% glue_data("{key}={value}")

  peaks <- peaks %>% mutate(relint = pmax(1, int / max(int) * 999))
  spec_block <- peaks %>% glue_data("{mz} {int}")
  spectrum <- c("BEGIN IONS",
                var_render,
                spec_block,
                "END IONS",
                "\n") %>% paste(collapse = "\n")
  
  if(!is.null(progress))
    progress$tick()
  return(spectrum)
}

.mgf_writer <- function() {
  .mgf_writer_ <- function(data, backend, progress = NULL) {
    # variables <- backend@variables %>% 
    #   filter(spectrum_id %in% backend@spectraData[,"spectrum_id"]) %>%
    #   group_by(spectrum_id) %>% 
    #   nest() %>% 
    #   ungroup() %>% 
    #   rename(variables=data)
    # peaks <- backend@peaks %>% 
    #   filter(spectrum_id %in% backend@spectraData[,"spectrum_id"]) %>%
    #   group_by(spectrum_id) %>% 
    #   nest() %>%
    #   ungroup() %>% 
    #   rename(peaks=data)
    # data <- left_join(variables, peaks) 
    
    file <- data %>% pmap_chr(.write_mgf_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .mgf_writer_
}
