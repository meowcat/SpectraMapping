




.write_mgf_spectrum <- function(spectrum_id, variables, peaks, backend, progress) {
  
  variables_ <- variables %>% as.list()
  var_factor <- factor(names(variables_), levels = backend@sourceVariables)
  var_order <- order(var_factor, na.last = NA)
  var_print <- variables_[var_order]
  var_tbl_ <- tibble(key = names(var_print), value = var_print)
  var_tbl <- var_tbl_ %>% unnest(value) %>% unnest(value)
  var_render <- var_tbl %>% glue_data("{key}={value}")

  peaks <- peaks %>% mutate(relint = pmax(1, int / max(int) * 999))
  spec_block <- peaks %>% glue_data("  {mz} {int} {round(relint, 0)}")
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
    variables <- backend@variables %>% 
      filter(spectrum_id %in% backend@spectraData[,"spectrum_id"]) %>%
      group_by(spectrum_id) %>% 
      nest() %>% 
      ungroup() %>% 
      rename(variables=data)
    peaks <- backend@peaks %>% 
      filter(spectrum_id %in% backend@spectraData[,"spectrum_id"]) %>%
      group_by(spectrum_id) %>% 
      nest() %>%
      ungroup() %>% 
      rename(peaks=data)
    data <- left_join(variables, peaks) 
    
    file <- data %>% pmap_chr(.write_mgf_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .mgf_writer_
}
