

.write_massbank_spectrum <- function(spectrum_id, variables, peaks, backend, progress) {
  variables_ <- variables %>% as.list()
  # Select and order variables according to backend@sourceVariables selection and order
  var_factor <- factor(names(variables_), levels = backend@sourceVariables)
  var_order <- order(var_factor, na.last = NA)
  var_print <- variables_[var_order]
  var_tbl_ <- tibble(key = names(var_print), value = var_print)
  var_tbl <- var_tbl_ %>% unnest(value) %>% unnest(value)
  var_render <- var_tbl %>% glue_data("{key}: {value}")
  # Verify that NUM_PEAKS is there
  if(!("PK$NUM_PEAK" %in% names(var_print))) {
    warning(glue("PK$NUM_PEAK missing for record {spectrum_id}. Recalculating"))
    var_render <- c(var_render, glue("PK$NUM_PEAK: {nrow(peaks)}"))
  }
  peaks <- peaks %>% mutate(relint = pmax(1, int / max(int) * 999))
  spec_block <- peaks %>% glue_data("  {mz} {int} {round(relint, 0)}")
  spectrum <- c(var_render,
                "PK$PEAK: m/z int. rel.int.",
                spec_block,
                "//") %>% paste(collapse = "\n")
  if(!is.null(progress))
    progress$tick()
  return(spectrum)
}




.massbank_writer <- function() {
  .massbank_writer_ <- function(backend, progress = NULL) {
    variables <- backend@variables %>% 
      group_by(spectrum_id) %>% 
      nest() %>% 
      ungroup() %>% 
      rename(variables=data)
    peaks <- backend@peaks %>% 
      group_by(spectrum_id) %>% 
      nest() %>%
      ungroup() %>% 
      rename(peaks=data)
    data <- left_join(variables, peaks, by="spectrum_id") 
    file <- data %>% pmap_chr(.write_massbank_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .massbank_writer_
}
