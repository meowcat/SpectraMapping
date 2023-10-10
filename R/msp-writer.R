

.write_msp_spectrum <- function(spectrum_id, variables, peaks, backend, progress) {
  variables_ <- variables %>% as.list()
  # Select and order variables according to backend@sourceVariables selection and order
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
  var_render <- var_tbl %>% glue_data("{key}: {value}")
  # Verify that NUM_PEAKS is there
  if(!("Num Peaks" %in% names(var_print))) {
    # warning(glue("PK$NUM_PEAK missing for record {spectrum_id}. Recalculating"))
    var_render <- c(var_render, glue("Num Peaks: {nrow(peaks)}"))
  }
  spec_block <- peaks %>% 
    mutate(text = if_else(
      !is.na(annotation), 
      glue("{mz} {int} {annotation}"),
      glue("{mz} {int}")
    )) %>%
    pull(text)
  spectrum <- c(var_render,
                spec_block,
                "\n") %>% paste(collapse = "\n")
  if(!is.null(progress))
    progress$tick()
  return(spectrum)
}



.msp_writer <- function() {
  
  .msp_writer_  <- function(data, backend, progress = NULL) {
    # variables <- backend@variables %>% 
    #   group_by(spectrum_id) %>% 
    #   nest() %>% 
    #   ungroup() %>% 
    #   rename(variables=data)
    # peaks <- backend@peaks %>% 
    #   group_by(spectrum_id) %>% 
    #   nest() %>%
    #   ungroup() %>% 
    #   rename(peaks=data)
    # data <- left_join(variables, peaks, by="spectrum_id") 
    file <- data %>% pmap_chr(.write_msp_spectrum, backend = backend, progress = progress)
    return(file)
  }
  
  .msp_writer_
}
