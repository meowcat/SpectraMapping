

.write_massbank_spectrum <- function(spectrum_id, variables, peaks, backend, progress) {
  variables_ <- variables %>% as.list()
  # Select and order variables according to backend@sourceVariables selection and order
  var_factor <- factor(names(variables_), levels = backend@sourceVariables)
  
  # Find which columns should be treated as tables
  table_vars_ <- map_lgl(
    colnames(backend@variables),
    ~ isTRUE(attr(backend@variables[[.x]], "table"))
  )
  table_vars <- colnames(backend@variables)[table_vars_]
  
  var_order <- order(var_factor, na.last = NA)
  var_print <- variables_[var_order]
  var_tbl_ <- tibble(key = names(var_print), value = var_print)
  var_tbl_ <- var_tbl_ %>% unnest(value)
  # Render tables according to MassBank specifications
  var_tbl_ <- var_tbl_ %>% mutate(
    value = ifelse(key %in% table_vars,
                   map(value, ~ paste(.x, collapse = "\n  ")), 
                   value)
  )
  var_tbl <- var_tbl_ %>% unchop(value)
  var_render <- var_tbl %>% glue_data("{key}: {value}")
  # Verify that NUM_PEAKS is there
  if(!("PK$NUM_PEAK" %in% names(var_print))) {
    # warning(glue("PK$NUM_PEAK missing for record {spectrum_id}. Recalculating"))
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
  .massbank_writer_ <- function(data, backend, progress = NULL) {
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
    file <- data %>% pmap_chr(.write_massbank_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .massbank_writer_
}

.massbank_joiner <- function(files, out_file) {
  warning("MassBank spectra are typically one per file. Joining will produce non-conformant files.")
  write_lines("", out_file)
  walk(files, function(f) {
    d <- read_lines(f)
    write_lines(d, out_file, append=TRUE)
  })
}

.massbank_splitter <- function(input_file, out_file_schema = "chunk_{i}", spectra_per_file = 1) {
  warning("MassBank spectra are typically one per file. Attempting to split anyway.")
  data <- read_lines(input_file)
  split_pos <- (data == "//")
  split_assign <- cumsum(split_pos) - split_pos
  split_data <- split(data, split_assign %/% spectra_per_file)
  filenames <- imap_chr(split_data, function(d, i) {
    filename <- glue(out_file_schema)
    write_lines(d, filename, append=TRUE)
    return(filename)
  })
  return(filenames)
}