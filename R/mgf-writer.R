




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
    
    file <- data %>% pmap_chr(.write_mgf_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .mgf_writer_
}



.mgf_joiner <- function(files, out_file) {
  write_lines("", out_file)
  
  walk(files, function(f) {
    d <- read_lines(f)
    # Make sure there are precisely two newlines after the last spectrum
    nonemptylines <- (d != "") * seq_along(d)
    d <- d[seq(max(nonemptylines))]
    d <- c(d, c("", ""))
    write_lines(d, out_file, append=TRUE)
  })}


.mgf_splitter <- function(input_file, out_file_schema = "chunk_{i}", spectra_per_file = 1) {
  data <- read_lines(input_file)
  split_pos <- (str_starts(data, fixed("BEGIN IONS")))
  split_assign <- cumsum(split_pos) - 1
  split_data <- split(data, split_assign %/% spectra_per_file)
  
  filenames <- imap_chr(split_data, function(d, i) {
    filename <- glue(out_file_schema)
    write_lines(d, filename, append=TRUE)
    return(filename)
  })
  return(filenames)
}