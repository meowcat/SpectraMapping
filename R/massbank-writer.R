

.write_massbank_spectrum <- function(variables, peaks) {
  var_block <- variables %>%
    # Put "Num Peaks" last:
    arrange(formatKey == "Num Peaks") %>%
    mutate(text = paste0(formatKey, ": ", value))
  spec_block <- peaks %>% mutate(text = paste(mz, int, annotation, sep='\t'))
  spectrum <- c(var_block %>% pull(text),
                spec_block %>% pull(text),
                "\n") %>% paste(collapse = "\n")
  return(spectrum)
}

.massbank_writer <- function() {
  .massbank_writer_ <- function(backend) {
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
    data <- left_join(variables, peaks) 
    file <- data %>% select(-spectrum_id) %>% pmap_chr(.write_massbank_spectrum)
    return(file)
  }
  .massbank_writer_
}
