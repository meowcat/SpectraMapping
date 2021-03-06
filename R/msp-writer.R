

.write_msp_spectrum <- function(variables, peaks) {
  var_block <- variables %>% 
    # Put "Num Peaks" last:
    arrange(order) %>%
    mutate(text = paste0(formatKey, ": ", value))
  spec_block <- peaks %>% mutate(text = paste(mz, int, annotation, sep='\t'))
  spectrum <- c(var_block %>% pull(text),
                spec_block %>% pull(text),
                "\n") %>% paste(collapse = "\n")
  return(spectrum)
}

.msp_writer <- function() {
  .msp_writer_ <- function(backend) {
    variables <- backend@variables %>%
      left_join(backend@format$mapping %>% filter(type=="write") %>% select(formatKey, order)) %>%
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
    file <- data %>% select(-spectrum_id) %>% pmap_chr(.write_msp_spectrum)
    return(file)
  }
  .msp_writer_
}
