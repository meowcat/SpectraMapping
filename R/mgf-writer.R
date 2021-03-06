

.write_mgf_spectrum <- function(variables, peaks) {
  var_block <- variables %>% mutate(text = paste0(formatKey, "=", value))
  spec_block <- peaks %>% mutate(text = paste(mz, int, annotation, sep='\t'))
  spectrum <- c("BEGIN IONS",
                var_block %>% pull(text),
                spec_block %>% pull(text),
                "END IONS",
                "\n") %>% paste(collapse = "\n")
  return(spectrum)
}

.mgf_writer <- function() {
  .mgf_writer_ <- function(backend) {
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
    file <- data %>% select(-spectrum_id) %>% pmap_chr(.write_mgf_spectrum)
    return(file)
  }
  .mgf_writer_
}
