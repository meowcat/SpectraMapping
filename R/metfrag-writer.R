# # SampleName = Nordiazepam
# # InChI = InChI=1S/C15H11ClN2O/c16-11-6-7-13-12(8-11)15(17-9-14(19)18-13)10-4-2-1-3-5-10/h1-8H,9H2,(H,18,19)
# # InChIKey = AKPLHCDWDRPJGD-UHFFFAOYSA-N
# # IsPositiveIonMode = False
# # PrecursorIonMode = -1
# # MassError = -0.014652000004389265
# # MSLevel = MS2
# # IonizedPrecursorMass = 269.0487
# # NumPeaks = 9
# # MolecularFingerPrint = 000000000000000000100000000000000000000000000000000000000000010000000000000001110000001000010010000110111010011100001011100011100001011011100111100001001011110011111000000000000000000000000000
# 205.0532 1.84271
# 206.0611 0.248158
# 210.0557 0.125783
# 224.0715 0.394667
# 235.0514 0.104158
# 240.0221 1.743484
# 241.0299 100
# 242.0377 3.201689
# 267.0333 0.295093

.write_metfrag_spectrum <- function(spectrum_id, variables, peaks, backend, progress, sep = " = ") {
  variables_ <- variables %>% as.list()
  # Select and order variables according to backend@sourceVariables selection and order
  var_factor <- factor(names(variables_), levels = backend@sourceVariables)
  var_order <- order(var_factor, na.last = NA)
  var_print <- variables_[var_order]
  var_tbl_ <- tibble(key = names(var_print), value = var_print)
  var_tbl <- var_tbl_ %>% 
    unnest(value) %>% 
    mutate(value = map(value, ~ as.character(.x))) %>%
    unnest(value)
  var_render <- var_tbl %>% glue_data("# {key}{sep}{value}")
  peaks <- peaks %>% mutate(relint = pmax(1, int / max(int) * 100))
  spec_block <- peaks %>% glue_data("{mz} {round(relint, 0)}")
  spectrum <- c(var_render,
                spec_block,
                "") %>% paste(collapse = "\n")
  if(!is.null(progress))
    progress$tick()
  return(spectrum)
}




.metfrag_writer <- function() {
  .metfrag_writer_ <- function(data, backend, progress = NULL) {
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
    file <- data %>% pmap_chr(.write_metfrag_spectrum, backend = backend, progress = progress)
    return(file)
  }
  .metfrag_writer_
}
