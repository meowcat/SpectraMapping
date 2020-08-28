
source("mgf-parser.R")
source("mgf-format.R")
source("functions-schema.R")
source("MsBackendSchema.R")

plan(multiprocess)
sp <- Spectra("../inst/test_spectra/sample.mgf", source = MsBackendSchema(format = MsFormatMgf(parallel=TRUE)))

# 
# library(MsBackendMgf)
# spsp <- Spectra("../inst/test_spectra/sample.mgf", source=MsBackendMgf())
# sp2sub <- spsp[4:10]

# 
# path_ms2 <- "../inst/test_spectra/sample.mgf"
# data_ms2 <- path_ms2 %>% read_file() %>% str_remove_all(fixed("\r"))
# 
# plan
# mgf_reader <- .mgf_reader(parallel = TRUE)
# 
# 
# 
# rr <- mgf_reader(data_ms2)
# 
# data_ms2_split <- data_ms2 %>% str_split(fixed("\n\n"))
# data_ms2_split <- data_ms2_split[[1]]
# mgf_reader(data_ms2_split[[4]])
# 
# system.time(spectra <-  data_ms2_split %>% map(mgf_reader))
# plan(multiprocess)
# data <- mgf_reader(data_ms2)
# 
# 
# 
# system.time(spectra2 <-  data_ms2_split %>% future_map(mgf_reader))
# 
# system.time(data <- msp_reader(data_ms2))
# 
# example <- "\nBEGIN IONS\nSCANS=NA\nblub=TEST\n121.1212\t4343\n121.3333  3434\nEND IONS\n"
# res <- compact(spectrum(example)$result)
