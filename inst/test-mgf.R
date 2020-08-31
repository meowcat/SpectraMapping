
library(furrr)
library(devtools)
load_all()
plan(multiprocess)
sp <- Spectra(
  system.file("test_spectra/sample.mgf", package="MsBackendSchema"),
  source = MsBackendSchema(format = MsFormatMgf(parallel=TRUE)))

