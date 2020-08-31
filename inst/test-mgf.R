
library(furrr)
library(devtools)
library(SpectraMapping)
plan(multiprocess)
system.time(
  sp1 <- Spectra(
    system.file("test_spectra/sample.mgf", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE)))
)
system.time(
  sp2 <- Spectra(
    system.file("test_spectra/sample.msp", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE)))
)


massbank <- r"(C:\Daten\AnnotationFlow\AnnotationFlow\libraries\MASSBANK.mgf)"
system.time(sp3 <- Spectra(
  massbank,
  source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE, progress = TRUE)))
)


system.time(
  sp2 <- Spectra(
    system.file("test_spectra/long_spectrum_test.mgf", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE)))
)



rdr <- MsFormatMgf(parallel=FALSE, progress = TRUE)$reader
res <- rdr(read_lines(system.file("test_spectra/sample.mgf", package="SpectraMapping")))
