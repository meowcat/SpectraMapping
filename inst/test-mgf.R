
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
library(MsBackendMgf)
system.time(sp3 <- Spectra(
  massbank,
  source = MsBackendMgf())
)

massbank <- r"(C:\Daten\AnnotationFlow\AnnotationFlow\libraries\MASSBANK.mgf)"
system.time(sp3 <- Spectra(
  massbank,
  source = MsBackendMapping(format = MsFormatMgf(parallel=TRUE, progress = TRUE)))
)


massbank <- system.file("test_spectra/massbank_short.mgf", package="SpectraMapping")
Rprof("prof.o")
sp3 <- Spectra(
  massbank,
  source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE, progress = TRUE)))
Rprof(NULL)


system.time(
  sp2 <- Spectra(
    system.file("test_spectra/long_spectrum_test.mgf", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE)))
)


Rprof("profmsp.o")
sp_arus <- Spectra(
  r"(C:\Daten\AnnotationFlow\AnnotationFlow\libraries\plasma_hcd_pos_rec.msp)",
  source = MsBackendMapping(format = MsFormatMsp(progress=TRUE))
)
Rprof(NULL)


rdr <- MsFormatMgf(parallel=FALSE, progress = TRUE)$reader
res <- rdr(read_file(system.file("test_spectra/sample.mgf", package="SpectraMapping")) %>% str_remove_all("\r"))
