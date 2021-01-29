
library(furrr)
library(devtools)
#library(SpectraMapping)
load_all()
library(readr)
library(Spectra)
#plan(multiprocess)
system.time(
  spMgf <- Spectra(
    system.file("test_spectra/sample.mgf", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE)))
)
system.time(
  spMsp <- Spectra(
    system.file("test_spectra/sample.msp", package="SpectraMapping"),
    source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE)))
)


spMgfTarget <- spMgf
spMspTarget <- spMsp
# MSP to MSP


spectraData(spMspTarget@backend) <- spectraData(spMsp@backend)
plain <- spMspTarget@backend@format$writer(spMspTarget@backend)
write_lines(plain, "msp_to_msp.msp")

# MGF to MGF
plain <- spMgfTarget@backend@format$writer(spMgfTarget@backend)
write_lines(plain, "mgf_to_mgf.msp")
# MSP to MGF
spectraData(spMgfTarget@backend) <- spectraData(spMsp@backend)
plain <- spMgfTarget@backend@format$writer(spMgfTarget@backend)
write_lines(plain, "msp_to_mgf.mgf")
# MGF to MSP
spectraData(spMspTarget@backend) <- spectraData(spMgf@backend)
plain <- spMspTarget@backend@format$writer(spMspTarget@backend)
write_lines(plain, "mgf_to_msp.msp")



massbank <- r"(C:\Daten\AnnotationFlow\AnnotationFlow\libraries\MASSBANK.mgf)"
system.time(sp3 <- Spectra(
  massbank,
  source = MsBackendMapping(format = MsFormatMgf(parallel=TRUE, progress = TRUE)))
)
library(MsBackendMgf)
system.time(sp4 <- Spectra(
  massbank,
  source = MsBackendMgf())
)

massbank <- r"(C:\Daten\AnnotationFlow\AnnotationFlow\libraries\MASSBANK.mgf)"
system.time(sp3 <- Spectra(
  massbank,
  source = MsBackendMapping(format = MsFormatMgf(parallel=FALSE, progress = TRUE)))
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
