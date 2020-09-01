
library(furrr)
library(devtools)
#library(SpectraMapping)
load_all()
library(readr)
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
plain <- spMspTarget@backend@format$writer(spMspTarget@backend)
write_lines(plain, "msp_to_msp.msp")
# MGF to MGF
plain <- spMgfTarget@backend@format$writer(spMgfTarget@backend)
write_lines(plain, "mgf_to_mgf.msp")
# MSP to MGF
asDataFrame(spMgfTarget@backend) <- asDataFrame(spMsp@backend)
plain <- spMgfTarget@backend@format$writer(spMgfTarget@backend)
write_lines(plain, "msp_to_mgf.mgf")
# MGF to MSP
asDataFrame(spMspTarget@backend) <- asDataFrame(spMgf@backend)
plain <- spMspTarget@backend@format$writer(spMspTarget@backend)
write_lines(plain, "mgf_to_msp.msp")



asDataFrame(sp3@backend) <- asDataFrame(sp2@backend)
plain <- sp3@backend@format$writer(sp3@backend)

write_lines(plain, "out.mgf")


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
