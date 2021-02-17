library(devtools)
library(tidyverse)
library(Spectra)
load_all()



msp_arus <- system.file("test_spectra/sample.msp", package="SpectraMapping")
sp_arus <- Spectra(
  msp_arus,
  source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE, progress = TRUE)))
msp_arus_mapping <- system.file("mapping/nist-msp-arus.yaml", package="SpectraMapping")

sp_arus_mapped <- mapVariables(sp_arus, msp_arus_mapping)

sp_arus_msp <- Spectra(
  msp_arus,
  MsBackendMsp
)
massbank_read_test <- list.files(
  system.file("test_spectra/massbank", package="SpectraMapping"), full.names = TRUE)
sp_massbank <- Spectra(
  massbank_read_test,
  source = MsBackendMapping(format = MsFormatMassbank(parallel=FALSE, progress = TRUE)))

massbank_mapping <- system.file("mapping/massbank.yaml", package="SpectraMapping")

sp_massbank_mapped <- mapVariables(sp_massbank, massbank_mapping)


# #v <- sp4@backend@variables
# v_wide <- sp4@backend@variables %>% pivot_wider(names_from = "formatKey", values_from = "value", values_fn = list)
# v_wide_multi <- v_wide[c(1,1,1),]
# 
# sp4@backend@variables <- v_wide_multi

wf <- yaml.load_file("inst/mapping/massbank.yaml")
actions <- get_actions(wf)

sp4@backend <- reduce(actions, ~ .y$execute_read(.x), .init = sp4@backend)
spectraVariables(sp4)
rtime(sp4)

collisionEnergy(sp4)
sp4@backend@spectraVariables
sp4@backend@variables -> vvv

precursorMz(sp4)
precScanNum(sp4)
collisionEnergy(sp4) <- seq_along(sp4)
collisionEnergy(sp4)
polarity(sp4)
