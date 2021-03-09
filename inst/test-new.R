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

sp_arus_map_directly <- Spectra(
  msp_arus,
  source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE, 
                                                 progress = TRUE,
                                                 mapping = msp_arus_mapping))
)



massbank_read_test <- list.files(
  system.file("test_spectra/massbank", package="SpectraMapping"), full.names = TRUE)
sp_massbank <- Spectra(
  massbank_read_test,
  source = MsBackendMapping(format = MsFormatMassbank(parallel=FALSE, 
                                                      progress = TRUE,
                                                      mapping = "default")))


arus_to_massbank <- sp_arus_mapped %>%
  writeVariables(mapping = system.file("mapping/massbank.yaml", package="SpectraMapping"))

export(
  sp_arus_mapped,
  MsBackendMapping(format = MsFormatMassbank(
    parallel = FALSE,
    progress = TRUE,
    mapping = "default"
  ))
)
#   file = "massbank_out/{spectrum_id}.txt")
# )


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



msp_nist <- system.file("test_spectra/nist_msp.msp", package="SpectraMapping")
sp_nist <- Spectra(
  msp_nist,
  source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE, progress = TRUE)))

sp_nist_sub <- sp_nist[sample.int(size=200, n=length(sp_nist))]

nist_mapping <- system.file("mapping/nist-msp.yaml", package="SpectraMapping")

sp_nist_mapped <- mapVariables(sp_nist_sub, nist_mapping)

sp_arus_msp <- Spectra(
  msp_arus,
  MsBackendMsp
)



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
