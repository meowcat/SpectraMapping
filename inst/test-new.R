library(devtools)
library(tidyverse)
library(Spectra)
#load_all()
library(SpectraMapping)



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





lipidblast_read <- Spectra(
  system.file("test_spectra/lipidblast-riken-short.msp", package="SpectraMapping"),
  source = MsBackendMapping(format = MsFormatMsp(parallel=FALSE, progress = TRUE))
)

#save(lipidblast_read, file="lipidblast.RData")

load_all()
lipidblast_map <- lipidblast_read %>%
  mapVariables(system.file("mapping/lipidblast-riken-msp.yaml", package = "SpectraMapping"))

lipidblast_map_subset <- lipidblast_map[1:100]
library(rcdk)
library(rinchi)



lipidblast_map_testcopy <- lipidblast_map_subset
spectraData(lipidblast_map_testcopy)$segment <- seq_along(lipidblast_map_testcopy) %% 10
spectraData(lipidblast_map_testcopy)$dataStorage <- "<memory>"
mergy <- combineSpectra(lipidblast_map_testcopy, f = spectraData(lipidblast_map_testcopy)$segment)


#spectraData(lipidblast_map)$instrument_type <- "LC-ESI-QTOF"
spectraData(lipidblast_map_subset)$accession <- sprintf("LB%06d", seq_along(lipidblast_map_subset))
spectraData(lipidblast_map_subset)$date <- format(Sys.Date(), "%Y.%m.%d")
spectraData(lipidblast_map_subset)$molecule <- 
  parse.smiles(spectraData(lipidblast_map_subset)$smiles)

spectraData(lipidblast_map_subset)$molecule_charge <- 
  spectraData(lipidblast_map_subset)$molecule %>% 
    map_int(get.total.charge)

rewrite_charged_formula <- function(formula, charge) {
  if(charge == 0)
    return(formula)
  if(charge > 0)
    charge_sign <- rep("+", charge)
  else
    charge_sign <- rep("-", -charge)
  return(glue("[{formula}]{charge_sign}"))
}

spectraData(lipidblast_map_subset)$exactmass <- map2_dbl(
  spectraData(lipidblast_map_subset)$formula,
  spectraData(lipidblast_map_subset)$molecule_charge,
  ~ get.formula(.x, .y)@mass
)

spectraData(lipidblast_map_subset)$formula <-
  map2_chr(
    spectraData(lipidblast_map_subset)$formula,
    spectraData(lipidblast_map_subset)$molecule_charge,
    rewrite_charged_formula
  )


spectraData(lipidblast_map_subset)$inchi <- 
  spectraData(lipidblast_map_subset)$molecule %>% map_chr(get.inchi)
spectraData(lipidblast_map_subset)$inchikey <- 
  spectraData(lipidblast_map_subset)$molecule %>% map_chr(get.inchi.key)

spectraData(lipidblast_map_subset)$splash <- map(
  peaksData(lipidblast_map_subset) %>% as.list(),
  RMassBank:::getSplash
)

spectraData(lipidblast_map_subset)$molecule <- NULL



lipidblast_to_massbank <- lipidblast_map_subset %>%
  writeVariables(mapping = system.file("mapping/massbank.yaml",
                                       package="SpectraMapping"))

lipidblast_to_massbank@backend@variables -> sd

export(lipidblast_map_subset,
       MsBackendMapping(format = MsFormatMassbank(
         parallel = FALSE,
         progress = TRUE,
         mapping = system.file("mapping/massbank.yaml", package="SpectraMapping")
       )),
       file = "X:/massbank-lb/MassBank-data/LB/{accession}.txt")


# lb_subset <- lipidblast_to_massbank[1:100]

massbank_read_test <- list.files(
  system.file("test_spectra/massbank", package="SpectraMapping"), full.names = TRUE)
sp_massbank <- Spectra(
  massbank_read_test,
  source = MsBackendMapping(format = MsFormatMassbank(parallel=FALSE, 
                                                      progress = TRUE,
                                                      mapping = "default")))



sp_massbank <- sp_massbank %>% 
  mapVariables(mapping = system.file("mapping/massbank.yaml", package="SpectraMapping"))

arus_to_massbank <- sp_arus_mapped %>%
  writeVariables(mapping = system.file("mapping/massbank.yaml", package="SpectraMapping"))

res <- export(
  sp_arus_mapped,
  MsBackendMapping(format = MsFormatMassbank(
    parallel = FALSE,
    progress = TRUE,
    mapping = "default"
  )),
  file = "massbank_out/{spectrum_id %% 3}.txt")
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
