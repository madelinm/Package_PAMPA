# Attention : ne fonctionne qu'avec R 4.1.0 au minimum
# Warning : only works with R 4.1.0 at least

library(testthat)
library(vdiffr)

.baseEnv <- environment()
.dataEnv <- new.env()

assign(".baseEnv", .baseEnv, .GlobalEnv)
assign(".dataEnv", .dataEnv, .GlobalEnv)

ws_path <- system.file('example_data/COTE BLEUE/Data/', package = 'PAMPA')
unitobs_path <- system.file('example_data/COTE BLEUE/Data/UnitObs_Staviro_CB191110_280720.txt', package = 'PAMPA')
obs_path <- system.file('example_data/COTE BLEUE/Data/Obs_Staviro_CB191110_AllIdentifiedSpecies_280720.txt', package = 'PAMPA')
refesp_path <- system.file('example_data/COTE BLEUE/Data/refEspecesMed_general_270720 - Int peche est intCH_CB.txt', package = 'PAMPA')
filePathes <- c(unitobs = unitobs_path, obs = obs_path, refesp =  refesp_path, refspa = NULL, ws = ws_path)

data <- load_files.f(filePathes, dminMax = 5, .dataEnv, .baseEnv)

testthat::test_that("plots have known output", {
  lm_esp_density_family <- function() lm.f(
    agregation = "espece",
    metrique = "density",
    factAna = "family",
    factAnaSel = "Sparidae",
    listFact = c("year", "protection.status"),
    listFactSel = NA,
    tableMetrique = "unitSp",
    dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("lm-especes-density-family", lm_esp_density_family)

  lm_unitobs_sr_family <- function() lm.f(
    agregation = "unitobs",
    metrique = "species.richness",
    factAna = "family",
    factAnaSel = "Sparidae",
    listFact = c("year", "protection.status"),
    listFactSel = NA,
    tableMetrique = "unit",
    dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("lm-unitobs-species_richness-family", lm_unitobs_sr_family)
})
