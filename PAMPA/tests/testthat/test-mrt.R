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

# Attention : ne fonctionne pas si la fonction openDevice.f est active.
# Voir dans la fonction testée pour faire les changements qui s'imposent.
# Warning : doesn't word if the openDevice.f function is used.
# See the tested function to make the necessary changes.

testthat::test_that("plots have known output", {
  mrt_esp_density_family <- function() mrt.f(
    agregation = "espece",
    metrique = "density",
    factGraph = "family",
    factGraphSel = "Pomacentridae",
    listFact = c("year", "protection.status"),
    listFactSel = NA,
    tableMetrique = "unitSp",
    dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("mrt-especes-density-family", mrt_esp_density_family)

  mrt_unitobs_sr_family <- function() mrt.f(
    agregation = "unitobs",
    metrique = "species.richness",
    factGraph = "family",
    factGraphSel = "Pomacentridae",
    listFact = c("year", "protection.status"),
    listFactSel = NA,
    tableMetrique = "unit",
    dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("mrt-unitobs-species_richness-family", mrt_unitobs_sr_family)
})
