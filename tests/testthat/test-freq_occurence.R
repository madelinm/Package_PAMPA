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
  freq_occu_esp_site <- function() freq_occurrence.f(
    agregation = "espece",
    factGraph = "site",
    factGraphSel = NA,
    listFact = c("year", "family"),
    listFactSel = NA,
    new_window = FALSE, dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("frequence_occurrence-especes-site", freq_occu_esp_site)

  freq_occu_unitobs_family <- function() freq_occurrence.f(
    agregation = "unitobs",
    factGraph = "family",
    factGraphSel = "Sparidae",
    listFact = c("year", "site"),
    listFactSel = NA,
    new_window = FALSE, dataEnv = .dataEnv, baseEnv = .baseEnv)
  vdiffr::expect_doppelganger("frequence_occurrence-unitobs-family", freq_occu_unitobs_family)
})
