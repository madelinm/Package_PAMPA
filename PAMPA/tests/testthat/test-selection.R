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

# Tests qui passent individuellement mais pas avec le boutton "Run Tests" à cause de l'intéraction avec l'utilisateur
# Tests pass individually but don't pass with the "Run Tests" button because of the user interaction

# On teste que la sélection sur les unitobs et l'année 2010 renvoie le bon nombre de lignes dans la table des observations
# Test that selection on unitobs and year 2010 return the right number of lines in the observation table
test_that("selection on unitobs change number of rows in observation file", {
  expect_equal(nrow(selection.f('unitobs', .dataEnv, .baseEnv)$obs), length(which(dplyr::left_join(data$obs, data$unitobs)$year == "2010")))
  restored_data <- restoreData.f(.dataEnv, .baseEnv)
  expect_equal(nrow(selection.f('unitobs', .dataEnv, .baseEnv)$obs), length(which(dplyr::left_join(data$obs, data$unitobs)$min.depth == 10)))
})

restored_data <- restoreData.f(.dataEnv, .baseEnv)

# On teste que la sélection sur le référentiel spatialrenvoie le bon nombre de lignes dans la table des observations
# Test that selection on spatial reference table return the right number of lines in the observation table
test_that("selection on refesp change number of rows in observation file", {
  expect_equal(nrow(selection.f('refesp', .dataEnv, .baseEnv)$obs), length(which(dplyr::left_join(data$obs, data$refesp)$family == "Mullidae")))
  restored_data <- restoreData.f(.dataEnv, .baseEnv)
  # renvoie des warnings
  # return warnings
  expect_equal(nrow(selection.f('refesp', .dataEnv, .baseEnv)$obs), length(which(dplyr::left_join(data$obs, data$refesp)$family == "Labridae")))
  restored_data <- restoreData.f(.dataEnv, .baseEnv)
})
