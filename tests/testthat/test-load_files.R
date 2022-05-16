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

# On teste le nombre de ligne du fichier des observations
# Test the number of row in the observation file
test_that("nrow of obs file doesn't change", {
  expect_equal(nrow(data$obs), nrow(read.csv(obs_path, header = TRUE)))
})

# On teste le nombre de ligne du fichier des unités d'observation
# Test the number of row in the unitobs file
test_that("nrow of unitobs file doesn't change", {
  expect_equal(nrow(data$unitobs), nrow(read.csv(unitobs_path, header = TRUE)))
})

# On teste le nombre de ligne du référentiel des espèces
# (on retire une ligne car il y a une ligne de NAs dans le référentiel qui est supprimé lors de l'importation)
# Test the number of row in the spatial reference table
# (one row is removed because the table has a row of NAs which is delete at the importation )
test_that("nrow of refesp file doesn't change", {
  expect_equal(nrow(data$refesp), nrow(read.csv(refesp_path, header = TRUE))-1)
})
