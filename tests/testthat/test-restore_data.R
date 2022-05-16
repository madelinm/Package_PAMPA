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
selected_data <- selection.f('unitobs', .dataEnv, .baseEnv)

# On teste que le nombre de lignes des observations revient au nombre de lignes initial après restoreData.f
# Test that the number of row of observations table is equal at the initial number of rows after doing restoreData.f
test_that("retore data restore the number of rows", {
  expect_equal(nrow(restoreData.f(.dataEnv, .baseEnv)$obs), nrow(read.csv(obs_path, header = TRUE)))
})
