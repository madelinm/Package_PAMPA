#' Cette fonction permet de faire une permanova.


#' @import vegan
#' @import dplyr


#' @title Permanova
#'
#' @description Calcul d'une permanova selon les parametres fournis
#'
#' @param tableMetrique chr, le nom de la table de metrique
#' @param etude chr, donnees etudiees (presences/absence (pres.abs) ou abondance ("abundance.prop.SC"))
#' @param fact chr, liste des facteurs explicatifs
#' @param method chr, methode utilisee pour la creation de la matrice de dissimilarites
#' @param nb_perm int, nombre de permutations
#' @param dataEnv environnemant de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::permanova_pampa.f("unitSp", "pres.abs", c("site", "year"), method = "bray",
#'   nb_perm = 1000, .dataEnv, .baseEnv)
#'
#' @export
permanova_pampa.f <- function(tableMetrique, etude, fact, method = "bray", nb_perm = 1000,
  dataEnv, baseEnv = .GlobalEnv){

  # Récupération des données
  # Get the data
  unitobs <- get("unitobs", envir = dataEnv)
  metric <- get(tableMetrique, envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  # On ne garde que les données qui nous intéressent, on joint les tableau
  # et on enlève les colonnes devenues inutiles
  # Keep only data with interest, join the table and remove useless column
  data_metric <- metric[, c("species.code", "observation.unit", etude)]
  data_refesp <- refesp[, c("species.code", "scient.name")]

  data <- dplyr::left_join(data_metric, data_refesp, by = "species.code")

  data <- data[, c("scient.name", "observation.unit", etude)]

  # On trie le tableau par scient.name puis par observation.unit
  # Sort the data set by scient.name and by observation.unit
  data <- data[order(data$observation.unit),]
  data <- data[order(data$scient.name),]

  # On crée la matrice et on renomme les colonnes et les lignes
  # Create the matrix and rename columns and rows
  matrix_data <- matrix(data[,etude], nrow = length(unique(data$observation.unit)),
    ncol = length(unique(data$scient.name)), byrow = TRUE)
  row.names(matrix_data) <- unique(data$observation.unit)
  colnames(matrix_data) <- unique(data$scient.name)

  # On enlève les lignes ne contenant que des 0 ou des NA
  # Remove rows with only 0 or NAs
  zero_row <- sapply(seq(nrow(matrix_data)), function(x){
    if (all(is.na(matrix_data[x,]) | matrix_data[x,] == 0)) {
      return(x)
    }
    else {
      return(NA)
    }
  })

  zero_row <- which(!is.na(zero_row))

  zero_obs <- row.names(matrix_data[zero_row,])
  matrix_data <- matrix_data[-zero_row,]
  data.matrix <- matrix_data[order(row.names(matrix_data)),]

  # On enlève ces observation de la table des unitobs
  # Remove these observation in the unitobs table too
  data_unitobs <- unitobs[-which(is.element(unitobs$observation.unit, zero_obs)),]
  data_unitobs <- data_unitobs[order(data_unitobs$observation.unit),]

  # On crée la matrice de dissimilarité
  # Creation of the dissimilarity matrix
  dissimilarity_matrix <- vegan::vegdist(matrix_data, method = method,
    binary = ifelse(etude == "pres.abs", TRUE, FALSE))

  # On crée la formule à partir de la liste de facteurs et on lance la fonction
  # Get the formula from the list of factors and launch the function
  formula <- parse(text = paste("dissimilarity_matrix ~ ", paste(fact, collapse = " * "), sep = ""))[[1]]
  permanova <- adonis2(eval(formula), data = data_unitobs, permutations = nb_perm, method = method)

  return(permanova)
}
