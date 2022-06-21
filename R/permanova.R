#' Cette fonction permet de faire une permanova.


#' @import vegan
#' @import dplyr


#' @title Permanova
#'
#' @description Calcul d'une permanova selon les parametres fournis
#'
#' @param metric_table chr, le nom de la table de metrique
#' @param metric chr, donnees etudiees (pres_abs ou abundance ("abundance.prop.SC"))
#' @param fact chr, liste des facteurs explicatifs
#' @param formula ..., formule du modele. Le LHS doit etre "dissimilarity_matrix" afin que la
#' matrice de dissimilarite creee soit utilisee. Le RHS defini les variables independantes.
#' Si elle est laissee nulle (la valeur par defaut), alors la fomule est cree sous la forme
#' \code{dissimilarity_matrix ~ fact1 * fact2 * ... * factn}.
#' @param method chr, methode utilisee pour la creation de la matrice de dissimilarites
#' @param nb_perm int, nombre de permutations
#' @param square_roots booleen, utilisation des square roots ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::permanova_pampa.f("unitSp", "pres.abs", c("site", "year"), method = "bray",
#'   nb_perm = 1000, .dataEnv, .baseEnv)
#'
#' @export
permanova_pampa.f <- function(metric_table, metric, fact, formula = NULL, method = "bray",
  nb_perm = 1000, square_roots = FALSE, dataEnv, baseEnv = .GlobalEnv){

  metric_studied <- switch(metric,
    "pres_abs" = "pres.abs",
    "abundance" = "abundance.prop.SC",
    stop(
      "Veuillez choisir une valeur de 'metric' parmi 'presabs' ou 'abundance'.",
      "Please, choose a metric between 'presabs' and 'abundance'."
    )
  )

  # Recuperation des donnees
  # Get the data
  unitobs <- get("unitobs", envir = dataEnv)
  metricTable <- get(metric_table, envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  # On ne garde que les donnees qui nous interessent, on joint les tableau
  # et on enleve les colonnes devenues inutiles
  # Keep only data with interest, join the table and remove useless column
  data_metric <- metricTable[, c("species.code", "observation.unit", metric_studied)]
  data_refesp <- refesp[, c("species.code", "scient.name")]

  data <- dplyr::left_join(data_metric, data_refesp, by = "species.code")

  data <- data[, c("scient.name", "observation.unit", metric_studied)]

  # On trie le tableau par scient.name puis par observation.unit
  # Sort the data set by scient.name and by observation.unit
  data <- data[order(data$observation.unit),]
  data <- data[order(data$scient.name),]

  # On cree la matrice et on renomme les colonnes et les lignes
  # Create the matrix and rename columns and rows
  matrix_data <- matrix(data[,metric_studied], nrow = length(unique(data$observation.unit)),
    ncol = length(unique(data$scient.name)), byrow = TRUE)
  row.names(matrix_data) <- unique(data$observation.unit)
  colnames(matrix_data) <- unique(data$scient.name)

  # On enleve les lignes ne contenant que des 0 ou des NA
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

  # On enleve ces observation de la table des unitobs
  # Remove these observation in the unitobs table too
  data_unitobs <- unitobs[-which(is.element(unitobs$observation.unit, zero_obs)),]
  data_unitobs <- data_unitobs[order(data_unitobs$observation.unit),]

  # On cree la matrice de dissimilarite
  # Creation of the dissimilarity matrix
  dissimilarity_matrix <- vegan::vegdist(matrix_data, method = method,
    binary = ifelse(metric_studied == "pres.abs", TRUE, FALSE))

  # Si aucune formule renseignee, on cree la formule a partir de la liste de facteurs et on lance la fonction
  # sinon, utilisation de la formule donnee
  # If formula is null, get the formula from the list of factors and launch the function
  # else, the given formula is used
  if (is.null(formula)){
    formula <- parse(text = paste("dissimilarity_matrix ~ ", paste(fact, collapse = " * "), sep = ""))[[1]]
  }
  permanova <- vegan::adonis2(eval(formula), data = data_unitobs, permutations = nb_perm, method = method, sqrt.dist = square_roots)

  filename <- resFilePerm.f(metric_studied, metric_table, fact, method, dataEnv, ext = "csv")

  tryCatch(
    write.csv(
      permanova,
      file = filename,
      row.names = TRUE),
    error = function(e){
      message(mltext("writeData.f.msg"), filename)
  })

  return(permanova)
}

resFilePerm.f <- function(metric, tableMetrique, fact, method, dataEnv, ext = "txt"){

  ## Purpose: Definit les noms du fichiers pour les resultats de la
  ##          permanova. L'extension peut etre precisee, mais par defaut,
  ##          c'est le fichier de sorties texte qui est cree.
  ## ----------------------------------------------------------------------
  ## Arguments: metric : donnees etudiees ("pres.abs" ou "abundance.prop.SC").
  ##            tableMetrique : le nom de la table de metrique.
  ##            fact : facteurs explicatifs.
  ##            method : methode utilisee pour la matrice de dissimilarites.
  ##            ext : extension du fichier.
  ## ----------------------------------------------------------------------

  # Nom de fichier :
  filename <- paste(get("filePathes", envir = dataEnv)["results"], "permanova", "_",
    # Metrique analysee :
    metric, "_",
    # table de metriques :
    tableMetrique, "_",
    # liste des facteurs de l'analyse
    paste(fact, collapse = "-"), "_",
    # Methode utilisee
    method,
    # Extension du fichier :
    ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl = TRUE), # nettoyage de l'extension si besoin.
    sep = "")

  # Retourne le nom de fichier :
  return(filename)
}

