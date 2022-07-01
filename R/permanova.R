#' Cette fonction permet de faire une permanova.


#' @importFrom vegan vegdist adonis2
#' @import dplyr


#' @title Permanova
#'
#' @description Calcul d'une permanova selon les parametres fournis
#'
#' @param metric_table chr, le nom de la table de metrique
#' @param metric chr, donnees etudiees ("pres_abs" (presence-absence), "abundance" (abondance) ou "density" (densite))
#' @param fact chr, liste des facteurs explicatifs. Utilise si aucune formule n'est renseignee.
#' La fomule est cree sous la forme \code{dissimilarity_matrix ~ fact1 * fact2 * ... * factn}.
#' @param formula chr, RHS de la formule du modele. Si elle est laissee nulle (la valeur par defaut),
#' alors les facteurs explicatifs sont utilises pour creer la formule.
#' @param method chr, methode utilisee pour la creation de la matrice de dissimilarites
#' @param nb_perm int, nombre de permutations
#' @param square_roots booleen, utilisation des square roots ?
#' @param post_hoc booleen, faire un post hoc apres la permanova ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::permanova_pampa.f(
#'   metric_table = "unitSp",
#'   metric = "pres_abs",
#'   fact = c("site", "year"),
#'   method = "bray",
#'   nb_perm = 1000,
#'   square_roots = FALSE,
#'   post_hoc = FALSE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export
permanova_pampa.f <- function(metric_table, metric, fact = NULL, formula = NULL, method = "bray",
  nb_perm = 1000, square_roots = FALSE, post_hoc = TRUE, dataEnv, baseEnv = .GlobalEnv){

  metric_studied <- switch(metric,
    "pres_abs" = "pres.abs",
    "abundance" = "abundance.prop.SC",
    "density" = "density",
    stop(
      "Veuillez choisir une valeur de 'metric' parmi 'pres_abs', 'abundance' et 'density'.",
      "Please, choose a metric between 'presabs', 'abundance' and 'density'."
    )
  )

  if (is.null(fact) & is.null(formula)){
    stop(
      "Un ou plusieurs facteur(s) explicatif(s) ou une formule sont requis",
      "Explanatory factor(s) or formula are required."
    )
  }

  if (nb_perm < 1){
    stop(
      "Le nombre de permutations ne peut pas être nul ou inférieur à 0.",
      "Number of permutations can be null or under 0."
    )
  }

  # Recuperation des donnees
  # Get the data
  unitobs <- get("unitobs", envir = dataEnv)
  metricTable <- get(metric_table, envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  obs_unit_unitobs <- as.character(unique(unitobs$observation.unit))
  obs_unit_metric_table <- as.character(unique(metricTable$observation.unit))

  if (length(obs_unit_unitobs) > length(obs_unit_metric_table)){
    supp_obs_unit <- obs_unit_unitobs[which(!is.element(obs_unit_unitobs, obs_unit_metric_table))]
    unitobs <- unitobs[-which(unitobs$observation.unit %in% supp_obs_unit),]
  } else if (length(obs_unit_metric_table) > length(obs_unit_unitobs)){
    supp_obs_unit <- obs_unit_metric_table[which(!is.element(obs_unit_metric_table, obs_unit_unitobs))]
    metricTable <- metricTable[-which(metricTable$observation.unit %in% supp_obs_unit),]
  }

  # On ne garde que les donnees qui nous interessent, on joint les tableau
  # et on enleve les colonnes devenues inutiles
  # Keep only data with interest, join the table and remove useless column
  data_metric <- metricTable[, c("species.code", "observation.unit", metric_studied)]
  data_refesp <- refesp[, c("species.code", "scient.name")]

  data <- dplyr::left_join(data_metric, data_refesp, by = "species.code")

  data <- data[, c("species.code", "observation.unit", metric_studied)]

  # On trie le tableau par species.code puis par observation.unit
  # Sort the data set by species.code and by observation.unit
  data <- data[order(data$observation.unit),]
  data <- data[order(data$species.code),]

  # On cree la matrice et on renomme les colonnes et les lignes
  # Create the matrix and rename columns and rows
  matrix_data <- matrix(data[,metric_studied], nrow = length(unique(data$observation.unit)),
    ncol = length(unique(data$species.code)), byrow = TRUE)
  row.names(matrix_data) <- unique(data$observation.unit)
  colnames(matrix_data) <- unique(data$species.code)

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
    model_formula <- paste("dissimilarity_matrix ~ ", paste(fact, collapse = " * "), sep = "")
  } else {
    model_formula <- paste("dissimilarity_matrix ~ ", formula)
  }
  permanova <- vegan::adonis2(eval(parse(text = model_formula)[[1]]), data = data_unitobs,
    permutations = nb_perm, method = method, sqrt.dist = square_roots)

  filename <- resFilePerm.f(metric_studied, metric_table, fact, method, dataEnv, ext = "csv")

  tryCatch(
    write.csv(
      permanova,
      file = filename,
      row.names = TRUE),
    error = function(e){
      message(mltext("writeData.f.msg"), filename)
  })

  if (!post_hoc) return(permanova)

  posthoc <- pairwiseAdonis::pairwise.adonis(dissimilarity_matrix, row.names(matrix_data))

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

