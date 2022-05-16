#' Fonction pour creer des arbres de regression multivariee.
#'
#' \code{agregation} peut prendre la valeur 'espece' ou 'unitobs'. Si 'espece', l'agregation se fera
#' par especes, si unitobs, elle se fera par groupe d'especes.
#'
#' \code{tableMetrique} peut prendre les valeurs 'unitSpSz' (/station /especes /classe de taille),
#' 'unitSp' (/station /especes) ou 'unit' (de biodiversite (/station)). Ce dernier cas n'est
#' possible uniquement lorsque \code{agregation == 'unitobs'}. La table de metriques 'unitSpSz'
#' n'est pas disponible si le jeu de donnees est un jeu de donnees de benthos.
#'
#' \code{metrique} peut prendre differentes valeurs en fonction de celle de \code{tableMetrique} et
#' du jeu de donnees.
#'
#' \code{factGraphSel} peut prendre differentes valeurs en fonction de celle de \code{factGraph}. Ce
#' parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' L'ordre des facteurs de \code{listFact} n'est pas important.
#'
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s) de
#' regroupement selectionne(s) seront prises en compte.


#' @import rpart


#' @title Arbres de regression multivariee
#'
#' @description Creation d'arbres multivaries selon les parametres fournis
#'
#' @param agregation chr, type d'agregation, par espece (espece) ou par groupe d'espece (unitobs)
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite(s) selectionnee(s) pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' mrt.f(agregation = "espece", metrique = "density", factGraph = "scient.name",
#'   factGraphSel = "Chromis_chromis", listFact = c("year", "protection.status"),
#'   listFactSel = list(c("2011", "2019"), NA), tableMetrique = "unitSp",
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export

mrt.f <- function(agregation, metrique, factGraph, factGraphSel = NA, listFact, listFactSel = NA,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  tableMetrique_possible <- c("unit", "unitSp", "unitSpSz")
  nextStep <- switch(agregation,
    "espece" = "MRT.esp",
    "unitobs" = "MRT.unitobs",
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes).",
      "Please, choose an agregation between 'espece' and 'unitobs'."
    )
  )

  # Verification des parametres
  # Check of the parameters
  # ...de la metrique et de la table de metrique :
  # ...the metric and the metric table :
  if (!is.element(tableMetrique, tableMetrique_possible)){
    stop(
      "Veuillez choisir une valeur de 'tableMetrique' entre 'unitSp' (/station /especes),
        'unitSpSz' (/station /especes /classe de taille) et 'unit' (de biodiversite (/station)).",
      "Please, choose a value for 'tableMetrique' between 'unitSp', (/station /species),
        'unitSpSz' (/station /species /size classes) and 'unit' (of biodiversity (/station))."
    )
  }

  # S'il s'agit d'un jeu de données benthos, ou qu'il n'y a pas de classes tailles disponibles :
  # If it's a benthos data set, or no size classes are available :
  if ((is.benthos.f() | nrow(get("unitSpSz", envir = dataEnv)) == 0) & tableMetrique == "unitSpSz"){
    stop(
      "La table de métrique 'unitSpSz' n'est pas disponible pour ce jeu de données.",
      "The metric table 'unitSpSz' is not available for this data set."
    )
  }

  if (tableMetrique == "unit" & agregation == "espece"){
    stop(
      "La valeur de 'tableMetrique' ne peut pas etre 'unit' quand 'agregation' == 'espece'.",
      "The value of 'tableMetrique' cannot be 'unit' when 'agregation' == 'espece'."
    )
  }

  metriques_possibles <- MetricsField.aliases(tableMetrique, nextStep, dataEnv)
  if (!is.element(metrique, metriques_possibles)){
    stop(
      paste("La valeur de 'metrique' n'est pas valide.\n"),
      paste("Veuillez choisir une metrique parmi :\n"),
      paste(metriques_possibles, collapse = ", "),
      paste("The value of 'metrique' isn't correct.\n"),
      paste("Please, choose a metric in the following list :\n"),
      paste(metriques_possibles, collapse = ", ")
    )
  }

  # ...du facteur de separation des graphiques :
  # ...the factor for the graphic separation :
  if (agregation == 'espece'){
    factGraph_possible_refesp <- spRefFields.aliases(site = getOption("P.MPA"), dataEnv = dataEnv,
      ordered = FALSE, tableMetrique = tableMetrique)
    factGraph_possible_unitobs <- UnitobsFields.aliases(ordered = FALSE, dataEnv = dataEnv,
      tableMetrique = tableMetrique)

    if (!is.element(factGraph, factGraph_possible_refesp) & !is.element(factGraph, factGraph_possible_unitobs)){
      stop(
        paste("La valeur '", factGraph, "' du paramètre 'factGraph' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(factGraph_possible_refesp, collapse = ", "),
        paste ("\n ou :\n"),
        paste(factGraph_possible_unitobs, collapse = ", "),
        paste("The value '", factGraph, "' for the 'factGraph' parameter isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(factGraph_possible_refesp, collapse = ", "),
        paste ("\n or :\n"),
        paste(factGraph_possible_unitobs, collapse = ", ")
      )
    }
  }
  else{
    factGraph_possible_refesp <- spRefFields.aliases(site = getOption("P.MPA"), dataEnv = dataEnv,
      ordered = FALSE, tableMetrique = tableMetrique)
    if (!is.element(factGraph, factGraph_possible_refesp)){
      stop(
        paste("La valeur '", factGraph, "' du paramètre 'factGraph' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(factGraph_possible_refesp, collapse = ", "),
        paste("The value '", factGraph, "' of the 'factGraph' parameter isn't correct.\n", sep = ""),
        paste("please, choose one in the following list :\n"),
        paste(factGraph_possible_refesp, collapse = ", ")
      )
    }
  }


  # ...des modalites du facteur de separation des graphiques :
  # ...the modalities of the factor for the  graphic separation :
  factGraphSel_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
    facts = factGraph, selections = append(list(NA), NA), metrique = metrique,
    nextStep = nextStep, dataEnv, level = 0)[, factGraph])
  if (!is.na(factGraphSel) && !is.element(factGraphSel, factGraphSel_possible)){
    stop(
      paste("La valeur '", factGraphSel,
        "' du paramètre 'factGraphSel' n'est pas valide.\n", sep = ""),
      paste("Veillez choisir parmi :\n"),
      paste(factGraphSel_possible, collapse = ", "),
      paste("The value '", factGraphSel,
        "' of the 'factGraphSel' parameter isn't correct.\n", sep = ""),
      paste("Please, choose one in the following list :\n"),
      paste(factGraphSel_possible, collapse = ", ")
    )
  }

  # ...des facteurs explicatifs :
  # ...the explanatory factors :
  listFact_possible <- refTablesFields.aliases(nomTable = tableMetrique, dataEnv = dataEnv)
  for (i in seq(length(listFact))){
    if (!is.element(listFact[i], listFact_possible)){
      stop(
        paste("La valeur '", listFact[i], "' du paramètre 'listFact' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(listFact_possible, collapse = ", "),
        paste("The value '", listFact[i], "' of the 'listFact' parameter isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(listFact_possible, collapse = ", ")
      )
    }
  }

  # ...des modalites des facteurs explicatifs :
  # ...the modalities of the explanatory factors :
  if (length(listFactSel) != length(listFact)){
    if (length(listFactSel) == 1 & is.na(listFactSel)){
      listFactSel <- lapply(listFact, function(x) NA)
    }
    else{
      stop(
        "'listFact' et 'listFactSel' doivent avoir la même longueur.",
        "'listFact' and 'listFactSel' must have the same length."
      )
    }
  }

  for (i in seq(length(listFact))){
    listFactSel_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
      facts = listFact[i], selections = append(list(NA), NA), metrique = metrique,
      nextStep = nextStep, dataEnv, level = 1)[, listFact[i]])
    for (j in seq(length(listFactSel[[i]]))){
      if (!is.na(listFactSel[[i]][j]) && !is.element(listFactSel[[i]][j], listFactSel_possible)){
        stop(
          paste("La valeur '", listFactSel[[i]][j], "' du paramètre 'listFactGraph' pour le facteur '",
                listFact[i], "' n'est pas valide.\n", sep = ""),
          paste("Veuillez choisir parmi :\n"),
          paste(listFactSel_possible, collapse = ", "),
          paste("The value '", listFactSel[[i]][j], "' of the 'listFactGraph' parameter for the factor '",
                listFact[i], "' isn't correct.\n", sep = ""),
          paste("Please, choose one in the following list :\n"),
          paste(listFactSel_possible, collapse = ", ")
        )
      }
    }
  }

  # Verification que les parametres sont "compatibles" et correspondent a des donnees :
  # Check that the parameter are "compatibles" and correspond to data :
  modalites_trouvees <- selectModalites.f(tableMetrique = tableMetrique,
    facts = c(factGraph, listFact), selections = append(list(factGraphSel), listFactSel),
    metrique = metrique, nextStep = nextStep, dataEnv, level = length(listFact))

  if (nrow(modalites_trouvees) == 0){
    stop(
      "Aucune donnée trouvée avec ces paramètres.",
      "No data found with these parameters."
    )
  }

  # Lancement de la fonction de graphique
  # Launch of the graphic function
  if (agregation == "espece"){
    WP2MRT.esp.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
  else{
    WP2MRT.unitobs.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
}


#' @title Arbres de régression multivariée (données agrégées par espèces)
#'
#' @description Cette fonction permet de faire des arbres de régression multivariée des données
#' agrégées par espèces
#'
#' @param metrique chr, métrique choisie
#' @param factGraph chr, facteur de séparation des graphiques
#' @param factGraphSel chr, sélection des modalité pour le facteur de séparation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalités sélectionnées pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de métriques
#' @param dataEnv environnement de stockage des données
#' @param baseEnv environnement parent
#'
#' @return none

WP2MRT.esp.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
  new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des arbres de régression multivariée en tenant
  ##          compte des options graphiques + Sorties texte.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s).
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 11 mai 2011, 10:09

#  pampaProfilingStart.f()

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leur liste de modalités sélectionnées.

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL)

  # Formule du boxplot
  exprMRT <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))

  # Identification des différents graphiques à générer:
  if (factGraph == ""){                # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){       # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                      # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }


  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list()

  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :
  for (modGraphSel in iFactGraphSel){
    # Préparation des données pour un graphique :
    if (modGraphSel == ""){          # ...si pas de facteur de séparation des graphiques

      tmpDataMod <- tmpData
    }else{                           # ...sinon.
      tmpDataMod <- subset(tmpData, tmpData[ , factGraph] == modGraphSel) # Subset des données pour la modalité.
    }

    # Passage au graphique suivant si le nombre d'observations  < au minimum défini dans les options.
    if (dim(tmpDataMod)[1] < getOption("P.MinNbObs")){
      warning(mltext("WP2boxplot.W.n.1"), modGraphSel, " < ", getOption("P.MinNbObs"),
        mltext("WP2boxplot.W.n.2"))
      next()
    }else{}

    # Suppression des 'levels' non utilisés :
    tmpDataMod <- dropLevels.f(tmpDataMod)

    # Sauvegarde temporaire des données :
    DataBackup[[modGraphSel]] <<- tmpDataMod

    if (new_window){
      # Ouverture et configuration du périphérique graphique :
      graphFileTmp <- openDevice.f(
        noGraph = which(modGraphSel == iFactGraphSel),
        metrique = metrique,
        factGraph = factGraph,
        modSel = if (getOption("P.plusieursGraphPage")){
          iFactGraphSel      # toutes les modalités.
        }else{
          modGraphSel        # la modalité courante uniquement.
        },
        listFact = listFact,
        dataEnv = dataEnv,
        type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
          "unitSp" = {"espece"},
          "unitSpSz" = {"CL_espece"},
          "espece"),
        typeGraph = "MRT")
    } else{
      modSel <- if (getOption("P.plusieursGraphPage")){
        iFactGraphSel      # toutes les modalités.
      }else{
        modGraphSel        # la modalité courante uniquement.
      }

      graphFileTmp <- resFileGraph.f(
        metrique = metrique,
        factGraph = factGraph,
        modSel = modSel,
        listFact = listFact,
        dataEnv = dataEnv,
        ext = "wmf",
        prefix = "MRT",
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
          "unitSp" = {"espece"},
          "unitSpSz" = {"CL_espece"},
          "espece"))
    }

    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    par(mar = c(2, 5, 8, 5), mgp = c(3.5, 1, 0)) # paramètres graphiques.

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    mainTitle <- graphTitle.f(
      metrique = metrique,
      modGraphSel = modGraphSel,
      factGraph = factGraph,
      listFact = listFact,
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"espece"},
        "unitSpSz" = {"CL_espece"},
        "unit" = {"biodiv"},
        "espece"),
      model = mltext("WP2MRT.esp.graphTitle.pfx", language = getOption("P.lang")))

    # MRT!
    tmpMRT <- rpart:::rpart(exprMRT, data = tmpDataMod)

    # rpart:::plot.rpart(tmpMRT, main = mainTitle)
    # rpart:::text.rpart(tmpMRT, use.n = TRUE, pretty = 1, all = TRUE, xpd = NA, fancy = TRUE)

    rpart:::plot.rpart(tmpMRT, main = mainTitle, margin = 0.00)
    par(xpd = NA)
    rpart:::text.rpart(tmpMRT, use.n = TRUE, pretty = 1, all = TRUE, xpd = NA,
      fancy = TRUE, adj = c(0.5, 0.75))
    # text.rpart.new(tmpMRT, use.n = TRUE, pretty = 0, all = TRUE, xpd = NA)

    # Écriture des résultats formatés dans un fichier :
    tryCatch(
      sortiesMRT.f(objMRT = tmpMRT, formule = exprMRT,
        metrique = metrique,
        factAna = factGraph, modSel = modGraphSel,
        listFact = listFact, listFactSel = listFactSel,
        Data = tmpDataMod, dataEnv = dataEnv,
        type = ifelse(tableMetrique == "unitSpSz",
          "CL_espece",
          "espece"),
        baseEnv = baseEnv),
      error = function(e){
        errorLog.f()
        print("error")
      }
    )

    # On ferme les périphériques PNG en mode fichier individuel :
    if (isTRUE(getOption("P.graphPNG"))){
      if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1))
        dev.off()
    }else{
      # Sauvegarde en wmf si pertinent et souhaité :
      if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
          (! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
          !getOption("P.graphPDF"))
      {
        savePlot(graphFile, type = "wmf", device = dev.cur())
      }else{}
    }
  }

  # On ferme les périphériques PDF ou PNG restants :
  if (getOption("P.graphPDF") || (isTRUE(getOption("P.graphPNG"))
      && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1))
  {
    dev.off()

    # Inclusion des fontes dans le pdf si souhaité :
    if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
      i <- 1

      # On parcours tous les fichiers qui correspondent au motif :
      while (is.element(basename(tmpFile <- sub("\\%03d", formatC(i, width = 3, flag = "0"),
             graphFile)), dir(dirname(graphFile))) &&
             # Si pas de remplacement effectif, application pour i == 1 uniquement :
             (i == 1 || grepl(pattern = "\\%03d", graphFile)))
      {
        tryCatch(embedFonts(file = tmpFile),
          error = function(e){
            warning(mltext("WP2boxplot.W.pdfFonts"))
        })

        i <- i + 1
      }
    }else{}
  }else{}

  # Sauvegarde en wmf restants si pertinent et souhaité :
  if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
      !(getOption("P.graphPNG") || getOption("P.graphPDF")) &&
      getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
  {
    savePlot(graphFile, type = "wmf", device = dev.cur())
  }else{}

#  pampaProfilingEnd.f()
}


# subsetToutesTables.f <- function(metrique, facteurs, selections,
#   dataEnv, tableMetrique = "", exclude = NULL, add = c("species.code", "observation.unit")){
#
#   ## Purpose: Extraire les données utiles uniquement, d'après les métrique
#   ##          et facteur(s) séléctionnés, ainsi que leur(s) sélection(s) de
#   ##          modalité(s).
#   ## ----------------------------------------------------------------------
#   ## Arguments: metrique : la métrique choisie.
#   ##            facteurs : les facteurs sélectionnés (tous)
#   ##            selections : les sélections de modalités correspondantes
#   ##                         (liste).
#   ##            tableMetrique : le nom de la table des métriques.
#   ##            exclude : niveau de facteur à ne pas prendre en compte pour
#   ##                      le subset.
#   ##            add : champ(s) (de la table de métrique) à ajouter aux
#   ##                  données.
#   ##            dataEnv : l'environnement des données.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date:  6 août 2010, 16:46
#
#   # Récupération des référentiels :
#   unitobs <- get("unitobs", envir = dataEnv)
#   refesp <- get("refesp", envir = dataEnv)
#
#   # Si pas de table de métrique disponible ou déjà calculée
#   # ("TableOcurrences" est calculée à partir de la sélection) :
#   if (is.element(tableMetrique, c("", "TableOccurrences", "TablePresAbs"))){
#     tableMetrique <- "unitSp"
#   }else{}
#
#   casTables <- c("unitSp" = "unitSp",
#     "TablePresAbs" = "unitSp",
#     "unitSpSz" = "unitSpSz")
#
#   # Récupération de la table de métriques :
#   dataMetrique <- get(tableMetrique, envir = dataEnv)
#
#   # Si pas de métrique disponible ou déjà calculée ("freq.occurrence" est calculée à partir de la sélection) :
#   if (is.element(metrique, c("", "occurrence.frequency"))){
#     metrique <- "tmp"
#     dataMetrique$tmp <- 0
#     dataMetrique$tmp[dataMetrique[ , getOption("P.nbName")] > 0] <- 1
#   }else{}
#
#   if (!is.null(add)){
#     metriques <- c(metrique, add[is.element(add, colnames(dataMetrique))])
#   }else{
#     metriques <- metrique
#   }
#
#   # Subset en fonction de la table de métrique
#   switch(casTables[tableMetrique],
#     # Cas de la table d'observation ou des tables de présence :
#     unitSp = {
#       restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop = FALSE],
#         unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
#           unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
#           facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
#         refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
#           refesp$species.code),        # ajout des colonnes sélectionnées d'especes
#           facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
#     },
#     # Cas de la table d'observations par classes de taille :
#     unitSpSz = {
#       restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) ,
#         c(metriques, "size.class"), drop = FALSE],
#         unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
#           unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
#           facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
#         refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
#           refesp$species.code),        # ajout des colonnes sélectionnées d'especes
#           facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
#     },
#     # Autres cas :
#     restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop = FALSE],
#       unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
#         unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs.
#         facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE])
#   )
#
#   selCol <- which(!is.na(selections))
#   if (!is.null(exclude)){
#     selCol <- selCol[selCol != exclude]
#   }
#   for (i in selCol){
#     restmp <- subset(restmp, is.element(restmp[ , facteurs[i]], selections[[i]]))
#   }
#
#   # Traitement particulier des classes de taille (mise en facteur avec ordre défini selon le context) :
#   if (is.element("size.class", colnames(restmp))){
#     if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$size.class)), perl = TRUE)) ==
#         length(unique(as.character(restmp$size.class))))
#     {
#       restmp$size.class <-
#         factor(as.character(restmp$size.class),
#           levels = unique(as.character(restmp$size.class))[
#             order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
#               "\\1",
#               unique(as.character(restmp$size.class)),
#               perl = TRUE)),
#             na.last = FALSE)])
#     }else{
#       restmp$size.class <- factor(restmp$size.class)
#     }
#   }else{}
#
#   # Conversion des biomasses et densités -> /100m² :
#   if (any(is.element(colnames(restmp), c("biomass", "density",
#     "biomass.max", "density.max", "biomass.sd", "density.sd"))) && ! is.peche.f())
#   {
#     restmp[ , is.element(colnames(restmp),
#       c("biomass", "density",
#         "biomass.max", "density.max",
#         "biomass.sd", "density.sd"))] <- 100 *
#     restmp[, is.element(colnames(restmp),
#       c("biomass", "density",
#         "biomass.max", "density.max",
#         "biomass.sd", "density.sd"))]
#   }else{}
#
#   return(restmp)
# }
#
#
# openDevice.f <- function(noGraph, metrique, factGraph, modSel, listFact, dataEnv,
#   type = "espece", typeGraph = "boxplot", large = FALSE){
#
#   ## Purpose: Ouvrir les périphériques graphiques avec les bonnes options
#   ## ----------------------------------------------------------------------
#   ## Arguments: noGraph : le numéro de graphique (integer)
#   ##            metrique : la métrique choisie.
#   ##            factGraph : le facteur de séparation des graphiques.
#   ##            modSel :  modalité(s) de factGraph sélectionnée(s).
#   ##            listFact : liste du (des) facteur(s) de regroupement.
#   ##            type : type de données (traitement conditionnel).
#   ##            typeGraph : type de graphique.
#   ##            large : pour des traitements particuliers (e.g. MRT)
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 12 août 2010, 14:54
#
#   fileName <- NULL
#
#   if (!getOption("P.graphPDF")){ # sorties graphiques à l'écran ou PNG.
#     if (isTRUE(getOption("P.graphPNG"))){
#       if (noGraph == 1 || ! getOption("P.plusieursGraphPage") ||
#           grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph))
#       {
#         pngFileName <- resFileGraph.f(metrique = metrique, factGraph = factGraph, modSel = modSel,
#           listFact = listFact, dataEnv = dataEnv, ext = "png", prefix = typeGraph,
#           sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
#             "%03d",
#             ""),
#           type = type)
#
#         # Si plusieurs graphiques par page :
#         if (getOption("P.plusieursGraphPage") && length(modSel) > 1 & # Regrouper dans une fonction de test
#             ! is.element(type, c("unitobs")) &&                       # (mutualiser le code). [!!!]
#             ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph))
#         {
#           png(pngFileName,
#               width = ifelse(large, 120, 90) * 15,
#               height = ifelse(large, 75, 55) * 15,
#               pointsize = 14)
#           par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
#         }else{
#           png(pngFileName,
#             width = ifelse(large, 100,
#               ifelse(isTRUE(getOption("P.graphPaper")), 50, 75)) * 15,
#             height = ifelse(large, 55,
#               ifelse(isTRUE(getOption("P.graphPaper")), 30, 40)) * 15, pointsize = 14)
#         }
#
#         # Pour retourner le nom de fichier malgré tout :
#         fileName <- pngFileName
#       }else{}
#
#     }else{   # Graphiques à l'écran :
#       # Des fonctions différentes pour l'affichage à l'écran, selon la plateforme :
#       if (.Platform$OS.type == "windows"){
#         winFUN <- "windows"
#       }else{
#         winFUN <- "X11"
#       }
#
#       if (getOption("P.plusieursGraphPage") &&                           # Plusieurs graphs par page...
#           length(modSel) > 1 &&                                          # ...plus d'un facteur sélectionné...
#           ! is.element(type, c("unitobs")) &&                            # ...pas d'agrégation...
#           ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph)) # ...et pas des cartes.
#       {
#         if ((noGraph %% # ...et page remplie.
#              (getOption("P.nrowGraph") * getOption("P.ncolGraph"))) == 1)
#         {
#           # [!!!] Limiter aux cas nécessaires... (cf. plus haut).
#           eval(call(winFUN,
#             width = ifelse(large, 40, 30),  # 80, 60
#             height = ifelse(large, 26, 20), # 45, 35
#             pointsize = ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))
#
#           par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
#         }else{                    # Pas plusieurs graphs par page.
#         }
#       }else{                      # Pas plusieurs graphs par page.
#         eval(call(winFUN,
#           width = ifelse(large, 35,
#             ifelse(isTRUE(getOption("P.graphPaper")), 10, 25)), # 10, 50
#           height = ifelse(large, 15,
#             ifelse(isTRUE(getOption("P.graphPaper")), 6, 12)), # 6, 20
#           pointsize = ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))
#       }
#
#       fileName <- resFileGraph.f(metrique = metrique, factGraph = factGraph, modSel = modSel,
#         listFact = listFact, dataEnv = dataEnv, ext = "wmf", prefix = typeGraph,
#         sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
#           "%03d",
#           ""),
#         type = type)
#     }
#   }else{ # Sorties graphiques en pdf :
#     if (noGraph == 1){
#       # Nom de fichier de fichier :
#       if (getOption("P.PDFunFichierPage")){ # Un fichier par graphique avec numéro.
#         pdfFileName <- paste(get("filePathes", envir = dataEnv)["results"],
#           typeGraph, "_", metrique, "_", factGraph, "_", paste(listFact, collapse = "-"),
#           "-%03d.pdf", sep = "")
#         onefile <- FALSE
#
#       }else{                          # Tous les graphiques dans des pages séparées d'un même fichier.
#         pdfFileName <- paste(get("filePathes", envir = dataEnv)["results"],
#           typeGraph, "_", metrique, "_", factGraph, "_", paste(listFact, collapse = "-"),
#             ".pdf", sep = "")
#         onefile <- TRUE
#       }
#       # Ouverture de fichier :
#       pdf(pdfFileName, encoding = "ISOLatin1", family = "URWHelvetica", onefile = onefile,
#         width = ifelse(large, 30,
#           ifelse(isTRUE(getOption("P.graphPaper")), 12, 20)),
#         height = ifelse(large, 20,
#           ifelse(isTRUE(getOption("P.graphPaper")), 8, 12)),
#         pointsize = 14)
#
#       # Si plusieurs graphiques par page :
#       if (getOption("P.plusieursGraphPage") &&
#           length(modSel) > 1 &&                                          # Plus d'un facteur sélectionné.
#           ! is.element(type, c("unitobs")) &&                            # Pas d'agrégation.
#           ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph)) # ...et pas des cartes.
#       {
#         par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
#       }else{}
#
#       # Pour retourner le nom de fichier également :
#       fileName <- pdfFileName
#     }else{}
#   }
#
#   par(cex = getOption("P.cex"))
#   return(fileName)
# }
#
#
# resFileGraph.f <- function(metrique, factGraph, modSel, listFact, ext, dataEnv,
#   prefix = "boxplot", sufixe = NULL, type = "espece"){
#
#   ## Purpose: Définit les noms du fichiers pour les résultats des modèles
#   ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
#   ##          mais par défaut, c'est le fichier de sorties texte qui est
#   ##          créé.
#   ## ----------------------------------------------------------------------
#   ## Arguments: metrique : nom de la métrique analysée.
#   ##            factGraph : nom du facteur de séprataion des analyses/
#   ##                        de selection d'espèce(s).
#   ##            modSel : modalité(s) de factGraph sélectionnée(s).
#   ##            listFact : vecteur des noms de facteurs de l'analyse.
#   ##            prefix : préfixe du nom de fichier.
#   ##            sufixe : un sufixe pour le nom de fichier.
#   ##            ext : extension du fichier.
#   ##            type : type de modèle (traitement conditionnel).
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 21 janv. 2011, 10:38
#
#   # Nom de fichier :
#   filename <- paste(get("filePathes", envir = dataEnv)["results"], prefix, "_",
#     # Métrique analysée :
#     metrique,
#     ifelse(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1,
#       paste("(", round(100 * getOption("P.GraphPartMax")),"pc-max)", sep = ""),
#       ""),
#     "_",
#     # si facteur de séparation des analyses :
#     mltext("resFileGraph.aggreg.abv"),
#     switch(type,
#       "espece" = mltext("resFileGraph.pfx.spSt"),
#       "CL_espece" = mltext("resFileGraph.pfx.SCspSt"),
#       "unitobs" = mltext("resFileGraph.pfx.St"),
#       "CL_unitobs" = mltext("resFileGraph.pfx.SCSt"),
#       ""),
#     switch(type,
#       "espece" = {
#         ifelse(factGraph == "",
#           "",
#           paste(factGraph, "(", ifelse(modSel[1] != "",
#             collapse.max.f(modSel, collapse = "+"),
#             mltext("resFileGraph.all.spSt")),
#             ")_", sep = ""))
#       },
#       "CL_espece" = {
#         ifelse(factGraph == "",
#           "",
#           paste(factGraph, "(", ifelse(modSel[1] != "",
#             collapse.max.f(modSel, collapse = "+"),
#             mltext("resFileGraph.all.SCspSt")),
#             ")_", sep = ""))
#       },
#       "unitobs" = {
#         ifelse(factGraph == "",
#           mltext("resFileGraph.allSp.St"),
#           paste(factGraph, "(", ifelse(modSel[1] != "",
#             collapse.max.f(modSel, collapse = "+"),
#             mltext("resFileGraph.all.St")),
#             ")_", sep = ""))
#       },
#       "CL_unitobs" = {
#         ifelse(factGraph == "",
#           mltext("resFileGraph.allSp.St"),
#           paste(factGraph, "(", ifelse(modSel[1] != "",
#             collapse.max.f(modSel, collapse = "+"),
#             mltext("resFileGraph.all.St")),
#             ")_", sep = ""))
#       },
#     ""),
#     # liste des facteurs de l'analyse
#     paste(listFact, collapse = "-"),
#     # sufixe :
#     ifelse(is.null(sufixe) || sufixe == "",
#       "",
#       paste("_", sufixe, sep = "")),
#     # Extension du fichier :
#       ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl = TRUE), # nettoyage de l'extension si besoin.
#     sep = "")
#
#   # Retourne le nom de fichier :
#   return(filename)
# }
#
#
# collapse.max.f <- function(x, nmax = 120, collapse = "+", fillstr = "..."){
#
#   ## Purpose: assemblage des noms de modalités avec contrôle sur la nombre
#   ##          de caractères
#   ## ----------------------------------------------------------------------
#   ## Arguments: x : chaîne de caractères à assembler.
#   ##            nmax : nombre maximum de lettres dans la chaîne assemblée.
#   ##            collapse : caractère d'assemblage ("+" par défaut).
#   ##            fillstr : chaîne de remplissage de dépassement.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date:  2 mai 2013, 15:56
#
#   # Collapse :
#   res <- paste(x, collapse = collapse)
#
#   # test de longueur de chaîne :
#   if (nchar(res) > nmax){
#     res <- paste(substr(res, 1, as.integer(nmax / 2) - 1),
#       fillstr,
#       substr(res, nchar(res) - as.integer(nmax / 2) + 2, nchar(res)), sep = "")
#   }
#
#   return(res)
# }
#
#
# graphTitle.f <- function(metrique, modGraphSel, factGraph, listFact, model = NULL, type = "espece",
#   graphType = c("generic", "boxplot", "barplot", "occFrequency"),
#   lang = getOption("P.lang")){
#
#   ## Purpose:
#   ## ----------------------------------------------------------------------
#   ## Arguments:
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 14 oct. 2010, 15:44
#   graphType <- match.arg(arg = graphType,
#     choices = c("generic", "boxplot", "barplot", "occFrequency"),
#     several.ok = FALSE)
#
#   # Factors names in the chosen language:
#   factNames <- sapply(listFact[length(listFact):1],
#     function(x)paste(c(# varNames.f(x, "article"),
#       "",
#       varNames.f(x, "nom")), collapse = ""))
#
#   return(paste(
#     ifelse(is.null(model),
#       switch(graphType,
#         "generic" = {
#           mltext("graphTitle.vals",
#             language = lang)
#         },
#         "boxplot" = {
#           mltext("graphTitle.boxplots",
#             language = lang)
#         },
#         "barplot" = {
#           paste0(switch(getOption("P.barplotStat"),
#             "mean" = ,
#             "moyenne" = {
#               Capitalize.f(mltext("stat.mean", language = lang))
#             },
#             "median" = ,
#             "médianne" = {
#               Capitalize.f(mltext("stat.median",
#                 language = lang))
#             }),
#             "(")
#           },
#           "occFrequency" = {""}),
#       paste(model,
#         mltext("graphTitle.for",
#           language = lang),
#        varNames[metrique, "article"], sep = "")),
#     ifelse(graphType == "occFrequency",
#       Capitalize.f(varNames[metrique, "nom"]),
#       varNames[metrique, "nom"]),
#     ifelse(test = graphType == "barplot",
#       yes = ")",
#       no = ""),
#     # ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
#     #   paste(mltext("graphTitle.agg", language = lang),
#     #     switch(varNames[metrique, "genre"], # for languages with genre concordence.
#     #       f = mltext("graphTitle.f", language = lang),
#     #       fp = mltext("graphTitle.fp", language = lang),
#     #       mp = mltext("graphTitle.mp", language = lang), ""),
#     #     sep = ""),
#     #   ""),
#     switch(type,
#       "espece" = mltext("graphTitle.bySpSt", language = lang),
#       "CL_espece" = mltext("graphTitle.bySCSpSt", language = lang),
#       "unitobs" = mltext("graphTitle.bySt", language = lang),
#       "unitobs(CL)" = mltext("graphTitle.byStSC", language = lang),
#       "CL_unitobs" = mltext("graphTitle.bySCSt", language = lang),
#       "biodiv" = mltext("graphTitle.biodiv", anguage = lang),
#       ""),
#     switch(type,
#       "espece" = {
#         ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
#           "",
#           paste(mltext("graphTitle.sep.SpSt", language = lang),
#             " '", factGraph, "' = ", modGraphSel, sep = ""))
#       },
#       "CL_espece" = {
#         ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
#           "",
#           paste(mltext("graphTitle.sep.SCSpSt", language = lang),
#             " '", factGraph, "' = ", modGraphSel, sep = ""))
#       },
#       "unitobs" = {
#         ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
#           mltext("graphTitle.sep.St.all", language = lang),
#           paste(mltext("graphTitle.sep.St", language = lang),
#             " '", factGraph, "' = (",
#             paste(modGraphSel, collapse = ", "), ")", sep = ""))
#       },
#       "unitobs(CL)" = {
#         ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
#           mltext("graphTitle.sep.StSC.all", language = lang),
#             paste(mltext("graphTitle.sep.StSC", language = lang),
#               " '", factGraph, "' = (",
#               paste(modGraphSel, collapse = ", "), ")", sep = ""))
#       },
#       "CL_unitobs" = {
#         ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
#           mltext("graphTitle.sep.SCSt.all", language = lang),
#             paste(mltext("graphTitle.sep.SCSt", language = lang),
#               " '", factGraph, "' = (",
#               paste(modGraphSel, collapse = ", "), ")", sep = ""))
#       },
#       "biodiv" = {
#         ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
#           "",
#           paste(mltext("graphTitle.sep.biodiv", language = lang),
#             " '", factGraph, "' = (",
#             paste(modGraphSel, collapse = ", "), ")", sep = ""))
#       },
#       ""),
#     mltext("graphTitle.by", language = lang),
#     if (length(factNames) > 1){
#       paste(paste(head(factNames, -1), collapse = ", "),
#         tail(factNames, 1),
#         sep = mltext("graphTitle.and", language = lang))
#     }else{
#       factNames
#     },
#     "\n", sep = ""))
# }


sortiesMRT.f <- function(objMRT, formule, metrique, factAna, modSel, listFact, listFactSel, Data,
  dataEnv = dataEnv, sufixe = NULL, type = "espece", baseEnv = .GlobalEnv){

  ## Purpose: Formater les résultats des MRT et les écrire dans un fichier
  ## ----------------------------------------------------------------------
  ## Arguments: objMRT : un objet de classe 'rpart'.
  ##            formule : la formule utilisée (pas lisible dans le call).
  ##            metrique : la métrique choisie.
  ##            factAna : le facteur de séparation des analyses.
  ##            modSel : la modalité courante.
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s).
  ##            Data : les données utilisées.
  ##            sufixe : un sufixe pour le nom de fichier.
  ##            type : type d'analyse, pour traitement conditionnel des
  ##                   titres et noms de fichiers.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 25 août 2010, 16:19

  # longueur des lignes pour les sorties textes :
  oOpt <- options()
  on.exit(options(oOpt))

  options(width = 120)

  # Formule de modèle lisible:
  objMRT$call$formula <- formule
  formule <<- formule
  resMRT <<- objMRT

  # Chemin et nom de fichier :
  resFile <- resFileMRT.f(metrique = metrique, factAna = factAna,
    modSel = modSel, listFact = listFact,
    dataEnv = dataEnv,
    sufixe = sufixe, type = type)
  on.exit(close(resFile), add = TRUE)

  # Écriture des résultats :

  cat(mltext("sortiesMRT.call"), "\n", file = resFile, append = FALSE)
  dput(objMRT$call, file = resFile)

  cat("\n\n----------------------------------------------------------------------------------------------------",
    file = resFile, append = TRUE)

  cat("\n", mltext("sortiesMRT.printRes"), "\n\n", file = resFile, append = TRUE)

  capture.output(print.rpart.ml(objMRT), file = resFile, append = TRUE)

  cat("\n\n----------------------------------------------------------------------------------------------------",
    file = resFile, append = TRUE)

  cat("\n", mltext("sortiesMRT.details"), "\n\n", file = resFile, append = TRUE)

  capture.output(summary.rpart.ml(objMRT), file = resFile, append = TRUE)

  # ##################################################
  # Sauvegarde des données :
  filename <- summary(resFile)$description

  # close(resFile)                      # Maintenant seulement on peut fermer ce fichier.

  if (getOption("P.saveData") &&  ! isTRUE(sufixe == "(red)")) {
    writeData.f(filename = filename, Data = Data, cols = NULL)
  }else{}

  # Sauvegarde des infos sur les données et statistiques :
  if (getOption("P.saveStats") &&  ! isTRUE(sufixe == "(red)")){
    infoStats.f(filename = filename, Data = Data, agregLevel = type, type = "stat",
      metrique = metrique, factGraph = factAna, factGraphSel = modSel,
      listFact = listFact, listFactSel = listFactSel,
      dataEnv = dataEnv, baseEnv = baseEnv)
  }else{}
}


resFileMRT.f <- function(metrique, factAna, modSel, listFact, dataEnv,
  prefix = NULL, ext = "txt", sufixe = NULL, type = "espece"){

  ## Purpose: Définit les noms du fichiers pour les résultats des arbres
  ##          de régression multivariée.
  ##          L'extension et un prefixe peuvent êtres précisés,
  ##          mais par défaut, c'est le fichier de sorties texte qui est
  ##          créé.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : nom de la métrique analysée.
  ##            factAna : nom du facteur de séprataion des analyses.
  ##            modSel : modalité de factAna sélectionnée.
  ##            listFact : vecteur des noms de facteurs de l'analyse.
  ##            prefix : préfixe du nom de fichier.
  ##            sufixe : un sufixe pour le nom de fichier.
  ##            ext : extension du fichier.
  ##            type : type de modèle (traitement conditionnel).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 15:48

  # si pas de préfix fourni :
  if (is.null(prefix)){
    prefix <- "MRT"
  }else{}

  # Nom de fichier :
  filename <- paste(get("filePathes", envir = dataEnv)["results"], prefix, "_",
    # Métrique analysée :
    metrique, "_",
    # si facteur de séparation des analyses :
    mltext("resFileGraph.aggreg.abv"),
    switch(type,
      "espece" = mltext("resFileGraph.pfx.spSt"),
      "CL_espece" = mltext("resFileGraph.pfx.ScspSt"),
      "unitobs" = mltext("resFileGraph.pfx.St"),
      "CL_unitobs" = mltext("resFileGraph.pfx.SCSt"),
      ""),
    switch(type,
      "espece" = {
        ifelse(factAna == "",
          "",
          paste(factAna, "(", ifelse(modSel[1] != "", modSel,
            mltext("resFileGraph.all.spSt")), ")_", sep = ""))
      },
      "CL_espece" = {
        ifelse(factAna == "",
          "",
          paste(factAna, "(", ifelse(modSel[1] != "",
            paste(modSel, collapse = "+"),
            mltext("resFileGraph.all.ScspSt")), ")_", sep = ""))
      },
      "unitobs" = {
        ifelse(factAna == "",
          mltext("resFileGraph.allSp.St"),
          paste(factAna, "(", ifelse(modSel[1] != "",
            paste(modSel, collapse = "+"),
            mltext("resFileGraph.all.St")), ")_", sep = ""))
      },
      "CL_unitobs" = {
        ifelse(factAna == "",
          mltext("resFileGraph.allSp.SCSt"),
          paste(factAna, "(", ifelse(modSel[1] != "",
            paste(modSel, collapse = "+"),
            mltext("resFileGraph.all.SCSt")), ")_", sep = ""))
      },
      ""),
    # liste des facteurs de l'analyse
    paste(listFact, collapse = "-"),
    # sufixe :
    ifelse(is.null(sufixe), "", paste("_", sufixe, sep = "")),
    # Extension du fichier :
    ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl = TRUE), # nettoyage de l'extension si besoin.
    sep = "")

  # Ouverture de la connection (retourne l'objet de type 'connection') si pas un fichier avec
  # extension graphique, retourne le nom de fichier sinon :
  if (!is.element(gsub("^\\.([^.]*)", "\\1", ext[1], perl = TRUE),
      c("pdf", "PDF", "png", "PNG", "jpg", "JPG")))
  {
    return(resFile <- file(filename, open = "w"))
  }else{
    return(filename)
  }
}


print.rpart.ml <- function (x, minlength = 0, spaces = 2, cp, digits = getOption("digits"), ...){

  ## Purpose:Francisation de la fonction print.rpart() du package
  ##          "mvpart".
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de mvpart::print.rpart()
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 mai 2011, 15:17

  if (!inherits(x, "rpart"))
    stop("Not legitimate rpart object")
  if (!is.null(x$frame$splits))
    x <- rpconvert(x)
  if (!missing(cp))
    x <- prune.rpart(x, cp = cp)
  frame <- x$frame
  ylevel <- attr(x, "ylevels")
  node <- as.numeric(row.names(frame))
  depth <- rpart:::tree.depth(node)
  indent <- paste(rep(" ", spaces * 32), collapse = "")
  if (length(node) > 1){
    indent <- substring(indent, 1, spaces * seq(depth))
    indent <- paste(c("", indent[depth]), format(node), ")", sep = "")
  }else{
    indent <- paste(format(node), ")", sep = "")
  }

  tfun <- (x$functions)$print
  if (!is.null(tfun)) {
    if (is.null(frame$yval2))
      yval <- tfun(frame$yval, ylevel, digits)
    else yval <- tfun(frame$yval2, ylevel, digits)
  }
  else yval <- format(signif(frame$yval, digits = digits))
  term <- rep(" ", length(depth))
  term[frame$var == "<leaf>"] <- "*"
  # browser()
  z <- labels(x, digits = digits, minlength = minlength, ...)
  n <- frame$n
  z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)), yval, term, sep = "\t")
  omit <- x$na.action
  if (length(omit))
    cat("n=", n[1], " (", naprint(omit), ")\n\n", sep = "")
  else cat("n=", n[1], "\n\n")
  if (x$method == "class")
    cat(mltext("print.rpart.ml.node.header.prob"), "\n")
  else cat(mltext("print.rpart.ml.node.header"), "\n")
  cat("\t\t\t\t* ", mltext("print.rpart.ml.term.node"), "\n\n")
  cat(z, sep = "\n")
  return(invisible(x))
}


summary.rpart.ml <- function (object, cp = 0, digits = getOption("digits"), file, ...){

  ## Purpose: Francisation de la fonction summary.rpart() du package
  ##          "mvpart".
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de summary.rpart()
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 mai 2011, 13:55

  if (!inherits(object, "rpart")){
    stop("Not legitimate rpart object")
  }else{}

  if (!is.null(object$frame$splits)){
    x <- rpconvert(object)
  }else{
    x <- object
  }

  if (!missing(file)){
    sink(file)
    on.exit(sink())
  }else{}

  if (!is.null(x$call)){
    cat("Appel :\n")
    dput(x$call)
  }else{}

  omit <- x$na.action
  n <- x$frame$n
  if (length(omit)){
    cat("  n=", n[1], " (", naprint(omit), ")\n\n", sep = "")
  }else{
    cat("  n=", n[1], "\n\n")
  }

  print(x$cptable, digits = digits)
  ff <- x$frame
  ylevel <- attr(x, "ylevels")
  id <- as.integer(row.names(ff))
  parent.id <- ifelse(id == 1, 1, floor(id/2))
  parent.cp <- ff$complexity[match(parent.id, id)]
  rows <- (1:length(id))[parent.cp > cp]

  if (length(rows) > 0){
    rows <- rows[order(id[rows])]
  }else{
    rows <- 1
  }

  is.leaf <- (ff$var == "<leaf>")
  index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1 * (!is.leaf)))

  if (!all(is.leaf)){
    sname <- dimnames(x$splits)[[1]]
    cuts <- vector(mode = "character", length = nrow(x$splits))
    temp <- x$splits[, 2]
    for (i in 1:length(cuts)){
      if (temp[i] == -1){
        cuts[i] <- paste("<", format(signif(x$splits[i, 4], digits = digits)))
      }else{
        if (temp[i] == 1){
          cuts[i] <- paste("<", format(signif(x$splits[i, 4], digits = digits)))
        }else{
          cuts[i] <- paste(mltext("summary.rpart.ml.split"),
            paste(mltext(c("summary.rpart.ml.L",
              "summary.rpart.ml.dash",
              "summary.rpart.ml.R"))[x$csplit[x$splits[i, 4],
              1:temp[i]]],
              collapse = "", sep = ""),
            collapse = "")
        }
      }
    }

    if (any(temp < 2)){
      cuts[temp < 2] <- format(cuts[temp < 2], justify = "left")
    }else{}

    cuts <- paste(cuts, ifelse(temp >= 2,
      ",",
      ifelse(temp == 1L,
        mltext("summary.rpart.ml.2R"),
        mltext("summary.rpart.ml.2L"))),
      sep = "")
  }

  if (is.null(ff$yval2)){
    tprint <- x$functions$summary(ff$yval[rows], ff$dev[rows], ff$wt[rows], ylevel, digits)
  }else{
    tprint <- x$functions$summary(ff$yval2[rows, ], ff$dev[rows], ff$wt[rows], ylevel, digits)
  }

  for (ii in 1:length(rows)){
    i <- rows[ii]
    nn <- ff$n[i]
    twt <- ff$wt[i]
    cat("\n", mltext("summary.rpart.ml.nodeNum"), id[i], ": ",
      nn, " ", mltext("summary.rpart.ml.obsNum"),
      sep = "")
    if (ff$complexity[i] < cp || is.leaf[i])
    {
      cat("\n")
    }else{
      cat(",    ", mltext("summary.rpart.ml.complPar"), "=",
        format(signif(ff$complexity[i], digits)), "\n", sep = "")
    }

    cat(tprint[ii], "\n")
    if (ff$complexity[i] > cp && !is.leaf[i]){
      sons <- 2 * id[i] + c(0, 1)
      sons.n <- ff$n[match(sons, id)]
      cat("  ", mltext("summary.rpart.ml.leftSon"), "=",
        sons[1], " (", sons.n[1], " ", mltext("summary.rpart.ml.obsAbrev"), ")",
        " ", mltext("summary.rpart.ml.rightSon"), "=",
        sons[2], " (", sons.n[2], " ", mltext("summary.rpart.ml.obsAbrev"), ")",
        sep = "")
      j <- nn - (sons.n[1] + sons.n[2])
      if (j > 1){
        cat(", ", j, " ", mltext("summary.rpart.ml.remObs"), "\n", sep = "")
      }else{
        if (j == 1){
          cat(", ", mltext("summary.rpart.ml.1remObs"), "\n")
        }else{
          cat("\n")
        }
      }
      cat("  ", mltext("summary.rpart.ml.primSplit"), "\n")
      j <- seq(index[i], length = 1 + ff$ncompete[i])
      if (all(nchar(cuts[j]) < 25)){
        temp <- format(cuts[j], justify = "left")
      }else{
        temp <- cuts[j]
      }

      cat(paste("      ", format(sname[j], justify = "left"),
        " ", temp, " ", mltext("summary.rpart.ml.improve"), "=",
        format(signif(x$splits[j, 3], digits)), ", (", nn - x$splits[j, 1], " ",
        mltext("summary.rpart.ml.missing"),")",
        sep = ""), sep = "\n")

      if (ff$nsurrogate[i] > 0){
        cat("  ", mltext("summary.rpart.ml.surrogateSplit"), "\n")
        j <- seq(1 + index[i] + ff$ncompete[i], length = ff$nsurrogate[i])
        agree <- x$splits[j, 3]
        if (all(nchar(cuts[j]) < 25)){
          temp <- format(cuts[j], justify = "left")
        }else{
          temp <- cuts[j]
        }

        if (ncol(x$splits) == 5){
          adj <- x$splits[j, 5]
          cat(paste("      ", format(sname[j], justify = "left"),
            " ", temp, " agree=", format(round(agree, 3)),
            ", adj=", format(round(adj, 3)), ", (",
            x$splits[j, 1], " split)", sep = ""), sep = "\n")
        }else{
          cat(paste("      ", format(sname[j], justify = "left"),
            " ", temp, " agree=", format(round(agree, 3)),
            ", (", x$splits[j, 1], " split)",
            sep = ""), sep = "\n")
        }
      }
    }
  }

  cat("\n")
  invisible(x)
}


#' @title Arbre de régression multivariée (données agrégées par unités d'observation)
#'
#' @description Cette fonction permet de faire des arbres de régression multivariée pour des données
#' agrégées par unités d'observation
#'
#' @param metrique chr, métrique choisie
#' @param factGraph chr, facteur de sélection des espèces
#' @param factGraphSel chr, sélection de modalités pour le facteur de sélection des espèces
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalité sélectionnées pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de métriques
#' @param dataEnv environnement de stockage des données
#' @param baseEnv environnement parent
#'
#'@return none

WP2MRT.unitobs.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv = dataEnv, baseEnv = .GlobalEnv){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factGraph : le facteur de sélection des espèces.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 mai 2011, 14:24

#  pampaProfilingStart.f()

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leurs listes de modalités sélectionnées

  # Données pour la série de MRT :
  if (tableMetrique == "unit"){
    # Pour les indices de biodiversité, il faut travailler sur les nombres... :
    tmpData <- subsetToutesTables.f(metrique = getOption("P.nbName"), facteurs = facteurs,
      selections = selections, dataEnv = dataEnv, tableMetrique = "unitSp",
      exclude = NULL, add = c("observation.unit", "species.code"))
  }else{
    # ...sinon sur la métrique choisie :
    tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs,
      selections = selections, dataEnv = dataEnv, tableMetrique = tableMetrique,
      exclude = NULL, add = c("observation.unit", "species.code"))
  }

  # Formule du MRT :
  exprMRT <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))

  # Identification des différents modalités (espèces) du graphique à générer :
  if (factGraph == ""){               # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){      # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                      # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }

  # Agrégation des observations / unité d'observation :
  if (tableMetrique == "unitSpSz" && factGraph != "size.class"){
    tmpData <- na.omit(
      agregationTableParCritere.f(Data = tmpData,
        metrique = metrique,
        facteurs = c("observation.unit", "size.class"),
        dataEnv = dataEnv,
        listFact = listFact))
  }else{
    if (tableMetrique == "unit"){
      # Calcul des indices de biodiversité sur sélection d'espèces :
      tmp <- do.call(rbind,
        lapply(getOption("P.MPA"),
          function(MPA){
            calcBiodiv.f(Data = tmpData,
              refesp = get("refesp", envir = dataEnv),
              MPA = MPA,
              unitobs = "observation.unit", code.especes = "species.code",
              nombres = getOption("P.nbName"),
              indices = metrique,
              dataEnv = dataEnv)
          }))

      # On rajoute les anciennes colonnes :
      tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "number" désormais inutile.
        tmpData[match(tmp$observation.unit, tmpData$observation.unit),
          !is.element(colnames(tmpData),
            c(colnames(tmp), getOption("P.nbName"), "species.code")), drop = FALSE])
    }else{
      tmpData <- na.omit(
        agregationTableParCritere.f(Data = tmpData,
          metrique = metrique,
          facteurs = c("observation.unit"),
          dataEnv = dataEnv,
          listFact = listFact))
    }
  }

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques/analyses) :
  DataBackup <<- list(tmpData)

  # Suppression des 'levels' non utilisés :
  tmpData <- dropLevels.f(tmpData)

  if (new_window){
    # Ouverture et configuration du périphérique graphique :
    graphFile <- openDevice.f(noGraph = 1,
      metrique = metrique,
      factGraph = factGraph,
      modSel = iFactGraphSel,
      listFact = listFact,
      dataEnv = dataEnv,
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs"),
      typeGraph = "MRT",
      large = TRUE)
  } else{
    graphFile <- resFileGraph.f(
      metrique = metrique,
      factGraph = factGraph,
      modSel = iFactGraphSel,
      listFact = listFact,
      dataEnv = dataEnv,
      ext = "wmf",
      prefix = "MRT",
      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
        "%03d",
        ""),
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs"))
  }

  par(mar = c(1.5, 7, 7, 7), mgp = c(3.5, 1, 0)) # paramètres graphiques.

  # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
  mainTitle <- graphTitle.f(metrique = metrique,
    modGraphSel = iFactGraphSel,
    factGraph = factGraph,
    listFact = listFact,
    type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
      "CL_unitobs",
      ifelse(tableMetrique == "unitSpSz",
        "unitobs(CL)",
        "unitobs")),
    model = mltext("WP2MRT.esp.graphTitle.pfx", language = getOption("P.lang")))

  tmpMRT <- rpart:::rpart(exprMRT, data = tmpData)

  rpart:::plot.rpart(tmpMRT, main = mainTitle, margin = 0.00)
  par(xpd = NA)
  rpart:::text.rpart(tmpMRT, use.n = TRUE, pretty = 1, all = TRUE, xpd = NA,
    fancy = TRUE, adj = c(0.5, 0.75))

  # Écriture des résultats formatés dans un fichier :
  tryCatch(
    sortiesMRT.f(objMRT = tmpMRT, formule = exprMRT,
      metrique = metrique,
      factAna = factGraph, modSel = iFactGraphSel,
      listFact = listFact, listFactSel = listFactSel,
      Data = tmpData, dataEnv = dataEnv,
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs"),
      baseEnv = baseEnv),
    error = function(e){
      errorLog.f()
      print("error")
    }
  )


  # On ferme les périphériques PDF :
  if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG"))){
    dev.off()

    # Inclusion des fontes dans le pdf si souhaité :
    if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
      tryCatch(embedFonts(file = graphFile),
        error = function(e){
          warning(mltext("WP2boxplot.W.pdfFonts"))
        })
    }else{}
  }else{
    if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
      # Sauvegarde en wmf si pertinent et souhaité :
      savePlot(graphFile, type = "wmf", device = dev.cur())
    }else{}
  }

#  pampaProfilingEnd.f()
}


# agregationTableParCritere.f <- function(Data, metrique, facteurs, dataEnv, listFact = NULL,
#   nbName = "number"){
#
#   ## Purpose: Agréger les données selon un ou plusieurs facteurs.
#   ## ----------------------------------------------------------------------
#   ## Arguments: Data : Le jeu de données à agréger.
#   ##            metrique : la métrique agrégée.
#   ##            facteurs : les facteurs
#   ##            listFact : noms des facteurs supplémentaires (agrégés et
#   ##                       ajoutés à la table de sortie).
#   ##            dataEnv : l'environnement des données.
#   ##            nbName : nom de la colonne nombre.
#   ##
#   ## Output: une data.frame agrégée.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47
#
#   # Récupération des données
#
#   # Informations (l'étape peut être longue) :
# #  WinInfo <- agregation.info.f()
#
#   # traitements selon le type de métrique :
#   casMetrique <- c("number" = "sum",
#     "mean.length" = "w.mean",
#     "taille_moy" = "w.mean",
#     "biomass" = "sum",
#     "Biomass" = "sum",
#     "weight" = "sum",
#     "mean.weight" = "w.mean",
#     "density" = "sum",
#     "Density" = "sum",
#     "CPUE" = "sum",
#     "CPUE.biomass" = "sum",
#     "pres.abs" = "pres",
#     "abundance.prop.SC" = "w.mean.prop", # Pas bon [!!!]
#     "biomass.prop.SC" = "w.mean.prop.bio",  # Pas bon [!!!]
#     # Benthos :
#     "colonies" = "sum",
#     "coverage" = "sum",
#     "mean.size.colonies" = "w.mean.colonies",
#     # SVR (expérimental) :
#     "number.max" = "nbMax",
#     "number.sd" = "nbSD",
#     "density.max" = "densMax",
#     "density.sd" = "densSD",
#     "biomass.max" = "sum",
#     "spawning.success" = "%.nesting",
#     "spawnings" = "sum",
#     "readable.tracks" = "sum",
#     "tracks.number" = "sum")
#
#
#   # Ajout de "readable.tracks" pour le pourcentage de ponte :
#   if (any(casMetrique[metrique] == "%.nesting")){
#     if (is.element("size.class", colnames(Data))){
#       unitSpSz <- get("unitSpSz", envir = dataEnv)
#
#       if (is.null(unitSpSz)) stop("unitSpSz must be defined")
#
#       Data <- merge(Data,
#         unitSpSz[ , c("species.code", "observation.unit", "size.class", "readable.tracks")],
#         by = c("species.code", "observation.unit", "size.class"),
#         suffixes = c("", ".y"))
#     }else{
#       unitSp <- get("unitSp", envir = dataEnv)
#
#       if (is.null(unitSp)) stop("unitSp must be defined")
#
#       Data <- merge(Data,
#         unitSp[ , c("species.code", "observation.unit", "readable.tracks")],
#         by = c("species.code", "observation.unit"),
#         suffixes = c("", ".y"))
#     }
#   }else{}
#
#   # Ajout du champ nombre pour le calcul des moyennes pondérées s'il est absent :
#   if ((casMetrique[metrique] == "w.mean" || casMetrique[metrique] == "w.mean.prop")){
#     if (is.element("size.class", colnames(Data))){
#       unitSpSz <- get("unitSpSz", envir = dataEnv)
#
#       Data <- merge(Data,
#         unitSpSz[ , c("species.code", "observation.unit", "size.class", nbName)],
#         by = c("species.code", "observation.unit", "size.class"))
#
#       # Ajout de l'abondance totale /espèce/unité d'observation :
#       nbTot <- tapply(unitSpSz[ , nbName],
#         as.list(unitSpSz[ , c("species.code", "observation.unit")]),
#         sum, na.rm = TRUE)
#
#       Data <- merge(Data,
#         as.data.frame(as.table(nbTot), responseName = "nombre.tot"))
#     }else{
#       Data <- merge(Data,
#         get("unitSp", envir = dataEnv)[ , c("species.code", "observation.unit", nbName)],
#         by = c("species.code", "observation.unit"))
#     }
#   }else{}
#
#   # Ajout du champ biomasse pour les proportions de biomasses par classe de taille :
#   if (casMetrique[metrique] == "w.mean.prop.bio"){
#     unitSpSz <- get("unitSpSz", envir = dataEnv)
#
#     biomass <- colnames(unitSpSz)[is.element(colnames(unitSpSz), c("biomass", "CPUE.biomass"))][1]
#
#     Data <- merge(Data,
#       unitSpSz[ , c("species.code", "observation.unit", "size.class", biomass)],
#       by = c("species.code", "observation.unit", "size.class"))
#
#     # Ajout de la biomasse totale /espèce/unité d'observation :
#     biomTot <- tapply(unitSpSz[ , biomass],
#       as.list(unitSpSz[ , c("species.code", "observation.unit")]),
#       function(x){
#         ifelse(all(is.na(x)),
#           NA,
#           sum(x, na.rm = TRUE))
#     })
#
#     Data <- merge(Data, as.data.frame(as.table(biomTot), responseName = "tot.biomass"))
#   }
#
#   # Ajout du champ colonie pour le calcul des moyennes pondérées s'il est absent :
#   if (casMetrique[metrique] == "w.mean.colonies" && ! is.element("colonies", colnames(Data))){
#     unitSp <- get("unitSp", envir = dataEnv)
#
#     Data$colonies <- unitSp$colonies[match(
#       apply(Data[ , c("species.code", "observation.unit")], 1, paste, collapse = "*"),
#       apply(unitSp[ , c("species.code", "observation.unit")], 1, paste, collapse = "*"))]
#   }else{}
#
#   # Agrégation de la métrique selon les facteurs :
#   switch(casMetrique[metrique],
#     "sum" = {
#       res <- tapply(Data[ , metrique],
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(x){
#           ifelse(all(is.na(x)),
#             NA,
#             sum(x, na.rm = TRUE))
#       })
#     },
#     "w.mean" = {
#       res <- tapply(1:nrow(Data),
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(ii){
#           ifelse(all(is.na(Data[ii, metrique])),
#             NA,
#             weighted.mean(Data[ii, metrique], Data[ii, nbName], na.rm = TRUE))
#       })
#     },
#     "w.mean.colonies" = {
#       res <- tapply(1:nrow(Data),
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(ii){
#           ifelse(all(is.na(Data[ii, metrique])),
#             NA,
#             weighted.mean(Data[ii, metrique],
#               Data[ii, "colonies"],
#               na.rm = TRUE))
#       })
#     },
#     "w.mean.prop" = {
#       res <- tapply(1:nrow(Data),
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(ii){
#           ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "nombre.tot"], na.rm = TRUE) == 0,
#             NA,
#             ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
#               0,
#               (sum(Data[ii, nbName][ !is.na(Data[ii, metrique])], na.rm = TRUE) /
#                 sum(Data[ii, "nombre.tot"], na.rm = TRUE)) *
#                 # Correction si la classe de taille n'est pas un facteur d'agrégation
#                 # (sinon valeur divisée par le nombre de classes présentes) :
#                 ifelse(is.element("size.class", facteurs),
#                   100,
#                   100 * length(unique(Data$size.class)))))
#       })
#     },
#     "w.mean.prop.bio" = {
#       res <- tapply(1:nrow(Data),
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(ii){
#           ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "tot.biomass"], na.rm = TRUE) == 0,
#             NA,
#             ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
#               0,
#               (sum(Data[ii, biomass][ !is.na(Data[ii, metrique])], na.rm = TRUE) /
#                 sum(Data[ii, "tot.biomass"], na.rm = TRUE)) *
#                 # Correction si la classe de taille n'est pas un facteur d'agrégation
#                 # (sinon valeur divisée par le nombre de classes présentes) :
#                 ifelse(is.element("size.class", facteurs),
#                   100,
#                   100 * length(unique(Data$size.class)))))
#       })
#
#     },
#     "pres" = {
#       res <- tapply(Data[ , metrique],
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(x){
#           ifelse(all(is.na(x)), # Cas où il n'y a que des NAs.
#             NA,
#             ifelse(any(x > 0, na.rm = TRUE), # Sinon...
#               1,                           # ...présence si au moins une observation dans le groupe.
#               0))
#                          })
#     },
#     "nbMax" = {
#       # Récupération des nombres brutes avec sélections :
#       nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)
#
#       # Somme par croisement de facteur / rotation :
#       nbTmp2 <- apply(nbTmp,
#         which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
#         function(x){
#           ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
#         })
#
#       # Somme par croisement de facteur :
#       res <- as.array(apply(nbTmp2,
#         which(is.element(names(dimnames(nbTmp)), facteurs)),
#         function(x){
#           ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
#         }))
#     },
#     "nbSD" = {
#       # Récupération des nombres brutes avec sélections :
#       nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)
#
#       # Somme par croisement de facteur / rotation :
#       nbTmp2 <- apply(nbTmp,
#         which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
#         function(x){
#           ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
#         })
#
#       # Somme par croisement de facteur :
#       res <- as.array(apply(nbTmp2,
#         which(is.element(names(dimnames(nbTmp)), facteurs)),
#         function(x){
#           ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
#         }))
#     },
#     "densMax" = {
#       # Récupération des nombres brutes avec sélections :
#       densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)
#
#       # Somme par croisement de facteur / rotation :
#       densTmp2 <- apply(densTmp,
#         which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
#         function(x){
#           ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
#         })
#
#       # Somme par croisement de facteur :
#       res <- as.array(apply(densTmp2,
#         which(is.element(names(dimnames(densTmp)), facteurs)),
#         function(x){
#           ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
#         }))
#     },
#     "densSD" = {
#       # Récupération des nombres brutes avec sélections :
#       densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)
#
#       # Somme par croisement de facteur / rotation :
#       densTmp2 <- apply(densTmp,
#         which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
#         function(x){
#           ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
#         })
#
#       # Somme par croisement de facteur :
#       res <- as.array(apply(densTmp2,
#         which(is.element(names(dimnames(densTmp)), facteurs)),
#         function(x){
#           ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
#         }))
#     },
#     "%.nesting" = {
#       res <- tapply(1:nrow(Data),
#         as.list(Data[ , facteurs, drop = FALSE]),
#         function(ii){
#           ifelse(all(is.na(Data[ii, metrique])),
#             NA,
#             weighted.mean(Data[ii, metrique],
#               Data[ii, "readable.tracks"],
#               na.rm = TRUE))
#         })
#     },
#     stop("Operation not implemented!")
#   )
#
#   # Nom des dimensions
#   names(dimnames(res)) <- c(facteurs)
#
#   # Transformation vers format long :
#   reslong <- as.data.frame(as.table(res), responseName = metrique)
#   reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.
#
#   # Agrégation et ajout des facteurs supplémentaires :
#   if (!is.null(listFact)){
#     reslong <- cbind(reslong, sapply(Data[ , listFact, drop = FALSE],
#       function(fact){
#         tapply(fact, as.list(Data[ , facteurs, drop = FALSE]),
#           function(x){
#             if (length(x) > 1 && length(unique(x)) > 1){ # On doit n'avoir qu'une seule modalité...
#               return(NULL)                               # ...sinon on retourne NULL
#             }else{
#               unique(as.character(x))
#             }
#           })
#       }))
#   }else{}
#
#   # Si certains facteurs ne sont pas de classe facteur, il faut les remettre dans leur classe d'origine :
#   if (any(tmp <- sapply(reslong[ , listFact, drop = FALSE], class)
#       != sapply(Data[ , listFact, drop = FALSE], class)))
#     {
#     for (i in which(tmp)){
#       switch(sapply(Data[ , listFact, drop = FALSE], class)[i],
#         "integer" = {
#           reslong[ , listFact[i]] <- as.integer(as.character(reslong[ , listFact[i]]))
#         },
#         "numeric" = {
#           reslong[ , listFact[i]] <- as.numeric(as.character(reslong[ , listFact[i]]))
#         },
#         reslong[ , listFact[i]] <- eval(call(paste(
#           "as", sapply(Data[ , listFact, drop = FALSE], class)[i], sep = "."),
#           reslong[ , listFact[i]]))
#       )
#     }
#   }else{}
#
#   # Rétablir l'ordre initial des nivaux de facteurs :
#   reslong <- as.data.frame(sapply(colnames(reslong),
#     function(x){
#       if (is.factor(reslong[ , x])){
#         return(factor(reslong[ , x], levels = levels(Data[ , x])))
#       }else{
#         return(reslong[ , x])
#       }
#     }, simplify = FALSE))
#
#
#   # Fermeture de la fenêtre d'information
# #  close.info.f(WinInfo)
#
#   # Vérification des facteurs supplémentaires agrégés.
#   # Il ne doit pas y avoir d'élément nul (la fonction précédente renvoie NULL si plusieurs
#   # niveaux de facteurs, i.e. le facteur est un sous ensemble d'un des facteurs d'agrégation
#   # des observations) :
#   if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))}))){
#     warning(paste(mltext("agregationTableParCritere.f.W.1"),
#       mltext("agregationTableParCritere.f.W.2"), sep = ""))
#     return(NULL)
#   }else{
#     return(reslong)
#   }
# }
#
#
# getReducedSVRdata.f <- function(dataName, data, dataEnv){
#
#   ## Purpose: Récupérer des données brutes SVR (nombres, densités) réduites
#   ##  aux sélections.
#   ## ---------------------------------------------------------------------------
#   ## Arguments: dataName : nom de tableau à récupérer.
#   ##            data : données associées (avec sélections).
#   ##            dataEnv : environnement des données.
#   ## ---------------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 31 oct. 2012, 12:21
#
#   res <- get(dataName, envir = dataEnv)
#
#   # Limitations au classes de tailles, espèces et unité d'observations sélectionnées :
#   if (is.element("species.code", colnames(data)) &&
#       is.element("species.code", names(dimnames(res)))){
#
#     species <- dimnames(res)[["species.code"]]
#     res <- extract(res,
#       indices = list(species[is.element(species, data[ , "species.code"])]),
#       dims = which(is.element(names(dimnames(res)), "species.code")))
#
#   }else{}
#
#   if (is.element("observation.unit", colnames(data)) &&
#       is.element("observation.unit", names(dimnames(res)))){
#
#     unitObs <- dimnames(res)[["observation.unit"]]
#     res <- extract(res,
#       indices = list(unitObs[is.element(unitObs, data[ , "observation.unit"])]),
#       dims = which(is.element(names(dimnames(res)), "observation.unit")))
#
#   }else{}
#
#   if (is.element("size.class", colnames(data)) &&
#       is.element("size.class", names(dimnames(res)))){
#
#     CL <- dimnames(res)[["size.class"]]
#     res <- extract(res,
#       indices = list(CL[is.element(CL, data[ , "size.class"])]),
#       dims = which(is.element(names(dimnames(res)), "size.class")))
#
#   }else{}
#
#   return(res)
# }
#
#
# calcBiodiv.f <- function(Data, refesp, MPA, unitobs = "observation.unit",
#   code.especes = "species.code", nombres = "number",
#   indices = "all", global = FALSE, printInfo = FALSE, dataEnv = .GlobalEnv){
#
#   ## Purpose: calcul des indices de biodiversité
#   ## ----------------------------------------------------------------------
#   ## Arguments: Data : les données à partir desquelles calculer les
#   ##                   indices. Doivent comporter au minimum (colones) :
#   ##                     * unités d'observations/sites
#   ##                     * espèces présentes
#   ##                     * nombre d'individus /espèce/unitobs.
#   ##            refesp : le référentiel espèces.
#   ##            MPA : l'AMP (chaîne de charactères).
#   ##            unitobs : nom de la colone d'unités d'observation.
#   ##            code.especes : nom de la colone d'espèces.
#   ##            nombres : nom de la colone de nombres.
#   ##            indices : liste des indices à calculer
#   ##                      (vecteur de caractères)
#   ##            global : est-ce que les résultats doivent être exportés
#   ##                     globalement (booléen).
#   ##            printInfo : affichage des infos (chargement) ? (booléen).
#   ##            dataEnv : environnement des données
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 29 oct. 2010, 08:58
#
#   # Unitobs appartenant a l'AMP courante:
#   unitobsData <- get("unitobs", envir = dataEnv)
#
#   Data <- subset(Data,
#     is.element(Data[ , unitobs],
#       unitobsData[unitobsData[ , getOption("P.MPAfield")] == MPA ,
#         unitobs]))
#
#   DataTmp <- Data
#
#   # Supression de tout ce qui n'a pas d'espèce précisee
#   # (peut être du non biotique ou identification >= genre) :
#   if (! nrow(Data <- Data[(
#     spTmp <- refesp$species[match(Data[ , code.especes], refesp$species.code)])
#     != "sp." & !is.na(spTmp), ])){
#     return(Data)
#
#     if (printInfo){
# #      infoLoading.f(msg = paste(mltext("calcBiodiv.f.info.1")# ,
# #        # "\n   La table de contingence n'a pas été calculée."
# #      ), icon = "warning")
#       print(mltext("calcBiodiv.f.info.1"))
#     }else{}
#
#   }else{}
#
#   # Suppression des niveaux de facteur inutilisés :
#   Data <- dropLevels.f(df = Data)
#
#   if (printInfo){
#     if (nlevels(DataTmp[ , code.especes]) > nlevels(Data[ , code.especes])){
#       nsup <- nlevels(DataTmp[ , code.especes]) - nlevels(Data[ , code.especes])
# #      infoLoading.f(msg = paste(
# #        nsup, " \"species.code\" ",
# #        ifelse(nsup > 1 ,
# #          mltext("calcBiodiv.f.info.2.p"),
# #          mltext("calcBiodiv.f.info.2.s")),
# #        mltext("calcBiodiv.f.info.3"),
# #        ifelse(nsup > 1,
# #          mltext("calcBiodiv.f.info.4.p"),
# #          mltext("calcBiodiv.f.info.4.s")),
# #        mltext("calcBiodiv.f.info.5"),
# #        sep = ""))
#
#       print(paste(
#         nsup, " \"species.code\" ",
#         ifelse(nsup > 1 ,
#           mltext("calcBiodiv.f.info.2.p"),
#           mltext("calcBiodiv.f.info.2.s")),
#         mltext("calcBiodiv.f.info.3"),
#         ifelse(nsup > 1,
#           mltext("calcBiodiv.f.info.4.p"),
#           mltext("calcBiodiv.f.info.4.s")),
#         mltext("calcBiodiv.f.info.5"),
#         sep = ""))
#
#     }else{}
#   }else{}
#
#   # Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
#   if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes])))){
#     Data <- agregations.generic.f(Data = Data, metrics = nombres,
#       factors = c(unitobs, code.especes),
#       listFact = NULL, dataEnv = dataEnv)
#   }else{}
#
#   df.biodiv <- as.data.frame(as.table(tapply(Data[ , nombres],
#     Data[ , unitobs],
#     sum, na.rm = TRUE)))
#
#   colnames(df.biodiv) <- c(unitobs, nombres)
#
#   # ##################################################
#   # Richesse spécifique :
#   Data$pres.abs <- presAbs.f(nombres = Data[ , nombres], logical = FALSE)
#
#   df.biodiv$species.richness <- as.vector(tapply(
#     Data$pres.abs, Data[ , unitobs], sum, na.rm = TRUE), "integer")
#   # ... as.vector to avoid the class "array".
#
#   # richesses specifiques relatives :
#
#   # Phylum(s) présent(s) dans le jeux de données :
#   phylums <- as.character(unique(na.omit(refesp$phylum[match(Data[ , code.especes],
#     refesp$species.code)])))
#
#   # RS relative par rapp. au nombre d'espèces du site :
#   if (any(is.element(c("all", "relative.SR.site"), indices))){
#     if (getOption("P.refesp.Coefs") == "new"){
#       # Nouveau référentiel espèce ET fichier local chargé :
#       if (is.element("observed", colnames(refesp))){
#         df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
#           nrow(subset(refesp,
#             is.element(observed, c("oui", "O"))))) * 100
#       }else{}
#     }else{
#       df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
#         nrow(subset(refesp,
#           is.element(eval(parse(text = paste("Obs", MPA, sep = ""))),
#             c("oui", "O"))))) * 100
#     }
#   }
#
#   # RS relative par rapp. au nombre d'espèces du site et
#   # du(des) phylum(s) concerné(s) (jeu de données) :
#   if (any(is.element(c("all", "relative.SR.site.phylum"), indices))){
#     if (getOption("P.refesp.Coefs") == "new"){
#       # Nouveau référentiel espèce ET fichier local chargé :
#       if (is.element("observed", colnames(refesp))){
#         df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
#           nrow(subset(refesp,
#             is.element(observed, c("oui", "O", "yes", "Y")) &
#               is.element(phylum, phylums)))) * 100 # [ml?]
#       }else{}
#     }else{
#       df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
#         nrow(subset(refesp,
#           is.element(eval(parse(text = paste("Obs", MPA, sep = ""))),
#             c("oui", "O", "yes", "Y")) &
#           is.element(phylum, phylums)))) * 100
#     }
#   }
#
#   # RS relative par rapp. au nombre d'espèces des données :
#   if (any(is.element(c("all", "relative.SR.data"), indices))){
#     df.biodiv$relative.SR.data <- (df.biodiv$species.richness /
#       nrow(subset(refesp,
#         is.element(species.code, Data[ , code.especes])))) * 100
#   }
#
#   # # RS relative par rapp. au nombre d'espèces des données + des phyla présents :
#   # Inutile : "RS.relative.donnees" est par définition limitée au phyla présents !
#
#   # RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) :
#   if (any(is.element(c("all", "relative.SR.region"), indices))){
#     df.biodiv$relative.SR.region <- (df.biodiv$species.richness /
#       nrow(refesp)) * 100
#   }
#
#   # RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) et
#   # du(des) phylum(s) concerné(s) (jeu de données) :
#   if (any(is.element(c("all", "relative.SR.region.phylum"), indices))){
#     df.biodiv$relative.SR.region.phylum <- (df.biodiv$species.richness /
#       nrow(subset(refesp, is.element(phylum, phylums)))) * 100
#   }
#
#   # ##################################################
#   # Indices de Simpson et Shannon et dérivés :
#
#   matNombres <- tapply(Data[ , nombres], # Matrice de nombres d'individus /espèce/unitobs.
#     list(Data[ , unitobs], Data[ , code.especes]),
#     sum, na.rm = TRUE)
#
#   matNombres[is.na(matNombres)] <- 0  # Vrais zéros
#
#   # Proportion d'individus de chaque espèce dans l'unitobs :
#   propIndiv <- sweep(matNombres, 1,                           #
#     apply(matNombres, 1, sum, na.rm = TRUE), # Nombre d'individus / unitobs ; équiv df.biodiv$nombre.
#     FUN = "/")
#
#   # Indices de Simpson :
#   df.biodiv$simpson <- apply(propIndiv^2, 1, sum, na.rm = TRUE)
#
#   if (any(is.element(c("all", "simpson.l"), indices))){
#     df.biodiv$simpson.l <- 1 - df.biodiv$simpson
#   }
#
#   # calcul de l'indice de Shannon :
#   df.biodiv$shannon <- -1 * apply(propIndiv * log(propIndiv), 1, sum, na.rm = TRUE)
#
#   # calcul de l'indice de Pielou :
#   if (any(is.element(c("all", "pielou"), indices))){
#     df.biodiv$pielou <- df.biodiv$shannon / log(df.biodiv$species.richness)
#   }
#
#   # calcul de l'indice de Hill :
#   if (any(is.element(c("all", "hill"), indices))){
#     df.biodiv$hill <- (1 - df.biodiv$simpson) / exp(df.biodiv$shannon)
#     # équiv df.biodiv$l.simpson / exp(df.biodiv$shannon)
#   }
#
#   # suppression de l'indice de shannon (non pertinent)
#   df.biodiv$shannon <- NULL
#
#   # ##################################################
#   # Indices de biodiversité taxonomique :
#   df.biodivTaxo <- calcBiodivTaxo.f(Data = Data,
#     refesp = refesp,
#     unitobs = unitobs, code.especes = code.especes, nombres = nombres,
#     global = global, printInfo = printInfo,
#     indices = indices,
#     dataEnv = dataEnv)
#
#   if (!is.null(dim(df.biodivTaxo))){
#     df.biodiv <- cbind(df.biodiv,
#       df.biodivTaxo[match(df.biodiv[ ,unitobs], row.names(df.biodivTaxo)), , drop = FALSE])
#   }else{}
#
#   for (ind in c("simpson", "shannon", "species.richness")){
#     if (! any(is.element(c(ind, "all"), indices))){
#       df.biodiv[ , ind] <- NULL
#     }else{}
#   }
#
#   # # On retablit les niveaux de facteurs:
#   # colFact <- colnames(df.biodiv)[is.element(sapply(df.biodiv, class), "factor")]
#
#   # for (col in colFact)
#   # {
#   #     levels(df.biodiv[ , colFact]) <- levels(DataTmp[ , colFact])
#   # }
#
#   return(df.biodiv)
# }
#
#
# presAbs.f <- function(nombres, logical = FALSE){
#
#   ## Purpose: Renvoie les présences/absences d'après les nombres.
#   ## ----------------------------------------------------------------------
#   ## Arguments: nombres : vecteur de nombre d'individus.
#   ##            logical : faut-il renvoyer les résultats sous forme de
#   ##                      booléens, ou 0/1 (booléen).
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 29 oct. 2010, 10:20
#
#   if (any(nombres < 0, na.rm = TRUE)){
#     stop("Negative abundances!")
#   }else{}
#
#   if (logical){
#     return(nombres > 0)
#   }else{
#     nombres[nombres > 0] <- 1
#     return(nombres)
#   }
# }
#
#
# calcBiodivTaxo.f <- function(Data, refesp, unitobs = "observation.unit",
#   code.especes = "species.code", nombres = "number",
#   global = FALSE, printInfo = FALSE,
#   indices = "all", dataEnv = .GlobalEnv){
#
#   ## Purpose: Calcul des indices de biodiversité basés sur la taxonomie.
#   ## ----------------------------------------------------------------------
#   ## Arguments: Data : les données à partir desquelles calculer les
#   ##                   indices. Doivent comporter au minimum (colones) :
#   ##                     * unités d'observations/sites
#   ##                     * espèces présentes
#   ##                     * nombre d'individus /espèce/unitobs.
#   ##            refesp : référentiel espèces.
#   ##            unitobs : nom de la colone d'unités d'observation.
#   ##            especes : nom de la colone d'espèces.
#   ##            nombres : nom de la colone de nombres.
#   ##            global : est-ce que les résultats doivent être exportés
#   ##                     globalement (booléen).
#   ##            printInfo : affichage des infos ? (booléen).
#   ##            indices : liste des indices à calculer
#   ##                      (vecteur de caractères), tous par défaut.
#   ##            dataEnv : environnement des données
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 29 oct. 2010, 14:30
#
#   # Indices proposés :
#   proposed.indices <- c("D" = "Delta",
#     "Dstar" = "DeltaStar",
#     "Lambda" = "LambdaPlus",
#     "Dplus" = "DeltaPlus",
#     "SDplus" = "SDeltaPlus")
#
#   # On sort de la fonction si elle n'a pas d'intéret :
#   if (! any(is.element(c(proposed.indices, "all"), indices))){
#     return(NULL)                    # Rien !
#   }else{
#     # Suppression de tout ce qui n'a pas de genre (peut être du non biotique) :
#     Data <- Data[refesp$species[match(Data$species.code, refesp$species.code)] != "sp.", ]
#
#     # Suppression des niveaux de facteur inutilisés :
#     Data <- dropLevels.f(df = Data)
#
#     # Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
#     if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes])))){
#       Data <- agregations.generic.f(Data = Data, metrics = nombres,
#         factors = c(unitobs, code.especes),
#         listFact = NULL, dataEnv = dataEnv)
#     }else{}
#
#     # Table de contingence unitobs-espèces :
#     contingence <- tapply(Data[ , nombres],
#       list(Data[ , unitobs], Data[ , code.especes]),
#       sum, na.rm = TRUE)
#
#     contingence[is.na(contingence)] <- 0 # Vrais zéros.
#
#     # tableau avec genre, famille, etc.
#     sp.taxon <- dropLevels.f(refesp[match(colnames(contingence),
#       refesp$species.code, nomatch = NA, incomparables = FALSE),
#       c("species", "genus", "family", "order", "class", "phylum")])
#
#     # colnames(sp.taxon) <- c("genre", "famille", "ordre", "classe", "phylum")
#     rownames(sp.taxon) <- colnames(contingence)
#
#     # retrait des lignes ayant un niveau taxonomique manquant dans sp.taxon et dans contingence (en colonnes) :
#     manque.taxon <- apply(sp.taxon, 1, function(x){any(is.na(x))})
#     sp.taxon <- sp.taxon[! manque.taxon, , drop = FALSE]
#     contingence <- contingence[, ! manque.taxon, drop = FALSE]
#
#
#     # Calcul des indices (librairie "vegan") :
#     if (sum(sapply(sp.taxon, function(x)length(unique(x))) > 1) > 2){ # typiquement : une seule famille ou même genre.
#       # Indices retenus :
#       if (is.element("all", indices)){
#         retained.indices <- proposed.indices
#       }else{
#         retained.indices <- proposed.indices[is.element(proposed.indices, indices)]
#       }
#
#       # calcul des distances taxonomiques entre les especes
#       if (!is.null(taxdis <- tryCatch(taxa2dist(sp.taxon, varstep = TRUE, check = TRUE),
#         error = function(e){
#           # errorLog.f(error = e, niv = -3)
#           return(NULL)
#         }))){
#         # Function finds indices of taxonomic diversity and distinctiness,
#         # which are averaged taxonomic distances among
#         # species or individuals in the community...
#         divTaxo <- taxondive(contingence, taxdis)
#
#         # mise de divTaxo sous forme de data.frame :
#         df.biodivTaxo <- as.data.frame(divTaxo[names(retained.indices)])
#
#         colnames(df.biodivTaxo) <- retained.indices # [!!!] "LambdaPlus" ? vraiment ? [???]
#
#       }else{
#         divTaxo <- NULL
#         df.biodivTaxo <- NULL
#       }
#
#       # Résultats :
#       if (global){
#         # Création des objets dans l'environnement global
#         assign("div", divTaxo, envir = .GlobalEnv)
#         assign("taxdis", taxdis, envir = .GlobalEnv)
#         assign("ind_div", df.biodivTaxo, envir = .GlobalEnv)
#       }else{
#         return(df.biodivTaxo)
#       }
#     }else{                              # nombre de genre < 2.
#       switch(sum(sapply(sp.taxon, function(x)length(unique(x))) > 1),
#         "1" = {
#           warning(mltext("calcBiodivTaxo.f.Warn.1"))
#         },
#         "0" = {
#           warning(mltext("calcBiodivTaxo.f.Warn.2"))
#         })
#     }
#   }
# }
