#' Fonction pour tracer un barplot.
#'
#' \code{agregation} peut prendre la valeur 'espece' ou 'unitobs'. Si 'espece', l'agregation se fera
#' par especes, si unitobs, elle se fera par groupe d'especes.
#'
#' \code{tableMetrique} peut prendre les valeurs 'unitSpSz' (/station /especes /classe de taille),
#' 'unitSp' (/station /especes) ou 'unit' (de biodiversite (/station)). Ce dernier cas n'est
#' possible uniquement lorsque \code{agregation == 'unitobs'}. La table de metriques 'unitSpSz'
#' n'est pas disponible si le jeu de donnees est un jeu de donnees de benthos.
#'
#' \code{metrique} peut prendre differentes valeurs en fonction de celle de \code{tableMetrique}
#'  et du jeu de donnees.
#'
#' \code{factGraphSel} peut prendre differentes valeurs en fonction de celle de \code{factGraph}. Ce
#' parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' \code{listFact} ne peut pas avoir plus de 2 facteurs.
#'
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}.
#' Ce parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s)
#' de regroupement selectionne(s) seront prises en compte.


#' @title Barplot
#'
#' @description Creation d'un barplot selon les parametres fournis
#'
#' @param agregation chr, type d'agregation, par espece (espece) ou par groupe d'espece (unitobs)
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de separation des graphique
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param new_window bool, affichage du graphique dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' barplot_pampa.f(
#'   agregation = "espece",
#'   metrique = "density",
#'   factGraph = "family",
#'   factGraphSel = "Acanthuridae",
#'   listFact = c("year", "protection.status"),
#'   listFactSel = NA,
#'   tableMetrique = "unitSp",
#'   new_window = TRUE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export

barplot_pampa.f <- function(agregation, metrique, factGraph = NULL, factGraphSel = NA, listFact,
  listFactSel = NA, tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  tableMetrique_possible <- c("unit", "unitSp", "unitSpSz")
  nextStep <- switch(agregation,
    "espece" = "barplot.esp",
    "unitobs" = "barplot.unitobs",
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes).",
      "Please, choose an agregation between 'espece' and 'unitobs'."
    )
  )
  if (is.null(factGraph)){
    factGraph <- ""
  }

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

  # S'il s'agit d'un jeu de donnees benthos, ou qu'il n'y a pas de classes tailles disponibles :
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
  if (factGraph != ""){
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
  }

  # ...des modalites du facteur de separation des graphiques :
  # ...the modalities of the factor for the  graphic separation :
  if (factGraph != ""){
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
  }

  # ...des facteurs explicatifs :
  # ...the explanatory factors :
  if (length(listFact) > 2){
    stop(
      "Veuillez ne sélectionner que 2 facteurs au maximum pour le paramètre 'listFact'.",
      "Please, select only 2 factors at most for the 'listFact' parameter."
    )
  }
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
  if (agregation == 'espece'){
    WP2barplot.esp.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
  else{
    WP2barplot.unitobs.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
}


#' @title Barplot (données agrégées par espèces)
#'
#' @description Cette fonction est appelée par selectionVariables.f. Elle permet la création de
#' barplot pour des données agrégées par espèces selon les paramètres définis par l'utilisateur.
#'
#' @param metrique chr, métrique choisie
#' @param factGraph chr, facteur de séparation des graphiques
#' @param factGraphSel chr, sélection de modalité pour le facteur de séparation des graphiques
#' @param listFact chr,  facteur(s) de regroupement
#' @param listFactSel list, modalité sélectionnées pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de métrique
#' @param dataEnv environnement de stockage des données
#' @param baseEnv environnement parent
#'
#' @noRd

WP2barplot.esp.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des barplots génériques par espèce en tenant compte
  ##          des options graphiques
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 août 2012, 10:39

#  pampaProfilingStart.f()

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leurs listes de modalités sélectionnées.

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL)

  # # Construction de la formule du boxplot :
  # exprBP <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))
  # [!!!]

  # Identification des différents graphiques à générer:
  if (factGraph == ""){                   # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){          # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                                # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list()

  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :
  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :
  for (modGraphSel in iFactGraphSel){
    # Préparation des données pour un graphique :
    if (modGraphSel == "") {         # ...si pas de facteur de séparation des graphiques
      tmpDataMod <- tmpData
    }else{                           # ...sinon.
      tmpDataMod <- subset(tmpData, tmpData[ , factGraph] == modGraphSel) # Subset des données pour la modalité.
    }

    # Passage au graphique suivant si le nombre d'observations < au minimum défini dans les options.
    if (nrow(tmpDataMod) < getOption("P.MinNbObs")){
      warning(mltext("WP2boxplot.W.n.1"), modGraphSel, " < ", getOption("P.MinNbObs"),
        mltext("WP2boxplot.W.n.2"))

      plotted <- FALSE
      next()
    }else{
      plotted <- TRUE
    }

    # Suppression des 'levels' non utilisés :
    tmpDataMod <- dropLevels.f(tmpDataMod)

    # Sauvegarde temporaire des données :
    DataBackup[[modGraphSel]] <<- tmpDataMod

    if (new_window){
      # Ouverture et configuration du périphérique graphique :
      graphFileTmp <- openDevice.f(noGraph = which(modGraphSel == iFactGraphSel),
        metrique = metrique,
        factGraph = factGraph,
        modSel =
          if (getOption("P.plusieursGraphPage")){
            iFactGraphSel        # toutes les modalités.
          }else{
            modGraphSel          # la modalité courante uniquement.
          },
        listFact = listFact,
        dataEnv = dataEnv,
        type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
          "unitSp" = {"espece"},
          "unitSpSz" = {"CL_espece"},
          "unit" = {"unitobs"},
          "espece"),
        typeGraph = paste("barplot", getOption("P.barplotStat"), sep = "-")) # moyenne/médiane !?
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
        prefix = paste("barplot", getOption("P.barplotStat"), sep = "-"),
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
          "unitSp" = {"espece"},
          "unitSpSz" = {"CL_espece"},
          "unit" = {"unitobs"},
          "espece"))
    }

    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    par(mar = c(9, 5, 8, 1), mgp = c(3.5, 1, 0)) # paramètres graphiques.

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :

    mainTitle <- graphTitle.f(
      metrique = metrique,
      modGraphSel = modGraphSel,
      factGraph = factGraph,
      listFact = listFact,
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"espece"},
        "unitSpSz" = {"CL_espece"},
        "espece"),
      graphType = "barplot")

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
          ""),
        sep = "")),
       "")

    # Barplot !
    tmpBaP <- barplotPAMPA.f(metrique = metrique, listFact = listFact, Data = tmpDataMod,
      main = mainTitle, ylab = ylab)


    # #################### Informations supplémentaires sur les graphiques ####################

    # Affichage des warnings (petits effectifs) :
    if (isTRUE(getOption("P.warnings"))){
      # Avertissement pour les petits effectifs :
      pointsSmallSample.f(objBaP = tmpBaP, nbmin = 5)
    }else{}

    # Nombres d'observations :
    if (getOption("P.NbObs")){
      nbObs <- tmpBaP$n # Retourné par la fonction 'barplot'

      # Nombres sur l'axe supérieur :
      axis(3, as.vector(nbObs), at = as.vector(tmpBaP$x),
        col.ticks = getOption("P.NbObsCol"), col.axis = getOption("P.NbObsCol"),
        lty = 2, lwd = 0.5,
        mgp = c(2, 0.5, 0))

      legend("topleft", mltext("WP2barplot.esp.leg", language = getOption("P.lang")),
        cex = 0.9, col = getOption("P.NbObsCol"), text.col = getOption("P.NbObsCol"), merge = FALSE)
    }else{}

    # ###################################################
    # Fermeture de graphiques et sauvegarde de fichiers :

    # On ferme les périphériques PNG en mode fichier individuel :
    if (isTRUE(getOption("P.graphPNG"))){
      if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) && plotted){
        dev.off()

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
            metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }
    }else{
      # Sauvegarde en wmf si pertinent et souhaité :
      if (( ! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
          plotted && ! getOption("P.graphPDF"))
      {
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
          savePlot(graphFile, type = "wmf", device = dev.cur())
        }else{}

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
            metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }

  }  # Fin de boucle graphique.

  # On ferme les périphériques PDF ou PNG restants :
  if (getOption("P.graphPDF") ||
      (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
      && plotted)
  {
    dev.off()

    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = sub("\\%03d", "00X", graphFile),
        Data = DataBackup, cols = NULL)
    }else{}

    # Sauvegarde des statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup,
        agregLevel = "species", type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}

    # Inclusion des fontes dans le pdf si souhaité :
    if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
      i <- 1

      # On parcours tous les fichiers qui correspondent au motif :
      while (is.element(basename(tmpFile <- sub("\\%03d", formatC(i, width = 3, flag = "0"), graphFile)),
        dir(dirname(graphFile))) &&
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

  # Sauvegarde en wmf + données restants si pertinent et souhaité :
  if ( ! (getOption("P.graphPNG") || getOption("P.graphPDF")) && # Si pas d'autre sortie fichier.
       getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1
       && plotted)
  {
    if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
      savePlot(graphFile, type = "wmf", device = dev.cur())
    }else{}

    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = sub("\\%03d", "00X", graphFile),
        Data = DataBackup, cols = NULL)
    }else{}

    # Sauvegarde des statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup,
        agregLevel = "species", type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}
  }else{}

  #    pampaProfilingEnd.f()
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
#           typeGraph, "_", metrique, "_", factGraph, "_", paste(listFact, collapse = "-"), ".pdf", sep = "")
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


barplotPAMPA.f <- function(metrique, listFact, Data, main = NULL, cex = getOption("P.cex"), ...){

  ## Purpose: Barplots avec formatage pour PAMPA
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique à représenter.
  ##            listFact : liste des facteurs de regroupement.
  ##            Data : les données à utiliser.
  ##            main : titre du graphique.
  ##            cex : taille des caractères.
  ##            ... : arguments optionnels (passés à la fonction boxplot).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 28 août 2012, 17:32

  # Calcul des moyennes/médianes :

  heights <- switch(getOption("P.barplotStat"),
    "moyenne" = ,
    "mean" = {
      with(Data,
        tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
          mean, na.rm = TRUE))
    },
    "médiane" = ,
    "median" = {
      with(Data,
        tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
          median, na.rm = TRUE))
    })

  # Calcul des écarts types (pour IC paramétriques) :
  if (is.element(getOption("P.barplotStat"), c("mean", "moyenne"))){
    SD <- with(Data,
      tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
        sd, na.rm = TRUE))

    # Fixé à 0 si les barres d'erreur ne doivent pas être affichées :
    if ( ! getOption("P.barplotErrorBar")){
      SD[1:length(SD)] <- 0
    }else{}
  }else{}
  # ... ou des quantiles :
  if (is.element(getOption("P.barplotStat"), c("median", "médiane"))){
    # Lower:
    quantL <- with(Data,
      tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
        quantile, probs = c(0.25), na.rm = TRUE))
    # Higher:
    quantH <- with(Data,
      tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
        quantile, probs = c(0.75), na.rm = TRUE))

    # Fixé à 0 si les barres d'erreur ne doivent pas être affichées :
    if ( ! getOption("P.barplotErrorBar")){
      quantL[1:length(quantL)] <- 0
      quantH[1:length(quantH)] <- 0
    }else{}
  }else{}

  # Nombre d'observation par croisement de facteur :
  N <- with(Data,
    tapply(eval(parse(text = metrique)), lapply(listFact, function(y) eval(parse(text = y))),
      function(x){
        sum( ! is.na(x))
  }))

  # Intervalle de confiance :
  CIplus <- switch(getOption("P.barplotStat"),
    "moyenne" = ,
    "mean" = {
      SD * qt(0.975, df = N-1) / sqrt(N)
    },
    "médiane" = ,
    "median" = {
      quantH - heights
    })
  CIminus <- switch(getOption("P.barplotStat"),
    "moyenne" = ,
    "mean" = {
      SD * qt(0.975, df = N-1) / sqrt(N)
    },
    "médiane" = ,
    "median" = {
      heights - quantL
    })

  # Paramètres graphiques :
  # Marge dynamiques (adaptation à la longueur des labels) :
  optim(par = unlist(par("mai")),   # Le rapport inch/ligne est modifié en changeant les marges => besoin
    # de l'optimiser.
    fn = function(x){
      par(mai = c(
        # Marge du bas :
        lineInchConvert.f()$V * cex * unlist(par("lheight")) * 4.5,
        # Marge de gauche dynamique :
        tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) *
          (ifelse(isTRUE(getOption("P.graphPaper")),
            ifelse(isSubplot(), 0.8, 1.4),
            ifelse(isSubplot(), 0.8, 2.4))
            + 0.4 + ifelse(isSubplot(), 0.5, 0.9)) + # marge supplémentaire.
          max(strDimRotation.f(as.graphicsAnnot(pretty(range(c(heights,
            heights + CIplus), na.rm = TRUE))),
          srt = 0,
          unit = "inches",
          cex = cex)$width, na.rm = TRUE)) > 0.7 * unlist(par("pin"))[1],
          0.7 * unlist(par("pin"))[1], # marge maximale.
          tmp),
        # Marge supérieure augmentée s'il y a un titre :
        ifelse(isSubplot(),
          2.5 * lineInchConvert.f()$V, # cas des subplots.
          # ...sinon cas normal :
          ifelse(isTRUE(getOption("P.graphPaper")) || (! isTRUE(getOption("P.title"))),
            3 * lineInchConvert.f()$V,
            8 * lineInchConvert.f()$V)),
        # Marge de droite :
        lineInchConvert.f()$H * cex * unlist(par("lheight")) *  ifelse(isSubplot(), 1.0, 7.0)) +
          lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
        # Distance du nom d'axe dépendante de la taille de marge gauche :
        mgp = c(tmp2 / lineInchConvert.f()$H - ifelse(isSubplot(), 1.4, 1.4),
          ifelse(isSubplot(), 0.4, 0.9), 0))

      # Valeur à minimiser :
      return(sum(abs(x - unlist(par("mai")))))
    },
    control = list(abstol = 0.01))    # Tolérance.

  #browser()

  # On retire les noms de colonnes de "heights" pour les rajouter manuellement ensuite sur le
  # graphique (meilleurs contrôle) ; uniquement si deux dimensions :

  if (length(dim(heights)) > 1){
    xnames <- colnames(heights)
    colnames(heights) <- NULL
  }else{
    xnames <- row.names(heights)
    row.names(heights) <- NULL
  }

  # Suppression des valeurs infinies (plante ylims les graphiques) :
  tmpHeights <- replace(heights, is.infinite(heights), NA)
  tmpCIplus <- replace(CIplus, is.infinite(CIplus), NA)

  ylims <- c(0,
    1.13 * max(tmpHeights +
      replace(tmpCIplus, is.na(tmpCIplus), 0), # Éviter d'avoir NA si IC non calculable.
      na.rm = TRUE)) # max des ordonnées tenant compte de l'intervalle de confiance paramétrique.

  barPlotTmp <- barplot(heights,
    beside = TRUE,
    main =
      if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
        main
      }else{
        NULL
      },
    xlab = "",
    ylim = ylims,
    las = 1,
    col = PAMPAcolors.f(n = nrow(heights)),
    cex.lab = cex,
    cex.axis = cex,
    legend.text = ifelse(length(listFact) > 1 && ! isSubplot(), TRUE, FALSE),
    args.legend = list("x" = "topright", "inset" = -0.08, "xpd" = NA,
      "title" = Capitalize.f(varNames[listFact[1], "nom"])),
    ...)

  # Axe des abs. (facteurs) :
  mtext(text = xnames, side = 1, line = ifelse(isSubplot(), 0.5, 0.9),
    at = if (length(dim(heights)) > 1) {apply(barPlotTmp, 2, mean)}else{barPlotTmp},
    cex = cex * par("cex"))

  # Barres d'erreur (si souhaitées) :
  if (getOption("P.barplotErrorBar")){
    errbar(x = barPlotTmp, y = heights, yplus = heights + CIplus, yminus = heights - CIminus,
      add = TRUE, pch = NA)
  }else{}


  # Labels des axes :
  if (getOption("P.axesLabels")){
    mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
      side = 1, line = ifelse(isSubplot(), 1.6, 2.3), cex = cex)

    # Précision du type de statistique :
    if ( ! isTRUE(getOption("P.graphPaper")) && ! isSubplot()){
      mtext(switch(paste(getOption("P.lang"), getOption("P.barplotStat"), sep = "-"),
        "fr-moyenne" = ,
        "fr-mean" = {
          ifelse(getOption("P.barplotErrorBar"),
            expression((italic("moyenne")~+~italic("intervalle de confiance à 95%"))),
            expression((italic("moyenne"))))
        },
        "fr-médiane" = ,
        "fr-median" = {
          ifelse(getOption("P.barplotErrorBar"),
            expression((italic("médiane")~+~italic("écart interquartile"))),
            expression((italic("médiane"))))
        },
        "en-moyenne" = ,
        "en-mean" = {
          ifelse(getOption("P.barplotErrorBar"),
            expression((italic("mean")~+~italic("95% confidence interval"))),
            expression((italic("mean"))))
        },
        "en-médiane" = ,
        "en-median" = {
          ifelse(getOption("P.barplotErrorBar"),
            expression((italic("median")~+~italic("interquartile range"))),
            expression((italic("median"))))
        }),
      side = 2, line = par("mgp")[1] - 1.1, cex = 0.9 * getOption("P.cex"), font = 2)
    }else{}
  }else{}

  # Résultats :
  return(list(x = barPlotTmp, n = N, ylims = ylims))
}


lineInchConvert.f <- function(){

  ## Purpose: Calcul du facteur de conversion inch/ligne.
  ## ----------------------------------------------------------------------
  ## Arguments: Aucun
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 août 2011, 13:07

  pars <- par(c("mai", "mar"))

  return(list(H = (pars$mai / pars$mar)[2],
    V = (pars$mai / pars$mar)[1]))
}


isSubplot <- function(){

  ## Purpose: Déterminer si l'on est dans un subplot ou non.
  ## ----------------------------------------------------------------------
  ## Arguments: aucun.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 janv. 2013, 17:03

  return(isTRUE(all.equal(getOption("P.pinSubplot"), par("fin"))))
}


strDimRotation.f <- function(x, srt = 0, unit = "user", cex = getOption("P.cex"), ...){

  ## Purpose: Calcul des dimensions d'une chaîne de caractère à laquelle
  ##          on applique une rotation
  ## ----------------------------------------------------------------------
  ## Arguments: x : vecteur de classe 'character'.
  ##            srt : angle de rotation en degrés.
  ##            unit : unité de sortie.
  ##            ... : arguments supplémentaires passés à str(height|width).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  9 févr. 2011, 16:15

  # browser()

  # Dimensions en pouces :
  W.inches <- strwidth(x, unit = "inches", cex = cex, ...)
  H.inches <- strheight(x, unit = "inches", cex = cex, ...)

  # Facteur de conversion avec l'unité souhaitée :
  X.inchesVSunit <- W.inches / strwidth(x, unit = unit, cex = cex, ...)
  Y.inchesVSunit <- H.inches / strheight(x, unit = unit, cex = cex, ...)

  # Calcul des largeurs et hauteurs en rotations :
  X.calc <- abs(W.inches * cos(srt * base:::pi / 180)) + abs(H.inches * sin(srt * base:::pi / 180))
  Y.calc <- abs(W.inches * sin(srt * base:::pi / 180)) + abs(H.inches * cos(srt * base:::pi / 180))

  # Conversion dans l'unité souhaitée :
  return(list(width = X.calc / X.inchesVSunit,
    height = Y.calc / Y.inchesVSunit))
}


# PAMPAcolors.f <- function(n = 1, palette = getOption("P.colPalette"), list = FALSE,
#   FUNname = FALSE, cartoOnly = FALSE){
#
#   ## Purpose: retourner n couleurs de la palette "palette".
#   ## ----------------------------------------------------------------------
#   ## Arguments: n : nombre de couleurs.
#   ##            palette : une des palettes de couleurs prédéfinies
#   ##            cartoOnly : uniquement les palettes cartographiques (si
#   ##                        list == TRUE).
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 26 oct. 2012, 17:08
#
#   if (list){
#     return(switch(as.character(cartoOnly),
#       "1" = ,
#       "TRUE" = {
#         mltext(paste0("PAMPAcolors.pal.",
#           c("map1", "map2", "topo")))
#       },
#       mltext(paste0("PAMPAcolors.pal.",
#         c("default", "blue", "heat", "grey", "map1", "map2", "topo")))))
#   }else{}
#
#
#   palKey <- c("default", "blue", "heat", "grey", "map1", "map2", "topo")
#   names(palKey) <- mltext(paste0("PAMPAcolors.pal.",
#     c("default", "blue", "heat", "grey", "map1", "map2", "topo")))
#
#   palette <- if (palette[1] %in% names(palKey)){
#     palKey[palette[1]]   # get the hard coded palette name from the translation.
#   }else{
#     palette[1]
#   }
#
#   if (FUNname){
#     return(switch(palette,
#       "default" = ,
#       "defaut" = ,        # Accepts some values by default, even if not in the translation table.
#       "défaut" = {".ColorPaletteDefault"},
#       "bleu" = ,
#       "blue" = {".ColorPaletteBlue"},
#       "chaud" = ,
#       "heat" = {".ColorPaletteHeat"},
#       "grey" = ,
#       "gris" = ,
#       "gray" = {".ColorPaletteGray"},
#       "carto1" = ,
#       "map1" = {".ColorPaletteCarto1"},
#       "carto2" = ,
#       "map2" = {".ColorPaletteCarto2"},
#       "topo" = {"topo.colors"}))
#   }
#
#   # Creates the palette function if does not exists.
#   if (! is.null(PAMPAcolors.f(palette = palette, FUNname = TRUE)) &&
#       ! exists(PAMPAcolors.f(palette = palette, FUNname = TRUE), envir = .GlobalEnv))
#   {
#     makeColorPalettes.f()
#   }else{
#     if (is.null(PAMPAcolors.f(palette = palette, FUNname = TRUE)))
#       stop("Undefined color palette '", palette, "'")
#   }
#
#   res <- switch(palette,
#     "default" = ,
#     "defaut" = ,            # Accepts some values by default, even if not
#     "défaut" = {            # in the translation table.
#       if (n <= 10){
#         .ColorPaletteDefault(10)[1:n]
#       }else{
#         .ColorPaletteDefault(n)
#       }
#     },
#     "bleu" = ,
#     "blue" = {
#       if (n <= 10){
#         .ColorPaletteBlue(10)[1:n]
#       }else{
#         .ColorPaletteBlue(n)
#       }
#     },
#     "chaud" = ,
#     "heat" = {
#       .ColorPaletteHeat(n)
#     },
#     "grey" = ,
#     "gris" = ,
#     "gray" = {
#       .ColorPaletteGray(n)
#     },
#     "carto1" = ,
#     "map1" = {
#       if (n <= 5){
#         .ColorPaletteCarto1(5)[1:n]
#       }else{
#         .ColorPaletteCarto1(n)
#       }
#     },
#     "carto2" = ,
#     "map2" = {
#       if (n <= 16){
#         .ColorPaletteCarto2(16)[1:n]
#       }else{
#         .ColorPaletteCarto2(n)
#       }
#     },
#     "topo" = {
#       if (n <= 5){
#         topo.colors(5)[1:n]
#       }else{
#         topo.colors(n)
#       }
#     })
#
#   return(res)
# }
#
#
# makeColorPalettes.f <- function(){
#
#   ## Purpose: Créer les palettes de couleurs pour les graphiques
#   ## ----------------------------------------------------------------------
#   ## Arguments: aucun !
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 26 oct. 2012, 17:19
#
#   # default:
#   assign(".ColorPaletteDefault",
#     colorRampPalette(c("#66FFFF", "#9966FF", "#009999", "#CC3399", "#FFCC99",
#       "#FFFF99", "#CCFF99", "#CC9900", "#C0504D", "#FF99CC")),
#     envir = .GlobalEnv)
#
#   # blue:
#   assign(".ColorPaletteBlue",
#     colorRampPalette(c("#66FFFF", "#215968", "#33CCCC", "#003366", "#CCFFFF",
#       "#009999", "#99CCFF", "#66FFFF", "#215968", "#33CCCC")),
#     envir = .GlobalEnv)
#
#   # heat:
#   assign(".ColorPaletteHeat",
#     colorRampPalette(heat.colors(5)),
#      envir = .GlobalEnv)
#
#   # gray:
#   assign(".ColorPaletteGray",
#     colorRampPalette(c("#787878", "#dddddd")),
#     envir = .GlobalEnv)
#
#   # carto1:
#   assign(".ColorPaletteCarto1",
#     colorRampPalette(c("cyan", "violet", "slateblue", "lightsalmon", "palegreen")),
#     envir = .GlobalEnv)
#
#   # carto2:
#   assign(".ColorPaletteCarto2",
#     colorRampPalette(c("cyan", "violet", "slateblue", "lightsalmon", "palegreen", "darkseagreen4",
#       "chocolate1", "slateblue1", "pink", "burlywood1", "hotpink3", "tomato3", "khaki2", "goldenrod",
#       "tan1", "violetred3")),
#     envir = .GlobalEnv)
# }


errbar <- function(x, y, yplus, yminus, cap = 0.015, main = NULL, sub = NULL,
  xlab = as.character(substitute(x)),
  ylab = if (is.factor(x) || is.character(x)) "" else as.character(substitute(y)),
  add = TRUE, lty = 1, type = "p", ylim = NULL, lwd = 1, pch = NA,
  errbar.col = par("fg"), Type = rep(1, length(y)), ...){

  ## Purpose: Tracer les barres d'erreur sur des barplots.
  ## ----------------------------------------------------------------------
  ## Arguments: Ceux de la fonction du package Hmisc.
  ## ----------------------------------------------------------------------
  ## Author: Copié de la fonction du même nom du package Hmisc.

  if (is.null(ylim))
    ylim <- range(y[Type == 1], yplus[Type == 1], yminus[Type == 1], na.rm = TRUE)
  if (is.factor(x) || is.character(x)) {
    x <- as.character(x)
    n <- length(x)
    t1 <- Type == 1
    t2 <- Type == 2
    n1 <- sum(t1)
    n2 <- sum(t2)
    omai <- par("mai")
    mai <- omai
    mai[2] <- max(strwidth(x, "inches")) + 0.25 * .R.
    par(mai = mai)
    on.exit(par(mai = omai))
    plot(NA, NA, xlab = ylab, ylab = "", xlim = ylim, ylim = c(1, n + 1), axes = FALSE, ...)
    axis(1)
    w <- if (any(t2))
        n1 + (1:n2) + 1
    else numeric(0)
    axis(2, at = c(seq.int(length.out = n1), w), labels = c(x[t1], x[t2]), las = 1, adj = 1)
    points(y[t1], seq.int(length.out = n1), pch = pch, type = type, ...)
    segments(yplus[t1], seq.int(length.out = n1), yminus[t1],
      seq.int(length.out = n1), lwd = lwd, lty = lty, col = errbar.col)
    if (any(Type == 2)) {
      abline(h = n1 + 1, lty = 2, ...)
      offset <- mean(y[t1]) - mean(y[t2])
      if (min(yminus[t2]) < 0 & max(yplus[t2]) > 0)
        lines(c(0, 0) + offset, c(n1 + 1, par("usr")[4]), lty = 2, ...)
      points(y[t2] + offset, w, pch = pch, type = type, ...)
      segments(yminus[t2] + offset, w, yplus[t2] + offset, w, lwd = lwd, lty = lty, col = errbar.col)
      at <- pretty(range(y[t2], yplus[t2], yminus[t2]))
      axis(side = 3, at = at + offset, labels = format(round(at, 6)))
    }
    return(invisible())
  }
  if (add)
    points(x, y, pch = pch, type = type, ...)
  else plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, pch = pch, type = type, ...)
  xcoord <- par()$usr[1:2]
  smidge <- cap * (xcoord[2] - xcoord[1]) / 2
  segments(x, yminus, x, yplus, lty = lty, lwd = lwd, col = errbar.col)
  if (par()$xlog) {
    xstart <- x * 10^(-smidge)
    xend <- x * 10^(smidge)
  }
  else {
    xstart <- x - smidge
    xend <- x + smidge
  }
  segments(xstart, yminus, xend, yminus, lwd = lwd, lty = lty, col = errbar.col)
  segments(xstart, yplus, xend, yplus, lwd = lwd, lty = lty, col = errbar.col)
  return(invisible())
}


pointsSmallSample.f <- function(objBaP, nbmin = 20){

  ## Purpose: Afficher des points pour les petits effectifs
  ## ----------------------------------------------------------------------
  ## Arguments: objBaP : objet retourné par barplotPAMPA.f().
  ##            nbmin: nombre mini au dessous duquel afficher un warning.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 sept. 2012, 16:49

  if (any(objBaP$n < nbmin & objBaP$n > 0, na.rm = TRUE)){
    msg <- paste(mltext("plotPetitsEffectifs.small.n", language = getOption("P.lang")),
      " (< ", nbmin, ")", sep = "")

    # "Légende" :
    legend("top",
      msg,
      cex =0.9, text.col = "red", merge = FALSE, adj = c(0, 0.2),
      pch = rev(c(24, NA)[seq_along(msg)]),
      col = "red3", pt.bg = "gold", pt.cex = 1.2)

    # Index des petits effectifs :
    idx <- which(objBaP$n < nbmin & objBaP$n > 0)

    # Points :
    points(x = objBaP$x[idx],
      y = rep((max(objBaP$ylims) / 1.1) * 0.97, length = length(idx)),
        pch = 24, col = "red3", bg = "gold", cex = 1.2)
  }else{}
}


# writeData.f <- function(filename, Data, cols = NULL){
#
#   ## Purpose: Écrire les données des graphiques ou analyses dans des
#   ##          fichiers csv (format français).
#   ## ----------------------------------------------------------------------
#   ## Arguments: filename : nom du fichier à créer.
#   ##            Data : jeu de données à sauvegarder.
#   ##            cols : colonnes à sauvegarder (toutes si NULL).
#   ##            filePathes : chemin des dossiers.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date:  6 sept. 2012, 11:37
#
#   # Ajout de l'extension si besoin :
#   if ( ! grepl("\\.csv$", filename[1], ignore.case = TRUE)){
#     filename <- paste(filename, ".csv", sep = "")
#   }else{}
#
#   # Si l'argument est une liste de data.frame, elles sont agrégées :
#   if (class(Data) == "list"){
#     if (all(sapply(Data, class) == "data.frame")){
#       Data <- do.call(rbind, Data)
#     }else{
#       warning("Data saving: programming error!")
#     }
#   }else{}
#
#   # Colonnes retenues :
#   if (is.null(cols)) cols <- colnames(Data)
#
#   cols <- cols[is.element(cols, colnames(Data))]
#
#
#   # Écriture du fichier
#   tryCatch(write.csv(Data[ , cols],
#     file = filename,
#     row.names = FALSE),
#     error = function(e){
#       message(mltext("writeData.f.msg"), filename)
# #     errorLog.f(error = e, niv = -4)
#   })
# }
#
#
# infoStats.f <- function(filename, Data, agregLevel = c("species", "unitobs"), type = c("graph", "stat"),
#   metrique, factGraph, factGraphSel, listFact, listFactSel, dataEnv, baseEnv = .GlobalEnv){
#
#   ## Purpose: Écrire les infos et statistique sur les données associées à
#   ##          un graphique ou analyse.
#   ## ----------------------------------------------------------------------
#   ## Arguments: filename : chemin du fichier de résultats.
#   ##            Data : données du graphique/de l'analyse.
#   ##            agregLevel : niveau d'agrégation de la fonction appelante.
#   ##            type : type de fonction appelante (grapique ou analyse).
#   ##            metrique : la métrique choisie.
#   ##            factGraph : le facteur sélection des espèces.
#   ##            factGraphSel : la sélection de modalités pour ce dernier
#   ##            listFact : liste du (des) facteur(s) de regroupement
#   ##            listFactSel : liste des modalités sélectionnées pour ce(s)
#   ##                          dernier(s)
#   ##            dataEnv : environnement de stockage des données.
#   ##            baseEnv : environnement de l'interface.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 10 sept. 2012, 15:26
#
#   # Ajout de l'extension si besoin :
#   if ( ! grepl("\\.stats$", filename[1], ignore.case = TRUE)){
#     filename <- paste(filename, ".stats", sep = "")
#   }else{}
#
#   # Ouverture du fichier :
#   File <- file(description = filename,
#     open = "w", encoding = "latin1")
#
#   # Si erreur, on referme le fichier à la sortie de fonction :
#   on.exit(if (exists("filename") &&
#     tryCatch(isOpen(File),
#       error = function(e)return(FALSE))) close(File))
#
#   # Informations générales sur les données :
# #  printGeneralDataInfo.f(dataEnv = dataEnv, baseEnv = baseEnv, File = File)
#
#   # Informations sur les métriques et facteurs du graphique :
# #  printSelectionInfo.f(metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
# #    listFact = listFact, listFactSel = listFactSel, File = File,
# #    agregLevel = agregLevel, type = type)
#
#   # Statistiques :
#   if (class(Data) == "list"){
#     cat("\n###################################################",
#       mltext("infoStats.f.1"),
#       sep = "", file = File)
#
#     invisible(sapply(1:length(Data),
#       function(i){
#         printStats.f(Data = Data[[i]], metrique = metrique, listFact = listFact, File = File,
#           headline = factGraphSel[i])
#     }))
#   }else{
#     printStats.f(Data = Data, metrique = metrique, listFact = listFact, File = File,
#       headline = NULL)
#   }
#
#   # Fermeture du fichier :
#   close(File)
#
# }
#
#
# printStats.f <- function(Data, metrique, listFact, File, headline = NULL){
#
#   ## Purpose: Écrire les tableaux de statistiques générales et par
#   ##          croisement de facteur dans un fichier.
#   ## ----------------------------------------------------------------------
#   ## Arguments: Data : les données du graphique/de l'analyse.
#   ##            metrique : nom de la métrique.
#   ##            listFact : liste des facteurs de regroupement/de l'analyse.
#   ##            File : la connection du fichier où écrire.
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09
#
#   # Ligne d'en-tête (si besoin : traitement par espèces uniquement) :
#   if ( ! is.null(headline)){
#     cat("\n", rep("#", nchar(headline) + 3), "\n",
#       "## ", headline, "\n",
#       sep = "", file = File)
#   }else{}
#
#   cat(mltext("printStats.f.1"), file = File)
#
#   capture.output(print(summary.fr(Data[ , metrique])), file = File, append = TRUE)
#
#   if ( ! is.null(listFact)){
#     cat("\n#########################################",
#       mltext("printStats.f.2"), file = File, sep = "")
#
#       # Calcul du summary pour chaque croisement (existant) de facteur :
#       res <- with(Data,
#         tapply(eval(parse(text = metrique)),
#           INDEX = do.call(paste,
#             c(lapply(listFact,
#               function(y) eval(parse(text = y))),
#               sep = ".")),
#           FUN = summary.fr))
#
#       # Assemblage du résultat dans un tableau
#       capture.output(print(do.call(rbind, res)), file = File, append = TRUE)
#   }else{}
#
#   # Ligne vide (pour l'esthétique) :
#   cat("\n", file = File)
# }
#
#
# summary.fr <- function(object, digits = max(3, getOption("digits") - 3), ...){
#
#   ## Purpose: Franciser les sorties d'un summary (numeric uniquement).
#   ## ----------------------------------------------------------------------
#   ## Arguments: object : objet à résumer.
#   ##            ... : argument supplémentaires passés à summary().
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47
#
#   if ( ! is.numeric(object)) stop("Programming error")
#
#   # Calcul du résumé :
#   res <- c(summary(object = object, digits, ...),
#     "sd" = signif(sd(x = object), digits= digits), "N" = length(object))
#
#   # Changement des noms d'éléments :
#   names(res) <- c(mltext("summary.min"), mltext("summary.1stQ"),
#     mltext("summary.med"), mltext("summary.mean"),
#     mltext("summary.3rdQ"), mltext("summary.max"),
#     mltext("summary.sd"), mltext("summary.N"))
#
#   return(res)
# }


#' @title Barplot (données agrégées par unités d'observation)
#'
#' @description Cette fonction permet la création de barplot pour les données agrégées par unités
#' d'observation.
#'
#' @param metrique chr, métrique choisie
#' @param factGraph chr, facteur de sélection des espèces
#' @param factGraphSel chr, sélection de modalités pour le facteur de sélection des espèces
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalités sélectionnées pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de métriques
#' @param dataEnv environnement de stockage des données
#' @param baseEnv environnement parent
#'
#' @noRd

WP2barplot.unitobs.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire les barplots en tenant compte des options graphiques
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factGraph : le facteur sélection des espèces.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 sept. 2012, 16:10

#  pampaProfilingStart.f()

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leur liste de modalités sélectionnées

  # Données pour la série de boxplots :
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

  # Identification des différents modalités (espèces) du graphique à générer :
  if (factGraph == ""){               # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){      # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                    # Modalités sélectionnées (et présentes parmi les données retenues).
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
              unitobs = "observation.unit",
              code.especes = "species.code",
              nombres = getOption("P.nbName"),
              indices = metrique,
              dataEnv = dataEnv)
          }))

      # On rajoute les anciennes colonnes :
      tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
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
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list(tmpData)

  # Création du graphique si le nombre d'observations < au minimum défini dans les options :
  if (nrow(tmpData) < getOption("P.MinNbObs")){
    warning(mltext("WP2boxplot.W.n.1"), " (", paste(iFactGraphSel, collapse = ", "), ") < ",
      getOption("P.MinNbObs"), mltext("WP2boxplot.W.n.2"))
  }else{
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
        typeGraph = paste("barplot", getOption("P.barplotStat"), sep = "-")) # moyenne/médiane !?
    } else{
      graphFile <- resFileGraph.f(
        metrique = metrique,
        factGraph = factGraph,
        modSel = iFactGraphSel,
        listFact = listFact,
        dataEnv = dataEnv,
        ext = "wmf",
        prefix = paste("barplot", getOption("P.barplotStat"), sep = "-"),
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
          "CL_unitobs",
          "unitobs"))
    }

    par(mar = c(9, 5, 8, 1), mgp = c(3.5, 1, 0)) # paramètres graphiques.

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
      graphType = "barplot")

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
          ""),
        sep = "")),
      "")

    # Boxplot !
    tmpBaP <- barplotPAMPA.f(metrique = metrique, listFact = listFact, Data = tmpData,
      main = mainTitle, ylab = ylab)


    # #################### Informations supplémentaires sur les graphiques ####################

    # Affichage des warnings (petits effectifs) :
    if (isTRUE(getOption("P.warnings"))){
      # Avertissement pour les petits effectifs :
      pointsSmallSample.f(objBaP = tmpBaP, nbmin = 5)
    }else{}

    # Nombres d'observations :
    if (getOption("P.NbObs")){
      nbObs <- tmpBaP$n # Retourné par la fonction 'barplot'

      # Nombres sur l'axe supérieur :
      axis(3, as.vector(nbObs), at = as.vector(tmpBaP$x),
        col.ticks = getOption("P.NbObsCol"), col.axis = getOption("P.NbObsCol"),
        lty = 2, lwd = 0.5,
        mgp = c(2, 0.5, 0))

      legend("topleft", mltext("WP2barplot.esp.leg", language = getOption("P.lang")),
        cex = 0.9, col = getOption("P.NbObsCol"), text.col = getOption("P.NbObsCol"), merge = FALSE)
    }else{}

    # ##################################################
    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
    }else{}

    # Sauvegarde des infos sur les données et statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "unitobs", type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}

    # On ferme les périphériques PDF :
    if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG"))){
      dev.off()

      # Inclusion des fontes dans le pdf si souhaité :
      if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
        tryCatch(embedFonts(file = graphFile),
          error = function(e){
            warning(mltext("WP2boxplot.W.pdfFonts"))
          })
      }

    }else{
      if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
        # Sauvegarde en wmf si pertinent et souhaité :
        savePlot(graphFile, type = "wmf", device = dev.cur())
      }else{}
    }
  }  # Fin de graphique.

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
