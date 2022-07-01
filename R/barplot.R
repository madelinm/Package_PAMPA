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


#' @title Barplot (donnees agregees par especes)
#'
#' @description Cette fonction permet la creation de barplot pour des donnees agregees par especes
#' selon les parametres definis par l'utilisateur.
#'
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection de modalite pour le facteur de separation des graphiques
#' @param listFact chr,  facteur(s) de regroupement
#' @param listFactSel list, modalite selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param dataEnv environnement de stockage des donnees
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
}


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


#' @title Barplot (donnees agregees par unites d'observation)
#'
#' @description Cette fonction permet la creation de barplot pour les donnees agregees par unites
#' d'observation.
#'
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de selection des especes
#' @param factGraphSel chr, selection de modalites pour le facteur de selection des especes
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metriques
#' @param dataEnv environnement de stockage des donnees
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
}
