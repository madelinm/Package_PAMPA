#' Fonction pour tracer un boxplot.
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
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s) de
#' regroupement selectionne(s) seront prises en compte.


#' @title Boxplot
#'
#' @description Creation d'un boxplot selon les parametres fournis
#'
#' @param agregation chr, type d'agregation par espece (espece) ou par groupe d'espece (unitobs)
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite(s) selectionnee(s) pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param new_window bool, affichage du graphique dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' boxplot_pampa.f(
#'   agregation = "espece",
#'   metrique = "density",
#'   factGraph = "species.code",
#'   factGraphSel = c("Suffchry", "Parubarb"),
#'   listFact = c("year", "protection.status"),
#'   listFactSel = NA,
#'   tableMetrique = "unitSp",
#'   new_window = TRUE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export

boxplot_pampa.f <- function(agregation, metrique, factGraph =  NULL, factGraphSel = NA, listFact,
  listFactSel = NA, tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  tableMetrique_possible <- c("unit", "unitSp", "unitSpSz")
  nextStep <- switch(agregation,
    "espece" = "boxplot.esp",
    "unitobs" = "boxplot.unitobs",
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes).",
      "Please, choose an agregation between 'espece' and 'unitobs'."
    )
  )
  if (is.null(factGraph)) {
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
    WP2boxplot.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
  else{
    WP2boxplot.unitobs.f(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique,
      new_window, dataEnv, baseEnv)
  }
}


#' @title Boxplot (donnees agregees par especes)
#'
#' @description Cette fonction permet la creation du boxplot pour les donnees agregees par especes
#'
#' @param metrique : chr, metrique choisie
#' @param factGraph : chr, facteur de separation des graphiques
#' @param factGraphSel : chr, selection de modalite du facteur de separation des graphiques
#' @param listFact : chr, facteur(s) de regroupement
#' @param listFactSel : list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique : chr,  nom de la table de metrique
#' @param dataEnv : environnement de stockage des donnees
#' @param baseEnv : environnement parent
#'
#' @noRd

WP2boxplot.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire les boxplots en tenant compte des options graphiques
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface principale.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  6 août 2010, 16:34

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leurs listes de modalités sélectionnées.

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs,
    selections = selections, dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL)

  # Formule du boxplot
  exprBP <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))

  # Identification des différents graphiques à générer:
  if (factGraph == ""){             # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){    # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{          # Modalités sélectionnées (et présentes parmi les données retenues).
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
    if (modGraphSel == ""){         # ...si pas de facteur de séparation des graphiques
      tmpDataMod <- tmpData
    }else{                          # ...sinon.
      tmpDataMod <- subset(tmpData, tmpData[ , factGraph] == modGraphSel) # Subset des données pour la modalité.
    }

    # Passage au graphique suivant si le nombre d'observations < au minimum défini dans les options.
    if (nrow(tmpDataMod) < getOption("P.MinNbObs")){
      warning(mltext("WP2boxplot.W.n.1"),
        modGraphSel, " < ", getOption("P.MinNbObs"),
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
            iFactGraphSel      # toutes les modalités.
          }else{
            modGraphSel        # la modalité courante uniquement.
          },
        listFact = listFact,
        dataEnv = dataEnv,
        type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
          "unitSp" = {"espece"},
          "unitSpSz" = {"CL_espece"},
          "unit" = {"unitobs"},
          "espece"),
        typeGraph = "boxplot")
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
        prefix = "boxplot",
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
    mainTitle <- graphTitle.f(metrique = metrique,
      modGraphSel = modGraphSel, factGraph = factGraph,
      listFact = listFact,
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"espece"},
        "unitSpSz" = {"CL_espece"},
        "espece"),
      graphType = "boxplot")

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
        ""),
      sep = "")),
    "")

    # Boxplot !
    tmpBP <- boxplotPAMPA.f(exprBP, data = tmpDataMod, main = mainTitle, ylab = ylab)


    # #################### Informations supplémentaires sur les graphiques ####################

    # Séparateurs de facteur de premier niveau :
    if (getOption("P.sepGroupes")){
      sepBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = tmpDataMod)
    }

    # Séparateurs par défaut :
    abline(v = 0.5 + (0:length(tmpBP$names)), col = "lightgray", lty = "dotted") # Séparations.

    # Légende des couleurs (facteur de second niveau) :
    if (getOption("P.legendeCouleurs")){
      legendBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = tmpDataMod)
    }else{}

    # Moyennes :
    Moyenne <- as.vector(tapply(X = tmpDataMod[, metrique], # moyenne par groupe.
      INDEX = as.list(tmpDataMod[ , attr(terms(exprBP),
        "term.labels"), drop = FALSE]),
      FUN = mean, na.rm = TRUE))

    # ... points :
    if (getOption("P.pointMoyenne")){
      points(Moyenne,
        pch = getOption("P.pointMoyennePch"),
        col = getOption("P.pointMoyenneCol"), lwd = 2.5,
        cex = getOption("P.pointMoyenneCex"))
    }else{}

    # ... valeurs :
    if (getOption("P.valMoyenne")){
      plotValMoyennes.f(moyennes = Moyenne, objBP = tmpBP)
    }else{}

    if (isTRUE(getOption("P.warnings"))){
      # Avertissement pour les petits effectifs :
      plotPetitsEffectifs.f(objBP = tmpBP, nbmin = 5)
    }else{}

    # Nombres d'observations :
    if (getOption("P.NbObs")){
      nbObs <- tmpBP$n # Retourné par la fonction 'boxplot'

      # Nombres sur l'axe supérieur :
      axis(3, as.vector(nbObs), at = 1:length(as.vector(nbObs)),
        col.ticks = getOption("P.NbObsCol"), col.axis = getOption("P.NbObsCol"),
        lty = 2, lwd = 0.5,
        mgp = c(2, 0.5, 0))

      legend("topleft", mltext("WP2boxplot.n.rec", language = getOption("P.lang")),
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
        plotted && ! getOption("P.graphPDF")){
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
    && plotted){
    dev.off()

    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup, cols = NULL)
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
      && plotted){
    if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
      savePlot(graphFile, type = "wmf", device = dev.cur())
    }else{}

    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup, cols = NULL)
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


subsetToutesTables.f <- function(metrique, facteurs, selections,
  dataEnv, tableMetrique = "", exclude = NULL, add = c("species.code", "observation.unit")){

  ## Purpose: Extraire les données utiles uniquement, d'après les métrique
  ##          et facteur(s) séléctionnés, ainsi que leur(s) sélection(s) de
  ##          modalité(s).
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            facteurs : les facteurs sélectionnés (tous)
  ##            selections : les sélections de modalités correspondantes
  ##                         (liste).
  ##            tableMetrique : le nom de la table des métriques.
  ##            exclude : niveau de facteur à ne pas prendre en compte pour
  ##                      le subset.
  ##            add : champ(s) (de la table de métrique) à ajouter aux
  ##                  données.
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  6 août 2010, 16:46

  # Récupération des référentiels :
  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  # Si pas de table de métrique disponible ou déjà calculée
  # ("TableOcurrences" est calculée à partir de la sélection) :
  if (is.element(tableMetrique, c("", "TableOccurrences", "TablePresAbs"))){
    tableMetrique <- "unitSp"
  }else{}

  casTables <- c("unitSp" = "unitSp",
    "TablePresAbs" = "unitSp",
    "unitSpSz" = "unitSpSz")

  # Récupération de la table de métriques :
  dataMetrique <- get(tableMetrique, envir = dataEnv)

  # Si pas de métrique disponible ou déjà calculée ("freq.occurrence" est calculée à partir de la sélection) :
  if (is.element(metrique, c("", "occurrence.frequency"))){
    metrique <- "tmp"
    dataMetrique$tmp <- 0
    dataMetrique$tmp[dataMetrique[ , getOption("P.nbName")] > 0] <- 1
  }else{}

  if (!is.null(add)){
    metriques <- c(metrique, add[is.element(add, colnames(dataMetrique))])
  }else{
    metriques <- metrique
  }

  # Subset en fonction de la table de métrique
  switch(casTables[tableMetrique],
    # Cas de la table d'observation ou des tables de présence :
    unitSp = {
      restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop = FALSE],
        unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
          unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
          facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
        refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
          refesp$species.code),        # ajout des colonnes sélectionnées d'especes
          facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
    },
    # Cas de la table d'observations par classes de taille :
    unitSpSz = {
      restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) ,
        c(metriques, "size.class"), drop = FALSE],
        unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
          unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
          facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
        refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
          refesp$species.code),        # ajout des colonnes sélectionnées d'especes
          facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
    },
    # Autres cas :
    restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop = FALSE],
      unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
        unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs.
        facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE])
  )

  selCol <- which(!is.na(selections))
  if (!is.null(exclude)){
    selCol <- selCol[selCol != exclude]
  }
  for (i in selCol){
    restmp <- subset(restmp, is.element(restmp[ , facteurs[i]], selections[[i]]))
  }

  # Traitement particulier des classes de taille (mise en facteur avec ordre défini selon le context) :
  if (is.element("size.class", colnames(restmp))){
    if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$size.class)), perl = TRUE)) ==
        length(unique(as.character(restmp$size.class))))
    {
      restmp$size.class <-
        factor(as.character(restmp$size.class),
          levels = unique(as.character(restmp$size.class))[
            order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
              "\\1",
              unique(as.character(restmp$size.class)),
              perl = TRUE)),
            na.last = FALSE)])
    }else{
      restmp$size.class <- factor(restmp$size.class)
    }
  }else{}

  # Conversion des biomasses et densités -> /100m² :
  if (any(is.element(colnames(restmp), c("biomass", "density",
    "biomass.max", "density.max", "biomass.sd", "density.sd"))) && ! is.peche.f())
  {
    restmp[ , is.element(colnames(restmp),
      c("biomass", "density",
        "biomass.max", "density.max",
        "biomass.sd", "density.sd"))] <- 100 *
    restmp[, is.element(colnames(restmp),
      c("biomass", "density",
        "biomass.max", "density.max",
        "biomass.sd", "density.sd"))]
  }else{}

  return(restmp)
}


openDevice.f <- function(noGraph, metrique, factGraph, modSel, listFact, dataEnv,
  type = "espece", typeGraph = "boxplot", large = FALSE){

  ## Purpose: Ouvrir les périphériques graphiques avec les bonnes options
  ## ----------------------------------------------------------------------
  ## Arguments: noGraph : le numéro de graphique (integer)
  ##            metrique : la métrique choisie.
  ##            factGraph : le facteur de séparation des graphiques.
  ##            modSel :  modalité(s) de factGraph sélectionnée(s).
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            type : type de données (traitement conditionnel).
  ##            typeGraph : type de graphique.
  ##            large : pour des traitements particuliers (e.g. MRT)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 août 2010, 14:54

  fileName <- NULL

  if (!getOption("P.graphPDF")){ # sorties graphiques à l'écran ou PNG.
    if (isTRUE(getOption("P.graphPNG"))){
      if (noGraph == 1 || ! getOption("P.plusieursGraphPage") ||
          grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph))
      {
        pngFileName <- resFileGraph.f(metrique = metrique, factGraph = factGraph, modSel = modSel,
          listFact = listFact, dataEnv = dataEnv, ext = "png", prefix = typeGraph,
          sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
            "%03d",
            ""),
          type = type)

        # Si plusieurs graphiques par page :
        if (getOption("P.plusieursGraphPage") && length(modSel) > 1 & # Regrouper dans une fonction de test
            ! is.element(type, c("unitobs")) &&                       # (mutualiser le code). [!!!]
            ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph))
        {
          png(pngFileName,
              width = ifelse(large, 120, 90) * 15,
              height = ifelse(large, 75, 55) * 15,
              pointsize = 14)
          par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
        }else{
          png(pngFileName,
            width = ifelse(large, 100,
              ifelse(isTRUE(getOption("P.graphPaper")), 50, 75)) * 15,
            height = ifelse(large, 55,
              ifelse(isTRUE(getOption("P.graphPaper")), 30, 40)) * 15, pointsize = 14)
        }

        # Pour retourner le nom de fichier malgré tout :
        fileName <- pngFileName
      }else{}

    }else{   # Graphiques à l'écran :
      # Des fonctions différentes pour l'affichage à l'écran, selon la plateforme :
      if (.Platform$OS.type == "windows"){
        winFUN <- "windows"
      }else{
        winFUN <- "X11"
      }

      if (getOption("P.plusieursGraphPage") &&                           # Plusieurs graphs par page...
          length(modSel) > 1 &&                                          # ...plus d'un facteur sélectionné...
          ! is.element(type, c("unitobs")) &&                            # ...pas d'agrégation...
          ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph)) # ...et pas des cartes.
      {
        if ((noGraph %%                                                  # ...et page remplie.
             (getOption("P.nrowGraph") * getOption("P.ncolGraph"))) == 1)
        {
          # [!!!] Limiter aux cas nécessaires... (cf. plus haut).
          eval(call(winFUN,
            width = ifelse(large, 40, 30),  # 80, 60
            height = ifelse(large, 26, 20), # 45, 35
            pointsize = ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))

          par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
        }else{                    # Pas plusieurs graphs par page.
        }
      }else{                      # Pas plusieurs graphs par page.
        eval(call(winFUN,
          width = ifelse(large, 35,
            ifelse(isTRUE(getOption("P.graphPaper")), 10, 25)), # 10, 50
          height = ifelse(large, 15,
            ifelse(isTRUE(getOption("P.graphPaper")), 6, 12)), # 6, 20
          pointsize = ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))
      }

      fileName <- resFileGraph.f(metrique = metrique, factGraph = factGraph, modSel = modSel,
        listFact = listFact, dataEnv = dataEnv, ext = "wmf", prefix = typeGraph,
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = type)
    }
  }else{ # Sorties graphiques en pdf :
    if (noGraph == 1){
      # Nom de fichier de fichier :
      if (getOption("P.PDFunFichierPage")){ # Un fichier par graphique avec numéro.
        pdfFileName <- paste(get("filePathes", envir = dataEnv)["results"],
          typeGraph, "_", metrique, "_", factGraph, "_", paste(listFact, collapse = "-"),
          "-%03d.pdf", sep = "")
        onefile <- FALSE

      }else{                          # Tous les graphiques dans des pages séparées d'un même fichier.
        pdfFileName <- paste(get("filePathes", envir = dataEnv)["results"],
          typeGraph, "_", metrique, "_", factGraph, "_", paste(listFact, collapse = "-"), ".pdf", sep = "")
        onefile <- TRUE
      }
      # Ouverture de fichier :
      pdf(pdfFileName, encoding = "ISOLatin1", family = "URWHelvetica", onefile = onefile,
        width = ifelse(large, 30,
          ifelse(isTRUE(getOption("P.graphPaper")), 12, 20)),
        height = ifelse(large, 20,
          ifelse(isTRUE(getOption("P.graphPaper")), 8, 12)),
        pointsize = 14)

      # Si plusieurs graphiques par page :
      if (getOption("P.plusieursGraphPage") &&
          length(modSel) > 1 &&                                          # Plus d'un facteur sélectionné.
          ! is.element(type, c("unitobs")) &&                            # Pas d'agrégation.
          ! grepl(pattern = "^carte_(boxplot|barplot)$", x = typeGraph)) # ...et pas des cartes.
      {
        par(mfrow = c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
      }else{}

      # Pour retourner le nom de fichier également :
      fileName <- pdfFileName
    }else{}
  }

  par(cex = getOption("P.cex"))
  return(fileName)
}


resFileGraph.f <- function(metrique, factGraph, modSel, listFact, ext, dataEnv,
  prefix = "boxplot", sufixe = NULL, type = "espece"){

  ## Purpose: Définit les noms du fichiers pour les résultats des modèles
  ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
  ##          mais par défaut, c'est le fichier de sorties texte qui est
  ##          créé.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : nom de la métrique analysée.
  ##            factGraph : nom du facteur de séprataion des analyses/
  ##                        de selection d'espèce(s).
  ##            modSel : modalité(s) de factGraph sélectionnée(s).
  ##            listFact : vecteur des noms de facteurs de l'analyse.
  ##            prefix : préfixe du nom de fichier.
  ##            sufixe : un sufixe pour le nom de fichier.
  ##            ext : extension du fichier.
  ##            type : type de modèle (traitement conditionnel).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 janv. 2011, 10:38

  # Nom de fichier :
  filename <- paste(get("filePathes", envir = dataEnv)["results"], prefix, "_",
    # Métrique analysée :
    metrique,
    ifelse(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1,
      paste("(", round(100 * getOption("P.GraphPartMax")),"pc-max)", sep = ""),
      ""),
    "_",
    # si facteur de séparation des analyses :
    mltext("resFileGraph.aggreg.abv"),
    switch(type,
      "espece" = mltext("resFileGraph.pfx.spSt"),
      "CL_espece" = mltext("resFileGraph.pfx.SCspSt"),
      "unitobs" = mltext("resFileGraph.pfx.St"),
      "CL_unitobs" = mltext("resFileGraph.pfx.SCSt"),
      ""),
    switch(type,
      "espece" = {
        ifelse(factGraph == "",
          "",
          paste(factGraph, "(", ifelse(modSel[1] != "",
            collapse.max.f(modSel, collapse = "+"),
            mltext("resFileGraph.all.spSt")),
            ")_", sep = ""))
      },
      "CL_espece" = {
        ifelse(factGraph == "",
          "",
          paste(factGraph, "(", ifelse(modSel[1] != "",
            collapse.max.f(modSel, collapse = "+"),
            mltext("resFileGraph.all.SCspSt")),
            ")_", sep = ""))
      },
      "unitobs" = {
        ifelse(factGraph == "",
          mltext("resFileGraph.allSp.St"),
          paste(factGraph, "(", ifelse(modSel[1] != "",
            collapse.max.f(modSel, collapse = "+"),
            mltext("resFileGraph.all.St")),
            ")_", sep = ""))
      },
      "CL_unitobs" = {
        ifelse(factGraph == "",
          mltext("resFileGraph.allSp.St"),
          paste(factGraph, "(", ifelse(modSel[1] != "",
            collapse.max.f(modSel, collapse = "+"),
            mltext("resFileGraph.all.St")),
            ")_", sep = ""))
      },
    ""),
    # liste des facteurs de l'analyse
    paste(listFact, collapse = "-"),
    # sufixe :
    ifelse(is.null(sufixe) || sufixe == "",
      "",
      paste("_", sufixe, sep = "")),
    # Extension du fichier :
      ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl = TRUE), # nettoyage de l'extension si besoin.
    sep = "")

  # Retourne le nom de fichier :
  return(filename)
}


collapse.max.f <- function(x, nmax = 120, collapse = "+", fillstr = "..."){

  ## Purpose: assemblage des noms de modalités avec contrôle sur la nombre
  ##          de caractères
  ## ----------------------------------------------------------------------
  ## Arguments: x : chaîne de caractères à assembler.
  ##            nmax : nombre maximum de lettres dans la chaîne assemblée.
  ##            collapse : caractère d'assemblage ("+" par défaut).
  ##            fillstr : chaîne de remplissage de dépassement.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  2 mai 2013, 15:56

  # Collapse :
  res <- paste(x, collapse = collapse)

  # test de longueur de chaîne :
  if (nchar(res) > nmax){
    res <- paste(substr(res, 1, as.integer(nmax / 2) - 1),
      fillstr,
      substr(res, nchar(res) - as.integer(nmax / 2) + 2, nchar(res)), sep = "")
  }

  return(res)
}


graphTitle.f <- function(metrique, modGraphSel, factGraph, listFact, model = NULL, type = "espece",
  graphType = c("generic", "boxplot", "barplot", "occFrequency", "occFamily"),
  lang = getOption("P.lang"), nbObs = NULL){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 14 oct. 2010, 15:44
  graphType <- match.arg(arg = graphType,
    choices = c("generic", "boxplot", "barplot", "occFrequency", "occFamily"),
    several.ok = FALSE)

  # Factors names in the chosen language:
  factNames <- sapply(listFact[length(listFact):1],
    function(x)paste(c(# varNames.f(x, "article"),
      "",
      varNames.f(x, "nom")), collapse = ""))

  return(paste(
    ifelse(is.null(model),
      switch(graphType,
        "generic" = {
          mltext("graphTitle.vals",
            language = lang)
        },
        "boxplot" = {
          mltext("graphTitle.boxplots",
            language = lang)
        },
        "barplot" = {
          paste0(switch(getOption("P.barplotStat"),
            "mean" = ,
            "moyenne" = {
              Capitalize.f(mltext("stat.mean", language = lang))
            },
            "median" = ,
            "médianne" = {
              Capitalize.f(mltext("stat.median",
                language = lang))
            }),
            "("
          )
        },
        "occFrequency" = {""},
        "occFamily" = {""}),
      paste(model,
        mltext("graphTitle.for",
          language = lang),
       varNames[metrique, "article"], sep = "")),
    ifelse(graphType == "occFrequency" | graphType == "occFamily",
      Capitalize.f(varNames[metrique, "nom"]),
      varNames[metrique, "nom"]),
    ifelse(test = graphType == "barplot",
      yes = ")",
      no = ""),
    switch(type,
      "espece" = mltext("graphTitle.bySpSt", language = lang),
      "CL_espece" = mltext("graphTitle.bySCSpSt", language = lang),
      "unitobs" = mltext("graphTitle.bySt", language = lang),
      "unitobs(CL)" = mltext("graphTitle.byStSC", language = lang),
      "CL_unitobs" = mltext("graphTitle.bySCSt", language = lang),
      "biodiv" = mltext("graphTitle.biodiv", language = lang),
      ""),
    switch(type,
      "espece" = {
        ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
          "",
          paste(mltext("graphTitle.sep.SpSt", language = lang),
            " '", factGraph, "' = ", modGraphSel, sep = ""))
      },
      "CL_espece" = {
        ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
          "",
          paste(mltext("graphTitle.sep.SCSpSt", language = lang),
            " '", factGraph, "' = ", modGraphSel, sep = ""))
      },
      "unitobs" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          mltext("graphTitle.sep.St.all", language = lang),
          paste(mltext("graphTitle.sep.St", language = lang),
            " '", factGraph, "' = (",
            paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      "unitobs(CL)" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          mltext("graphTitle.sep.StSC.all", language = lang),
            paste(mltext("graphTitle.sep.StSC", language = lang),
              " '", factGraph, "' = (",
              paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      "CL_unitobs" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          mltext("graphTitle.sep.SCSt.all", language = lang),
            paste(mltext("graphTitle.sep.SCSt", language = lang),
              " '", factGraph, "' = (",
              paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      "biodiv" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          "",
          paste(mltext("graphTitle.sep.biodiv", language = lang),
            " '", factGraph, "' = (",
            paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      ""),
    mltext("graphTitle.by", language = lang),
    if (length(factNames) > 1){
      paste(paste(head(factNames, -1), collapse = ", "),
        tail(factNames, 1),
        sep = mltext("graphTitle.and", language = lang))
    }else{
      factNames
    },
    "\n",
    if (graphType == "occFamily"){
      paste("(number of observations = ", nbObs, ")", "\n", sep = "")
    },
    sep = ""))
}


boxplotPAMPA.f <- function(exprBP, data, main = NULL, cex = getOption("P.cex"), ylim = NULL, ...){

  ## Purpose: Boxplot avec un formatage pour pampa
  ## ----------------------------------------------------------------------
  ## Arguments: exprBP : expression décrivant le modèle du boxplot.
  ##            data : les données à utiliser.
  ##            main : titre du graphique.
  ##            cex : taille des caractères.
  ##            ylim : limites desordonnées.
  ##            ... : arguments optionnels (passés à la fonction boxplot).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 févr. 2011, 17:05

  # Extraction du nom de la métrique :
  metrique <- deparse(exprBP[[2]])

  # Les couleurs pour l'identification des modalités du facteur de second niveau :
  colors <- colBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = data)

  # Suppression des valeurs infinies (plante ylims dans les graphiques) :
  tmpMetric <- replace(data[ , metrique], is.infinite(data[ , metrique]), NA)

  # ylims :
  if (is.null(ylim)){
    ylim <- c(min(tmpMetric, na.rm = TRUE),
      ifelse(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1,
        getOption("P.GraphPartMax") * max(tmpMetric, na.rm = TRUE),
        max(tmpMetric, na.rm = TRUE) +
          0.1*(max(tmpMetric, na.rm = TRUE) -
          min(tmpMetric, na.rm = TRUE))))
  }else{}

  # Plot sans affichage pour récupérer l'objet :
  tmpBP <- boxplot(exprBP, data = data,
    varwidth = TRUE, las = 2,
    col = colors,
    ylim = ylim,
    cex.lab = cex,
    cex.axis = cex,
    plot = FALSE,
    ...)

  # Marge dynamiques (adaptation à la longueur des labels) :
  optim(par = unlist(par("mai")),       # Le rapport inch/ligne est modifié en changeant les marges
                                        # => besoin de l'optimiser.

    fn = function(x){
      par(mai = c(
        # Marge du bas dynamique :
        ifelse((tmp <- lineInchConvert.f()$V * cex * unlist(par("lheight")) *
          ifelse(isSubplot(),
            (0.2 + 1.0),   # cas des subplots.
            (0.2 + 0.9)) + # marge supplémentaire.
          max(strDimRotation.f(tmpBP$names,
            srt = 45,
            unit = "inches",
            cex = cex)$height, na.rm = TRUE)) > 0.65 * unlist(par("pin"))[2],
          0.65 * unlist(par("pin"))[2],
          tmp),
        # Marge de gauche dynamique :
        tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) *
          ifelse(isSubplot(),
            (0.8 +0.4 + 0.9),   # cas des subplots.
            (1.4 +0.4 + 0.9)) + # marge supplémentaire.
          max(strDimRotation.f(as.graphicsAnnot(pretty(range(
            if(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1){
              data[data[ , metrique] <
                getOption("P.GraphPartMax") *
                max(data[ , metrique], na.rm = TRUE) ,
                metrique]
            }else{
              data[ , metrique]
            }, na.rm = TRUE))),
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
            2 * lineInchConvert.f()$V,
            8 * lineInchConvert.f()$V)),
        # Marge de droite :
        lineInchConvert.f()$H * cex * unlist(par("lheight")) *
          ifelse(isSubplot(),
            1, 0.5)
        ) +
        lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
        # Distance du nom d'axe dépendante de la taille de marge gauche :
        mgp = c(tmp2 / lineInchConvert.f()$H -  ifelse(isSubplot(), 1.1, 1.4),
          ifelse(isSubplot(), 0.7, 0.9), 0))

      # Valeur à minimiser :
        return(sum(abs(x - unlist(par("mai")))))
    },
    control = list(abstol = 0.01))    # Tolérance.

  # Plot avec affichage cette fois :
  tmpBP <- boxplot(exprBP, data = data,
    varwidth = TRUE, las = 2,
    col = colors,
    ylim = ylim,
    xaxt = "n",
    main = if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
      main
    }else{
      NULL
    },
    cex.lab = cex,
    cex.axis = cex,
    ...)

  # Ajout de l'axe des abscisses :
  axis(side = 1, at = seq_along(tmpBP$names), labels = FALSE, tick = TRUE)

  # Ajout des labels :
  text(x = seq_along(tmpBP$names),
    y = par("usr")[3] -
    ifelse(isTRUE(getOption("P.graphPDF")), # Coef différent pour les PDFs.
      ifelse(isSubplot(), 0.04, 0.02),
      ifelse(isSubplot(), 0.050, 0.027)) * cex *
    diff(range(par("usr")[3:4])),
    labels = tmpBP$names,
    xpd = TRUE, srt = 45, adj = c(1, 1),
    cex = cex)

  # Stockage des ylim pour les traitements ultérieurs :
  tmpBP$ylim <- ylim

  return(tmpBP)
}


colBoxplot.f <- function(terms, data){

  ## Purpose: Définitions des couleurs pour le facteur de second
  ##          niveau
  ## ----------------------------------------------------------------------
  ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
  ##                    caractères)
  ##            data : le jeu de données (data.frame)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 août 2010, 15:05

  if (length(terms) < 2){
    if (length(terms) == 1){
      col <- PAMPAcolors.f(n = length(unique(na.omit(data[ , terms[1]]))))

      return(col)
    }else{
      return(NULL)
    }
  }else{
    n <- length(terms)

    # Définition des couleurs :
    col <- rep(PAMPAcolors.f(n = length(unique(na.omit(data[ , terms[n - 1]])))),
      each = ifelse(n == 2,
        1,            # Pas de facteur imbriqué.
        prod(sapply(data[ , terms[1:(n-2)], drop = FALSE], # nombres de niveaux du (des) facteur(s)
          function(x){length(unique(na.omit(x)))}))))    # imbriqués.
    return(col)
  }
}


PAMPAcolors.f <- function(n = 1, palette = getOption("P.colPalette"), list = FALSE,
  FUNname = FALSE, cartoOnly = FALSE){

  ## Purpose: retourner n couleurs de la palette "palette".
  ## ----------------------------------------------------------------------
  ## Arguments: n : nombre de couleurs.
  ##            palette : une des palettes de couleurs prédéfinies
  ##            cartoOnly : uniquement les palettes cartographiques (si
  ##                        list == TRUE).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 oct. 2012, 17:08

  if (list){
    return(switch(as.character(cartoOnly),
      "1" = ,
      "TRUE" = {
        mltext(paste0("PAMPAcolors.pal.",
          c("map1", "map2", "topo")))
      },
      mltext(paste0("PAMPAcolors.pal.",
        c("default", "blue", "heat", "grey", "map1", "map2", "topo")))))
  }else{}


  palKey <- c("default", "blue", "heat", "grey", "map1", "map2", "topo")
  names(palKey) <- mltext(paste0("PAMPAcolors.pal.",
    c("default", "blue", "heat", "grey", "map1", "map2", "topo")))

  palette <- if (palette[1] %in% names(palKey)){
    palKey[palette[1]]   # get the hard coded palette name from the translation.
  }else{
    palette[1]
  }

  if (FUNname){
    return(switch(palette,
      "default" = ,
      "defaut" = ,        # Accepts some values by default, even if not in the translation table.
      "défaut" = {".ColorPaletteDefault"},
      "bleu" = ,
      "blue" = {".ColorPaletteBlue"},
      "chaud" = ,
      "heat" = {".ColorPaletteHeat"},
      "grey" = ,
      "gris" = ,
      "gray" = {".ColorPaletteGray"},
      "carto1" = ,
      "map1" = {".ColorPaletteCarto1"},
      "carto2" = ,
      "map2" = {".ColorPaletteCarto2"},
      "topo" = {"topo.colors"}))
  }

  # Creates the palette function if does not exists.
  if (! is.null(PAMPAcolors.f(palette = palette, FUNname = TRUE)) &&
      ! exists(PAMPAcolors.f(palette = palette, FUNname = TRUE), envir = .GlobalEnv))
  {
    makeColorPalettes.f()
  }else{
    if (is.null(PAMPAcolors.f(palette = palette, FUNname = TRUE)))
      stop("Undefined color palette '", palette, "'")
  }

  res <- switch(palette,
    "default" = ,
    "defaut" = ,            # Accepts some values by default, even if not
    "défaut" = {            # in the translation table.
      if (n <= 10){
        .ColorPaletteDefault(10)[1:n]
      }else{
        .ColorPaletteDefault(n)
      }
    },
    "bleu" = ,
    "blue" = {
      if (n <= 10){
        .ColorPaletteBlue(10)[1:n]
      }else{
        .ColorPaletteBlue(n)
      }
    },
    "chaud" = ,
    "heat" = {
      .ColorPaletteHeat(n)
    },
    "grey" = ,
    "gris" = ,
    "gray" = {
      .ColorPaletteGray(n)
    },
    "carto1" = ,
    "map1" = {
      if (n <= 5){
        .ColorPaletteCarto1(5)[1:n]
      }else{
        .ColorPaletteCarto1(n)
      }
    },
    "carto2" = ,
    "map2" = {
      if (n <= 16){
        .ColorPaletteCarto2(16)[1:n]
      }else{
        .ColorPaletteCarto2(n)
      }
    },
    "topo" = {
      if (n <= 5){
        topo.colors(5)[1:n]
      }else{
        topo.colors(n)
      }
    })

  return(res)
}


makeColorPalettes.f <- function(){

  ## Purpose: Créer les palettes de couleurs pour les graphiques
  ## ----------------------------------------------------------------------
  ## Arguments: aucun !
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 oct. 2012, 17:19

  # default:
  assign(".ColorPaletteDefault",
    colorRampPalette(c("#66FFFF", "#9966FF", "#009999", "#CC3399", "#FFCC99",
      "#FFFF99", "#CCFF99", "#CC9900", "#C0504D", "#FF99CC")),
    envir = .GlobalEnv)

  # blue:
  assign(".ColorPaletteBlue",
    colorRampPalette(c("#66FFFF", "#215968", "#33CCCC", "#003366", "#CCFFFF",
      "#009999", "#99CCFF", "#66FFFF", "#215968", "#33CCCC")),
    envir = .GlobalEnv)

  # heat:
  assign(".ColorPaletteHeat",
    colorRampPalette(heat.colors(5)),
     envir = .GlobalEnv)

  # gray:
  assign(".ColorPaletteGray",
    colorRampPalette(c("#787878", "#dddddd")),
    envir = .GlobalEnv)

  # carto1:
  assign(".ColorPaletteCarto1",
    colorRampPalette(c("cyan", "violet", "slateblue", "lightsalmon", "palegreen")),
    envir = .GlobalEnv)

  # carto2:
  assign(".ColorPaletteCarto2",
    colorRampPalette(c("cyan", "violet", "slateblue", "lightsalmon", "palegreen", "darkseagreen4",
      "chocolate1", "slateblue1", "pink", "burlywood1", "hotpink3", "tomato3", "khaki2", "goldenrod",
      "tan1", "violetred3")),
    envir = .GlobalEnv)
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


sepBoxplot.f <- function(terms, data){

  ## Purpose: Calculer les positions des séparateurs (facteur de premier
  ##          niveau).
  ## ----------------------------------------------------------------------
  ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
  ##                    caractères)
  ##            data : le jeu de données (data.frame)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 août 2010, 14:11

  if (length(terms) < 2){

  }else{
    n <- length(terms)
    # Positions :
    pos <- seq(from = 0.5,
      by = prod(sapply(data[ , terms[1:(n-1)], drop = FALSE],
        function(x){
          length(unique(na.omit(x)))
        })),
      length.out = length(unique(na.omit(data[ , terms[n]]))) + 1)
    # Lignes verticales :
    abline(v = pos,
      col = rep(getOption("P.sepGroupesCol"), length(pos)),
      lty = rep(1, length(pos)))
  }
}


legendBoxplot.f <- function(terms, data, pch = 15, pt.cex = 1.2, cex = 0.9){

  ## Purpose: Afficher la légende des couleurs (facteur de second
  ##          niveau)
  ## ----------------------------------------------------------------------
  ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
  ##                    caractères)
  ##            data : le jeu de données (data.frame)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 août 2010, 16:42
  if (length(terms) < 2){

  }else{
    n <- length(terms)

    # Couleurs :
    colors <- unique(colBoxplot.f(terms = terms, data =  data))

    # Noms :
    names <- levels(as.factor(data[ , terms[n - 1]]))

    # Légende :
    legend("topright", names, col = colors,
      pch = pch, pt.cex = pt.cex,
      cex = cex, title = varNames.f(terms[n - 1], "nom"))
  }
}


plotValMoyennes.f <- function(moyennes, objBP, adj = c(0.5, -0.4), cex = 0.9, ...){

  ## Purpose: Affichage des moyennes sur les boxplots en évitant le
  ##          recouvrement avec les lignes des boîtes.
  ## ----------------------------------------------------------------------
  ## Arguments: moyennes : les valeurs de moyennes.
  ##            objBP : un objet retourné par la fonction "boxplot".
  ##            adj : justification.
  ##            cex : taille de la police.
  ##            ... : paramètres optionnels supplémentaires passés à text()
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 oct. 2010, 15:43

  # Propriétés des boîtes à moustaches + points hors boîtes + maximum du graphique :
  pointsOut <- as.list(tapply(objBP$out, objBP$group, function(x) x))
  pointsOut[as.character(which(!is.element(seq(length.out = ncol(objBP$stats)),
    as.numeric(names(pointsOut)))))] <- NA

  x <- rbind(objBP$stats,
    matrix(sapply(pointsOut,
      function(x){
        c(sort(x, na.last = TRUE),
          rep(NA, max(sapply(pointsOut, length)) - length(x)))
      }),
      ncol = length(pointsOut))[ , order(as.numeric(names(pointsOut))), drop = FALSE],
      if (getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1){
        objBP$ylim[2]
      }else{
        max(c(objBP$out, objBP$stats), na.rm = TRUE)
      })

  x[x > objBP$ylim[2] | x < objBP$ylim[1]] <- NA

  x <- apply(x, 2, sort, na.last = TRUE)

  # Proportions occupées par les différentes parties des boites à moustaches :
  xprop <- apply(x, 2,
    function(cln){
      res <- (tail(cln, -1) - head(cln, -1)) / max(cln, na.rm = TRUE)
      if (all(na.omit(res) <= 0)){
        res[3] <- 1
     }else{}

     return(res)
  })

  # Ordre de priorité décroissante des positions où écrire :
  ord <- c(3, 4, 2, seq(from = 5, to = nrow(xprop)), 1)

  # Première position (dans l'ordre décroissant de priorité) remplissant le critère (> 5.5% de la zone graphique) :
  xi <- sapply(seq(length.out = ncol(xprop)), function(i) which(xprop[ord , i] > 0.055)[1])

  # Écriture des valeurs de moyennes sur le graphique :
  text(x = seq_along(xi), y = sapply(seq_along(xi), function(i) x[ord[xi][i], i]),
    labels = as.character(round(moyennes, digits = unlist(options("P.NbDecimal")))),
    col = getOption("P.valMoyenneCol"), adj = adj,
    cex = cex, ...)

}


plotPetitsEffectifs.f <- function(objBP, nbmin = 20){

  ## Purpose: Affichage des warnings sur les graphiques
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 27 oct. 2010, 10:39

  if (any(objBP$n < nbmin & objBP$n > 0, na.rm = TRUE)){
    msg <- c(
      # Affichage d'avertissement pour  > X% du max retiré :
      if (getOption("P.maxExclu") &&  getOption("P.GraphPartMax") < 1){
        paste(mltext("plotPetitsEffectifs.rec", language = getOption("P.lang")),
          " > ", 100 * getOption("P.GraphPartMax"),
          "% ",
          mltext("plotPetitsEffectifs.not.disp", language = getOption("P.lang")),
          sep = "")
      }else{},
      paste(mltext("plotPetitsEffectifs.small.n", language = getOption("P.lang")),
        " (< ", nbmin, ")", sep = ""))

    # "Légende" :
    legend("top",
      msg,
      cex = 0.9, text.col = "red", merge = FALSE, adj = c(0, 0.2),
      pch = rev(c(24, NA)[seq_along(msg)]),
      col = "red3", pt.bg = "gold", pt.cex = 1.2)


    # Propriétés des boîtes à moustaches + points hors boîtes + maximum du graphique :
    pointsOut <- as.list(tapply(objBP$out, objBP$group, function(x)x))
    pointsOut[as.character(which(!is.element(seq(length.out = ncol(objBP$stats)),
      as.numeric(names(pointsOut)))))] <- NA

    x <- rbind(min(c(objBP$out, objBP$stats), na.rm = TRUE),
      objBP$stats,
      matrix(sapply(pointsOut,
        function(x){
          c(sort(x, na.last = TRUE),
            rep(NA, max(sapply(pointsOut, length)) - length(x)))
        }),
        ncol = length(pointsOut))[ , order(as.numeric(names(pointsOut))), drop = FALSE],
      if (getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1){
        objBP$ylim[2]
      }else{
        max(c(objBP$out, objBP$stats), na.rm = TRUE)
      })

    x[x > objBP$ylim[2] | x < objBP$ylim[1]] <- NA

    x <- apply(x, 2, sort, na.last = TRUE)

    # Proportions occupées par les différentes parties des boites à moustaches :
    xprop <- apply(x, 2, function(cln)(tail(cln, -1) - head(cln, -1)) / max(cln, na.rm = TRUE))

    ord <- c(seq(from = nrow(xprop), to = 6), 1, 5, 2, 4, 3) # Ordre de priorité des positions où écrire.

    # Première position (ordre décroissant de priorité) remplissant le critère (> 8% de la zone graphique) :
    xi <- sapply(seq(length.out = ncol(xprop)), function(i){which(xprop[ord , i] > 0.08)[1]})

    idx <- which(objBP$n < nbmin & objBP$n > 0)

    ampli <- max(c(objBP$out, objBP$stats), na.rm = TRUE) - min(c(objBP$out, objBP$stats), na.rm = TRUE)

    invisible(sapply(seq_along(xi)[idx],
      function(i){
        points(x = i,
          y = x[ifelse(ord[xi][i] == 1, 1, ord[xi][i] + 1), i] +
            ifelse(ord[xi][i] == 1, # Ajustement vertical si en bas.
              0.04 * ampli,
              ifelse(ord[xi][i] == nrow(xprop), #... si tout en haut.
                -0.04 * ampli,
                -0.04 * ampli)),
            pch = 24, col = "red3", bg = "gold", cex = 1.2)
      }))

  }else{
    # Affichage d'avertissement pour  > X% du max retiré :
    if (getOption("P.maxExclu")){
      legend("top",
        paste(mltext("plotPetitsEffectifs.rec", language = getOption("P.lang")),
          " > ", 100 * getOption("P.GraphPartMax"),
          "% ",
          mltext("plotPetitsEffectifs.not.disp", language = getOption("P.lang")),
          sep = ""),
        cex = 0.9, col = "red", text.col = "red", merge = FALSE)
    }else{}
  }
}


writeData.f <- function(filename, Data, cols = NULL){

  ## Purpose: Écrire les données des graphiques ou analyses dans des
  ##          fichiers csv (format français).
  ## ----------------------------------------------------------------------
  ## Arguments: filename : nom du fichier à créer.
  ##            Data : jeu de données à sauvegarder.
  ##            cols : colonnes à sauvegarder (toutes si NULL).
  ##            filePathes : chemin des dossiers.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  6 sept. 2012, 11:37

  # Ajout de l'extension si besoin :
  if ( ! grepl("\\.csv$", filename[1], ignore.case = TRUE)){
    filename <- paste(filename, ".csv", sep = "")
  }else{}

  # Si l'argument est une liste de data.frame, elles sont agrégées :
  if (class(Data) == "list"){
    if (all(sapply(Data, class) == "data.frame")){
      Data <- do.call(rbind, Data)
    }else{
      warning("Data saving: programming error!")
    }

  }else{}

  # Colonnes retenues :
  if (is.null(cols)) cols <- colnames(Data)

  cols <- cols[is.element(cols, colnames(Data))]


  # Écriture du fichier
  tryCatch(write.csv(Data[ , cols],
    file = filename,
    row.names = FALSE),
    error = function(e){
      message(mltext("writeData.f.msg"), filename)
     errorLog.f(error = e, niv = -4)
  })
}


infoStats.f <- function(filename, Data, agregLevel = c("species", "unitobs"), type = c("graph", "stat"),
  metrique, factGraph, factGraphSel, listFact, listFactSel, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Écrire les infos et statistique sur les données associées à
  ##          un graphique ou analyse.
  ## ----------------------------------------------------------------------
  ## Arguments: filename : chemin du fichier de résultats.
  ##            Data : données du graphique/de l'analyse.
  ##            agregLevel : niveau d'agrégation de la fonction appelante.
  ##            type : type de fonction appelante (grapique ou analyse).
  ##            metrique : la métrique choisie.
  ##            factGraph : le facteur sélection des espèces.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 sept. 2012, 15:26

  # Ajout de l'extension si besoin :
  if ( ! grepl("\\.stats$", filename[1], ignore.case = TRUE)){
    filename <- paste(filename, ".stats", sep = "")
  }else{}

  # Ouverture du fichier :
  File <- file(description = filename,
    open = "w", encoding = "latin1")

  # Si erreur, on referme le fichier à la sortie de fonction :
  on.exit(if (exists("filename") &&
    tryCatch(isOpen(File),
      error = function(e)return(FALSE))) close(File))

  # Statistiques :
  if (class(Data) == "list"){
    cat("\n###################################################",
      mltext("infoStats.f.1"),
      sep = "", file = File)

    invisible(sapply(1:length(Data),
      function(i){
        printStats.f(Data = Data[[i]], metrique = metrique, listFact = listFact, File = File,
          headline = factGraphSel[i])
    }))
  }else{
    printStats.f(Data = Data, metrique = metrique, listFact = listFact, File = File,
      headline = NULL)
  }

  # Fermeture du fichier :
  close(File)

}


printStats.f <- function(Data, metrique, listFact, File, headline = NULL){

  ## Purpose: Écrire les tableaux de statistiques générales et par
  ##          croisement de facteur dans un fichier.
  ## ----------------------------------------------------------------------
  ## Arguments: Data : les données du graphique/de l'analyse.
  ##            metrique : nom de la métrique.
  ##            listFact : liste des facteurs de regroupement/de l'analyse.
  ##            File : la connection du fichier où écrire.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09

  # Ligne d'en-tête (si besoin : traitement par espèces uniquement) :
  if ( ! is.null(headline)){
    cat("\n", rep("#", nchar(headline) + 3), "\n",
      "## ", headline, "\n",
      sep = "", file = File)
  }else{}

  cat(mltext("printStats.f.1"), file = File)

  capture.output(print(summary.fr(Data[ , metrique])), file = File, append = TRUE)

  if ( ! is.null(listFact)){
    cat("\n#########################################",
      mltext("printStats.f.2"), file = File, sep = "")

      # Calcul du summary pour chaque croisement (existant) de facteur :
      res <- with(Data,
        tapply(eval(parse(text = metrique)),
          INDEX = do.call(paste,
            c(lapply(listFact,
              function(y) eval(parse(text = y))),
              sep = ".")),
          FUN = summary.fr))

      # Assemblage du résultat dans un tableau
      capture.output(print(do.call(rbind, res)), file = File, append = TRUE)
  }else{}

  # Ligne vide (pour l'esthétique) :
  cat("\n", file = File)
}


summary.fr <- function(object, digits = max(3, getOption("digits") - 3), ...){

  ## Purpose: Franciser les sorties d'un summary (numeric uniquement).
  ## ----------------------------------------------------------------------
  ## Arguments: object : objet à résumer.
  ##            ... : argument supplémentaires passés à summary().
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47

  if ( ! is.numeric(object)) stop("Programming error")

  # Calcul du résumé :
  res <- c(summary(object = object, digits, ...),
    "sd" = signif(sd(x = object), digits= digits), "N" = length(object))

  # Changement des noms d'éléments :
  names(res) <- c(mltext("summary.min"), mltext("summary.1stQ"),
    mltext("summary.med"), mltext("summary.mean"),
    mltext("summary.3rdQ"), mltext("summary.max"),
    mltext("summary.sd"), mltext("summary.N"))

  return(res)
}


selectModalites.f <- function(tableMetrique, facts = NULL, selections = NULL,
  metrique = NULL, nextStep, dataEnv, level = 0){

  ## Purpose: Sélection et stockage des modalités d'un facteur
  ## ----------------------------------------------------------------------
  ## Arguments: factor : le nom du facteur sélectionné.
  ##            tableMetrique : nom de la table des métriques.
  ##            nextStep : étape suivante.                     [!!!] on devrait pouvoir s'en passer  [yr: 18/1/2012]
  ##            level : l'ordre du facteur (0 pour celui de séparation des
  ##                    graphiques, 1, 2,... pour les suivants).
  ##            env : environnement de la fonction appelante.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 août 2010, 14:21

  factGraphAliases <- spRefFields.aliases(site = getOption("P.MPA"), dataEnv = dataEnv,
    ordered = TRUE, tableMetrique = tableMetrique, nextStep = nextStep)

  listFacteursAliases <- refTablesFields.aliases(nomTable = tableMetrique, dataEnv = dataEnv,
    nextStep = nextStep)

  preselect <- NULL                   # sélections persistantes d'une fois sur l'autre
  if (!is.na(selections[level + 1])){
    preselect <- selections[[level + 1]]
  }

  # Pour les indices de biodiversité recalculés, il faut utiliser "unitSp" et une métrique adaptée.
  if (is.element(nextStep, c("boxplot.unitobs", "modele_lineaire.unitobs",
      "MRT.unitobs", "barplot.unitobs", "spBarBoxplot.unitobs", "spSymbols.unitobs")) &&
      tableMetrique == "unit")
  {
    tableMetrique <- "unitSp"
    metrique <- getOption("P.nbName")
  }else{}

  tmp <- subsetToutesTables.f(metrique = metrique, facteurs = facts, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique , exclude = level + 1)


  return(tmp)
}

updateFactGraph.f <- function(nomTable, env){

  ## Purpose: MàJ des champs dans la combobox de choix du facteur de
  ##          séparation des graphiques. Si la table d'origine (référentiel
  ##          d'espèces ou unitobs) a changé, la sélection est
  ##          réinitialisée.
  ## ----------------------------------------------------------------------
  ## Arguments: nomTable : le nom de la table actuellement sélectionnée.
  ##            env : l'environnement de la fonction appelante.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  3 août 2010, 14:01

  switch(nomTable,
    # Si table "référentiel d'espèce" :
    refesp = {
      evalq({
        factGraphAliases <- spRefFields.aliases(site = getOption("P.MPA"),
          dataEnv = dataEnv, ordered = TRUE,
          tableMetrique = tcltk::tclvalue(TableMetrique),
          nextStep = nextStep)
        tcltk::tkconfigure(CB.factGraph,
          value = names(factGraphAliases))
        }, envir = env)

      evalq(
        if (!is.element(tcltk::tclvalue(FacteurGraph), names(factGraphAliases))){
          tcltk::tclvalue(FacteurGraph) <- "" # réinitialisation de la sélection.
        }, envir = env)
    },
    # Si table des "unités d'observation" :
    unitobs = {
      evalq({
        factGraphAliases <- UnitobsFields.aliases(dataEnv = dataEnv, ordered = TRUE,
          tableMetrique = tcltk::tclvalue(TableMetrique))
        tcltk::tkconfigure(CB.factGraph, value = names(factGraphAliases))
        }, envir = env)

      evalq(
        if (!is.element(tcltk::tclvalue(FacteurGraph), names(factGraphAliases))){
          tcltk::tclvalue(FacteurGraph) <- "" # réinitialisation de la sélection.
        }, envir = env)
    },
    # Comportement par défaut (pas de table sélectionnée) :
    evalq(tcltk::tkconfigure(CB.factGraph, value = ""), envir = env)
  )
}


refTablesFields.aliases <- function(nomTable, dataEnv, nextStep=NA){

  ## Purpose: Retourne la liste des facteurs pertinents en fonction de la
  ##          table de métriques retenue.
  ## ----------------------------------------------------------------------
  ## Arguments: nomTable : nom de la table de métriques (chaîne de
  ##                       charactère)
  ##            nextStep : étape suivante, pas obligatoire.
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 août 2010, 10:18

  if (!is.character(nomTable)){
    stop("'nomTable' must be a character string!")
  }else{
    # Champs principaux :
    cPrincip <- aliases(c(
      # table "unitobs" :
      "year", "protection.status", "site", "annee.campagne", "biotop",
      "geogr.descriptor1", "geogr.descriptor2",
      # table "especes" :
      "species.code", "benthic.categ", "family", "genus", "scient.name",
      "CategB_general", "CategB_groupe" # [ml?]
    ),
    reverse = TRUE)

    # Champs des unités d'observation :
    cUnitobs <- UnitobsFields.aliases(dataEnv=dataEnv)
    cUnitobs <- cUnitobs[as.logical(nchar(cUnitobs))] # remove empty field

    # Champs du référentiel espèces :
    if (length(grep("\\.unitobs$", nextStep)) > 0){ # [!!!] Dangereux  [yr: 18/1/2012]
      cEspeces <- NA              # les champs du ref espèce n'ont pas de raison d'apparaitre dans
      # les cas de tables agrégées par unitobs.
    }else{
      cEspeces <- spRefFields.aliases(site=getOption("P.MPA"), dataEnv=dataEnv)
      cEspeces <- cEspeces[as.logical(nchar(cEspeces))] # remove empty field
    }

    casTables <- c("unitSp"="unitSp",
      "TablePresAbs"="unitSp",
      "TableOccurrences"="unitSp",
      "unitSpSz"="unitSpSz",
      "unit"="unit")

    switch(casTables[nomTable],
      unit={
        res <- cUnitobs[order(names(cUnitobs))]
      },
      unitSp=,
      unitSpSz={
        res <- c(cUnitobs, cEspeces)
        res <- res[order(names(res))]
      },
      {
        warning("Table de métrique '", nomTable, "' inconnu")

        res <- c(cUnitobs, cEspeces)
        res <- res[order(names(res))]
    })

    return(c(
      if (casTables[nomTable] == "unitSpSz"){
        aliases(c("", "size.class", ""), reverse = TRUE)
      }else{
        ""
      },
      cPrincip[is.element(cPrincip, res)],   # champs principaux...
      "", res[!is.element(res, cPrincip)])) # autres.

  }
}


spRefFields.aliases <- function(site, dataEnv, ordered = FALSE, tableMetrique = "", nextStep = NA){

  ## Purpose: Retourne la liste des champs du référentiel espèces après
  ##          avoir supprimé ceux ne correspondant pas au site étudié ainsi
  ##          que les champs vides.
  ## ----------------------------------------------------------------------
  ## Arguments: site : le site étudié (chaîne de caractères).
  ##            ordered : faire apparaître les champs principaux en
  ##                      premiers ? (booléen, optionnel)
  ##            tableMetrique : nom de la table de métriques (pour pouvoir
  ##                            ajouter le champ classe de taille, même
  ##                            si ne fait pas partie de cette table).
  ##            nextStep : étape suivante.
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  3 août 2010, 11:16

  # Récupération des données (référentiel espèce et observations) :
  refesp <- get("refesp", envir = dataEnv)
  obs <- get("obs", envir = dataEnv)

  # Champs principaux (externaliser par la suite) :
  if (is.element(tableMetrique, c("unitSp", "TableOccurrences", "unitSpSz")) &&
    is.element(nextStep,
      c("boxplot.esp", "modele_lineaire", "freq_occurrence", "MRT.esp", "barplot.esp")))
  {
    cPrincip <- aliases(c("species.code", "species", "scient.name"), reverse = TRUE)
  }else{
    cPrincip <- aliases(c(
      # table "refesp" :
      "species.code", "benthic.categ", "family", "genus", "scient.name",
      "CategB_general", "CategB_groupe"
    ), reverse = TRUE)
  }

  # Externaliser la définition des sites par la suite...
  listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")

  # Noms des sites dont on doit exclure les colonnes :
  sitesExclus <- listeSite[ ! grepl(pattern = paste("^(",
    paste(site, collapse = "|"),
    ")$", sep = ""), x = listeSite)]

  # champs ne correspondant pas au motif "(Site1|Site2|...)$" :
  champs <- sort(colnames(refesp))[! grepl(paste("(",
    paste(sitesExclus, collapse = "|"),
    ")$", sep = ""), sort(colnames(refesp)))]

  # Champs non-vides de la table 'espèces' :
  res <- aliases(champs[sapply(champs,
    function(i){
      !all(is.na(refesp[is.element(refesp$species.code,
        obs$species.code) , i]))
    })], reverse = TRUE)

  res <- res[order(names(res))]

  # Champs principaux en premiers :
  if (ordered){
    res <- c(
      if (tableMetrique == "unitSpSz"){
        aliases(c("size.class", ""), reverse = TRUE)
      }else{
        ""
      },
      cPrincip[is.element(cPrincip, res)],
      "", res[!is.element(res, cPrincip)])
  }else{
    res <- c(
      if (tableMetrique == "unitSpSz"){
        aliases(c("size.class", ""), reverse = TRUE)
      }else{
        ""
      },
      res)
  }

  return(res)
}


UnitobsFields.aliases <- function(dataEnv, ordered = FALSE, tableMetrique = ""){

  ## Purpose: Retourne la liste des champs du référentiel d'unités
  ##          d'observations après avoir supprimé les champs vides.
  ## ----------------------------------------------------------------------
  ## Arguments: ordered : faire apparaître les champs principaux en
  ##                      premiers ? (booléen, optionnel)
  ##            tableMetrique : nom de la table de métriques (pour pouvoir
  ##                            ajouter le champ classe de taille, même
  ##                            si ne fait pas partie de cette table).
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  3 août 2010, 11:12

  # Récupération des données :
  unitobs <- get("unitobs", envir = dataEnv)

  # Champs principaux :
  cPrincip <- aliases(c(
    # table "unitobs" :
    "year", "protection.status", "annee.campagne", "site", "biotop",
    "geogr.descriptor1", "geogr.descriptor2"
  ), reverse = TRUE)

  # Champs non-vides de la table 'unitobs' :
  res <- aliases(names(unitobs)[
    sapply(names(unitobs)[!is.na(names(unitobs))],
      function(i){
        !all(is.na(unitobs[ , i]))
    })], reverse = TRUE)

  res <- res[order(names(res))]       # in alphabetical order anyway.

  # Champs principaux en premiers :
  if (ordered){
    res <- c(
      if (tableMetrique == "unitSpSz"){
        aliases(c("size.class", ""), reverse = TRUE)
      }else{
        ""
      },
      cPrincip[is.element(cPrincip, res)],
      "", res[!is.element(res, cPrincip)])
  }else{
    res <- c(
      if (tableMetrique == "unitSpSz"){
        aliases(c("size.class", ""), reverse = TRUE)
      }else{
        ""
      },
      res)
  }

  return(res)
}


#' @title Boxplot (donnees agregees par unites d'observation)
#'
#' @description Cette fonction permet la creation de boxplot pour les donnees agregees par groupes
#' d'especes
#'
#' @param metrique : chr, metrique choisie
#' @param factGraph : chr, facteur de selection des especes
#' @param factGraphSel : chr, selection de modalites pour le facteur de selection des especes
#' @param listFact : chr, facteur(s) de regroupement
#' @param listFactSel : list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique : chr, nom de la table de metriques
#' @param dataEnv : environnement de stockage des donnees
#' @param baseEnv : environnement parent
#'
#' @noRd

WP2boxplot.unitobs.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire les boxplots en tenant compte des options graphiques
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
  ## Author: Yves Reecht, Date:  6 août 2010, 16:34

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

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

  # Formule du boxplot
  exprBP <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))

  # Identification des différents modalités (espèces) du graphique à générer :
  if (factGraph == ""){                 # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){        # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                              # Modalités sélectionnées (et présentes parmi les données retenues).
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
    warning(mltext("WP2boxplot.W.n.1"), " (",
      paste(iFactGraphSel, collapse = ", "), ") < ", getOption("P.MinNbObs"),
      mltext("WP2boxplot.W.n.2"))
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
        typeGraph = "boxplot")
    } else{
      graphFile <- resFileGraph.f(
        metrique = metrique,
        factGraph = factGraph,
        modSel = iFactGraphSel,
        listFact = listFact,
        dataEnv = dataEnv,
        ext = "wmf",
        prefix = "boxplot",
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
      graphType = "boxplot")

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
          ""),
        sep = "")),
      "")

    # Boxplot !
    tmpBP <- boxplotPAMPA.f(exprBP, data = tmpData,
      main = mainTitle, ylab = ylab)  # Capitalize.f(varNames[metrique, "nom"]),


    # #################### Informations supplémentaires sur les graphiques ####################

    # Séparateurs de facteur de premier niveau :
    if (getOption("P.sepGroupes")){
      sepBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = tmpData)
    }

    # Séparateurs par défaut :
    abline(v = 0.5+(0:length(tmpBP$names)) , col = "lightgray", lty = "dotted") # Séparations.

    # Légende des couleurs (facteur de second niveau) :
    if (getOption("P.legendeCouleurs")){
      legendBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = tmpData)
    }else{}

    # Moyennes :
    Moyenne <- as.vector(tapply(X = tmpData[, metrique], # moyenne par groupe.
      INDEX = as.list(tmpData[ , attr(terms(exprBP), "term.labels"), drop = FALSE]),
      FUN = mean, na.rm = TRUE))

    # ... points :
    if (getOption("P.pointMoyenne")){
      points(Moyenne,
        pch = getOption("P.pointMoyennePch"),
        col = getOption("P.pointMoyenneCol"), lwd = 2.5,
        cex = getOption("P.pointMoyenneCex"))
    }else{}

    # ... valeurs :
    if (getOption("P.valMoyenne")){
      plotValMoyennes.f(moyennes = Moyenne, objBP = tmpBP)
    }else{}

    if (isTRUE(getOption("P.warnings"))){
      # Avertissement pour les petits effectifs :
      plotPetitsEffectifs.f(objBP = tmpBP, nbmin = 5)
    }else{}

    # Nombres d'observations :
    if (getOption("P.NbObs")){
      nbObs <- tmpBP$n # Retourné par la fonction 'boxplot'

      # Nombres sur l'axe supérieur :
      axis(3, as.vector(nbObs), at = 1:length(as.vector(nbObs)),
        col.ticks = getOption("P.NbObsCol"), col.axis = getOption("P.NbObsCol"),
        lty = 2, lwd = 0.5,
        mgp = c(2, 0.5, 0))

      legend("topleft", mltext("WP2boxplot.n.rec", language = getOption("P.lang")),
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


agregationTableParCritere.f <- function(Data, metrique, facteurs, dataEnv, listFact = NULL,
  nbName = "number"){

  ## Purpose: Agréger les données selon un ou plusieurs facteurs.
  ## ----------------------------------------------------------------------
  ## Arguments: Data : Le jeu de données à agréger.
  ##            metrique : la métrique agrégée.
  ##            facteurs : les facteurs
  ##            listFact : noms des facteurs supplémentaires (agrégés et
  ##                       ajoutés à la table de sortie).
  ##            dataEnv : l'environnement des données.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: une data.frame agrégée.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47

  # traitements selon le type de métrique :
  casMetrique <- c("number" = "sum",
    "mean.length" = "w.mean",
    "taille_moy" = "w.mean",
    "biomass" = "sum",
    "Biomass" = "sum",
    "weight" = "sum",
    "mean.weight" = "w.mean",
    "density" = "sum",
    "Density" = "sum",
    "CPUE" = "sum",
    "CPUE.biomass" = "sum",
    "pres.abs" = "pres",
    "abundance.prop.SC" = "w.mean.prop", # Pas bon [!!!]
    "biomass.prop.SC" = "w.mean.prop.bio",  # Pas bon [!!!]
    # Benthos :
    "colonies" = "sum",
    "coverage" = "sum",
    "mean.size.colonies" = "w.mean.colonies",
    # SVR (expérimental) :
    "number.max" = "nbMax",
    "number.sd" = "nbSD",
    "density.max" = "densMax",
    "density.sd" = "densSD",
    "biomass.max" = "sum",
    "spawning.success" = "%.nesting",
    "spawnings" = "sum",
    "readable.tracks" = "sum",
    "tracks.number" = "sum")

  # Ajout de "readable.tracks" pour le pourcentage de ponte :
  if (any(casMetrique[metrique] == "%.nesting")){
    if (is.element("size.class", colnames(Data))){
      unitSpSz <- get("unitSpSz", envir = dataEnv)

      if (is.null(unitSpSz)) stop("unitSpSz must be defined")

      Data <- merge(Data,
        unitSpSz[ , c("species.code", "observation.unit", "size.class", "readable.tracks")],
        by = c("species.code", "observation.unit", "size.class"),
        suffixes = c("", ".y"))
    }else{
      unitSp <- get("unitSp", envir = dataEnv)

      if (is.null(unitSp)) stop("unitSp must be defined")

      Data <- merge(Data,
        unitSp[ , c("species.code", "observation.unit", "readable.tracks")],
        by = c("species.code", "observation.unit"),
        suffixes = c("", ".y"))
    }
  }else{}

  # Ajout du champ nombre pour le calcul des moyennes pondérées s'il est absent :
  if ((casMetrique[metrique] == "w.mean" || casMetrique[metrique] == "w.mean.prop")){
    if (is.element("size.class", colnames(Data))){
      unitSpSz <- get("unitSpSz", envir = dataEnv)

      Data <- merge(Data,
        unitSpSz[ , c("species.code", "observation.unit", "size.class", nbName)],
        by = c("species.code", "observation.unit", "size.class"))

      # Ajout de l'abondance totale /espèce/unité d'observation :
      nbTot <- tapply(unitSpSz[ , nbName],
        as.list(unitSpSz[ , c("species.code", "observation.unit")]),
        sum, na.rm = TRUE)

      Data <- merge(Data,
        as.data.frame(as.table(nbTot), responseName = "nombre.tot"))
    }else{
      Data <- merge(Data,
        get("unitSp", envir = dataEnv)[ , c("species.code", "observation.unit", nbName)],
        by = c("species.code", "observation.unit"))
    }
  }else{}

  # Ajout du champ biomasse pour les proportions de biomasses par classe de taille :
  if (casMetrique[metrique] == "w.mean.prop.bio"){
    unitSpSz <- get("unitSpSz", envir = dataEnv)

    biomass <- colnames(unitSpSz)[is.element(colnames(unitSpSz), c("biomass", "CPUE.biomass"))][1]

    Data <- merge(Data,
      unitSpSz[ , c("species.code", "observation.unit", "size.class", biomass)],
      by = c("species.code", "observation.unit", "size.class"))

    # Ajout de la biomasse totale /espèce/unité d'observation :
    biomTot <- tapply(unitSpSz[ , biomass],
      as.list(unitSpSz[ , c("species.code", "observation.unit")]),
      function(x){
        ifelse(all(is.na(x)),
          NA,
          sum(x, na.rm = TRUE))
    })

    Data <- merge(Data, as.data.frame(as.table(biomTot), responseName = "tot.biomass"))
  }

  # Ajout du champ colonie pour le calcul des moyennes pondérées s'il est absent :
  if (casMetrique[metrique] == "w.mean.colonies" && ! is.element("colonies", colnames(Data))){
    unitSp <- get("unitSp", envir = dataEnv)

    Data$colonies <- unitSp$colonies[match(
      apply(Data[ , c("species.code", "observation.unit")], 1, paste, collapse = "*"),
      apply(unitSp[ , c("species.code", "observation.unit")], 1, paste, collapse = "*"))]
  }else{}

  # Agrégation de la métrique selon les facteurs :
  switch(casMetrique[metrique],
    "sum" = {
      res <- tapply(Data[ , metrique],
        as.list(Data[ , facteurs, drop = FALSE]),
        function(x){
          ifelse(all(is.na(x)),
            NA,
            sum(x, na.rm = TRUE))
      })
    },
    "w.mean" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteurs, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metrique])),
            NA,
            weighted.mean(Data[ii, metrique], Data[ii, nbName], na.rm = TRUE))
      })
    },
    "w.mean.colonies" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteurs, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metrique])),
            NA,
            weighted.mean(Data[ii, metrique],
              Data[ii, "colonies"],
              na.rm = TRUE))
      })
    },
    "w.mean.prop" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteurs, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "nombre.tot"], na.rm = TRUE) == 0,
            NA,
            ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
              0,
              (sum(Data[ii, nbName][ !is.na(Data[ii, metrique])], na.rm = TRUE) /
                sum(Data[ii, "nombre.tot"], na.rm = TRUE)) *
                # Correction si la classe de taille n'est pas un facteur d'agrégation
                # (sinon valeur divisée par le nombre de classes présentes) :
                ifelse(is.element("size.class", facteurs),
                  100,
                  100 * length(unique(Data$size.class)))))
      })
    },
    "w.mean.prop.bio" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteurs, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "tot.biomass"], na.rm = TRUE) == 0,
            NA,
            ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
              0,
              (sum(Data[ii, biomass][ !is.na(Data[ii, metrique])], na.rm = TRUE) /
                sum(Data[ii, "tot.biomass"], na.rm = TRUE)) *
                # Correction si la classe de taille n'est pas un facteur d'agrégation
                # (sinon valeur divisée par le nombre de classes présentes) :
                ifelse(is.element("size.class", facteurs),
                  100,
                  100 * length(unique(Data$size.class)))))
      })

    },
    "pres" = {
      res <- tapply(Data[ , metrique],
        as.list(Data[ , facteurs, drop = FALSE]),
        function(x){
          ifelse(all(is.na(x)), # Cas où il n'y a que des NAs.
            NA,
            ifelse(any(x > 0, na.rm = TRUE), # Sinon...
              1,                           # ...présence si au moins une observation dans le groupe.
              0))
                         })
    },
    "nbMax" = {
      # Récupération des nombres brutes avec sélections :
      nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      nbTmp2 <- apply(nbTmp,
        which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
        })

      # Somme par croisement de facteur :
      res <- as.array(apply(nbTmp2,
        which(is.element(names(dimnames(nbTmp)), facteurs)),
        function(x){
          ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
        }))
    },
    "nbSD" = {
      # Récupération des nombres brutes avec sélections :
      nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      nbTmp2 <- apply(nbTmp,
        which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
        })

      # Somme par croisement de facteur :
      res <- as.array(apply(nbTmp2,
        which(is.element(names(dimnames(nbTmp)), facteurs)),
        function(x){
          ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
        }))
    },
    "densMax" = {
      # Récupération des nombres brutes avec sélections :
      densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      densTmp2 <- apply(densTmp,
        which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
        })

      # Somme par croisement de facteur :
      res <- as.array(apply(densTmp2,
        which(is.element(names(dimnames(densTmp)), facteurs)),
        function(x){
          ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
        }))
    },
    "densSD" = {
      # Récupération des nombres brutes avec sélections :
      densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      densTmp2 <- apply(densTmp,
        which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
        })

      # Somme par croisement de facteur :
      res <- as.array(apply(densTmp2,
        which(is.element(names(dimnames(densTmp)), facteurs)),
        function(x){
          ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
        }))
    },
    "%.nesting" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteurs, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metrique])),
            NA,
            weighted.mean(Data[ii, metrique],
              Data[ii, "readable.tracks"],
              na.rm = TRUE))
        })
    },
    stop("Operation not implemented!")
  )

  # Nom des dimensions
  names(dimnames(res)) <- c(facteurs)

  # Transformation vers format long :
  reslong <- as.data.frame(as.table(res), responseName = metrique)
  reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

  # Agrégation et ajout des facteurs supplémentaires :
  if (!is.null(listFact)){
    reslong <- cbind(reslong, sapply(Data[ , listFact, drop = FALSE],
      function(fact){
        tapply(fact, as.list(Data[ , facteurs, drop = FALSE]),
          function(x){
            if (length(x) > 1 && length(unique(x)) > 1){ # On doit n'avoir qu'une seule modalité...
              return(NULL)                               # ...sinon on retourne NULL
            }else{
              unique(as.character(x))
            }
          })
      }))
  }else{}

  # Si certains facteurs ne sont pas de classe facteur, il faut les remettre dans leur classe d'origine :
  if (any(tmp <- sapply(reslong[ , listFact, drop = FALSE], class)
      != sapply(Data[ , listFact, drop = FALSE], class)))
    {
    for (i in which(tmp)){
      switch(sapply(Data[ , listFact, drop = FALSE], class)[i],
        "integer" = {
          reslong[ , listFact[i]] <- as.integer(as.character(reslong[ , listFact[i]]))
        },
        "numeric" = {
          reslong[ , listFact[i]] <- as.numeric(as.character(reslong[ , listFact[i]]))
        },
        reslong[ , listFact[i]] <- eval(call(paste(
          "as", sapply(Data[ , listFact, drop = FALSE], class)[i], sep = "."),
          reslong[ , listFact[i]]))
      )
    }
  }else{}

  # Rétablir l'ordre initial des nivaux de facteurs :
  reslong <- as.data.frame(sapply(colnames(reslong),
    function(x){
      if (is.factor(reslong[ , x])){
        return(factor(reslong[ , x], levels = levels(Data[ , x])))
      }else{
        return(reslong[ , x])
      }
    }, simplify = FALSE))

  # Vérification des facteurs supplémentaires agrégés.
  # Il ne doit pas y avoir d'élément nul (la fonction précédente renvoie NULL si plusieurs
  # niveaux de facteurs, i.e. le facteur est un sous ensemble d'un des facteurs d'agrégation
  # des observations) :
  if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))}))){
    warning(paste(mltext("agregationTableParCritere.f.W.1"),
      mltext("agregationTableParCritere.f.W.2"), sep = ""))
    return(NULL)
  }else{
    return(reslong)
  }
}


MetricsField.aliases <- function(nomTable, nextStep, dataEnv){

  ## Purpose: Retourne la liste des champs pouvant être utilisés comme
  ##          métriques en fonction du nom de table
  ## ----------------------------------------------------------------------
  ## Arguments: nomTable : nom de la table dans laquelle doivent être
  ##                       cherchées les métriques disponibles (chaine de
  ##                       caractères).
  ##            nextStep : identifiant de l'étape suivante.
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 11 ao?t 2010, 16:14

  # On récupère la table de données :
  assign(nomTable, get(nomTable, envir = dataEnv), envir = environment())

  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  if (!is.character(nomTable)){
    stop("'nomTable' must be a character string!")
  }else{
    switch(nomTable,
      # Table unitSp (métriques d'observation) :
      unitSp = {
        res <- aliases(colnames(unitSp)[
          sapply(unitSp, function(x){
            is.numeric(x) & !all(is.na(x))
          }) &
          !is.element(colnames(unitSp), c("year",
            has.no.pres.abs(nextStep, nomTable, dataEnv = dataEnv),
            ifelse(is.benthos.f(),
              "number",
              "")))],
          reverse = TRUE)

        res <- res[order(names(res))]
      },
      # Table unitSpSz (métriques d'observation par classes de taille) :
      unitSpSz = {
        res <- aliases(colnames(unitSpSz)[
          sapply(unitSpSz, function(x){
            is.numeric(x) & !all(is.na(x))
          }) &
          !is.element(colnames(unitSpSz), c("year",
            has.no.pres.abs(nextStep, nomTable, dataEnv = dataEnv),
            "longitude", "latitude"))],
          reverse = TRUE)

        res <- res[order(names(res))]
      },
      # Table unit (indices de biodiversité) :
      unit = {
        columns <- c("species.richness", "simpson", "simpson.l", "pielou", "hill",
          "Delta", "DeltaStar", "LambdaPlus", "DeltaPlus", "SDeltaPlus",
          grep("relative.SR", colnames(eval(parse(text = nomTable))), value = TRUE))

        columnsAls <- aliases(columns, reverse = TRUE) # with aliases as names

        validCols <- colnames(unit)[
          sapply(unit, function(x){
            is.numeric(x) & !all(is.na(x))
          }) &
          is.element(colnames(unit), columns)]

        res <- columnsAls[columnsAls %in% validCols]

      },
      # Autres cas :
      {
        res <- aliases(colnames(eval(parse(text = nomTable)))[
          sapply(eval(parse(text = nomTable)), function(x){
            is.numeric(x) & !all(is.na(x))
          }) &
          colnames(unitSp) != "year"],
          reverse = TRUE)

        res <- res[order(names(res))]
      }
    )
    return(res[!is.element(res, c(colnames(refesp), colnames(unitobs)))])
  }
}
