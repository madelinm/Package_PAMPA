#' Fonction pour tracer un barplot de frequences d'occurrence.
#'
#' \code{agregation} peut prendre la valeur 'espece' ou 'unitobs'. Si 'espece', l'agregation se fera
#' par especes, si unitobs, elle se fera par groupe d'especes.
#'
#' \code{factGraphSel} peut prendre differentes valeurs en fonction de celle de \code{factGraph}. Ce
#' parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' \code{listFact} ne peut pas avoir plus de 2 facteurs.
#'
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s) de
#' regroupement selectionne(s) seront prises en compte.


#' @title Frequences d'occurrence
#'
#' @description Creation d'un barplot de frequence d'occurrence selon les parametres fournis
#'
#' @param agregation chr, type d'agregation, par espece (espece) ou par groupe d'espece (unitobs)
#' @param factGraph chr, facteur de separation des graphique.
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite selectionnees pour le(s) facteur(s) de regroupement
#' @param new_window bool, affichage du graphique dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' freq_occurrence.f(
#'   agregation = "espece",
#'   factGraph = "species.code",
#'   factGraphSel = "Acanoliv",
#'   listFact = c("year", "protection.status"),
#'   listFactSel = NA,
#'   new_window = TRUE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export
freq_occurrence.f <- function(agregation, factGraph = NULL, factGraphSel = NA, listFact,
  listFactSel = NA, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  nextStep <- switch(agregation,
    "espece" = "freq_occurrence",
    "unitobs" = "freq_occurrence.unitobs",
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes)."
    )
  )
  if (is.null(factGraph)){
    factGraph <- ""
  }

  # Verification des parametres
  # Check of the parameters
  tableMetrique = "TablePresAbs"
  metrique = "pres.abs"

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
  if (agregation == "espece"){
    barplotOccurrence.f(factGraph, factGraphSel, listFact, listFactSel, new_window, dataEnv, baseEnv)
  }
  else{
    barplotOccurrence.unitobs.f(factGraph, factGraphSel, listFact, listFactSel, new_window, dataEnv, baseEnv)
  }
}


#' @title Frequences d'occurrence (donnees agregees par especes)
#'
#' @description Cette fonction permet la creation du barplot des frequences d'occurrence.
#'
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection de modalite du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @noRd

barplotOccurrence.f <- function(factGraph, factGraphSel, listFact, listFactSel, new_window = TRUE,
  dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: création des barplots d'après les sélections de facteurs et
  ##          modalités.
  ## ----------------------------------------------------------------------
  ## Arguments: factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s).
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface principale.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 14 oct. 2010, 10:51

  metrique <- "occurrence.frequency"

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation de leurs listes de modalités sélectionnées

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = "pres.abs", facteurs = facteurs, selections = selections,
    dataEnv = dataEnv, tableMetrique = "TablePresAbs", exclude = NULL)

  # Identification des différents graphiques à générer:
  if (factGraph == ""){               # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){      # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                    # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list()

  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :
  for (modGraphSel in iFactGraphSel){
    # Option graphique :
    cex <- getOption("P.cex")

    # Préparation des données pour un graphique :
    if (modGraphSel == ""){         # ...si pas de facteur de séparation des graphiques
      tmpDataMod <- tmpData
    }else{                          # ...sinon.
      tmpDataMod <- subset(tmpData, tmpData[ , factGraph] == modGraphSel) # Subset des données pour la modalité.
    }

    # Passage au graphique suivant si le nombre d'observations < au minimum défini dans les options.
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
      graphFileTmp <- openDevice.f(noGraph = which(modGraphSel == iFactGraphSel),
        metrique = metrique,
        factGraph = factGraph,
        modSel = if (getOption("P.plusieursGraphPage")){
          iFactGraphSel      # toutes les modalités.
        }else{
          modGraphSel        # la modalité courante uniquement.
        },
        listFact = listFact,
        dataEnv = dataEnv,
        type = "espece",
        typeGraph = "barplot")
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
        prefix = "barplot",
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = "espece")
    }

    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
      mainTitle <- graphTitle.f(metrique = metrique,
        modGraphSel = modGraphSel,
        factGraph = factGraph,
        listFact = listFact,
        type = "espece",
        graphType = "occFrequency")
    }else{
      mainTitle <- NULL
    }

    # Calcul des fréquences :
    heights <- with(tmpDataMod,
      tapply(pres.abs, lapply(listFact, function(y)eval(parse(text = y))),
        function(x){
          100 * sum(x, na.rm = TRUE) / length(na.omit(x))
    }))

    # Paramètres graphiques :
    # Marge dynamiques (adaptation à la longueur des labels) :
    optim(par = unlist(par("mai")),   # Le rapport inch/ligne est modifié en changeant les marges
      # => besoin de l'optimiser.
      fn = function(x){
        par(mai = c(
          # Marge du bas :
          lineInchConvert.f()$V * cex * unlist(par("lheight")) * 4.5,
          # Marge de gauche dynamique :
          tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) * (1.4 + 0.4 + 0.9) + # marge
            # supplémentaire.
            max(strDimRotation.f(as.graphicsAnnot(pretty(range(heights, na.rm = TRUE))),
              srt = 0,
              unit = "inches",
              cex = cex)$width, na.rm = TRUE)) > 0.7 * unlist(par("pin"))[1],
              0.7 * unlist(par("pin"))[1],
              tmp),
          # Marge supérieure augmentée s'il y a un titre :
          ifelse(isTRUE(getOption("P.graphPaper")) || (! isTRUE(getOption("P.title"))) ,
            3 * lineInchConvert.f()$V,
            8 * lineInchConvert.f()$V),
          # Marge de droite :
          lineInchConvert.f()$H * cex * unlist(par("lheight")) * 7) +
        lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
        # Distance du nom d'axe dépendante de la taille de marge gauche :
        mgp = c(tmp2 / lineInchConvert.f()$H - 1.4, 0.9, 0))

        # Valeur à minimiser :
        return(sum(abs(x - unlist(par("mai")))))
      },
      control = list(abstol = 0.01))    # Tolérance.

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
          ""),
        sep = "")),
      "")

    barPlotTmp <- barplot(heights,
      beside = TRUE,
      main = mainTitle,
      xlab = "",
      ylab = ylab,
      ylim = c(0, 1.1 * max(heights, na.rm = TRUE)),
      las = 1,
      col = PAMPAcolors.f(n = nrow(heights)),
      cex.lab = cex,
      cex.axis = cex,
      legend.text = ifelse(length(listFact) > 1, TRUE, FALSE),
      args.legend = list("x" = "topright", "inset"= -0.08, "xpd" = NA,
        "title" = Capitalize.f(varNames[listFact[1], "nom"])))

    if (getOption("P.axesLabels")){
      mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
        side = 1, line = 2.3, cex = cex)
    }else{}

    if (getOption("P.NbObs")){
      # Nombre d'"observations" :
      nbObs <- with(tmpDataMod,
        tapply(pres.abs,
          lapply(listFact, function(y)eval(parse(text = y))),
          function(x){
            length(na.omit(x))
      }))

      # Nombres sur l'axe supérieur :
      mtext(nbObs, side = 3, at = barPlotTmp, las = 2, col = getOption("P.NbObsCol"), adj = -0.2)

      legend(x = "topleft",
        legend = substitute(part1 == part2,
          list(part1 = mltext("barplotOccurrence.leg.1", language = getOption("P.lang")),
            part2 = mltext("barplotOccurrence.leg.2", language = getOption("P.lang")))),
        cex = 0.9, col = getOption("P.NbObsCol"), text.col = getOption("P.NbObsCol"), merge = FALSE)

    }else{}

    # ###################################################
    # Fermeture de graphiques et sauvegarde de fichiers :

    # On ferme les périphériques PNG en mode fichier individuel :
    if (isTRUE(getOption("P.graphPNG"))){
      if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1)){
        dev.off()

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
            metrique = "pres.abs", factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }
    }else{
      # Sauvegarde en wmf si pertinent et souhaité :
      if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
          !getOption("P.graphPDF")){
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
            metrique = "pres.abs", factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }
  }   # Fin de boucle graphique


  # On ferme les périphériques PDF ou PNG restants :
  if (getOption("P.graphPDF") ||
      (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)){
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
        metrique = "pres.abs", factGraph = factGraph, factGraphSel = factGraphSel,
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

  # Sauvegarde en wmf restants si pertinent et souhaité :
  if (!(getOption("P.graphPNG") || getOption("P.graphPDF")) &&
      getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1){
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
        metrique = "pres.abs", factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}
  }else{}
}


#' @title Frequences d'occurrence (donnees agregees par unites d'observation)
#'
#' @description Cette fonction permet de tracer le graphique des frequences d'occurrences
#' pour les donnees agregees par unites d'observation
#'
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection de modalite du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @noRd

barplotOccurrence.unitobs.f <- function(factGraph, factGraphSel, listFact, listFactSel,
  new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: création d'un barplot d'après les sélections de facteurs et
  ##          modalités, avec les présences/absences agrégées par unitobs.
  ## ----------------------------------------------------------------------
  ## Arguments: factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s).
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 14 oct. 2010, 10:51

  metrique <- "occurrence.frequency"

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = "pres.abs", facteurs = facteurs,
    selections = selections, dataEnv = dataEnv, tableMetrique = "TablePresAbs", exclude = NULL)


  # Identification des différents modalités (espèces) du graphique à générer :
  if (factGraph == ""){                 # Pas de facteur.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){        # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                              # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }


  # Agrégation des observations / unité d'observation :
  tmpData <- na.omit(
    agregationTableParCritere.f(Data = tmpData,
      metrique = "pres.abs",
      facteurs = c("observation.unit"),
      dataEnv = dataEnv,
      listFact = listFact))

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list(tmpData)

  # ###############################################################
  # Création du graphique si le nombre d'observations < au minimum défini dans les options :
  if (dim(tmpData)[1] < getOption("P.MinNbObs")){
    warning(mltext("WP2boxplot.W.n.1"), "(", paste(iFactGraphSel, collapse = ", "), ") < ",
      getOption("P.MinNbObs"), mltext("WP2boxplot.W.n.2"))
  }else{
    # Option graphique :
    cex <- getOption("P.cex")

    # Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    # Ouverture et configuration du périphérique graphique :
    if (new_window){
      graphFile <- openDevice.f(noGraph = 1,
        metrique = metrique,
        factGraph = factGraph,
        modSel = iFactGraphSel,
        listFact = listFact,
        dataEnv = dataEnv,
        type = "unitobs",
        typeGraph = "barplot")
    } else{
      graphFile <- resFileGraph.f(
        metrique = metrique,
        factGraph = factGraph,
        modSel = iFactGraphSel,
        listFact = listFact,
        dataEnv = dataEnv,
        ext = "wmf",
        prefix = "barplot",
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = "unitobs")
    }

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
      mainTitle <- graphTitle.f(metrique = metrique,
        modGraphSel = iFactGraphSel,
        factGraph = factGraph,
        listFact = listFact,
        type = "unitobs",
        graphType = "occFrequency")
    }else{
      mainTitle <- NULL
    }

    # Calcul des fréquences :
    heights <- with(tmpData,
      tapply(pres.abs, lapply(listFact, function(y)eval(parse(text = y))),
        function(x){
          100 * sum(x, na.rm = TRUE) / length(na.omit(x))
        }))


    # Paramètres graphiques :
    # Marge dynamiques (adaptation à la longueur des labels) :
    optim(par = unlist(par("mai")),   # Le rapport inch/ligne est modifié en changeant les marges
                                      # => besoin de l'optimiser.
      fn = function(x){
        par(mai = c(
          # Marge du bas :
          lineInchConvert.f()$V * cex * unlist(par("lheight")) * 4.5,
          # Marge de gauche dynamique :
          tmp2 <- ifelse((tmp <-
            lineInchConvert.f()$H * cex * unlist(par("lheight")) * (1.4 + 0.4 + 0.9) + # marge supplémentaire.
            max(strDimRotation.f(as.graphicsAnnot(pretty(range(heights, na.rm = TRUE))),
              srt = 0,
              unit = "inches",
              cex = cex)$width, na.rm = TRUE)) > 0.7 * unlist(par("pin"))[1],
            0.7 * unlist(par("pin"))[1],
            tmp),
          # Marge supérieure augmentée s'il y a un titre :
          ifelse(isTRUE(getOption("P.graphPaper"))  || (! isTRUE(getOption("P.title"))),
            3 * lineInchConvert.f()$V,
            8 * lineInchConvert.f()$V),
          # Marge de droite :
          lineInchConvert.f()$H * cex * unlist(par("lheight")) * 7) +
            lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
          # Distance du nom d'axe dépendante de la taille de marge gauche :
          mgp = c(tmp2 / lineInchConvert.f()$H - 1.4, 0.9, 0))

        # Valeur à minimiser :
        return(sum(abs(x - unlist(par("mai")))))
      },
      control = list(abstol = 0.01))    # Tolérance.

    # Label axe y :
    ylab <- ifelse(getOption("P.axesLabels"),
      parse(text = paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
        ifelse(varNames[metrique, "unite"] != "",
          paste("~~(", varNames[metrique, "unite"], ")", sep = ""),
          ""),
        sep = "")),
      "")

    barPlotTmp <- barplot(heights,
      beside = TRUE,
      main = mainTitle,
      xlab = "",
      ylab = ylab,
      ylim = c(0, 1.1 * max(heights, na.rm = TRUE)),
      las = 1,
      col = PAMPAcolors.f(n = nrow(heights)),
      cex.lab = cex,
      cex.axis = cex,
      legend.text = ifelse(length(listFact) > 1, TRUE, FALSE),
      args.legend = list("x" = "topright", "inset" = -0.08, "xpd" = NA,
        "title" = Capitalize.f(varNames[listFact[1], "nom"])))

    if (getOption("P.axesLabels")){
      mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
            side = 1, line = 2.3, cex = cex)
    }else{}

    if (getOption("P.NbObs")){
      # Nombre d'"observations" :
      nbObs <- with(tmpData,
        tapply(pres.abs,
          lapply(listFact, function(y)eval(parse(text = y))),
          function(x){
            length(na.omit(x))
          }))

      # Nombres sur l'axe supérieur :
      mtext(nbObs, side = 3, at = barPlotTmp, las = 2, col = getOption("P.NbObsCol"), adj = -0.2)

      legend(x = "topleft",
        legend = mltext("barplotOccurrence.unitobs.leg", language = getOption("P.lang")),
          cex = 0.9, col = getOption("P.NbObsCol"),
          text.col = getOption("P.NbObsCol"), merge = FALSE)

    }else{}

    # ##################################################
    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
    }else{}

    # Sauvegarde des infos sur les données et statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "unitobs", type = "graph",
        metrique = "pres.abs", factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = rev(listFact), listFactSel = rev(listFactSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}

  }                                   # Fin de boucle graphique

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
}
