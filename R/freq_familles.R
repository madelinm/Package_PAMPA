#' Fonction pour tracer un barplot de frequences d'occurrence de familles.
#'
#' \code{factGraphSel} peut prendre differentes valeurs en fonction de celle de \code{factGraph}.
#' Ce parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' \code{factSel} peut prendre differentes valeurs en fonction de celle de \code{fact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du facteur de
#' regroupement selectionne seront prises en compte.
#'
#' \code{families} correspond aux familles que l'on veut etudier. Si ce parametre est egal a NA,
#' alors toutes les familles seront prises en compte


#' @title Barplot des frequences de familles
#'
#' @description Creation d'un barplot de frequences d'occurence pour les familles
#'
#' @param factGaph chr, premier facteur explicatif
#' @param factGraphSel chr, selection de modalites du premier facteur explicatif
#' @param fact chr, second facteur explicatif (facultatif)
#' @param factSel chr, modalites selectionnees pour le second facteur explicatif
#' @param families chr, familles a prendre en compte
#' @param new_window bool, affichage du graphique dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::freq_occurrence_families.f(
#'   factGraph = "protection.status",
#'   factGraphSel = NA,
#'   fact = "year",
#'   factSel = NA,
#'   families = NA,
#'   new_window = TRUE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export

freq_occurrence_familles.f <- function(factGraph, factGraphSel = NA, fact, factSel = NA,
  families = NA, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  # Verification des parametres
  # Check of the parameters
  tableMetrique = "TablePresAbs"
  metrique = "pres.abs"
  agregation = "espece"
  if (is.null(factGraph)){
    factGraph <- ""
  }

  # ...du facteur de separation des graphiques :
  # ...the factor for the graphic separation :
  if (factGraph != ""){
    factGraph_possible_unitobs <- UnitobsFields.aliases(ordered = FALSE, dataEnv = dataEnv,
      tableMetrique = tableMetrique)

    if (!is.element(factGraph, factGraph_possible_unitobs)){
      stop(
        paste("La valeur '", factGraph, "' du paramètre 'factGraph' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(factGraph_possible_unitobs, collapse = ", "),
        paste("The value '", factGraph, "' for the 'factGraph' parameter isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(factGraph_possible_unitobs, collapse = ", ")
      )
    }
  }

  # ...des modalites du facteur de separation des graphiques :
  # ...the modalities of the factor for the  graphic separation :
  if (factGraph != ""){
    factGraphSel_possible <- unique(selectModalites.f( tableMetrique = tableMetrique,
      facts = factGraph, selections = append(list(NA), NA), metrique = metrique,
      nextStep = "freq_occurrence", dataEnv, level = 0)[, factGraph])

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
  if (length(fact) > 1){
    stop(
      "Veuillez ne sélectionner que 1 facteur pour le paramètre 'fact'.",
      "Please, select only 1 factor for the 'fact' parameter."
    )
  }
  fact_possible <- refTablesFields.aliases(nomTable = tableMetrique, dataEnv = dataEnv)
  if (!(is.na(fact)) && !is.element(fact, fact_possible)){
    stop(
      paste("La valeur '", fact, "' du paramètre 'fact' n'est pas valide.\n", sep = ""),
      paste("Veuillez choisir parmi :\n"),
      paste(fact_possible, collapse = ", "),
      paste("The value '", fact, "' of the 'fact' parameter isn't correct.\n", sep = ""),
      paste("Please, choose one in the following list :\n"),
      paste(fact_possible, collapse = ", ")
    )
  }

  # ...des modalites des facteurs explicatifs :
  # ...the modalities of the explanatory factors :
  if (length(factSel) != length(fact)){
    stop(
      "'fact' et 'factSel' doivent avoir la même longueur.",
      "'fact' and 'factSel' must have the same length."
    )
  }

  if (!is.na(fact)){
    factSel_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
      facts = fact, selections = append(list(NA), NA), metrique = metrique,
      nextStep = "freq_occurrence", dataEnv, level = 1)[, fact])
    if (!is.na(factSel) && !is.element(factSel, factSel_possible)){
      stop(
        paste("La valeur '", factSel, "' du paramètre 'factGraph' pour le facteur '",
          fact, "' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(factSel_possible, collapse = ", "),
        paste("The value '", factSel, "' of the 'factGraph' parameter for the factor '",
          fact, "' isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(factSel_possible, collapse = ", ")
      )
    }
  }

  # ...des familles selectionnees :
  # ...the selected families :
  families_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
    facts = "family", selections = append(list(NA), NA), metrique = metrique,
    nextStep = "freq_occurrence", dataEnv, level = 1)[, "family"])
  for (i in seq(length(families))){
    if (!is.na(families[i]) && !is.element(families[i], families_possible)){
      stop(
        paste("La valeur '", families[i], "' du paramètre 'families' n'est pas valide.\n", sep = ""),
        paste("Veuillez choisir parmi :\n"),
        paste(families_possible, collapse = ", "),
        paste("The value '", families[i], "' of the 'families' parameter isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(listFact_possible, collapse = ", ")
      )
    }
  }

  # Verification que les parametres sont "compatibles" et correspondent a des donnees :
  # Check that the parameter are "compatibles" and correspond to data :
  modalites_trouvees <- selectModalites.f(tableMetrique = tableMetrique,
    facts = c(factGraph, fact), selections = append(list(factGraphSel), factSel),
    metrique = metrique, nextStep = "freq_occurrence", dataEnv, level = length(fact))
  if (nrow(modalites_trouvees) == 0){
    stop(
      "Aucune donnée trouvée avec ces paramètres.",
      "No data found with these parameters."
    )
  }

  # Lancement de la fonction de graphique
  # Launch of the graphic function
  barplotOccurrenceFamille.f(factGraph = factGraph, factGraphSel = factGraphSel,
    fact = fact, factSel = factSel, families = families, new_window,
    dataEnv = dataEnv, baseEnv = baseEnv)
}


#' @title Barplot des frequences de familles
#'
#' @description Creation d'un barplot de frequences d'occurence pour les familles
#'
#' @param factGaph chr, premier facteur explicatif
#' @param factGraphSel chr, selection de modalites du premier facteur explicatif
#' @param fact chr, second facteur explicatif (facultatif)
#' @param factSel chr, modalites selectionnees pour le second facteur explicatif
#' @param families vector
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @noRd

barplotOccurrenceFamille.f <- function(factGraph, factGraphSel, fact, factSel, families = NA,
  new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: création des barplots d'après les sélections de facteurs et
  ##          modalités.
  ## ----------------------------------------------------------------------
  ## Arguments: factGraph : le facteur de séparation des graphiques.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            fact : liste du (des) facteur(s) de regroupement.
  ##            factSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s).
  ##            families : familles à considérer.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface principale.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 14 oct. 2010, 10:51

  metrique <- "occurrence.frequency"

  if (! is.na(fact)){
    fact <- c(fact, "family")
  }
  else{
    fact <- c("family")
  }

  factSel <- list(factSel, families)

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  factSel <- factSel[unlist(fact) != ""]
  factSel <- factSel[length(factSel):1]

  fact <- fact[unlist(fact) != ""]
  fact <- fact[length(fact):1]

  # Concaténation
  facteurs <- c(factGraph, unlist(fact)) # Concaténation des facteurs
  selections <- c(list(factGraphSel), factSel) # Concaténation de leurs listes de modalités sélectionnées

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
        listFact = fact,
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
        listFact = fact,
        dataEnv = dataEnv,
        ext = "wmf",
        prefix = "freq_familles",
        sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(modSel) > 1 || modSel[1] == ""),
          "%03d",
          ""),
        type = "espece")
    }

    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    # Calcul des fréquences :
    heights <- with(tmpDataMod,
      tapply(pres.abs, lapply(fact, function(y)eval(parse(text = y))),
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

    if (!is.matrix(heights)){   # cas où heights n'est pas une matrice (pas de facteur sélectionné)
      heights <- as.matrix(heights)
    }
    par(mfrow = c(1,ncol(heights)))
    colors <- PAMPAcolors.f(n = nrow(heights))

    for (i in seq(ncol(heights))){

      # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
      if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
        nbObs <- nbObs <- with(tmpDataMod,
          tapply(pres.abs,
            lapply(fact, function(y)eval(parse(text = y))),
            function(x){
              length(na.omit(x))
            }))
        if (ncol(heights) == 1){
          nbObs <- sum(nbObs)
        } else {
          nbObs <- sum(nbObs[,i])
        }

        mainTitle <- graphTitle.f(metrique = metrique,
          modGraphSel = modGraphSel,
          factGraph = factGraph,
          listFact = fact,
          type = "espece",
          graphType = "occFamily",
          nbObs = nbObs)
      }else{
        mainTitle <- NULL
      }

      ordre <- order(heights[,i], decreasing = TRUE)
      heights_ordered <- heights[ordre, i]
      heights_ordered <- as.matrix(heights_ordered)

      colors_ordered <- colors[ordre]

      barPlotTmp <- barplot(heights_ordered,
        beside = TRUE,
        main = mainTitle,
        xlab = "",
        ylab = ylab,
        ylim = c(0, 1.1 * max(heights_ordered, na.rm = TRUE)),
        las = 1,
        col = colors_ordered,
        cex.lab = cex,
        cex.axis = cex,
        legend.text = TRUE,
        args.legend = list("x" = "topright", "inset"= 0, "xpd" = NA,
          "title" = Capitalize.f(varNames[fact[1], "nom"])))

      if (getOption("P.axesLabels") & ncol(heights) > 1){
        mtext(
          names(heights[1,])[i],
          side = 1, line = 2.3, cex = cex)
      }else{}
    }
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
            listFact = rev(fact), listFactSel = rev(factSel), # On les remets dans un ordre intuitif.
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
            listFact = rev(fact), listFactSel = rev(factSel), # On les remets dans un ordre intuitif.
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
        listFact = rev(fact), listFactSel = rev(factSel), # On les remets dans un ordre intuitif.
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
        listFact = rev(fact), listFactSel = rev(factSel), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}
  }else{}
}
