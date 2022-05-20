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
#' @param factGaph chr, facteur de separation des graphs
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param fact chr, facteur de regroupement
#' @param factSel chr, modalites selectionnees pour le facteur de regroupement
#' @param families chr, familles a prendre en compte
#' @param new_window bool, affichage du graphique dans une nouvelle fenêtre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::freq_occurrence_families.f(factGraph = "protection.status", factGraphSel = NA,
#'   fact = "year", factSel = NA, families = NA, new_window = TRUE,
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

  # ...des familles sélectionnées :
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
#' @param factGaph chr, facteur de separation des graphs
#' @param factGraphSel chr, selection de modalites du facteur de separation des graphiques
#' @param fact chr, facteur de regroupement
#' @param factSel chr, modalites selectionnees pour le facteur de regroupement
#' @param families vector
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent

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
        nbObs <- sum(nbObs[,i])

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
#          Capitalize.f(varNames[tail(fact, 1), "nom"]),
          names(heights[1,])[i],
          side = 1, line = 2.3, cex = cex)
      }else{}

#      if (getOption("P.NbObs")){
#        # Nombre d'"observations" :
#        nbObs <- with(tmpDataMod,
#          tapply(pres.abs,
#            lapply(fact, function(y)eval(parse(text = y))),
#            function(x){
#              length(na.omit(x))
#        }))
#
#        if (!is.matrix(nbObs)){   # cas où nbObs n'est pas une matrice (pas de facteur sélectionné)
#          nbObs <- as.matrix(nbObs)
#        }
#
#        # Nombres sur l'axe supérieur :
#        mtext(nbObs[,i], side = 3, at = barPlotTmp, las = 2, col = getOption("P.NbObsCol"), adj = -0.2)
#
#        legend(x = "topleft",
#          legend = substitute(expression(part1 == part2),
#            list(part1 = mltext("barplotOccurrence.leg.1", language = getOption("P.lang")),
#              part2 = mltext("barplotOccurrence.leg.2", language = getOption("P.lang")))),
#          cex = 0.9, col = getOption("P.NbObsCol"), text.col = getOption("P.NbObsCol"), merge = FALSE)
#
#      }else{}

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
#
#
# lineInchConvert.f <- function(){
#
#   ## Purpose: Calcul du facteur de conversion inch/ligne.
#   ## ----------------------------------------------------------------------
#   ## Arguments: Aucun
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date: 26 août 2011, 13:07
#
#   pars <- par(c("mai", "mar"))
#
#   return(list(H = (pars$mai / pars$mar)[2],
#     V = (pars$mai / pars$mar)[1]))
# }
#
#
# strDimRotation.f <- function(x, srt = 0, unit = "user", cex = getOption("P.cex"), ...){
#
#   ## Purpose: Calcul des dimensions d'une chaîne de caractère à laquelle
#   ##          on applique une rotation
#   ## ----------------------------------------------------------------------
#   ## Arguments: x : vecteur de classe 'character'.
#   ##            srt : angle de rotation en degrés.
#   ##            unit : unité de sortie.
#   ##            ... : arguments supplémentaires passés à str(height|width).
#   ## ----------------------------------------------------------------------
#   ## Author: Yves Reecht, Date:  9 févr. 2011, 16:15
#
#   # browser()
#
#   # Dimensions en pouces :
#   W.inches <- strwidth(x, unit = "inches", cex = cex, ...)
#   H.inches <- strheight(x, unit = "inches", cex = cex, ...)
#
#   # Facteur de conversion avec l'unité souhaitée :
#   X.inchesVSunit <- W.inches / strwidth(x, unit = unit, cex = cex, ...)
#   Y.inchesVSunit <- H.inches / strheight(x, unit = unit, cex = cex, ...)
#
#   # Calcul des largeurs et hauteurs en rotations :
#   X.calc <- abs(W.inches * cos(srt * base:::pi / 180)) + abs(H.inches * sin(srt * base:::pi / 180))
#   Y.calc <- abs(W.inches * sin(srt * base:::pi / 180)) + abs(H.inches * cos(srt * base:::pi / 180))
#
#   # Conversion dans l'unité souhaitée :
#   return(list(width = X.calc / X.inchesVSunit,
#     height = Y.calc / Y.inchesVSunit))
# }
#
#
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
#
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

