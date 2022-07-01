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
#' @param new_window bool, affichage du graphique dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' mrt.f(
#'   agregation = "espece",
#'   metrique = "density",
#'   factGraph = "family",
#'   factGraphSel = "Acanthuridae",
#'   listFact = c("year", "protection.status", "site", "species"),
#'   listFactSel = NA,
#'   tableMetrique = "unitSp",
#'   new_window = TRUE,
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


#' @title Arbres de regression multivariee (donnees agregees par especes)
#'
#' @description Cette fonction permet de faire des arbres de regression multivariee des donnees
#' agregees par especes
#'
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de separation des graphiques
#' @param factGraphSel chr, selection des modalite pour le facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metriques
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @import rpart
#'
#' @noRd

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

    rpart:::plot.rpart(tmpMRT, main = mainTitle, margin = 0.00)
    par(xpd = NA)
    rpart:::text.rpart(tmpMRT, use.n = TRUE, pretty = 1, all = TRUE, xpd = NA,
      fancy = TRUE, adj = c(0.5, 0.75))

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
}


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

#' @import rpart

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


#' @title Arbre de regression multivariee (donnees agregees par unites d'observation)
#'
#' @description Cette fonction permet de faire des arbres de regression multivariee pour des donnees
#' agregees par unites d'observation
#'
#' @param metrique chr, metrique choisie
#' @param factGraph chr, facteur de selection des especes
#' @param factGraphSel chr, selection de modalites pour le facteur de selection des especes
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metriques
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @import rpart
#'
#' @noRd

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
}
