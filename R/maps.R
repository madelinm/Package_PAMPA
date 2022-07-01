#' Fonction pour tracer des cartes.
#'
#' \code{agregation} peut prendre la valeur 'espece' ou 'unitobs'. Si 'espece', l'agregation se fera
#' par especes, si 'unitobs', elle se fera par groupe d'especes.
#'
#' \code{graphType} peut prendre la valeur 'boxplot', 'barplot', 'symboles' ou 'couleurs'.
#' Si 'boxplot' ou 'barplot', le(s) boxplot(s) ou barplot(s) seront affiches sur la carte.
#' Si symboles, la metrique choisie sera representee par des cercles, si couleurs, elle sera
#' representee par des couleurs.
#'
#' \code{tableMetrique} peut prendre les valeurs 'unitSp' (/station /especes /classe de taille),
#' 'unitSpSz' (/station /especes) ou 'unit' (de biodiversite (/station)). Ce dernier cas n'est
#' possible uniquement lorsque \code{agregation == 'unitobs'}. La table de metriques 'unitSpSz'
#' n'est pas disponible si le jeu de donnees est un jeu de donnees de benthos.
#'
#' \code{metrique} peut prendre differentes valeurs en fonction de celle de tableMetrique et
#' du jeu de donnees.
#'
#' \code{factSpatialSel} peut prendre differentes valeurs en fonction de celle de factSpatial.
#' Ce parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' \code{factGraph} est facultatif.
#'
#' \code{factGraphSel} peut prendre differentes valeurs en fonction de celle de factGraph. Ce
#' parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' \code{listFact} n'est pas necessaire dans le cas ou \code{graphType} est egal a 'symboles' ou
#' 'couleurs'. De meme pour \code{listFactSel}.
#'
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s) de
#' regroupement selectionne(s) seront prises en compte.


#' @import leafpop
#' @import leaflet


#' @title Cartes
#'
#' @description Fonction permettant de creer des cartes en fonctions des parametres donnes
#' par l'utilisateur
#'
#' @param agregation chr, type d'agregation par espece (espece) ou par groupe d'espece (unitobs)
#' @param graphType chr, type de graphique (boxplot, barplot, symboles, couleurs)
#' @param metrique chr, metrique choisie
#' @param factSpatial chr, facteur de regroupement spatial
#' @param factSpatialSel chr, selection pour le facteur de regroupement spatial
#' @param factGraph chr, facteur de selection des especes
#' @param factGraphSel chr, selection de modalites pour le facteur de selection des especes
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel chr, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metriques
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @export

maps.f <- function(agregation, graphType, metrique, factSpatial, factSpatialSel = NA,
  factGraph = NULL, factGraphSel = NA, listFact, listFactSel = NA, tableMetrique,
  dataEnv, baseEnv = .GlobalEnv){

  barboxplot <- c("boxplot", "barplot")
  symbcol <- c("symboles", "couleurs")
  tableMetrique_possible <- c("unit", "unitSp", "unitSpSz")
  nextStep <- switch(agregation,
    "espece" = {
      ifelse(is.element(graphType, barboxplot),
        "spBarBoxplot.esp",
        "spSymbols.esp")
    },
    "unitobs" = {
      ifelse(is.element(graphType, barboxplot),
        "spBarBoxplot.unitobs",
        "spSymbols.unitobs")
    },
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes).",
      "Please, choose a value for 'agregation' between 'espece' and 'unitobs' (species group).")
    )
  if (is.null(factGraph)) {
    factGraph <- ""
  }

  # Verification des parametres
  # Check of the parameters
  # ... du type de graph :
  # ... the graph type :
  if (!is.element(graphType, c(barboxplot, symbcol))){
    stop(
      "La valeur de 'graphType' n'est pas valide.\n",
      paste("Veuillez choisir parmi :\n"),
      paste(c(barboxplot, symbcol), collapse = ", "),
      "The value of 'graphType' isn't correct.\n",
      paste("Please, choose one in the folowing list :\n"),
      paste(c(barboxplot, symbCol), collapse = ", ")
    )
  }

  # ... de la metrique et de la table des metriques :
  # ... the metric and the metrics table :
  if (!is.element(tableMetrique, tableMetrique_possible)){
    stop(
      "Veuillez choisir une valeur de 'tableMetrique' entre 'unitSp' (/station /especes),
        'unitSpSz' (/station /especes /classe de taille) et 'unit' (de biodiversite (/station)).",
      "Please, choose a value for 'tableMetrique' between 'unitSp', (/station /species),
        'unitSpSz' (/station /species /size classes) and 'unit' (of biodiversity (/station))."
    )
  }

  # S'il s'agit d'un jeu de donnees benthos, ou qu'il n'y a pas de classes tailles disponibles
  # If it's a benthos data set, or no size classes are available
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
  metriques_possibles <- MetricsField.aliases(tableMetrique, "boxplot", dataEnv)
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

  # ...du facteur de regroupement spatial
  # ...the factor for the spatial regroupment
  factSpatial_possible <- champsRefspa.f(dataEnv = dataEnv)
  if (!is.element(factSpatial, factSpatial_possible)){
    stop(
      paste("La valeur '", factSpatial, "' du paramètre 'factSpatial' n'est pas valide.\n", sep = ""),
      paste("Veuillez choisir parmi :\n"),
      paste(factSpatial_possible, collapse = ", "),
      paste("The value '", factSpatial, "' of the parameter 'factSpatial' isn't correct.\n", sep = ""),
      paste("Please, choose one in the following list :\n"),
      paste(factSpatial_possible, collapse = ", ")
    )
  }

  factSpatialSel_possible <- unique(selectModalitesSpatiales.f(tableMetrique = tableMetrique,
    facts = factSpatial, selections = append(list(NA), NA), metrique = metrique,
    nextStep = nextStep, dataEnv = dataEnv)[ ,factSpatial])
  if (!is.na(factSpatialSel) && !is.element(factSpatialSel, factSpatialSel_possible)){
    stop(
      paste("La valeur '", factSpatialSel, "' du paramètre 'factSpatialSel' n'est pas valide.\n", sep = ""),
      paste("Veuillez choisir parmi : \n"),
      paste(factSpatialSel_possible, collapse = ", ")
    )
  }

  # ...du facteur de separation des graphiques
  # ...the factor for the graphic separation
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
          paste("The value '", factGraph, "' for the parameter 'factGraph' isn't correct.\n", sep = ""),
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
          paste("The value '", factGraph, "' of the parameter 'factGraph' isn't correct.\n", sep = ""),
          paste("please, choose one in the following list :\n"),
          paste(factGraph_possible_refesp, collapse = ", ")
        )
      }
    }
  }

  # ...des modalites du facteur de separation des graphiques
  # ...the modalities of the factor for the  graphic separation
  if (factGraph != ""){
    factGraphSel_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
      facts = factGraph, selections = append(list(NA), NA), metrique = metrique,
      nextStep = nextStep, dataEnv, level = 0)[, factGraph])
    if (!is.na(factGraphSel[1]) && !all(is.element(factGraphSel, factGraphSel_possible))){
      stop(
        paste("La valeur '", factGraphSel,
          "' du paramètre 'factGraphSel' n'est pas valide.\n", sep = ""),
        paste("Veillez choisir parmi :\n"),
        paste(factGraphSel_possible, collapse = ", "),
        paste("The value '", factGraphSel,
          "' of the parameter 'factGraphSel' isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(factGraphSel_possible, collapse = ", ")
      )
    }
  }

  # ...des facteurs explicatifs
  # ...the explanatory factors
  if (is.element(graphType, barboxplot)){  # seulement pour les graphiques
    listFact_possible <- refTablesFields.aliases(nomTable = tableMetrique, dataEnv = dataEnv)
    for (i in seq(length(listFact))){
      if (!is.element(listFact[i], listFact_possible)){
        stop(
          paste("La valeur '", listFact[i], "' du paramètre 'listFact' n'est pas valide.\n", sep = ""),
          paste("Veuillez choisir parmi :\n"),
          paste(listFact_possible, collapse = ", "),
          paste("The value '", listFact[i], "' of the parameter 'listFact' isn't correct.\n", sep = ""),
          paste("Please, choose one in the following list :\n"),
          paste(listFact_possible, collapse = ", ")
        )
      }
    }

    # ...des modalites des facteurs explicatifs
    # ...the modalities of the explanatory factors
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
            paste("The value '", listFactSel[[i]][j], "' of the parameter 'listFactGraph' for the factor '",
              listFact[i], "' isn't correct.\n", sep = ""),
            paste("Please, choose one in the following list :\n"),
            paste(listFactSel_possible, collapse = ", ")
          )
        }
      }
    }
  }

  # Verification que les parametres sont "compatibles" et correspondent a des donnees :
  # Check that the parameter are "compatibles" and correspond to data :
  modalites_trouvees <- selectModalitesSpatiales.f(tableMetrique = tableMetrique,
    facts = c(factSpatial, factGraph, ifelse(is.element(graphType, barboxplot), listFact, "")),
    selections = append(list(factSpatialSel), c(list(factGraphSel), ifelse(is.element(graphType, barboxplot),
      ifelse(!is.list(listFactSel), list(listFactSel), NA), NA))),
    metrique = metrique, nextStep = nextStep, dataEnv)
  if (nrow(modalites_trouvees) == 0){
    stop(
      "Aucune donnée trouvée avec ces paramètres.",
      "No data found with these parameters."
    )
  }

  # Lancement de la fonction graphique
  # Launch of the graphic function
  if (is.element(graphType, barboxplot)){
    if (agregation == "espece"){
      map <- subplotCarto.esp.f(graphType, metrique, factSpatial, factSpatialSel,
        factGraph, factGraphSel, listFact, listFactSel, tableMetrique, dataEnv, baseEnv)
    }
    else{
      map <- subplotCarto.unitobs.f(graphType, metrique, factSpatial, factSpatialSel,
        factGraph, factGraphSel, listFact, listFactSel, tableMetrique, dataEnv, baseEnv)
    }
  }
  else{
    if (agregation == "espece"){
      map <- symbColCarto.esp.f(graphType, metrique, factSpatial, factSpatialSel,
        factGraph, factGraphSel, tableMetrique, dataEnv, baseEnv)
    }
    else{
      map <- symbColCarto.unitobs.f(graphType, metrique, factSpatial, factSpatialSel,
        factGraph, factGraphSel, tableMetrique, dataEnv, baseEnv)
    }
  }
  return(map)
}


subplotCarto.esp.f <- function(graphType, metrique, factSpatial, factSpatialSel,
  factGraph, factGraphSel, listFact, listFactSel, tableMetrique, #bbox = NULL,
  dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des boxplots répartis sur des cartes.
  ## ----------------------------------------------------------------------
  ## Arguments: graphType : type de graphique (bar|box-plot).
  ##            metrique : la métrique choisie.
  ##            factSpatial : facteur de regroupement spatial.
  ##            factSpatialSel : sélection sur le facteur de regroupement
  ##                             spatial.
  ##            factGraph : le facteur sélection des espèces.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            bbox : emprise spatial (bouding box) ; toute la carte si
  ##                   NULL.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  7 fevr. 2013, 17:18

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factSpatial, factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factSpatialSel), list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités
  # sélectionnées

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL)

  # Formule du boxplot
  exprBP <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " + "))))

  # Identification des différents graphiques à générer:
  if (factGraph == ""){             # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){    # Toutes les modalités.
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
  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :
  for (modGraphSel in iFactGraphSel){
    if (length(iFactGraphSel) > 1){
      print(paste("Création de la couche pour ", factGraph, " == '", modGraphSel, "'...", sep = ""))
      print(paste("Creation of the layer for ", factGraph, " == '", modGraphSel, "'...", sep = ""))
    }

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

    graphFileTmp <- resFileGraph.f(
      metrique = metrique,
      factGraph = factGraph,
      modSel = iFactGraphSel,
      listFact = listFact,
      dataEnv = dataEnv,
      ext = "wmf",
      prefix = paste("carte_", graphType, sep = ""),
      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(iFactGraphSel) > 1 || iFactGraphSel[1] == ""),
        "%03d",
        ""),
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs"))

    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    ##################################################

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    mainTitle <- graphTitle.carto.f(metrique = metrique,
      modGraphSel = modGraphSel, factGraph = factGraph,
      listFact = listFact,
      factSpatial = factSpatial,
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"espece"},
        "unitSpSz" = {"CL_espece"},
        "espece"))

    # Graphiques :
    refspa <- get("refspa", envir = dataEnv)

    # Affichage du fond de carte :
    if (exists("baseMap", envir = dataEnv)){
      baseMap <- get("baseMap", envir = dataEnv)
    }else{
      baseMap <- tryCatch(maptools::unionSpatialPolygons	(SpP = refspa,
        IDs = ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
          getOption("P.landMods")),
          "terre", "mer")),
        error = function(e){
          return(refspa)
      })
      assign("baseMap", baseMap, envir = dataEnv)
    }

    unitobs <- get("unitobs", envir = dataEnv)
    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = tmpDataMod, fact = factSpatial)

    if ( ! is.na(factSpatialSel[1])){
      refspaTmp <- subset(refspa, is.element(refspa@data[ , factSpatial], factSpatialSel))
    }else{
      refspaTmp <- subset(refspa,
        is.element(refspa@data[ , factSpatial], unitobs[ , factSpatial]) &
        ! is.na(refspa@data[ , factSpatial]))
    }

    # Agrégation des zones à l'échelle souhaitée :
    polyZones <- maptools::unionSpatialPolygons	(SpP = refspaTmp, IDs = refspaTmp@data[ , factSpatial])

    col <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))

    # Creation des subplots :
    plot_name <- switch(graphType,
      "boxplot" = {
        boxplotCarto.generic.f(polyZones = polyZones, Data = tmpDataMod,
          factSpatial = factSpatial, exprBP = exprBP)
      },
      "barplot" = {
        barplotCarto.generic.f(polyZones = polyZones, Data = tmpDataMod,
          factSpatial = factSpatial, metrique = metrique, listFact = listFact)
      }
    )

    x <- as.vector(unique(refspaTmp@data[[factSpatial]]))
    x <- x[order(x)]
    y <- seq(length(x))
    df <- as.data.frame(cbind(x, y))
    colnames(df) <- c(factSpatial, "row")

    spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
    spdf@plotOrder <- seq(length(spdf@plotOrder))

    if (!exists("map")){
      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
          opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)
      list_layers <- c("Base Map")
    }

    layer_name <- paste(factSpatial, modGraphSel, sep = " - ")
    list_layers <- c(list_layers, layer_name)

    map <- map %>%
      leaflet::addPolygons(data = spdf, group = layer_name, color = "#000000",
        label = spdf@data[[factSpatial]], weight = 1, opacity = 1, fillColor = col, fillOpacity = 0.75) %>%
      leafpop::addPopupImages(plot_name[[1]], group = layer_name, width = 300)

    do.call(file.remove, list(list.files(plot_name[[2]], full.names = TRUE)))
    unlink(plot_name[[2]], recursive = TRUE)

    # ###################################################
    # Fermeture de graphiques et sauvegarde de fichiers :

    # On ferme les périphériques PNG en mode fichier individuel :
    if (isTRUE(getOption("P.graphPNG"))){
      if (plotted){
        dev.off()

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
            metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = c(factSpatial, rev(listFact)),
            listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }else{
      # Sauvegarde en wmf si pertinent et souhaité :
      if (plotted && ! getOption("P.graphPDF")){
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
            listFact = c(factSpatial, rev(listFact)),
            listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre
            # intuitif.
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }

  }  # Fin de boucle graphique.

  map <- map %>%
    leaflet::addLayersControl(
      overlayGroups = list_layers,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  print("Affichage de la carte...")
  print("Print map...")
  if (length(iFactGraphSel) > 1){
    print("Vous pouvez naviguez entre les couches à l'aide du bouton à gauche.")
    print("You can switch between layers with the button at the left.")
  }

  # On ferme les périphériques PDF ou PNG restants :
  if (getOption("P.graphPDF") && plotted) {
    dev.off()

    # Sauvegarde des données :
    if (getOption("P.saveData")) {
      writeData.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup, cols = NULL)
    }else{}

    # Sauvegarde des statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup,
        agregLevel = "species", type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = c(factSpatial, rev(listFact)),
        listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}

    # Inclusion des fontes dans le(s) pdf(s) si souhaité :
    if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
      i <- 1

      # On parcours tous les fichiers qui correspondent au motif :
      tmpFile <- sub("\\%03d", formatC(i, width = 3, flag = "0"), graphFile)
      while (is.element(basename(tmpFile), dir(dirname(graphFile))) &&
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
  return(map)
}


graphTitle.carto.f <- function(metrique, modGraphSel, factGraph, listFact, factSpatial,
  model = NULL, type = "espece", lang = getOption("P.lang")){

  ## Purpose: produire un titre de graphique adapté.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : métrique.
  ##            modGraphSel : modalité(s) de factGraph.
  ##            factGraph : facteur de séparation des graphiques /
  ##                        sélection d'espèces.
  ##            listFact : facteurs d'agrégation.
  ##            factSpatial : facteur de niveau d'agrégation spatiale.
  ##            model :
  ##            type : type de niveaux d'agrégation.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 fevr. 2013, 17:44

  return(paste(
    ifelse(is.null(model),
      mltext("graphTitle.vals", language = lang),
      paste(model, mltext("graphTitle.for", language = lang),
        varNames.f(metrique, "article"), sep = "")),
    varNames.f(metrique, "nom"),
    ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
      paste(mltext("graphTitle.agg", language = lang),
        switch(varNames.f(metrique, "genre"), # Accord de "agrégé".
          f = mltext("graphTitle.f", language = lang),
          fp = mltext("graphTitle.fp", language = lang),
          mp = mltext("graphTitle.mp", language = lang), ""),
        sep = ""),
      ""),
    switch(type,
      "espece" = mltext("graphTitle.bySpSt", language = lang),
      "CL_espece" = mltext("graphTitle.bySCSpSt", language = lang),
      "unitobs" = mltext("graphTitle.bySt", language = lang),
      "unitobs(CL)" = mltext("graphTitle.byStSC", language = lang),
      "CL_unitobs" = mltext("graphTitle.bySCSt", language = lang),
      "biodiv" = mltext("graphTitle.biodiv", language = lang),
      "spEspece" = paste(mltext("graphTitle.maps.SpZone", language = lang),
        " '", varNames.f(factSpatial, "name", quote = FALSE), "'", sep = ""),
      "spCL_espece" = paste(mltext("graphTitle.maps.SCSpZone", language = lang),
        " '",
        varNames.f(factSpatial, "name", quote = FALSE), "'", sep = ""),
      "spUnitobs" = ,
      "spUnitobs(CL)" = paste(" par zone '",
        varNames.f(factSpatial, "name", quote = FALSE), "'", sep = ""),
      "spCL_unitobs" = paste(mltext("graphTitle.maps.SCZone", language = lang),
        " '",
        varNames.f(factSpatial, "name", quote = FALSE), "'", sep = ""),
      ""),
    switch(type,
      "spEspece" = ,
      "spCL_espece" = ,
      "spUnitobs" = ,
      "spUnitobs(CL)" = ,
      "spCL_unitobs" = mltext("graphTitle.maps.agg.Zone", language = lang),
      paste(mltext("graphTitle.maps.per.Zone", language = lang),
        " '", varNames.f(factSpatial, "nom", quote = FALSE), "'", sep = "")),
    switch(type,
      "espece" = ,
      "CL_espece" = ,
      "spEspece" = ,
      "spCL_espece" = {
        ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
          "",
          paste(mltext("graphTitle.maps.mod.field", language = lang),
            " '", factGraph, "' = ", modGraphSel, sep = ""))
      },
      "unitobs" = ,
      "CL_unitobs" = ,
      "spUnitobs" = ,
      "spCL_unitobs" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          mltext("graphTitle.maps.mod.allSp", language = lang),
          paste(mltext("graphTitle.maps.mod.selSp", language = lang),
            " '", factGraph, "' = (",
            paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      "unitobs(CL)" = ,
      "spUnitobs(CL)" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          mltext("graphTitle.maps.mod.allSC", language = lang),
          paste(mltext("graphTitle.maps.mod.selSC", language = lang),
            " '", factGraph, "' = (",
            paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      "biodiv" = {
        ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
          "",
          paste(mltext("graphTitle.maps.mod.selSt", language = lang),
            " '", factGraph, "' = (",
            paste(modGraphSel, collapse = ", "), ")", sep = ""))
      },
      ""),
    ifelse(listFact[1] == "",
      "",
      paste(mltext("graphTitle.by", language = lang),
        paste(sapply(listFact[length(listFact):1],
          function(x){
            paste(c(# varNames.f(x, "article")
              "",
              varNames.f(x, "nom")), collapse = "")
          }),
          collapse = "graphTitle.and"),
       "", sep = "")),
    sep = ""))
}


subsetRefspaToData.f <- function(refspa, unitobs, Data, fact = "observation.unit"){

  ## Purpose: réduire le référentiel spatial aux polygones présentant des
  ##          données.
  ## ----------------------------------------------------------------------
  ## Arguments: refspa : le référentiel spatial.
  ##            unitobs : la table d'unités d'observations.
  ##            Data : les données.
  ##            fact : facteur spatial (limité aux unités d'observations si
  ##                   non précisé)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 fevr. 2013, 17:38

  if (is.element("observation.unit", fact)){
    # Réduire le référentiel spatial aux OBJECTID avec des données :
    dataOBJECTID <- unitobs[is.element(unitobs[ , "observation.unit"],
      Data[ , "observation.unit"]),
      "OBJECTID"]

    refspa <- subset(refspa,
      is.element(refspa@data[ , "OBJECTID"], dataOBJECTID))

  }else{
    if (is.element(fact, colnames(Data))){
      refspa <- subset(refspa,
        is.element(refspa@data[ , fact], Data[ , fact]))
    }else{
      commonCol <- colnames(refspa@data)[which(is.element(colnames(refspa@data), colnames(Data)))[1]]

      refspa <- subset(refspa, is.element(refspa@data[ , commonCol], Data[ , commonCol]))
    }
  }

  return(refspa)
}


boxplotCarto.generic.f <- function(polyZones, Data, factSpatial, exprBP){

  ## Purpose: Créer des boxplots en subplots.
  ## ----------------------------------------------------------------------
  ## Arguments: polyZones: polygones des zones de regroupement spatial.
  ##            Data : données (data.frame).
  ##            factSpatial: facteur de regroupement spatial.
  ##            exprBP: formule du boxplot
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 janv. 2013, 16:52

  env <- environment()

  # Données séparées par subplot :
  tmpData <- split(Data, Data[ , factSpatial])

  # Couleurs de différentiation des zones :
  colZones <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))
  names(colZones) <- row.names(coordinates(polyZones))

  metrique <- as.character(exprBP)[2]

  file_temp <- tempfile()
  dir.create(file_temp)

  plot_name <- lapply(row.names(coordinates(polyZones)), function(i){
    flnm <- paste(file_temp, paste("boxplot_", as.integer(runif(n = 1, min = 1, max = 500)), ".png", sep = ""), sep = "/")
    png(filename = flnm)

    BPtmp <- boxplotPAMPA.f(exprBP, data = tmpData[[i]],
      main = i, cex = par("cex"), col.main = "black")
    title(main = i)

    # Légende du facteur de second niveau :
    legendBoxplot.f(terms = attr(terms(exprBP), "term.labels"), data = Data, cex = 1)

    dev.off()
    return(flnm)
  })

  plot_name <- list(plot_name, "dir" = file_temp)

  return(plot_name)
}


barplotCarto.generic.f <- function(polyZones, Data, factSpatial, metrique, listFact){

  ## Purpose: Créer des barplots en subplots.
  ## ----------------------------------------------------------------------
  ## Arguments: polyZones: polygones des zones de regroupmeent spatial.
  ##            Data : données (data.frame).
  ##            factSpatial: facteur de regroupement spatial.
  ##            metrique : la métrique choisie.
  ##            listFact : liste du (des) facteur(s) de regroupement
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 fevr. 2013, 16:15

  env <- environment()

  # Données séparées par subplot :
  tmpData <- split(Data, Data[ , factSpatial])

  # Couleurs de différentiation des zones :
  colZones <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))
  names(colZones) <- row.names(coordinates(polyZones))

  file_temp = tempfile()
  dir.create(file_temp)

  plot_name <- lapply(row.names(coordinates(polyZones)), function(i){
    flnm <- paste(file_temp, paste("barplot_", as.integer(runif(n = 1, min = 1, max = 500)), ".png", sep = ""), sep = "/")
    png(filename = flnm)

    BPtmp <- barplotPAMPA.f(metrique = metrique, listFact = listFact, Data = tmpData[[i]],
      cex = par("cex"), col.main = "black", cex.names = 0.8)
    # Titre :
    title(main = i)
    assign(x = "BPtmp", value = BPtmp, envir = env)

    dev.off()

    return(flnm)
  })

  plot_name <- list(plot_name, "dir" = file_temp)

  return(plot_name)
}


subplotCarto.unitobs.f <- function(graphType, metrique, factSpatial, factSpatialSel,
  factGraph, factGraphSel, listFact, listFactSel, tableMetrique, #bbox = NULL,
  dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des boxplots ou barplots répartis sur des cartes.
  ## ----------------------------------------------------------------------
  ## Arguments: graphType : type de graphique (bar|box-plot).
  ##            metrique : la métrique choisie.
  ##            factSpatial : facteur de regroupement spatial.
  ##            factSpatialSel : sélection sur le facteur de regroupement
  ##                             spatial.
  ##            factGraph : le facteur sélection des espéces.
  ##            factGraphSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            bbox : emprise spatial (bouding box) ; toute la carte si
  ##                   NULL.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 dec. 2012, 09:33

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]
  listFactSel <- listFactSel[length(listFactSel):1]

  listFact <- listFact[unlist(listFact) != ""]
  listFact <- listFact[length(listFact):1]

  # Concaténation
  facteurs <- c(factSpatial, factGraph, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factSpatialSel), list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités
  # sélectionnées

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

  # Identification des différentes modalités (espèces) du graphique à générer :
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
    tmpData <- na.omit(agregationTableParCritere.f(Data = tmpData, metrique = metrique,
      facteurs = c("observation.unit", "size.class"),
      dataEnv = dataEnv, listFact = c(listFact, factSpatial)))
  }else{
    if (tableMetrique == "unit"){
      # Calcul des indices de biodiversité sur sélection d'espèces :
      tmp <- do.call(rbind,
        lapply(getOption("P.MPA"), function(MPA){
          calcBiodiv.f(Data = tmpData,
            refesp = get("refesp", envir = dataEnv),
            MPA = MPA,
            unitobs = "observation.unit", code.especes = "species.code",
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
      tmpData <- na.omit(agregationTableParCritere.f(Data = tmpData,
        metrique = metrique,
        facteurs = c("observation.unit"),
        dataEnv = dataEnv,
        listFact = c(listFact, factSpatial)))
    }
  }

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list(tmpData)

  # Création du graphique si le nombre d'observations < au minimum défini dans les options :
  if (nrow(tmpData) < getOption("P.MinNbObs")){
    warning(mltext("WP2boxplot.W.n.1"),
      "(", paste(iFactGraphSel, collapse = ", "), ") < ", getOption("P.MinNbObs"),
      mltext("WP2boxplot.W.n.2"))
  }else{
    # Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    graphFile <- resFileGraph.f(
      metrique = metrique,
      factGraph = factGraph,
      modSel = iFactGraphSel,
      listFact = listFact,
      dataEnv = dataEnv,
      ext = "wmf",
      prefix = paste("carte_", graphType, sep = ""),
      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(iFactGraphSel) > 1 || iFactGraphSel[1] == ""),
        "%03d",
        ""),
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs"))

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :[!!!]
    mainTitle <- graphTitle.carto.f(metrique = metrique,
      modGraphSel = iFactGraphSel,
      factGraph = factGraph,
      listFact = listFact,
      factSpatial = factSpatial,
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        ifelse(tableMetrique == "unitSpSz",
          "unitobs(CL)",
          "unitobs")))

    # Graphiques :
    refspa <- get("refspa", envir = dataEnv)

    # Affichage du fond de carte :
    if (exists("baseMap", envir = dataEnv)){
      baseMap <- get("baseMap", envir = dataEnv)
    }else{
      baseMap <- tryCatch(maptools::unionSpatialPolygons	(SpP = refspa,
        IDs = ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
          getOption("P.landMods")),
          "terre", "mer")),
        error = function(e){
          return(refspa)
      })

      assign("baseMap", baseMap, envir = dataEnv)
    }

    unitobs <- get("unitobs", envir = dataEnv)
    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = tmpData, fact = factSpatial)

    if ( ! is.na(factSpatialSel[1])){
      refspaTmp <- subset(refspa, is.element(refspa@data[ , factSpatial], factSpatialSel))
    }else{
      refspaTmp <- subset(refspa,
        is.element(refspa@data[ , factSpatial], unitobs[ , factSpatial]) &
        ! is.na(refspa@data[ , factSpatial]))
    }

    # Agrégation des zones à l'échelle souhaitée :
    polyZones <- maptools::unionSpatialPolygons	(SpP = refspaTmp, IDs = refspaTmp@data[ , factSpatial])

    col <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))

    plot_name <- switch(graphType,
      "boxplot" = {
        boxplotCarto.generic.f(polyZones = polyZones, Data = tmpData,
          factSpatial = factSpatial, exprBP = exprBP)
      },
      "barplot" = {
        barplotCarto.generic.f(polyZones = polyZones, Data = tmpData,
          factSpatial = factSpatial, metrique = metrique, listFact = listFact)
      }
    )

    x <- as.vector(unique(refspaTmp@data[[factSpatial]]))
    x <- x[order(x)]
    y <- seq(length(x))
    df <- as.data.frame(cbind(x, y))
    colnames(df) <- c(factSpatial, "row")

    spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
    spdf@plotOrder <- seq(length(spdf@plotOrder))

    map <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles("CartoDB.Positron") %>%
      leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
        opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)

    map <- map %>%
      leaflet::addPolygons(data = spdf, group = factSpatial, color = "#000000",
        label = spdf@data[[factSpatial]], weight = 1, opacity = 1, fillColor = col, fillOpacity = 0.75) %>%
      leafpop::addPopupImages(plot_name[[1]], group = factSpatial, width = 300)

    list_layers <- c("Base Map", factSpatial)

    do.call(file.remove, list(list.files(plot_name[[2]], full.names = TRUE)))
    unlink(plot_name[[2]], recursive = TRUE)

    map <- map %>%
      leaflet::addLayersControl(
        overlayGroups = list_layers,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

    # ##################################################
    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
    }else{}

    # Sauvegarde des infos sur les données et statistiques :
    if (getOption("P.saveStats")) {
      infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "unitobs", type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = c(factSpatial, rev(listFact)),
        listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
        dataEnv = dataEnv, baseEnv = baseEnv)
    }else{}

    return(map)
  }
}


symbColCarto.esp.f <- function(graphType, metrique, factSpatial, factSpatialSel,
  factGraph, factGraphSel, tableMetrique,
  dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des cartes avec soit des symboles de taille variable
  ##          soit une échelle de couleur (variable simples, pas de facteur
  ##          explicatif). Agrégation par espèce par unitobs.
  ## ----------------------------------------------------------------------
  ## Arguments: graphType : type de graphique (symboles|couleurs).
  ##            metrique : la métrique choisie.
  ##            factSpatial : facteur de regroupement spatial.
  ##            factSpatialSel : sélection sur le facteur de regroupement
  ##                             spatial.
  ##            factGraph : le facteur sélection des espèces.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            tableMetrique : nom de la table de métriques.
  ##            bbox : emprise spatial (bouding box) ; toute la carte si
  ##                   NULL.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 27 fevr. 2013, 10:40

  # Concaténation
  facteurs <- c(factSpatial, factGraph) # Concaténation des facteurs

  selections <- c(list(factSpatialSel), list(factGraphSel)) # Concaténation des leurs listes de modalités
  # sélectionnées

  # Données pour la série de boxplots :
  tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL)

  # Identification des différents graphiques à générer:
  if (factGraph == ""){                # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){       # Toutes les modalités.
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
  # ###############################################################
  # Boucle de création des graphiques (par facteur de séparation) :

  mean_metrics <- sapply(as.character(unique(tmpData[, factGraph])), function(fact_agr){
    sapply(as.character(unique(tmpData[which(tmpData[,factGraph] == fact_agr), factSpatial])), function(fact_spa){
      mean(tmpData[which(tmpData[,factGraph] == fact_agr & tmpData[,factSpatial] == fact_spa), metrique])
    })
  })

  max_metric <- max(mean_metrics)
  zoom_factor <- 0
  if (max_metric*10 < 50){
    while (max_metric*10 < 50){
      max_metric <- max_metric*10
      zoom_factor <- zoom_factor + 1
    }
  } else if (max_metric > 50){
    while (max_metric > 50){
      max_metric <- max_metric/10
      zoom_factor <- zoom_factor - 1
    }
  }

  for (modGraphSel in iFactGraphSel){
    if (length(iFactGraphSel) > 1){
      print(paste("Création de la couche pour ", factGraph, " == '", modGraphSel, "'...", sep = ""))
      print(paste("Creation of the layer for ", factGraph, " == '", modGraphSel, "'...", sep = ""))
    }
    # Préparation des données pour un graphique :
    if (modGraphSel == ""){         # ...si pas de facteur de séparation des graphiques
      tmpDataMod <- tmpData
    }else{                          # ...sinon.
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

    # Agrégation spatiale (simple) :
    tmpDataMod2 <- agregationSpatiale.f(Data = tmpDataMod, metrique = metrique,
      facteur = factSpatial, dataEnv = dataEnv)

    # Sauvegarde temporaire des données :
    DataBackup[[modGraphSel]] <<- tmpDataMod2

    graphFileTmp <- resFileGraph.f(
      metrique = metrique,
      factGraph = factGraph,
      modSel = modGraphSel,
      listFact = factSpatial,
      dataEnv = dataEnv,
      ext = "wmf",
      prefix = paste("carte_", graphType, sep = ""),
      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(iFactGraphSel) > 1 || iFactGraphSel[1] == ""),
        "%03d",
        ""),
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"espece"},
        "unitSpSz" = {"CL_espece"},
        "unit" = {"unitobs"},
        "espece"))


    # graphFile uniquement si nouveau fichier :
    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    # ################################################

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    mainTitle <- graphTitle.carto.f(metrique = metrique,
      modGraphSel = modGraphSel, factGraph = factGraph,
      listFact = NULL,
      factSpatial = factSpatial,
      type = switch(tableMetrique, # différents types de graphs en fonction de la table de données.
        "unitSp" = {"spEspece"},
        "unitSpSz" = {"spCL_espece"},
        "spEspece"))

    # Graphiques :
    refspa <- get("refspa", envir = dataEnv)

    # Affichage du fond de carte :
    if (exists("baseMap", envir = dataEnv)){
      baseMap <- get("baseMap", envir = dataEnv)
    }else{
      baseMap <- tryCatch(maptools::unionSpatialPolygons	(SpP = refspa,
        IDs = ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
          getOption("P.landMods")),
          "terre", "mer")),
        error = function(e){
          return(refspa)
      })

      assign("baseMap", baseMap, envir = dataEnv)
    }

    unitobs <- get("unitobs", envir = dataEnv)
    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = tmpDataMod, fact = factSpatial)

    if ( ! is.na(factSpatialSel[1])){
      refspaTmp <- subset(refspa, is.element(refspa@data[ , factSpatial], factSpatialSel))
    }else{
      refspaTmp <- subset(refspa,
        is.element(refspa@data[ , factSpatial], unitobs[ , factSpatial]) &
        ! is.na(refspa@data[ , factSpatial]))
    }

    # Agrégation des zones à l'échelle souhaitée :
    polyZones <- maptools::unionSpatialPolygons	(SpP = refspaTmp, IDs = refspaTmp@data[ , factSpatial])

    col <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))

    if (graphType == "symboles"){
      x <- as.vector(unique(refspaTmp@data[[factSpatial]]))
      x <- x[order(x)]
      y <- seq(length(x))
      df <- as.data.frame(cbind(x, y))
      colnames(df) <- c(factSpatial, "row")

      spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
      spdf@plotOrder <- seq(length(spdf@plotOrder))

      map_data <- cbind(tmpDataMod2, coordinates(polyZones))
      colnames(map_data) <- c(metrique, factSpatial, "x", "y")

      if (!exists("map")){
        map <- leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addProviderTiles("CartoDB.Positron") %>%
          leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
             opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)
        map <- map %>%
          leaflet::addPolygons(data = spdf, group = factSpatial, color = "#000000",
            label = spdf@data[[factSpatial]], weight = 1, opacity = 1, fillColor = col, fillOpacity = 0.75)
        list_layers <- c("Base Map", factSpatial)
      }

      layer_name <- paste(factSpatial, modGraphSel, sep = " - ")
      list_layers <- c(list_layers, layer_name)

      map <- map %>%
        leaflet::addCircleMarkers(lng = ~x, lat = ~y, radius = map_data[,metrique]*10**zoom_factor,
          group = layer_name, label = spdf@data[[factSpatial]], popup = paste("<div>", metrique, " = ",
          map_data[,metrique], "</div>", sep = ""), color = "#000000", weight = 1, opacity = 1,
          fillColor = col, fillOpacity = 0.75, data = map_data)
    }
    else{
      x <- tmpDataMod2[order(tmpDataMod2[,factSpatial]),]
      df <- as.data.frame(x)

      spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
      spdf@plotOrder <- seq(length(spdf@plotOrder))

      if (!exists("map")){
        map <- leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addProviderTiles("CartoDB.Positron") %>%
          leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
            opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)
        list_layers <- c("Base Map")
      }
      rampcols = colorRampPalette(colors = c("white", "black"), space="Lab")(nrow(spdf@data)/2)
      pal <- colorNumeric(
        palette = rampcols, #"Blues", #YlGnBu,YlOrRd
        domain = spdf@data[, metrique])

      layer_name <- paste(factSpatial, modGraphSel, sep = " - ")
      list_layers <- c(list_layers, layer_name)
      map <- map %>%
        leaflet::addPolygons(data = spdf, group = layer_name, color = "#000000",
          label = spdf@data[[factSpatial]], popup = paste("<div>", metrique, " = ", spdf@data[, metrique], "</div>", sep = ""),
          weight = 1, opacity = 1, fillColor = ~pal(spdf@data[, metrique]), fillOpacity = 1) %>%
        leaflet::addLegend("bottomright", group = layer_name, pal = pal, values = spdf@data[, metrique],
          title = metrique, labFormat = leaflet::labelFormat(), opacity = 1)
    }

    # ###################################################
    # Fermeture de graphiques et sauvegarde de fichiers :

    # On ferme les périphériques PNG en mode fichier individuel :
    if (isTRUE(getOption("P.graphPNG"))){
      if (plotted){
        dev.off()

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpDataMod2, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpDataMod2,
            agregLevel = ifelse(tableMetrique == "unitSpSz", "spCL_espece", "spSpecies"),
            type = "graph", metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = factSpatial, listFactSel = factSpatialSel,
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }else{
      # Sauvegarde en wmf si pertinent et souhaité :
      if (plotted && ! getOption("P.graphPDF")){
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
          savePlot(graphFile, type = "wmf", device = dev.cur())
        }else{}

        # Sauvegarde des données :
        if (getOption("P.saveData")){
          writeData.f(filename = graphFile, Data = tmpDataMod2, cols = NULL)
        }else{}

        # Sauvegarde des statistiques :
        if (getOption("P.saveStats")){
          infoStats.f(filename = graphFile, Data = tmpDataMod2,
            agregLevel = ifelse(tableMetrique == "unitSpSz", "spCL_espece", "spSpecies"),
            type = "graph", metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
            listFact = factSpatial, listFactSel = factSpatialSel,
            dataEnv = dataEnv, baseEnv = baseEnv)
        }else{}
      }else{}
    }
  }  # Fin de boucle graphique.

  map <- map %>%
    leaflet::addLayersControl(
      overlayGroups = list_layers,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (graphType == "symboles"){
    labels <- c(5/10**zoom_factor, 15/10**zoom_factor, 25/10**zoom_factor)
    colors <- rep("#000000", times = length(labels))
    sizes <- c(10, 30, 50)
    map <- map %>%
      leaflet::addLegend(
        title = metrique,
        colors = paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px"),
        labels = paste0("<div style='display: inline-block; height: ", sizes,
          "px; margin-top: 4px; line-height: ", sizes, "px;'>", labels, "</div>"),
        opacity = 0.75)
  }
  return(map)
}


agregationSpatiale.f <- function(Data, metrique, facteur, dataEnv){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments: Data : Le jeu de données à agréger.
  ##            metrique : la métrique agrégée.
  ##            facteurs : le facteur
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  2 mai 2013, 16:52

  # traitements selon le type de métrique :
  casMetrique <- c("number" = "mean",
    "mean.length" = "w.mean.n",
    "taille_moy" = "w.mean",
    "biomass" = "w.mean",
    "Biomass" = "w.mean",
    "weight" = "mean",
    "mean.weight" = "w.mean",
    "density" = "w.mean",
    "Density" = "w.mean",
    "CPUE" = "w.mean",
    "CPUE.biomass" = "w.mean",
    "pres.abs" = "pres",
    "abundance.prop.SC" = "w.mean",
    "biomass.prop.SC" = "w.mean",
    # Benthos :
    "colonies" = "mean",
    "coverage" = "w.mean",
    "mean.size.colonies" = "w.mean",
    # SVR (expérimental) :
    "number.max" = "max",
    "number.sd" = "mean",
    "density.max" = "max",
    "density.sd" = "w.mean",
    "biomass.max" = "max",
    "spawning.success" = "w.mean",
    "spawnings" = "mean",
    "readable.tracks" = "mean",
    "tracks.number" = "mean",
    # Biodiversité :
    "Delta" = "mean",
    "DeltaStar" = "mean",
    "DeltaPlus" = "mean",
    "hill" = "mean",
    "LambdaPlus" = "mean",
    "simpson.l" = "mean",
    "pielou" = "mean",
    "species.richness" = "mean",
    "relative.SR.data" = "mean",
    "relative.SR.region" = "mean",
    "relative.SR.region.phylum" = "mean",
    "relative.SR.site" = "mean",
    "relative.SR.site.phylum" = "mean",
    "SDeltaPlus" = "mean",
    "simpson" = "mean")


  # Ajout des dimensions d'unitobs si moyennes pondérées :
  if (casMetrique[metrique] == "w.mean"){
    unitobs <- get("unitobs", envir = dataEnv)

    Data <- cbind(Data,
      data.frame(dimobs = do.call("*",
        unitobs[match(Data[ , "observation.unit"], unitobs[ , "observation.unit"]),
          c("obs.dim1", "obs.dim2")])))

    # Les dimobs peuvent être des NAs (notamment dans les cas SVR) :
    if (all(is.na(Data[ , "dimobs"]))){
      Data[ , "dimobs"] <- 1
    }else{}

  }else{}

  # Agrégation de la métrique selon le facteur spatial :
  switch(casMetrique[metrique],
    "mean" = {
      res <- tapply(Data[ , metrique],
        as.list(Data[ , facteur, drop = FALSE]),
        function(x){
          ifelse(all(is.na(x)),
            NA,
            mean(x, na.rm = TRUE))
        })
    },
    "w.mean" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , facteur, drop = FALSE]),
          function(ii){
            ifelse(all(is.na(Data[ii, metrique])),
              NA,
              weighted.mean(Data[ii, metrique],
                Data[ii, "dimobs"],
                na.rm = TRUE))
      })
    },
    stop("Not implemented!")
  )

  # Nom des dimensions
  names(dimnames(res)) <- c(facteur)

  # Transformation vers format long :
  reslong <- as.data.frame(as.table(res), responseName = metrique)
  reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

  return(reslong)
}


symbColCarto.unitobs.f <- function(graphType, metrique, factSpatial, factSpatialSel,
  factGraph, factGraphSel, tableMetrique, #bbox = NULL,
  dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Produire des cartes avec soit des symboles de taille variable
  ##          soit une échelle de couleur (variable simples, pas de facteur
  ##          explicatif). Agrégation par unitobs.
  ## ----------------------------------------------------------------------
  ## Arguments: graphType : type de graphique (symboles|couleurs).
  ##            metrique : la métrique choisie.
  ##            factSpatial : facteur de regroupement spatial.
  ##            factSpatialSel : sélection sur le facteur de regroupement
  ##                             spatial.
  ##            factGraph : le facteur sélection des espéces.
  ##            factGraphSel : la sélection de modalités pour ce dernier.
  ##            tableMetrique : nom de la table de métriques.
  ##            bbox : emprise spatial (bouding box) ; toute la carte si
  ##                   NULL.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 27 fevr. 2013, 10:40

  # Concaténation
  facteurs <- c(factSpatial, factGraph) # Concaténation des facteurs

  selections <- c(list(factSpatialSel), list(factGraphSel)) # Concaténation des leurs listes de modalités
  # sélectionnées

  # Données pour la série :
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

  # Identification des différentes modalités (espèces) du graphique à générer :
  if (factGraph == ""){                 # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factGraphSel[1])){        # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
    }else{                # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
    }
  }

  # Agrégation des observations / unité d'observation :
  if (tableMetrique == "unitSpSz" && factGraph != "size.class"){
    tmpData <- na.omit(agregationTableParCritere.f(Data = tmpData,
      metrique = metrique, facteurs = c("observation.unit", "size.class"),
      dataEnv = dataEnv, listFact = factSpatial))
  }else{
    if (tableMetrique == "unit"){
      # Calcul des indices de biodiversité sur sélection d'espèces :
      tmp <- do.call(rbind,
        lapply(getOption("P.MPA"), function(MPA) {
          calcBiodiv.f(Data = tmpData, refesp = get("refesp", envir = dataEnv),
          MPA = MPA, unitobs = "observation.unit", code.especes = "species.code",
          nombres = getOption("P.nbName"), indices = metrique, dataEnv = dataEnv)
      }))

      # On rajoute les anciennes colonnes :
      tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
        tmpData[match(tmp$observation.unit, tmpData$observation.unit),
          !is.element(colnames(tmpData),
            c(colnames(tmp), getOption("P.nbName"), "species.code")), drop = FALSE])
    }else{
      tmpData <- na.omit(agregationTableParCritere.f(Data = tmpData, metrique = metrique,
        facteurs = c("observation.unit"), dataEnv = dataEnv, listFact = factSpatial))
    }
  }

  # Suppression des 'levels' non utilisés :
  tmpData <- dropLevels.f(tmpData)

  # Agrégation spatiale (simple) :
  tmpData2 <- agregationSpatiale.f(Data = tmpData, metrique = metrique, facteur = factSpatial,
    dataEnv = dataEnv)

  # Sauvegarde temporaire des données utilisées pour les graphiques
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list(tmpData2)

  # Création du graphique si le nombre d'observations < au minimum défini dans les options :
  if (nrow(tmpData2) < getOption("P.MinNbObs")){
    warning(mltext("WP2boxplot.W.n.1"), "(", paste(iFactGraphSel, collapse = ", "), ") < ",
      getOption("P.MinNbObs"), mltext("WP2boxplot.W.n.2"))
  }else{
    graphFile <- resFileGraph.f(
      metrique = metrique,
      factGraph = factGraph,
      modSel = iFactGraphSel,
      listFact = factSpatial,
      dataEnv = dataEnv,
      ext = "wmf",
      prefix = paste("carte_", graphType, sep = ""),
      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(iFactGraphSel) > 1 || iFactGraphSel[1] == ""),
        "%03d",
        ""),
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "CL_unitobs",
        "unitobs")
    )

    # Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :[!!!]
    mainTitle <- graphTitle.carto.f(metrique = metrique,
      modGraphSel = iFactGraphSel,
      factGraph = factGraph,
      listFact = NULL,
      factSpatial = factSpatial,
      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
        "spCL_unitobs",
        ifelse(tableMetrique == "unitSpSz",
          "spUnitobs(CL)",
          "spUnitobs")))

    # Graphiques :
    refspa <- get("refspa", envir = dataEnv)

    # Affichage du fond de carte :
    if (exists("baseMap", envir = dataEnv)){
      baseMap <- get("baseMap", envir = dataEnv)
    }else{
      baseMap <- tryCatch(maptools::unionSpatialPolygons	(SpP = refspa,
        IDs = ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
          getOption("P.landMods")),
          "terre", "mer")),
        error = function(e){
          return(refspa)
      })

      assign("baseMap", baseMap, envir = dataEnv)
    }

    unitobs <- get("unitobs", envir = dataEnv)
    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = tmpData, fact = factSpatial)

    if ( ! is.na(factSpatialSel[1])){
      refspaTmp <- subset(refspa, is.element(refspa@data[ , factSpatial], factSpatialSel))
    }else{
      refspaTmp <- subset(refspa,
        is.element(refspa@data[ , factSpatial], unitobs[ , factSpatial]) &
        ! is.na(refspa@data[ , factSpatial]))
    }

    # Agrégation des zones à l'échelle souhaitée :
    polyZones <- maptools::unionSpatialPolygons	(SpP = refspaTmp, IDs = refspaTmp@data[ , factSpatial])

    col <- PAMPAcolors.f(n = length(polyZones), palette = getOption("P.zonesPalette"))

    if (graphType == "symboles"){
      x <- as.vector(unique(refspaTmp@data[[factSpatial]]))
      x <- x[order(x)]
      y <- seq(length(x))
      df <- as.data.frame(cbind(x, y))
      colnames(df) <- c(factSpatial, "row")

      spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
      spdf@plotOrder <- seq(length(spdf@plotOrder))

      map_data <- cbind(tmpData2, coordinates(polyZones))
      colnames(map_data) <- c(metrique, factSpatial, "x", "y")

      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
          opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)
      map <- map %>%
        leaflet::addPolygons(data = spdf, group = factSpatial, color = "#000000",
          label = spdf@data[[factSpatial]], weight = 1, opacity = 1, fillColor = col, fillOpacity = 0.75)
      list_layers <- c("Base Map", factSpatial)

      max_metric <- max(map_data[, metrique])
      zoom_factor <- 0
      if (max_metric*10 < 50){
        while (max_metric*10 < 50){
          max_metric <- max_metric*10
          zoom_factor <- zoom_factor + 1
        }
      } else if (max_metric > 50){
        while (max_metric > 50){
          max_metric <- max_metric/10
          zoom_factor <- zoom_factor - 1
        }
      }

      map <- map %>%
        leaflet::addCircleMarkers(lng = ~x, lat = ~y, radius = map_data[,metrique]*10**zoom_factor,
          group = metrique, label = spdf@data[[factSpatial]], popup = paste("<div>", metrique, " = ", map_data[, metrique], "</div>", sep = ""),
          color = "#000000", weight = 1, opacity = 1, fillColor = col, fillOpacity = 0.75, data = map_data)
      list_layers <- c(list_layers, metrique)
    }
    else{
      x <- tmpData2[order(tmpData2[,factSpatial]),]
      df <- as.data.frame(x)

      spdf <- sp::SpatialPolygonsDataFrame(polyZones, df, match.ID = FALSE)
      spdf@plotOrder <- seq(length(spdf@plotOrder))

      map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addPolygons(data = baseMap, group = "Base Map", color = "#000000", weight = 1,
          opacity = 1, fillColor = "#FFFFFF", fillOpacity = 0.5)
      list_layers <- c("Base Map")

      rampcols = colorRampPalette(colors = c("white", "black"), space = "Lab")(nrow(spdf@data)/2)
      pal <- colorNumeric(palette = rampcols, domain = spdf@data[, metrique])

      list_layers <- c(list_layers, metrique)
      map <- map %>%
        leaflet::addPolygons(data = spdf, group = metrique, color = "#000000",
          label = spdf@data[[factSpatial]], popup = paste("<div>", metrique, " = ", spdf@data[, metrique], "</div>", sep = ""),
          weight = 1, opacity = 1, fillColor = ~pal(spdf@data[, metrique]), fillOpacity = 1) %>%
        leaflet::addLegend("bottomright", group = metrique, pal = pal, values = spdf@data[, metrique],
          title = metrique, labFormat = leaflet::labelFormat(), opacity = 1)
    }

    # ##################################################
    # Sauvegarde des données :
    if (getOption("P.saveData")){
      writeData.f(filename = graphFile, Data = tmpData2, cols = NULL)
    }else{}

    # Sauvegarde des infos sur les données et statistiques :
    if (getOption("P.saveStats")){
      infoStats.f(filename = graphFile, Data = tmpData2,
        agregLevel = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
          "spCL_unitobs",
          ifelse(tableMetrique == "unitSpSz",
            "spUnitobs(CL)",
            "spUnitobs")),
        type = "graph",
        metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
        listFact = factSpatial, listFactSel = factSpatialSel,
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
    map <- map %>%
      leaflet::addLayersControl(
        overlayGroups = list_layers,
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )

    if (graphType == "symboles"){
      labels <- c(5/10**zoom_factor, 15/10**zoom_factor, 25/10**zoom_factor)
      colors <- rep("#000000", times = length(labels))
      sizes <- c(10, 30, 50)
      map <- map %>%
        leaflet::addLegend(
          colors = paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px"),
          labels = paste0("<div style='display: inline-block; height: ", sizes,
            "px; margin-top: 4px; line-height: ", sizes, "px;'>", labels, "</div>"),
          opacity = 0.75)
    }
    return(map)
  }
}


selectModalitesSpatiales.f <- function(tableMetrique, facts = NULL, selections = NULL,
  metrique = NULL, nextStep, dataEnv){

  ## Purpose: Sélection des modalités d'un facteur.
  ## ----------------------------------------------------------------------
  ## Arguments: factor : le nom du facteur sélectionné.
  ##            tableMetrique : nom de la table des métriques.
  ##            nextStep : étape suivante.                     [!!!] on devrait pouvoir s'en passer  [yr: 18/1/2012]
  ##            level : l'ordre du facteur (0 pour celui de séparation des
  ##                    graphiques, 1, 2,... pour les suivants).
  ##            env : environnement de la fonction appelante.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 dec. 2012, 16:47

  level <- 0                         # [!!!] à vérifier.

  # Pour les indices de biodiversité recalculés, il faut utiliser "unitSp" et une métrique adaptée.
  if (is.element(nextStep, c("spBarBoxplot.unitobs", "spSymbols.unitobs")) && tableMetrique == "unit"){
    tableMetrique <- "unitSp"
    metrique <- getOption("P.nbName")
  }else{}

  tmp <- subsetToutesTables.f(metrique = metrique, facteurs = facts, selections = selections,
    dataEnv = dataEnv, tableMetrique = tableMetrique , exclude = level + 1)

  return(tmp)
}


champsRefspa.f <- function(dataEnv, first = c("ZONE.SURVE", "GROUP.OF.SITES", "MPA")){

  ## Purpose: récupérer la liste des champs du référentiel spatial
  ##          (qui sont également disponible dans les unitobs).
  ## ----------------------------------------------------------------------
  ## Arguments: dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 d?c. 2012, 17:27

  refspa <- get("refspa", envir = dataEnv)

  isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa))
  if (isPoly){
    # Récupération de la table des données seule :
    refspa <- refspa@data
  }else{}

  # Noms des colonnes d'unitobs :
  unitobsColnames <- colnames(get("unitobs", envir = dataEnv))

  # Noms des colonnes du référentiel spatial, susceptibles d'être utilisées comme facteurs spatiaux :
  refspaColnames <- sort(colnames(refspa)[# présentes dans unitobs :
    is.element(colnames(refspa), unitobsColnames) &
      # ...à l'exception de :
      ! is.element(colnames(refspa), c("OBJECTID", "SITE.SURFACE", "SITE.centrX", "SITE.centrY")) &
      # ...et étant soit des facteurs soit des entiers :
      is.element(sapply(refspa, class), c("factor", "integer"))])

  # Liste ordonnée :
  return(c("",
    first[is.element(first, refspaColnames)],
    "",
    refspaColnames[! is.element(refspaColnames, first)]))
}
