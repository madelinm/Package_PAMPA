# Fonctions pour les statistiques descriptives


#' @import mapview
#' @import maptools
#' @import rgeos
#' @import leafpop
#' @import dplyr


#' @title Nombre de site visites par an
#'
#' @description Cree un graph (et une carte si refspa disponible) presentant le nombre de visites
#' de chaque sites par annees
#'
#' @param site selection des sites a etudier, si NA, tous les sites seront selectionnes
#' @param year selection des annees a etudier, si NA toutes les annees seront selectionnees
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' #test
#'
#' @export
station_year.f <- function(site = NA, year = NA, dataEnv, baseEnv){
  # Récupération des données
  # Data recovery
  unitobsData <- get("unitobs", envir = dataEnv)
  if (is.na(site[1])){
    site <- unique(unitobsData[["site"]])
  }
  if (is.na(year[1])){
    year <- unique(unitobsData[["year"]])
  }

  # Création de la table de données
  # creation of the data table
  station_data <- unitobsData[which(is.element(unitobsData[["site"]], site) &
    is.element(unitobsData[["year"]], year)), "site"]
  year_data <- unitobsData[which(is.element(unitobsData[["year"]], year) &
    is.element(unitobsData[["site"]], site)), "year"]

  station_year_table <- table(as.character(station_data), as.character(year_data))

  # Si pas de référentiel spatial, barplot normal
  # If no spatial reference table, basic barplot
  if (!exists("refspa", envir = dataEnv) | is.null(get("refspa", envir = dataEnv))){
    title <- "Nombre de visites par site par années"
    listFact <- c("site", "year")
    barplot_station_year.f(station_year_table, main = title, listFact = listFact)
  }
  # Sinon, carte avec le nombre de visites par année pour chaque site.
  # Else, map with the number of visit by year for each site.
  else{
    map_station_year.f(station_year_table, dataEnv, baseEnv)
  }

  # for (i in nrow(station_year_table)){
  #   barplot(station_year_table[i,], beside = TRUE)
  # }
  #
  # moyenne_annee_visite <- sapply(seq(nrow(station_year_table)-1), function(x){
  #   mean(station_year_table[x,] > 0)
  # })
  # sites <- row.names(station_year_table[1:nrow(station_year_table)-1,])
  # moyenne_annee_visite_table <- cbind(sites, moyenne_annee_visite)

}


barplot_station_year.f <- function(heights, main, listFact, cex = getOption("P.cex"), ...){

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

  # On retire les noms de colonnes de "heights" pour les rajouter manuellement ensuite sur le
  # graphique (meilleurs contrôle) ; uniquement si deux dimensions :
  # heights' colnames are removing to add them manually after on the graphic (better control) ;
  # only if two dimensions :

  hack_dim <- FALSE
  if (length(dim(heights)) > 1){
    xnames <- colnames(heights)
    colnames(heights) <- NULL
    if (is.null(xnames)){
      xnames <- row.names(heights)
      row.names(heights) <- NULL
      hack_dim <- TRUE
    }
  }else{
    xnames <- row.names(heights)
    row.names(heights) <- NULL

    if (is.null(xnames)){
      xnames <- names(heights)
    }
  }

  barPlotTmp <- barplot(heights,
    beside = TRUE,
    main =
      if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title"))){
        main
      }else{
        NULL
      },
    xlab = "",
    las = 1,
    col = PAMPAcolors.f(n = nrow(heights)),
    cex.lab = cex,
    cex.axis = cex,
    legend.text = ifelse(length(listFact) > 1 && ! isSubplot(), TRUE, FALSE),
    args.legend = list("x" = "topright", "inset" = 0, "xpd" = NA,
      "title" = Capitalize.f(varNames[listFact[1], "nom"])),
    ...)

  # Axe des abs. (facteurs) :
  mtext(text = xnames, side = 1, line = ifelse(isSubplot(), 0.5, 0.9),
    at = if (!isTRUE(hack_dim)){
      if (length(dim(heights)) > 1) {apply(barPlotTmp, 2, mean)}else{barPlotTmp}
    }else {barPlotTmp},
    cex = cex * par("cex"))

  # Labels des axes :
  if (getOption("P.axesLabels")){
    mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
      side = 1, line = ifelse(isSubplot(), 1.6, 2.3), cex = cex)
  }else{}
}


map_station_year.f <- function(data, dataEnv, baseEnv = .GlobalEnv){

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

#    graphFileTmp <- resFileGraph.f(
#      metrique = metrique,
#      factGraph = factGraph,
#      modSel = iFactGraphSel,
#      listFact = listFact,
#      dataEnv = dataEnv,
#      ext = "wmf",
#      prefix = paste("carte_", ifelse(graphType == "boxplot", "boxplot", "barplot"), sep = ""),
#      sufixe = ifelse(getOption("P.plusieursGraphPage") && (length(iFactGraphSel) > 1 || iFactGraphSel[1] == ""),
#        "%03d",
#        ""),
#      type = ifelse(tableMetrique == "unitSpSz" && factGraph != "size.class",
#        "CL_unitobs",
#        "unitobs"))
#
#    # graphFile uniquement si nouveau fichier :
#    # grahFile only if new file
#    if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

    refspa <- get("refspa", envir = dataEnv)
    unitobs <- get("unitobs", envir = dataEnv)
    fact <- "SITE"                                # Le mettre en argument ?

    # Création du fond de carte
    # Base map creation
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

    df <- as.vector(unique(unitobs[which(is.element(unitobs$site, row.names(data))), fact]))
    data_test <- cbind(df, seq(length(df)))
    colnames(data_test) <- c(fact, "row")

    # Réduction du référentiel spatial aux données d'intérêt
    # Spatial reference table reduction

    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = data_test, fact = fact)

    mapview::mapviewOptions(basemaps = "CartoDB.Positron")
    map <- mapview::mapview(baseMap, col.regions = "#FFFFFF", legend = FALSE,
      layer.name = "BaseMap", popup = NULL, label = FALSE, homebutton = FALSE)

    # Fonction pour agrégé les polygones correspondant à un même site
    # Function to merge polygons corresponding to the same site
    UnionSimplifyPolByPol <- function(subSite, precision = 0){
      out <- c()
      for(iSubSite in 1:length(subSite)){
#        cat("Adding:", subSite@data[iSubSite, "SITE"], "\n")
        toAdd <- rgeos::gSimplify(as(subSite[iSubSite,], "SpatialPolygons"),
          tol = precision, topologyPreserve = TRUE)
        if(is.null(out)){
          out <- toAdd
        }else{
          toUnite <- rbind(out, toAdd)
          out <- unionSpatialPolygons(toUnite,
            IDs = rep(1,2),
            threshold = precision)
        }
      }
      return(out)
    }

    list_site <- unique(unitobs[,c("site", "SITE", "CODE.SITE")])
    refspa@data$SITE <- as.character(refspa@data$SITE)

    vectSites <- row.names(data)
    col <- PAMPAcolors.f(n = length(vectSites), palette = getOption("P.zonesPalette"))

    file_temp = tempfile()
    dir.create(file_temp)

    for(iSite in 1:length(vectSites)){
      siteName <- vectSites[iSite]
      cat("Region:", siteName, "\n")

      # Création du barplot
      # Barplot creation
      data_plot <- as.matrix(data[vectSites[iSite],])
#      names(data_plot) <- colnames(data)
      flnm <- paste(file_temp, paste("barplot_", siteName, ".png", sep = ""), sep = "/")
      title <- paste("Nombre de visites par année \npour le site ", siteName, sep = "")
      listFact <- c("year")
      png(filename = flnm)

      barplot_station_year.f(data_plot, main = title, listFact = listFact)

      dev.off()

      # Regroupement des sites
      # Grouping of sites
      refspa_site <- list_site[which(list_site$site == siteName), "SITE"]
      polygons_of_interest <- which(is.element(refspa$SITE, refspa_site))

      region <- UnionSimplifyPolByPol(refspa[polygons_of_interest,], 0)
      region <- spChFIDs(region, siteName)

      # Transformation de region en Spatial Polygons Data Frame
      # Transform region in Spatial Polygons Data Frame
      df <- as.data.frame(cbind(siteName, 1))
      colnames(df) <- c("Site", "row")

      spdf <- SpatialPolygonsDataFrame(region, df, match.ID = FALSE)

      # Création de la carte
      # Map creation
      map <- map +
        mapview::mapview(spdf, col.regions = col[iSite], legend = TRUE,
          layer.name = siteName, label = FALSE, popup = leafpop::popupImage(flnm))
    }
    print(map)
    print("Vous pouvez naviguez entre les couches à l'aide du bouton à gauche.")
    print("You can switch between layer with the button at the left.")

    unlink(file_temp, recursive = TRUE)

#    # ###################################################
#    # Fermeture de graphiques et sauvegarde de fichiers :
#
#    # On ferme les périphériques PNG en mode fichier individuel :
#    if (isTRUE(getOption("P.graphPNG"))){
#      if (plotted){
#        dev.off()
#
#        # Sauvegarde des données :
#        if (getOption("P.saveData")){
#          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
#        }else{}
#
#        # Sauvegarde des statistiques :
#        if (getOption("P.saveStats")){
#          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
#            metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
#            listFact = c(factSpatial, rev(listFact)),
#            listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
#            dataEnv = dataEnv, baseEnv = baseEnv)
#        }else{}
#      }else{}
#    }else{
#      # Sauvegarde en wmf si pertinent et souhaité :
#      if (plotted && ! getOption("P.graphPDF")){
#        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF"))){
#          savePlot(graphFile, type = "wmf", device = dev.cur())
#        }else{}
#
#        # Sauvegarde des données :
#        if (getOption("P.saveData")){
#          writeData.f(filename = graphFile, Data = tmpData, cols = NULL)
#        }else{}
#
#        # Sauvegarde des statistiques :
#        if (getOption("P.saveStats")){
#          infoStats.f(filename = graphFile, Data = tmpData, agregLevel = "species", type = "graph",
#            metrique = metrique, factGraph = factGraph, factGraphSel = modGraphSel,
#            listFact = c(factSpatial, rev(listFact)),
#            listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre
#            # intuitif.
#            dataEnv = dataEnv, baseEnv = baseEnv)
#        }else{}
#      }else{}
#    }
#
#  }  # Fin de boucle graphique.
#
#   # On ferme les périphériques PDF ou PNG restants :
#   if (getOption("P.graphPDF") && plotted) {
#     dev.off()
#
#     # Sauvegarde des données :
#     if (getOption("P.saveData")) {
#       writeData.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup, cols = NULL)
#     }else{}
#
#     # Sauvegarde des statistiques :
#     if (getOption("P.saveStats")){
#       infoStats.f(filename = sub("\\%03d", "00X", graphFile), Data = DataBackup,
#         agregLevel = "species", type = "graph",
#         metrique = metrique, factGraph = factGraph, factGraphSel = factGraphSel,
#         listFact = c(factSpatial, rev(listFact)),
#         listFactSel = c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
#         dataEnv = dataEnv, baseEnv = baseEnv)
#     }else{}
#
#     # Inclusion des fontes dans le(s) pdf(s) si souhaité :
#     if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts")){
#       i <- 1
#
#       # On parcours tous les fichiers qui correspondent au motif :
#       tmpFile <- sub("\\%03d", formatC(i, width = 3, flag = "0"), graphFile)
#       while (is.element(basename(tmpFile), dir(dirname(graphFile))) &&
#         # Si pas de remplacement effectif, application pour i == 1 uniquement :
#         (i == 1 || grepl(pattern = "\\%03d", graphFile)))
#       {
#         tryCatch(embedFonts(file = tmpFile),
#           error = function(e){
#             warning(mltext("WP2boxplot.W.pdfFonts"))
#         })
#
#         i <- i + 1
#       }
#     }else{}
#   }else{}
}


#' @title Moyenne année visite
#'
#' @description Nombre moyen d'année de visite pour chaque site
#'
#' @param site selection des sites a etudier, si NA, tous les sites seront selectionnes
#' @param year selection des annees a etudier, si NA toutes les annees seront selectionnees
#' @param dataEnv environnement de stockage des données
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::moyenne_annee_visite.f(site = NA, year = NA, .dataEnv, .baseEnv)
#'
#' @export
moyenne_annee_visite.f <- function(site = NA, year = NA, dataEnv, baseEnv = .GlobalEnv){
  # Récupération des données
  # Data recovery
  unitobsData <- get("unitobs", envir = dataEnv)
  if (is.na(site[1])){
    site <- unique(unitobsData[["site"]])
  }
  if (is.na(year[1])){
    year <- unique(unitobsData[["year"]])
  }

  # Création de la table de données
  # creation of the data table
  station_data <- unitobsData[which(is.element(unitobsData[["site"]], site) &
    is.element(unitobsData[["year"]], year)), "site"]
  year_data <- unitobsData[which(is.element(unitobsData[["year"]], year) &
    is.element(unitobsData[["site"]], site)), "year"]

  station_year_table <- table(as.character(station_data), as.character(year_data))

  moyenne_annee_visite <- sapply(seq(nrow(station_year_table)), function(x){
    mean(station_year_table[x,] > 0)
  })
  moyenne_annee_visite_table <- as.matrix(moyenne_annee_visite)
  row.names(moyenne_annee_visite_table) <- row.names(station_year_table)
  if (!exists("refspa", envir = dataEnv) | is.null(get("refspa", envir = dataEnv))){
    title <- "Nombre moyen d'année de visite \npour chaque site"
    listFact <- c("year", "site")
    barplot_station_year.f(moyenne_annee_visite_table, main = title, listFact = listFact)
  }
  else{
    map_mean_year.f(moyenne_annee_visite_table, dataEnv, baseEnv)
  }
}


map_mean_year.f <- function(data, dataEnv, baseEnv = .GlobalEnv){

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

    refspa <- get("refspa", envir = dataEnv)
    unitobs <- get("unitobs", envir = dataEnv)
    fact <- "SITE"                                # Le mettre en argument ?

    # Création du fond de carte
    # Base map creation
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

    df <- as.vector(unique(unitobs[which(is.element(unitobs$site, row.names(data))), fact]))
    data_test <- cbind(df, seq(length(df)))
    colnames(data_test) <- c(fact, "row")

    # Réduction du référentiel spatial aux données d'intérêt
    # Spatial reference table reduction

    refspa <- subsetRefspaToData.f(refspa = refspa, unitobs = unitobs, Data = data_test, fact = fact)

    mapview::mapviewOptions(basemaps = "CartoDB.Positron")
    map <- mapview::mapview(baseMap, col.regions = "#FFFFFF", legend = FALSE,
      layer.name = "BaseMap", popup = NULL, label = FALSE, homebutton = FALSE)

    # Fonction pour agréger les polygones correspondant à un même site
    # Function to merge polygons corresponding to the same site
    UnionSimplifyPolByPol <- function(subSite, precision = 0){
      out <- c()
      for(iSubSite in 1:length(subSite)){
#        cat("Adding:", subSite@data[iSubSite, "SITE"], "\n")
        toAdd <- rgeos::gSimplify(as(subSite[iSubSite,], "SpatialPolygons"),
          tol = precision, topologyPreserve = TRUE)
        if(is.null(out)){
          out <- toAdd
        }else{
          toUnite <- rbind(out, toAdd)
          out <- unionSpatialPolygons(toUnite,
            IDs = rep(1,2),
            threshold = precision)
        }
      }
      return(out)
    }

    list_site <- unique(unitobs[,c("site", "SITE", "CODE.SITE")])
    refspa@data$SITE <- as.character(refspa@data$SITE)

    vectSites <- row.names(data)
    col <- PAMPAcolors.f(n = length(vectSites), palette = getOption("P.zonesPalette"))

    data_mean <- data.frame()
    for(iSite in 1:length(vectSites)){
      siteName <- vectSites[iSite]
      moyenne <- data[iSite,]
      cat("Region:", siteName, "\n")

      # Regroupement des sites
      # Grouping of sites
      refspa_site <- list_site[which(list_site$site == siteName), "SITE"]
      polygons_of_interest <- which(is.element(refspa$SITE, refspa_site))

      region <- UnionSimplifyPolByPol(refspa[polygons_of_interest,], 0)
      region <- spChFIDs(region, siteName)

      # Transformation de region en Spatial Polygons Data Frame
      # Transform region in Spatial Polygons Data Frame
      df <- as.data.frame(cbind(siteName, 1))
      colnames(df) <- c("Site", "row")

      spdf <- SpatialPolygonsDataFrame(region, df, match.ID = FALSE)

      # Création du jeu de données avec les moyennes
      # Creation of the dataset with the mean
      x <- mean(do.call(coordinates, list(refspa[polygons_of_interest,]))[,1])
      y <- mean(do.call(coordinates, list(refspa[polygons_of_interest,]))[,2])
      data_mean <- rbind(data_mean, cbind(siteName, x, y, moyenne))

      # Création de la carte
      # Map creation
      map <- map +
        mapview::mapview(spdf, col.regions = col[iSite], legend = FALSE,
          layer.name = siteName, label = FALSE, popup = FALSE)
    }
    data_mean$x <- as.numeric(as.character(data_mean$x))
    data_mean$y <- as.numeric(as.character(data_mean$y))
    data_mean$moyenne <- as.numeric(as.character(data_mean$moyenne))

    map <- map +
      mapview(data_mean, xcol = "x", ycol = "y", zcol = "siteName", col.region = col, cex = "moyenne",
        layer.name = "Moyenne annees visitees", grid = FALSE, label = "siteName",
        popup = "moyenne")
    print(map)
    print("Vous pouvez naviguez entre les couches à l'aide du bouton à gauche.")
    print("You can switch between layer with the button at the left.")
}

