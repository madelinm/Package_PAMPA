# Fonctions pour les statistiques descriptives


#' @import maptools
#' @importFrom rgeos gSimplify
#' @import leafpop
#' @import dplyr


#' @title Nombre de site visites par an
#'
#' @description Cree un graph (et une carte si refspa disponible) presentant le nombre de visites
#' de chaque sites par annees
#'
#' @param site chr, selection des sites a etudier, si NA, tous les sites seront selectionnes
#' @param year chr, selection des annees a etudier, si NA toutes les annees seront selectionnees
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' PAMPA::station_year.f(site = c("Mbe Kouen", "Laregnere"), year = c("2008", "2009", "2010"),
#'   .dataEnv, .baseEnv)
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
    args.legend = list("x" = "topright", "inset" = -0.08, "xpd" = NA,
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
}


#' @title Nombre moyen d'annee de visite pour chaque site
#'
#' @description Cree un graph (et une carte si refspa disponible) presentant le nombre de moyen
#' d'annee de visite par site
#'
#' @param site chr, selection des sites a etudier, si NA, tous les sites seront selectionnes
#' @param year chr, selection des annees a etudier, si NA toutes les annees seront selectionnees
#' @param dataEnv environnement de stockage des donnees
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


#' @title Occurence des especes par site et par annee
#'
#' @description Cree un graph (et une carte si refspa disponible) presentant le nombre d'occurrence
#' des especes par site et par annee
#'
#' @param site chr, selection des sites a etudier, si NA, tous les sites seront selectionnes
#' @param year chr, selection des annees a etudier, si NA toutes les annees seront selectionnees
#' @param species chr, selection des especes a etudier, si NA toutes les especes seront selectionnees
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#' @examples
#' PAMPA::occurence_especes_site_annee.f(site = NA, year = NA,
#'   species = c("Acanthurus_sp2", "Chaetodon_kleinii", "Naso_tonganus", "Scaridae"),
#'   .dataEnv, .baseEnv)
#'
#' @export
occurence_especes_site_annee.f <- function(site = NA, year = NA, species = NA,
  dataEnv, baseEnv = .GlobalEnv){

  # Récupération des données
  # Get the datas
  obs <- get("obs", envir = dataEnv)
  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  if (is.na(site[1])){
    site <- unique(unitobs[["site"]])
  }
  if (is.na(year[1])){
    year <- unique(unitobs[["year"]])
  }
  if (is.na(species[1])){
    species <- unique(refesp[["scient.name"]])
  }

  # Récupération des colonnes qui nous intéressent
  # Keep only columns with interest
  df_obs <- obs[c("species.code", "observation.unit")]
  df_unitobs <- unitobs[c("observation.unit", "site", "year")]
  df_refesp <- refesp[c("species.code", "scient.name")]

  # Fusions des tableaux
  # Merge the data table
  df_final <- dplyr::left_join(df_obs, df_unitobs, by = "observation.unit")
  df_final <- dplyr::left_join(df_final, df_refesp, by = "species.code")

  # Suppression des colonnes qui ne servent plus
  # Keep only interesting columns
  df_final <- df_final[c("scient.name", "site", "year")]

  # On ne garde que les données sélectionnées par l'utilisateur
  # Keep only data selected by user
  df_final <- df_final[which(
    is.element(df_final$site, site) &
    is.element(df_final$year, year) &
    is.element(df_final$scient.name, species)
  ),]

  # Conversion des données en caractères et suppression des facteurs pour éviter les surprises
  # Convert data in character and suppression of factors to have no troubles
  df_final$scient.name <- as.character(df_final$scient.name)
  df_final$site <- as.character(df_final$site)
  df_final$year <- as.character(df_final$year)

  # Création du tableau de données final
  # Creation of the final data frame
  df <- as.data.frame(table(df_final))

  if (!exists("refspa", envir = dataEnv) | is.null(get("refspa", envir = dataEnv))){
    for (iSite in unique(df$site)){
      barplotPAMPA.f(metrique = "Freq", listFact = c("scient.name", "year"), Data = df[which(df$site == iSite),])
    }
  }else{
    map_occurence_esp.f(df, dataEnv, baseEnv)
  }
}


map_occurence_esp.f <- function(data, dataEnv, baseEnv = .GlobalEnv){

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

  df <- as.vector(unique(unitobs[which(is.element(unitobs$site, unique(data)$site)), fact]))
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

  vectSites <- as.character(unique(data$site))
  col <- PAMPAcolors.f(n = length(vectSites), palette = getOption("P.zonesPalette"))

  file_temp = tempfile()
  dir.create(file_temp)

  for(iSite in 1:length(vectSites)){
    siteName <- vectSites[iSite]
    cat("Region:", siteName, "\n")

    # Création du barplot
    # Barplot creation
    data_plot <- data[which(data$site == siteName),]
    flnm <- paste(file_temp, paste("barplot_", siteName, ".png", sep = ""), sep = "/")
    title <- paste("Nombre d'occurence des espèces \npour le site ", siteName, sep = "")
    metrique <- "Freq"
    listFact <- c("scient.name", "year")
    png(filename = flnm)

    barplotPAMPA.f(metrique = metrique, listFact = listFact, Data = data_plot, main = title)

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
}
