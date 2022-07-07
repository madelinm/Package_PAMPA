#' Cette fonction permet de faire le chargement des donnees.
#'
#' Elle prend en entree les chemins des differents fichiers (sous forme d'un
#' vecteur nomme).
#'
#' Le referentiel espece local et le referentiel statial sont facultatifs.
#'
#' Le chemin du dossier "Results" est facultatif egalement. S'il n'est pas renseigne, les donnees
#' seront enregistrees dans un dossier nomme 'Results' dans le dossier de travail.
#' Elle prend egalement en entree deux environnements, celui ou stocker les donnees, et
#' l'environnement parent. Ces environnements peuvent tous les deux correspondre a l'environnement
#' global (.GlobalEnv).
#'
#' Le dossier de travail doit etre le dossier de stockage des donnees. Il s'agit d'un dossier
#' contenant un dossier nomme 'Data' ou sont stockees les donnees.
#'
#' Cette fonction charge les donnees a partir des chemins de fichiers donnes. Elle calcule ensuite
#' les poids pour chaque AMP. Puis, elle calcule les tables de metriques, qu'elle enregistre dans
#' l'environnement dataEnv afin de pouvoir les reutiliser plus tard. Elle renvoie ensuite les
#' differentes tables de donnees, sous forme d'une liste nommee.


#' @title Importation des donnees et calculs des poids
#'
#' @description Chargement des donnees depuis les chemins des fichiers et calcul des poids.
#'
#' @param filePathes chr, vecteur nomme contenant les chemins des fichiers :
#'  \itemize{unitobs}{ : chr, fichier unitobs}
#'  \itemize{obs}{ : chr, fichier d'observation}
#'  \itemize{refesp}{ : chr, referentiel especes}
#'  \itemize{locrefesp}{ : chr, referentiel especes local (facultatif)}
#'  \itemize{refspa}{ : chr, referentiel spatial (facultatif)}
#'  \itemize{ws}{ : chr, dossier de travail}
#'  \itemize{results}{ : chr, dossier ou enregistrer les donnees (facultatif)}
#'  \itemize{...}
#' @param dminMax seuil (en m) au-dessus duquel les observations ne sont pas prises en compte
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @return list, liste nommee contenant les tables de donnees :
#' \itemize{obs}{ : donnees d'observation}
#' \itemize{unitobs}{ : unites d'observation}
#' \itemize{refesp}{ : referentiel espece}
#'
#' @examples
#' # Creation d'environnements
#' .baseEnv <- environment()
#' .dataEnv <- new.env()
#'
#' assign(".baseEnv", .baseEnv, .GlobalEnv)
#' assign(".dataEnv", .dataEnv, .GlobalEnv)
#'
#' # Definition des chemins de l'environnement de travail et des fichiers
#' ws_path <- 'inst/example_data/'
#' unitobs_path <- 'inst/example_data/Data/UnitObs_Staviro_Example.txt'
#' obs_path <- 'inst/example_data/Data/Obs_Staviro_Example.txt'
#' refesp_path <- 'inst/example_data/Data/SpeciesReferenceTable_Example.txt'
#'
#' filePathes <- c(unitobs = unitobs_path, obs = obs_path, refesp =  refesp_path, ws = ws_path)
#'
#' # Lancement du chargement
#' data <- PAMPA::load_files.f(filePathes, dminMax = 5, .dataEnv, .baseEnv)
#'
#' @export

load_files.f <- function(filePathes, dminMax = 5, dataEnv, baseEnv){
  ## Purpose: fusionner les fonctions de chargement de données et de calcul des
  ##          poids (loadData.f & calcWeight.f)
  ## ---------------------------------------------------------------------------
  ## Arguments: filePathes : vecteur nommé des chemins de fichiers de
  ##                         données et dossiers, avec
  ##                         - unitobs : fichier d'unitobs.
  ##                         - obs : fichier d'observations.
  ##                         - refesp : référentiel espèces.
  ##                         - refspa : référentiel spatial (pas utilisé actuellement).
  ##                         - ws : dossier de travail.
  ##                         - ...
  ##            dataEnv : environnement où stocker les données.
  ##            baseEnv : environnement parent (interface).
  ##
  ## Output: liste nommé des tables de données, avec
  ##         - unitSpSz : table agrégée /classe de taille/sp/unitobs.
  ##         - unitSp : table agrégée /espèce/unitobs.
  ##         - unit : table agrégée /unitobs.
  ## ---------------------------------------------------------------------------

  # Lecture du fichier de configuration
  # Read the configuration file

  source(system.file("package_config/config.R", package = "PAMPA"))

  assign("varNames",
    read.csv(system.file(
      paste("file_translation/VariableNames_", tolower(ifelse(is.null(getOption("P.lang")), "en",
        getOption("P.lang"))), ".csv", sep = ""),
      package = "PAMPA"),
      header = TRUE, row.names = 1, stringsAsFactors = FALSE,
      fileEncoding = "latin1", quote = "\""),
    envir = .GlobalEnv)

  # Faire plus de vérifications ?

  if (is.na(filePathes["results"])){
    filePathes["results"] <- ifelse(as.logical(length(grep("Data/$|Data$", filePathes["ws"]))),
      sub("Data/$|Data$", "Results/", filePathes["ws"]),
      ifelse(as.logical(length(grep("/$", filePathes["ws"]))),
        paste(filePathes["ws"], "Results/", sep = ""),
        paste(filePathes["ws"], "/Results/", sep = "")
      )
    )
  }
  if (!dir.exists(filePathes["results"])){
    dir.create(filePathes["results"])
  }
  assign("filePathes", filePathes, envir = dataEnv)

  Data <- loadData.f(filePathes, dminMax, dataEnv, baseEnv = baseEnv)
  if (!is.benthos.f()){
    Data <- calcWeight.f(Data)
  }

  listInEnv.f(list = Data, env = dataEnv)

  # Calculation of metric tables :
  # Calcul des tables de metriques :
  metrics <- calcTables.f(obs = Data$obs, unitobs = Data$unitobs, refesp = Data$refesp, dataEnv = dataEnv)

  # Assigment of metric tables in the corresponding environment :
  # Assignement des tables de metriques dans l'environnement adequat :
  listInEnv.f(list = metrics, env = dataEnv)

  # Backup for restoration :
  # Sauvegarde pour restauration ulterieure :
  assign("backup", c(metrics,
    list(obs = Data$obs),
    tryCatch(list(".NombresSVR" = get(".NombresSVR", envir = dataEnv),
      ".DensitesSVR" = get(".DensitesSVR", envir = dataEnv)),
      error = function(e){NULL})),
    envir = dataEnv)

  # Export of metric tables
  # Export des tables de metriques
  exportMetrics.f(unitSpSz = metrics$unitSpSz, unitSp = metrics$unitSp, unit = metrics$unit,
    obs = Data$obs, unitobs = Data$unitobs, refesp = Data$refesp,
    filePathes = filePathes, baseEnv = baseEnv)

  # Export of species table
  # Exportation de la table des especes
  code_esp_obs <- Data$obs[,"species.code"]
  table_especes <- Data$refesp[which(Data$refesp$species.code %in% code_esp_obs),
    c('family', 'genus', 'scient.name')]
  table_especes <- table_especes[order(table_especes$family),]

  fileNm <- paste(filePathes["results"], "TableEspeces",
     ".csv", sep = "")
  tryCatch(write.csv(table_especes,
    file = fileNm,
    quote = TRUE, row.names = FALSE),
    error = function(e){
      warning(paste("Impossible d'écrire le fichier ", fileNm,
        ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""),
        call. = FALSE, immediate. = TRUE)
      errorLog.f(error = e, niv = -4)
    })

  # Calculation and exportation of year visited site number
  # Calcul et exportation du nombre de sites visites par an
  station <- Data[["unitobs"]][["site"]]
  year <- Data[["unitobs"]][["year"]]

  if (!all(is.na(station)) & !all(is.na(year))){
    station_year_table <- table(station, year)
    station_year_table <- cbind(station_year_table,
      Total = sapply(seq(nrow(station_year_table)), function(x){
        sum(station_year_table[x,])
      }))
    station_year_table <- rbind(station_year_table,
      Total = sapply(seq(ncol(station_year_table)), function(x){
        sum(station_year_table[,x])
      }))

    fileNm <- paste(filePathes["results"], "NombreSiteAnnee",
      ".csv", sep = "")
    tryCatch(write.csv(station_year_table,
      file = fileNm,
      quote = TRUE, row.names = TRUE),
      error = function(e){
        warning(paste("Impossible d'écrire le fichier ", fileNm,
          "\nIl est possibles qu'il soit ouvert par une autre application", sep = ""),
          call. = FALSE, immediate. = TRUE)
        errorLog.f(error = e, niv = -4)
      })

    moyenne_annee_visite <- sapply(seq(nrow(station_year_table)-1), function(x){
      mean(station_year_table[x,1:ncol(station_year_table)-1] > 0)
    })
    sites <- row.names(station_year_table[1:nrow(station_year_table)-1,])
    moyenne_annee_visite_table <- cbind(sites, moyenne_annee_visite)

    fileNm <- paste(filePathes["results"], "MoyenneAnneeVisiteSite",
      ".csv", sep = "")
    tryCatch(write.csv(moyenne_annee_visite_table,
      file = fileNm,
      quote = TRUE, row.names = FALSE),
      error = function(e){
        warning(paste("Impossible d'écrire le fichier ", fileNm,
          "\nIl est possibles qu'il soit ouvert par une autre application", sep = ""),
          call. = FALSE, immediate. = TRUE)
      })
  }
  return(Data)
}


loadData.f <- function(filePathes, dminMax = 5, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Lancement des différentes étapes du chargement des données.
  ## ---------------------------------------------------------------------------
  ## Arguments: filePathes : vecteur nommé des chemins de fichiers de
  ##                         données et dossiers, avec
  ##                         - unitobs : fichier d'unitobs.
  ##                         - obs : fichier d'observations.
  ##                         - refesp : référentiel espèces.
  ##                         - refspa : référentiel spatial (pas utilisé actuellement).
  ##                         - ws : dossier de travail.
  ##                         - ...
  ##            dataEnv : environnement où stocker les données.
  ##            baseEnv : environnement parent (interface).
  ##
  ## Output: liste nommé des tables de données, avec
  ##         - unitSpSz : table agrégée /classe de taille/sp/unitobs.
  ##         - unitSp : table agrégée /espèce/unitobs.
  ##         - unit : table agrégée /unitobs.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  2 déc. 2011, 10:59

  runLog.f(msg = c("--------------------------------------------------------------------------------",
    mltext("loadData.info.0")))

  # Réinitialisation de l'indicateur de sélection :
  options(P.selection = FALSE)

  ## ##################################################
  # Chargement des fichiers :

  # Fichier d'unités d'observations :
  refUnitobs <- loadUnitobs.f(pathUnitobs = filePathes["unitobs"])

  # Info sur les AMP du jeu actuel :
  options(P.MPA = as.character(unique(refUnitobs[ , getOption("P.MPAfield")])))

  # Fichier de référentiel spatial :
  refSpatial <- loadRefspa.f(pathRefspa = filePathes["refspa"])

  # [OBSIND]
  if (getOption("P.obsType") == "OBSIND"){
    refUnitobs <- unitobsNew.OBSIND.create.f(unitobs = refUnitobs,
      refspa = refSpatial, dataEnv = dataEnv)

  }else{}

  # Fusion de la table d'unités d'observation et de celle du référentiel spatial :
  if (!is.null(refSpatial)){
    assign(".unitobsSmall", refUnitobs, envir = dataEnv) # pour lien manuel.
    refUnitobs <- mergeSpaUnitobs.f(unitobs = refUnitobs, refspa = refSpatial)

  }

  # Fichier d'observations :
  tabObs <- loadObservations.f(pathObs = filePathes["obs"], dminMax)

  # Correspondance avec les nouvelles unités d'observations dans le cas "OBSIND" :
  if (getOption("P.obsType") == "OBSIND"){
    unitobsCorresp <- dataEnv$.unitobsCorresp

    tabObs[ , "observation.unit"] <- dataEnv$.unitobsCorresp[match(
      tabObs[ , "observation.unit"], unitobsCorresp[ , "observation.unit"]),
      "unitobsNew"]
  }else{}

  # Réduction aux unitobs existantes.
  tabObs <- checkUnitobs.in.obs.f(obs = tabObs, unitobs = refUnitobs)

  # Fichier du référentiel espèces :
  refEspeces <- loadRefEspeces.f(pathRefesp = filePathes["refesp"],
    pathRefesp.local = filePathes["locrefesp"], baseEnv = baseEnv)

  ## ##################################################
  ## Calcul des tables de métriques :
  Data <- list(obs = tabObs, unitobs = refUnitobs, refesp = refEspeces, refspa = refSpatial)

  return(Data)
}


loadUnitobs.f <- function(pathUnitobs) {

  ## Purpose: Chargement du fichier d'unités d'observations
  ## ---------------------------------------------------------------------------
  ## Arguments: pathUnitobs : chemin du fichier
  ##            dataEnv : environnement de l'interface
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 déc. 2011, 11:33

  # Lecture du fichier :
  unitobs <- read.table(pathUnitobs, sep = "\t", dec = ".", header = TRUE, encoding = "latin1",
    stringsAsFactors = TRUE)

  # Changement des noms de colonnes :
  colnames(unitobs) <- c(getOption("P.MPAfield"), "observation.unit",
    "observation.type", "site", "station", "geogr.descriptor1",
    "geogr.descriptor2", "sampling.rate", "day", "month", "year", "hour",
    "cloud.cover", "wind.direction", "wind.strength", "sea.condition", "current",
    "tide", "moon.phase", "latitude", "longitude", "protection.status",
    "before.after", "biotop", "biotop.2", "habitat1", "habitat2", "habitat3",
    "visibility", "min.depth", "max.depth", "obs.dim1", "obs.dim2", "nb.divers",
    "diver")

  # Traitement des NAs pour "geogr.descriptor1" :
  levels(unitobs$geogr.descriptor1) <- c(levels(unitobs$geogr.descriptor1), "NA")

  unitobs$geogr.descriptor1[is.na(unitobs$geogr.descriptor1)] <- "NA"

  # Vérification du type d'observations :
  unitobs <- checkType.unitobs.f(unitobs)

  # Si les unités d'observation sont ne sont pas des facteurs, on les force en facteur :
  # (typiquement si numérique)
  if (!is.factor(unitobs$observation.unit)){
    unitobs$observation.unit <- factor(as.character(unitobs$observation.unit))
  }

  # Si geogr.descriptor2 est au format année de campagne, renommer la colonne :
  if (is.temporal.f("geogr.descriptor2", unitobs)){
    colnames(unitobs)[colnames(unitobs) == "geogr.descriptor2"] <- "annee.campagne"
  }

  if (getOption("P.obsType") == "PecRec"){
    unitobs <- dimobsPecRec.f(refUnitobs = unitobs)
  }

  if (nrow(unitobs) != 0){
    unitobs[unitobs == "-999"] <- NA
  }

  # Reorganisation des niveaux de protection et autres facteurs :
  unitobs <- reorderFactors.f(Data = unitobs,
    which = c("protection.status", "geogr.descriptor1", "tide", "moon.phase"),
    type = c("protection", "protection", "tide", "moon"),
    warnings = FALSE)

  # Années : integer -> factor (nécessaire pour les analyses stats):
  unitobs$year <- factor(unitobs$year)

  return(unitobs)
}


checkType.unitobs.f <- function(unitobs){

  ## Purpose: test l'existence d'un seul type, sinon demande à
  ##          l'utilisateur d'en choisir un pour réduire le jeu de données.
  ##          Défini le type d'observation dans les options.
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : la table d'unités d'observation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 13 déc. 2011, 11:16

  if (length(unique(unitobs$observation.type)) > 1){
    print(paste(
      mltext("checkType.unitobs.msg.1"),
      mltext("checkType.unitobs.msg.2"),
      sep = ""))

    print(paste(mltext("checkType.unitobs.msg.3"), selectType))

    # Suppression des niveaux de facteur inutilisés :
    unitobs <- dropLevels.f(subset(unitobs, unitobs$observation.type == selectType))
  }

  options(P.obsType = unique(as.character(unitobs$observation.type)))

  return(unitobs)
}

#' @importFrom svDialogs dlg_list

chooseInList.f <- function(modList, fieldName, selectMode, ordered){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 janv. 2012, 18:00

  # création de la liste de choix :
  if (ordered){
    maliste <- sort(as.character(unique(modList)))
  }

  # On remplit la liste de choix :
  liste_choix = c()
  for (i in seq(along = maliste)){
    liste_choix <- c(liste_choix, maliste[i])
  }

  selectfact <- svDialogs::dlg_list(liste_choix,
    multiple = ifelse(selectMode == "single", FALSE, TRUE),
    title = paste(mltext("chooseInList.1"), "\"", fieldName, "\" :\n",
      ifelse(selectMode == "single",
        mltext("chooseInList.2"),
        mltext("chooseInList.3")), sep = ""))$res

  if (exists("selectfact") && length(selectfact)){
    return(selectfact)
  }else{
    return(NULL)
  }
}


dropLevels.f <- function(df, which = NULL){

  ## Purpose: Supprimer les 'levels' non utilisés des facteurs d'une
  ##          data.frame.
  ## ---------------------------------------------------------------------------
  ## Arguments: df : une data.frame
  ##            which : indice des colonnes à inclure (toutes par défaut).
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 10 août 2010, 13:29

  if (class(df) != "data.frame"){
    stop("'df' must be a data.frame")

  } else{
    if (is.null(which)){
      x <- as.data.frame(sapply(df, function(x)
      {
        return(x[ ,drop = TRUE])
      }, simplify = FALSE),
      stringsAsFactors = FALSE)
    }else{    # Cas où seulement certaines colonnes sont traitées.
      x <- df

      x[ , which] <- as.data.frame(sapply(df[ , which, drop = FALSE],
         function(x){
           return(x[ , drop = TRUE])
         }, simplify = FALSE),
         stringsAsFactors = FALSE)
    }

    return(x)
  }
}


is.temporal.f <- function(facteur, table){

  ## Purpose: test si un facteur est temporel ou non
  ## ---------------------------------------------------------------------------
  ## Arguments: facteur : le nom (chaîne de caractères) du facteur.
  ##            table : la table dans laquelle se trouve le champ.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 oct. 2010, 10:01

  res <- sapply(facteur, function(x){
    switch(x,
      an = {           # An est toujours censé être temporel.
        TRUE
      },
      annee.campagne = {           # Vérifié en amont.
        TRUE
      },
      geogr.descriptor2 = { # Dépend du format.
        ifelse(all(grepl("^[cC]?[[:digit:]]{4}$",
          as.character(table[ , "geogr.descriptor2"])), na.rm = TRUE),
          TRUE,
          FALSE)
      },
      FALSE)
  })
  return(res)
}


dimobsPecRec.f <- function(refUnitobs){

  ## Purpose: Transforme les DimObs en données exploitables
  ## (temps de pêche) pour la pêche récréative
  ## ----------------------------------------------------------------------
  ## Arguments: refUnitobs : table d'unitobs
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 déc. 2011, 11:46

  # Calcul de l'heure d'enquête :
  x.lt <- as.POSIXlt(as.character(refUnitobs$hour), format = "%Hh%M")
  refUnitobs$heureEnq <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600

  # Calcul de l'heure de début de pêche :
  x.lt <- as.POSIXlt(as.character(refUnitobs$obs.dim1), format = "%Hh%M")
  refUnitobs$heureDeb <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600

  # Calcul du temps de pêche :
  refUnitobs$obs.dim1 <- sapply(seq(length.out = nrow(refUnitobs)),
    function(i){
      switch(as.character(refUnitobs$heureEnq[i] < refUnitobs$heureDeb[i]),
        # Début le jour précédent (pas d'autre hypothèse sur plusieurs jours) :
        "TRUE" = (24 - refUnitobs$heureDeb[i]) + refUnitobs$heureEnq[i],
        # Début le jour même :
        "FALSE" = refUnitobs$heureEnq[i] - refUnitobs$heureDeb[i],
        # Incalculable :
        "NA" = NA)
    })

  # Temps calculé == 0 -> NA
  refUnitobs$obs.dim1[refUnitobs$obs.dim1 == 0] <- NA

  return(refUnitobs)
}


reorderFactors.f <- function(Data, which, type, warnings = FALSE){

  ## Purpose: Réordonner les modalités de facteurs selon des types
  ##          prédéfinis (avec tolérance à l'absence de colonne).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de données.
  ##            which : l'indice de la (des) colonne(s)
  ##                    (de préférence un nom).
  ##            type : type de facteur(s) (parmis "protection", "interest",
  ##                   "mobility", "position", "tide", "moon", "depth",
  ##                   "protection2", "size")
  ##            warnings : faut-il afficher les warnings pour colonne
  ##                       absente ou non ré-ordonnable ?
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  3 déc. 2012, 10:50

  env <- environment()

  for (i in 1:length(which)){
    tryCatch(
      {
        # Au besoin, on le transforme en facteur :
        if ( ! is.factor(Data[ , which[i]])){
          Data[ , which[i]] <- as.factor(Data[ , which[i]])
        }else{}

        # Option définissant les ordres pour ce type de facteur :
        option <- switch(type[i],
          "protection" = "P.statusOrder",
          "interest" = "P.interestOrder",
          "mobility" = "P.mobilityOrder",
          "position" = "P.positionOrder",
          "tide" = "P.tideOrder",
          "moon" = "P.moonOrder",
          "depth" = "P.depthOrder",
          "protection2" = "P.protection2Order",
          "size" = "P.sizeOrder")

        # Réorganisation des modalités :
        if (is.factor(Data[ , which[i]])){
          Data[ , which[i]] <- factor(Data[ , which[i]],
            levels = c(getOption(option)[is.element(getOption(option),
              levels(Data[ , which[i]]))],
            # ... les modalités pour lesquelles aucun ordre n'est spécifié
            # sont placées à la fin :
            levels(Data[ , which[i]])[!is.element(levels(Data[ , which[i]]),
              getOption(option))]))
        }else{
          stop("Not a factor!")
        }

        #assign("Data", Data, envir = env)
      },
      error = function(e)
      {
        # On affiche la nature de l'erreur (comme un warning) si besoin :
        if (warnings){
          print(mltext("warn.field.not.reordered.1"), which[i],
            mltext("warn.field.not.reordered.2"))
        }else{}
      }
    )
  }

  return(Data)
}


loadRefspa.f <- function(pathRefspa, baseEnv = .GlobalEnv){

  ## Purpose: chargement du référentiel spatial s'il existe.
  ## ---------------------------------------------------------------------------
  ## Arguments: pathRefspa : chemin vers le référentiel spatial.
  ##            baseEnv : environnement de l'interface principale.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  7 déc. 2011, 14:22

  if (missing(pathRefspa) || is.null(pathRefspa) || is.na(pathRefspa) ||
    length(pathRefspa) == 0 || ! file.exists(pathRefspa)){
    print(mltext("loadRefspa.info.1"))

    # Si type d'observation == "OBSIND", un shapefile est requis :
    if(getOption("P.obsType") == "OBSIND"){
      print(paste(
        mltext("loadRefspa.info.2"),
        mltext("loadRefspa.info.3"),
        sep = ""))

      stop(mltext("loadRefspa.info.4"))

    }else{}

    return(NULL)
  }else{
    if (grepl("Maps/[^/.]+\\.shp", pathRefspa, ignore.case = TRUE, perl = TRUE)){
      # Chargement du shapefile... :
      refSpatial <- loadShapefile.f(directory = dirname(pathRefspa),
        layer = sub(".shp$", "", basename(pathRefspa), ignore.case = TRUE, perl = TRUE))

      refSpatial@data <- reorderFactors.f(Data = refSpatial@data,
        which = c("STATUT.PRO", "STATUT.PAM", "PAMPA.STATUS", "PROTECTION.STATUS",
          "STATUT.PR2", "PROFONDEUR"),
        type = c(rep("protection", 4), "protection2", "depth"),
        warnings = FALSE)

    }else{
      # Si type d'observation == "OBSIND", un shapefile est requis :
      if(getOption("P.obsType") == "OBSIND"){
        print(paste(
          mltext("loadRefspa.info.2"),
          mltext("loadRefspa.info.3"),
          sep = ""))

        stop(mltext("loadRefspa.info.4"))
      }else{}

      # ...Sinon, chargement sous forme de fichier texte :
      refSpatial <- read.table(pathRefspa, sep = "\t", dec = ".", header = TRUE,
        encoding = "latin1", stringsAsFactors = TRUE)

      # Renommage des champs pour l'ancien format : [???]
      if (ncol(refSpatial) == 15){     # [!!!] à vérifier  [yr: 7/12/2011]
        colnames(refSpatial) <- c("ZONE.CODE", "ZONE", "MPA", "SITE", "STATION",
          "GROUP.OF.SITES", "ZONE.LONGITUDE", "ZONE.LATITUDE", "SURFACE",
          "SHORELINE", "PROTECTION.STATUS", "FISHING.ZONATION", "SIH.CODE",
          "PAMPA.STATUS", "NB.PERMANENT.MOORINGS")

      }else{}

      colnames(refSpatial) <- toupper(colnames(refSpatial))

      refSpatial <- reorderFactors.f(Data = refSpatial,
        which = c("STATUT.PRO", "STATUT.PAM", "PAMPA.STATUS", "PROTECTION.STATUS",
          "STATUT.PR2", "PROFONDEUR"),
        type = c(rep("protection", 4), "protection2", "depth"),
        warnings = FALSE)
    }
    return(refSpatial)
  }
}

#' @importFrom rgdal readOGR
#' @importFrom sp CRS spTransform coordinates
#' @importFrom maptools spCbind
#' @importFrom geosphere areaPolygon

loadShapefile.f <- function(directory, layer){

  ## Purpose: Load the spatial reference object from a shapefile.
  ## ---------------------------------------------------------------------------
  ## Arguments: directory : where the shapefile is located.
  ##            layer : layer name (filname, without extension).
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 nov. 2012, 17:12

  # Read the shapefile :
  refspa <- rgdal::readOGR(dsn = directory, layer = layer,
    use_iconv = TRUE, encoding = getOption("P.shapefileEncoding"), verbose = FALSE,
    stringsAsFactors = TRUE)

  colnames(refspa@data) <- gsub("_", ".", colnames(refspa@data), fixed = TRUE)
  colnames(refspa@data)[1] <- "OBJECTID"

  # Define the coordinate (without projection) for surface area processing :
  crsArea <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  refspa <- maptools::spCbind(refspa,
    data.frame("SITE.SURFACE" = geosphere::areaPolygon(
      sp::spTransform(x = refspa, CRSobj = crsArea)) / (10^6), # en km² !
        row.names = row.names(refspa@data), stringsAsFactors = TRUE))

  # Add centroids :
  tmpCentr <- as.data.frame(sp::coordinates(refspa), row.names = row.names(refspa@data),
    stringsAsFactor = TRUE)

  colnames(tmpCentr) <- paste("SITE.centr", c("X", "Y"), sep = "")

  refspa <- maptools::spCbind(refspa, tmpCentr)

}


unitobsNew.OBSIND.create.f <- function(unitobs, refspa, dataEnv){

  ## Purpose: Création d'une nouvelle table d'unités d'observations pour
  ##          les observations ponctuelles géoréférencées (OBSIND).
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : table des unités d'observations
  ##                      (réduite à ce stade).
  ##            refspa : référentiel spatial sous forme de shapefile.
  ##            dataEnv : environnmeent des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 25 avril 2013, 16:04

  # lien entre les unitobs "individuelles" et le référentiel spatial :
  unitobsTmp <- overlayUnitobs.f(unitobs = unitobs, refspa = refspa)

  # Construction d'un nouvel indice d'unités d'observations (intersection zone - jour
  # d'observation ; dites "regroupées") :
  unitobsTmp$unitobsNew <- paste("Z", unitobsTmp$OBJECTID, "-J", unitobsTmp$day,
    "-", unitobsTmp$month, "-", unitobsTmp$year, sep = "")

  # Sauvegarde d'une table de correspondance entre unités d'observations "individuelles" et
  # "regroupées" dans l'environnement des données :
  assign(x = ".unitobsCorresp",
    value = unitobsTmp[ , c("observation.unit", "unitobsNew")],
    envir = dataEnv)

  # Suppression des unités d'observation hors polygones :
  unitobsTmp <- unitobsTmp[ ! is.na(unitobsTmp$OBJECTID), ]

  # Réorganisation des colonnes :
  unitobsTmp <- unitobsTmp[ , c("study.area", "unitobsNew",
    "observation.type", "site", "station", "geogr.descriptor1",
    "geogr.descriptor2", "sampling.rate", "day", "month",
    "year", "hour", "cloud.cover", "wind.direction", "wind.strength",
    "sea.condition", "current", "tide", "moon.phase", "latitude",
    "longitude", "protection.status", "before.after", "biotop",
    "biotop.2", "habitat1", "habitat2", "habitat3", "visibility",
    "min.depth", "max.depth", "obs.dim1", "obs.dim2", "nb.divers",
    "diver", "OBJECTID")]

  colnames(unitobsTmp)[2] <- "observation.unit"

  # Agrégations (une ligne par unité d'observation) :
  unitobsNew <- do.call(rbind,
    lapply(split(unitobsTmp, unitobsTmp[ , "observation.unit"]),
      aggreg.unitobsNew.f, refspa = refspa))

  return(unitobsNew)
}

#' @importFrom sp SpatialPointsDataFrame over

overlayUnitobs.f <- function(unitobs, refspa){

  ## Purpose: Attribute the observation units to zones of the
  ##          spatial reference table.
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : observation unit table.
  ##            refspa : spatial reference object
  ##                     ("SpatialPolygonsDataFrame").
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 nov. 2012, 17:35

  # Georefences unitobs :
  spaUnitobs <- sp::SpatialPointsDataFrame(
    coords = cbind(x = as.numeric(sub(",", ".", unitobs$longitude, fixed = TRUE)),
      y = as.numeric(sub(",", ".", unitobs$latitude, fixed = TRUE))),
    data = unitobs,
    proj4string = refspa@proj4string,
    match.ID = TRUE)

  # Create the link to zones :
  unitobs <- cbind(unitobs,
    "OBJECTID" = as.character(sp::over(x = spaUnitobs, y = refspa)[ , "OBJECTID"]))

  return(unitobs)
}


aggreg.unitobsNew.f <- function(x, refspa){

  ## Purpose: Agréger des parties de la nouvelle table des unitobs pour
  ##          n'avoir plus qu'une ligne par unitobs.
  ## ----------------------------------------------------------------------
  ## Arguments: x : sous-table des unitobs avec les données d'une unitobs.
  ##            refspa : le référentiel spatial sous forme de shapefile.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 25 avril 2013, 17:04

  # Colonnes ne bénéficiant pas d'un traitement spécial :
  idx <- which(! is.element(colnames(x),
                            c("latitude", "longitude", "visibility", "min.depth", "max.depth",
                              "obs.dim1", "obs.dim2", "diver")))

  # On conserve la valeur si c'est la même partout dans la colonne :
  x[ , idx] <- sapply(x[ , idx],
    function(y){
      if (length(unique(y)) == 1){
        return(y)
      }else{
        return(rep(NA, length(y)))
      }
    }, simplify = FALSE)

  # Longitude/latitude = barycentre du polygone :
  x[ , c("latitude", "longitude")] <- refspa@data[x[ , "OBJECTID"],
    c("SITE.centrY", "SITE.centrX")]

  # Colonnes pour lesquelles on prend le minimum :
  x[ , c("visibility", "min.depth")] <- sapply(x[ , c("visibility", "min.depth")],
    function(x){
      if (all(is.na(x))){
        NA
      }else{
        min(x, na.rm = TRUE)
      }
    },
    simplify = FALSE)

  # Colonne pour laquelle on prend le maximum :
  x[ , "max.depth"] <- if (all(is.na(x[ , "max.depth"])))
  {
    NA
  }else{
    max(x[ , "max.depth"], na.rm = TRUE)
  }

  # Dimobs en km² :
  x[ , "obs.dim1"] <- refspa@data[x[ , "OBJECTID"],
    c("SITE.SURFACE")]

  x[ , "obs.dim2"] <- 1

  # Plusieurs observateurs possibles :
  x[ , "nb.divers"] <- length(unique(x[ , "diver"]))
  x[ , "diver"] <- paste(unique(x[ , "diver"]), collapse = ", ")

  return(x[1, !is.element(colnames(x), "OBJECTID"), drop = FALSE])
}


mergeSpaUnitobs.f <- function(unitobs, refspa, type = "auto"){

  ## Purpose: Fusion de la table des unitobs et du référentiel spatial si
  ##          adapté.
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : table des unités d'observation.
  ##            refspa : référentiel spatial ("SpatialPolygonsDataFrame" ou
  ##                     "data.frame").
  ##            type : type de lien (entre "auto" et "manual").
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  7 déc. 2011, 17:23

  # Si issu d'un shapefile
  isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa))
  if (isPoly){
    if (is.element("OBJECTID", colnames(slot(refspa, "data")))){
      slot(refspa, "data")[ , "OBJECTID"] <- as.character(slot(refspa, "data")[ , "OBJECTID"])
    }else{}
    # Correspondance automatique des unitobs et du référentiel spatial :
    unitobs <- overlayUnitobs.f(unitobs = unitobs, refspa = refspa)

    refspa <- refspa@data
  }else{}

  # Selection des colonnes de lien :
  links <- selectLink.f(unitobs = unitobs, refspa = refspa, type = type)

  if (is.null(links)){ # Rien n'est fait si pas de lien défini :
    res <- unitobs

    if (type[1] == "auto"){
      print(paste(
        mltext("mergeSpaUnitobs.info.1"),
        mltext("mergeSpaUnitobs.info.2"),
        sep = ""))
    }else{}


  }else{ # ...Sinon, merge des deux tables :
    res <- merge(unitobs, refspa,
      by.x = links["unitobs"], by.y = links["refspa"],
      all.x = TRUE, all.y = FALSE,
      suffixes = c(".KEEP",".SUPR"))

    res <- res[ , ! grepl("\\.SUPR$", colnames(res))]
    res <- res[ , colnames(res) != "station.1"]

    colnames(res) <- sub("\\.KEEP$", "", colnames(res))

    res <- res[ , c(colnames(unitobs),
      colnames(res)[!is.element(colnames(res), colnames(unitobs))])]

    if (type[1] == "auto"){
      if (isPoly){
        print(paste(
          mltext("mergeSpaUnitobs.info.3"),
          mltext("mergeSpaUnitobs.info.4"),
          sep = ""))
      }else{
        print(paste(
          mltext("mergeSpaUnitobs.info.5"),
          mltext("mergeSpaUnitobs.info.6"),
          sep = ""))
      }
    }else{}

  }

  return(res)
}


selectLink.f <- function(unitobs, refspa, type = "auto"){

  ## Purpose: Sélection des colonnes permettant un lien entre unitobs et
  ##          refspa.
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : table des unités d'observation ("data.frame").
  ##            refspa : référentiel spatial ("data.frame").
  ##            type : type de lien (entre "auto" et "manual").
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 nov. 2012, 18:18

  # Variable résultat,
  # indiquant si une correspondance a été trouvée (NULL == non-trouvée) :
  links <- NULL

  # Liens par défaut depuis les options :
  defaultLinks <- c(unitobs = getOption("P.linkUnitobs"),
    refspa = getOption("P.linkRefspa"))

  # Dans le cas "auto" :
  if (type[1] == "auto"){
    # Recherche de correspondances issues d'un shapefile :
    if (is.element("OBJECTID", colnames(unitobs)) &&
        is.element("OBJECTID", colnames(refspa)) &&
        ! all(is.na(unitobs$OBJECTID)) &&
        ! all(is.na(refspa$OBJECTID)) &&
        any(is.element(unitobs$OBJECTID, refspa$OBJECTID)))
    {
      links <- c(unitobs = "OBJECTID", refspa = "OBJECTID")
    }else{ # ...Sinon, vérification des défauts (options) :
      if (is.element(defaultLinks["unitobs"], colnames(unitobs)) &&
          is.element(defaultLinks["refspa"], colnames(refspa)) &&
          ! all(is.na(unitobs[ , defaultLinks["unitobs"]])) &&
          ! all(is.na(refspa[ , defaultLinks["refspa"]])) &&
          any(is.element(unitobs[ , defaultLinks["unitobs"]],
                         refspa[ , defaultLinks["refspa"]])))
      {
        links <- defaultLinks
      }else{}
    }
  }else{}

  # En cas de choix manuel ou échec des précédentes étapes :
  if (type[1] == "manual" || is.null(links)){
    links <- selectLink.interface.f(unitobs = unitobs, refspa = refspa, defaultLinks = defaultLinks)
  }else{}

  return(links)
}

#' @import tcltk

selectLink.interface.f <- function(unitobs, refspa,
  defaultLinks = c(unitobs = getOption("P.linkUnitobs"),
    refspa = getOption("P.linkRefspa"))){

  ## Purpose: Interface permettant à l'utilisateur de sélectionner les
  ##          champs de correspondance entre table d'unités d'observations
  ##          et référentiel spatial.
  ## ---------------------------------------------------------------------------
  ## Arguments: unitobs : table des unités d'observation ("data.frame").
  ##            refspa : référentiel spatial ("data.frame").
  ##            defaultLinks : vecteur des colonnes par défaut
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 22 nov. 2012, 11:01

  runLog.f(msg = c(mltext("logmsg.link.unitobs.refspa")))

  .BGcolor <- "#F7F5CE"

  # Fenêtre principale :
  WinLink <- tcltk::tktoplevel(background = "white")
  tcltk::tkwm.title(WinLink, mltext("selectLink.interface.title"))

  # Variables :
  Done <- tcltk::tclVar(0)
  env <- environment()

  # Valeur par défaut du champ pour unitobs :
  ColUnitobs <- if (is.element(defaultLinks["unitobs"], colnames(unitobs))){
    tcltk::tclVar(defaultLinks["unitobs"])
  }else{
    tcltk::tclVar(colnames(unitobs)[1])
  }

  # Valeur par défaut du champ pour refspa :
  ColRefspa <- if (is.element(defaultLinks["refspa"], colnames(refspa))){
    tcltk::tclVar(defaultLinks["refspa"])
  }else{
    tcltk::tclVar(colnames(refspa)[1])
  }

  #### définition des éléments de l'interface :
  # Frame d'aide :
  F.help <- tcltk::tkwidget(WinLink, "labelframe",
    text = "Aide", padx = 4, pady = 2,
    height = 30,
    borderwidth = 2, relief = "groove",
    font = tcltk::tkfont.create(weight = "bold", size = 10),
    foreground = "black",
    background = .BGcolor)

  L.help1 <- tcltk::tklabel(F.help,
    text = paste(
      mltext("selectLink.interface.H1.1"),
      mltext("selectLink.interface.H1.2"),
      sep = ""),
    wraplength = 500,
    font = tcltk::tkfont.create(weight = "normal", size = 10),
    foreground = "darkred",
    background = .BGcolor, justify = "left")

  L.help2 <- tcltk::tklabel(F.help,
    text = paste(
      mltext("selectLink.interface.H2.1"),
      mltext("selectLink.interface.H2.2"),
      sep = ""),
    wraplength = 500,
    font = tcltk::tkfont.create(weight = "normal", size = 10),
    foreground = "darkred",
    background = .BGcolor, justify = "left")

  # Logo :
  TableLink <- tcltk::tclVar()
  # crée un objet Tk image pour l'interface.
  tcltk::tcl("image", "create", "photo", TableLink, file = .fileimageLink)
  Img.tableLink <- tcltk::tklabel(F.help, image = TableLink, bg = "white") # -> label avec image.

  # Comboboxes :
  F.unitobs <- tcltk::tkframe(WinLink, background = "white")
  CB.unitobs <- tcltk::ttkcombobox(F.unitobs, value = colnames(unitobs),
    textvariable = ColUnitobs,
    state = "readonly", width = max(c(max(sapply(colnames(unitobs), nchar)), 23)),
    background = "white")

  F.refspa <- tcltk::tkframe(WinLink, background = "white")
  CB.refspa <- tcltk::ttkcombobox(F.refspa, value = colnames(refspa),
    textvariable = ColRefspa,
    state = "readonly", width = max(c(max(sapply(colnames(refspa), nchar)), 23)),
    background = "white")

  # Boutons :
  F.BT <- tcltk::tkframe(WinLink, background = "white")

  B.OK <- tcltk::tkbutton(F.BT, text = "OK", # bg = .BGcolor,
    command = function(){tcltk::tclvalue(Done) <- 1})

  B.Cancel <- tcltk::tkbutton(F.BT, text = "Annuler", # bg = .BGcolor,
    command = function(){tcltk::tclvalue(Done) <- 2})

  #### Placement des éléments :

  tcltk::tkgrid(L.help1, sticky = "w")
  tcltk::tkgrid(L.help2, sticky = "w")
  tcltk::tkgrid(Img.tableLink, sticky = "ew")

  tcltk::tkgrid(F.help, sticky = "ew", columnspan = 2)

  tcltk::tkgrid(tcltk::tklabel(F.unitobs, text = "Colonne des unités d'observation", background = "white"),
    padx = 4, pady = 4, sticky = "w")
  tcltk::tkgrid(CB.unitobs, padx = 4, pady = 4, sticky = "w")

  tcltk::tkgrid(tcltk::tklabel(F.refspa, text = "Colonne du référentiel spatial :", background = "white"),
    padx = 4, pady = 4, sticky = "w")
  tcltk::tkgrid(CB.refspa, padx = 4, pady = 4, sticky = "w")

  tcltk::tkgrid(F.unitobs, F.refspa, sticky = "w", padx = 10)
  tcltk::tkgrid.configure(F.refspa, sticky = "e")

  tcltk::tkgrid(tcltk::tklabel(WinLink, background = "white"))

  tcltk::tkgrid(B.OK,
    tcltk::tklabel(F.BT, text = "           ", background = "white"),
    B.Cancel, padx = 12, pady = 5, sticky = "w")
  tcltk::tkgrid(F.BT, padx = 4, pady = 8, columnspan = 2, sticky = "")
  tcltk::tkgrid.configure(B.Cancel, sticky = "e")

  #### évènements :

  tcltk::tkbind(WinLink, "<Destroy>", function(){
    tcltk::tclvalue(Done) <- 2 # En cas de destruction de la fenêtre.
  })

  tcltk::tkbind(CB.unitobs, "<Button-1>",
    expression(tcltk::tkconfigure(L.help1,
      font = tcltk::tkfont.create(weight = "normal", size = 10))))
  tcltk::tkbind(CB.refspa, "<Button-1>",
    expression(tcltk::tkconfigure(L.help1,
      font = tcltk::tkfont.create(weight = "normal", size = 10))))

  tcltk::tkfocus(WinLink)

  tcltk::tcl("update")

  repeat{
    tcltk::tkwait.variable(Done)

    if (tcltk::tclvalue(Done) == "1"){
      if ( # Il y a effectivement de correspondances (non-NA) :
        tmpUniq <- any(is.element(tmpColU <- na.omit(unitobs[ , tcltk::tclvalue(ColUnitobs)]),
                                  tmpColR <- na.omit(refspa[ , tcltk::tclvalue(ColRefspa)]))) &&
        # ...et les valeurs correspondantes de refspa sont uniques :
        length(tmpColR[is.element(tmpColR, tmpColU)]) ==
        length(unique(tmpColR[is.element(tmpColR, tmpColU)])))
      {
        links <- c(unitobs = tcltk::tclvalue(ColUnitobs),
                   refspa = tcltk::tclvalue(ColRefspa))
        break()
      }else{
        # Avertissement :
        tcltk::tkconfigure(L.help1,
          text = paste(
            ifelse(
              tmpUniq,
              mltext("selectLink.interface.H1.3"),
              mltext("selectLink.interface.H1.4")),
            mltext("selectLink.interface.H1.5"),
            sep = ""),
          font = tcltk::tkfont.create(weight = "bold", size = 10),
          justify = "left")

        tcltk::tkgrid.configure(L.help1, sticky = "w")

        tcltk::tclvalue(Done) <- "0"
      }
    }

    if (tcltk::tclvalue(Done) == "2"){
      links <- NULL
      break()
    }else{}
  }

  # Destruction si la fenêtre existe encore :
  if (tcltk::tclvalue(tcltk::tkwinfo("exists", WinLink)) == "1") tcltk::tkdestroy(WinLink)

  return(links)
}


loadObservations.f <- function(pathObs, dminMax = 5){

  ## Purpose: Chargement du fichier d'observations
  ## ---------------------------------------------------------------------------
  ## Arguments: pathObs : chemin (character) vers le fichier
  ##                      d'observations.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 déc. 2011, 14:38

  obs <- read.table(pathObs, sep = "\t", dec = ".", header = TRUE, encoding = "latin1",
    stringsAsFactors = TRUE)

  if (getOption("P.obsType") != "SVR"){ # [!!!] à modifier (et porter dans une autre fonction ?) [yr: 12/12/2011]
    colnames(obs) <- c("observation.unit", "sector", "species.code", "sex", "length",
      "size.class", "weight", "number", "min.distance", "max.distance")

    # Traitements particuliers pour les protocoles "traces de tortues" :
    if (getOption("P.obsType") == "TRATO"){
      obs <- obsFormatting.TRATO.f(obs)
    }else{}
  }else{
    # On renomme les colonnes + identification du type d'interpolation :
    switch(as.character(ncol(obs)),
      "10" = {
        colnames(obs) <- c("observation.unit", "rotation", "species.code", "sex", "length",
          "size.class", "weight", "number", "min.distance", "max.distance")
      },
      {
        print(paste(
          mltext("loadObservations.info.1"),
          mltext("loadObservations.info.2"),
          sep = ""))

        stop(mltext("loadObservations.info.3"))
    })

    obs$rotation <- as.numeric(obs$rotation)

    # On ne tient pas compte des observations à une distance > dminMax
    # (pas de subset car tendance à faire disparaître des unitobs des analyses) :
    idxSupr <- obs$min.distance > dminMax

    obs$number[idxSupr] <- 0
    obs$weight[idxSupr] <- NA
    obs$length[idxSupr] <- NA

    # obs <- subset(obs, min.distance <= dminMax)
  }

  # remplacement des -999 en NA
  if (as.logical(nrow(obs))){                      # != 0
    obs[obs == "-999"] <- NA
  }

  # nombre : numeric -> factor (nécessaire pour une bonne prise en compte dans
  # les analyses stat)...
  # uniquement si == entier :
  if (isTRUE(all.equal(obs$number, as.integer(obs$number)))){
    obs$number <- as.integer(obs$number)
  }else{}

  # Si les unités d'observation sont ne sont pas des facteurs :
  if (!is.factor(obs$observation.unit)){
    obs$observation.unit <- factor(as.character(obs$observation.unit))
  }

  return(obs)
}


obsFormatting.TRATO.f <- function(obs){

  ## Purpose: Mise en forme et nettoyage du fichier d'observations pour les
  ##          données d'observation de types TRATO
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table de données d'observations.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 janv. 2013, 17:40

  # Renommage des colonnes "number" et "min.distance" en "tracks.number" et "ponte" :
  colnames(obs)[match(c("number", "min.distance"), colnames(obs))] <- c("tracks.number", "ponte")

  ponte <- gsub("[[:blank:]]*", "", tolower(obs[ , "ponte"]))

  ponte[ponte == "nl"] <- "NL"
  ponte[ponte == ""] <- NA

  # Recherche des données incorrectes => NAs :
  if (sum(tmp <- ! (is.na(ponte) | is.element(ponte,
      c(paste0(rep(c(tolower(mltext("oui")),      #KW.yes
        tolower(mltext("non"))), each = 2),       #KW.no
        c("", "?")),
        "NL")))))                                 # [ml?]
  {
    # Changées en NAs :
    ponte[tmp] <- NA

    # Message au pluriel ?
    pl <- sum(tmp) > 1

    # Message d'avertissement :
    print(paste(
      ifelse(pl,
        mltext("obsFormatting.TRATO.info.1p"),
        mltext("obsFormatting.TRATO.info.1s")),
      sum(tmp),
      ifelse(pl,
        mltext("obsFormatting.TRATO.info.2p"),
        mltext("obsFormatting.TRATO.info.2s")),
      ifelse(pl,
        mltext("obsFormatting.TRATO.info.3p"),
        mltext("obsFormatting.TRATO.info.3s")),
      sep = ""))

  }else{}

  # Sauvegarde dans obs :
  obs[ , "ponte"] <- factor(ponte)

  return(obs)
}

checkUnitobs.in.obs.f <- function(obs, unitobs){

  ## Purpose: Vérifie que toutes les observations correspondent à des
  ##          unitobs existantes. Réduit le jeu de données si nécessaire.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : la table d'observations.
  ##            unitobs : la table d'unités d'observation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 13 déc. 2011, 11:34

  if ( ! all(idxTmp <- is.element(obs$observation.unit, unitobs$observation.unit))){
    # Ajout du message pour le chargement :
    print(paste(
      mltext("checkUnitobs.in.obs.info.1"),
      sum( ! idxTmp),
      mltext("checkUnitobs.in.obs.info.2"),
      nrow(obs),
      mltext("checkUnitobs.in.obs.info.3"),
      mltext("checkUnitobs.in.obs.info.4"),
      mltext("checkUnitobs.in.obs.info.5"),
      sep = ""))

    obs <- dropLevels.f(obs[idxTmp, ])
  }else{}

  return(obs)
}


loadRefEspeces.f <- function (pathRefesp, pathRefesp.local = NA, baseEnv = .GlobalEnv){

  runLog.f(msg = c(mltext("loadRefEspeces.info")))

  # Importation des caracteristiques des especes
  especes <- read.table(pathRefesp, sep = "\t", dec = ".", quote = "", header = TRUE,
    encoding = "latin1", stringsAsFactors = TRUE)

  # Traitement différent selon le nombre de colonnes :
  switch(as.character(ncol(especes)),
    "125" = {
      especes <- loadRefEspece.old.f(refesp = especes)
    },
    "33" = {
      especes <- loadRefEspeces.new.f(refesp = especes, pathRefesp.local = pathRefesp.local)
    },
    {
      print(paste(
        mltext("error.nbChampEsp.1"),
        mltext("error.nbChampEsp.2"),
        sep = ""))
      especes <- NULL
    })

  # Ajout de catégories benthiques supplémentaires lues dans un fichier de correspondance :
  correspCatBenthique <- read.csv(
    system.file("file_translation/corresp-cat-benth.csv", package = "PAMPA"),
    row.names = 1)

  especes <- cbind(especes, correspCatBenthique[
    as.character(especes$benthic.categ), , drop = FALSE])

  # Suppression de la ligne en NA
  especes <- subset(especes, !is.na(especes$species.code))

  # Réorganisation des modalités de certains facteurs :
  especes <- reorderFactors.f(Data = especes,
    which = c("water.column.position", "mobility",
      tmpcol <- colnames(especes)[grepl("^interet\\..*$", colnames(especes))]),
    type = c("position", "mobility", rep("interest", length(tmpcol))),
    warnings = FALSE)

  return(especes)
}


loadRefEspece.old.f <- function(refesp){

  ## Purpose: Traitements du référentiel espèces à l'ancien format.
  ## ---------------------------------------------------------------------------
  ## Arguments: refesp : référentiel "brut".
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 janv. 2013, 10:39

  # Stockage du type de référentiel dans une option
  options(P.refesp.Coefs = "old")

  # Renommage des colonnes :
  names(refesp) <- c("species.code", "SIH.group.code", "SIH.species.code",
    "ISSCAAP.group", "taxo.code", "FAO.code", "FishBase.code", "phylum",
    "benthic.categ", "class", "order", "family", "genus", "species",
    "scient.name", "ObsNC", "ObsRUN", "ObsMAY", "ObsSTM", "ObsCB", "ObsBA",
    "ObsBO", "ObsCR", "Lmax", "L50", "cryptic", "mobility", "territorial",
    "day.night.act", "aggreg.behaviour", "seasonal.aggreg", "water.column.position",
    "demogr.strategy", "spawning.type", "preferred.habitat", "sex.change",
    "adult.diet", "interet.chasseNC", "interet.chasseRUN", "interet.chasseMAY",
    "interet.chasseSTM", "interet.chasseCB", "interet.chasseBA", "interet.chasseBO",
    "interet.chasseCR", "interet.ligneNC", "interet.ligneRUN", "interet.ligneMAY",
    "interet.ligneSTM", "interet.ligneCB", "interet.ligneBA", "interet.ligneBO",
    "interet.ligneCR", "interet.filetNC", "interet.filetRUN", "interet.filetMAY",
    "interet.filetSTM", "interet.filetCB", "interet.filetBA", "interet.filetBO",
    "interet.filetCR", "interet.casierNC", "interet.casierRUN", "interet.casierMAY",
    "interet.casierSTM", "interet.casierCB", "interet.casierBA", "interet.casierBO",
    "interet.casierCR", "interet.piedNC", "interet.piedRUN", "interet.piedMAY",
    "interet.piedSTM", "interet.piedCB", "interet.piedBA", "interet.piedBO",
    "interet.piedCR", "interetComMAY", "Coeff.a.Med", "Coeff.b.Med", "Coeff.a.NC",
    "Coeff.a.MAY", "Coeff.b.NC", "Coeff.b.MAY", "mean.weight.small", "mean.weight.medium",
    "mean.weight.large", "max.length.small", "max.length.medium", "niveau.a.et.b.MED",
    "niveau.a.et.b.NC", "niveau.a.et.b.MAY", "emblematiqueNC", "emblematiqueRUN",
    "emblematiqueMAY", "emblematiqueSTM", "emblematiqueCB", "emblematiqueBA",
    "emblematiqueBO", "emblematiqueCR", "IUCN.status", "autre.statutNC",
    "autre.statutRUN", "autre.statutMAY", "autre.statutSTM", "autre.statutCB",
    "autre.statutBA", "autre.statutBO", "autre.statutCR", "etat.pop.localNC",
    "etat.pop.localRUN", "etat.pop.localMAY", "etat.pop.localSTM", "etat.pop.localCB",
    "etat.pop.localBA", "etat.pop.localBO", "etat.pop.localCR", "endemiqueNC",
    "endemiqueRUN", "endemiqueMAY", "endemiqueSTM", "endemiqueCB", "endemiqueBA",
    "endemiqueBO", "endemiqueCR")

  # Remplacement des -999 par NA :
  if (nrow(refesp) != 0){
    refesp[refesp == "-999"] <- NA
  }

  print(paste(
    mltext("loadRefEspece.old.info.1"),
    mltext("loadRefEspece.old.info.2"),
    sep = ""))

  return(refesp)
}


loadRefEspeces.new.f <- function(refesp, pathRefesp.local){

  ## Purpose: Traitement du référentiel especes général et complétion avec
  ##          les données d'un éventuel (optionnel) référentiel espèces
  ##          local.
  ## ---------------------------------------------------------------------------
  ## Arguments: refesp : le référentiel espèces général "brut".
  ##            pathRefesp.local : chemin vers le référentiel espèces local.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 janv. 2013, 10:50

  options(P.refesp.Coefs = "new")

  # Renommage des colonnes :
  names(refesp) <- c("species.code", "SIH.group.code", "SIH.species.code",
    "ISSCAAP.group", "taxo.code", "FAO.code", "FishBase.code", "phylum",
    "benthic.categ", "class", "order", "family", "genus", "species", "scient.name",
    "Lmax", "L50", "cryptic", "mobility", "territorial", "day.night.act",
    "aggreg.behaviour", "seasonal.aggreg", "water.column.position", "demogr.strategy",
    "spawning.type", "preferred.habitat", "sex.change", "adult.diet", "IUCN.status",
    "a.coeff", "b.coeff", "taxo.level.a.and.b")

  # Remplacement des -999 par NA :
  if (nrow(refesp) != 0){
    refesp[refesp == "-999"] <- NA
  }

  if ( ! is.na(pathRefesp.local) && file.exists(pathRefesp.local)){
    refesp.local <- read.table(pathRefesp.local, sep = "\t", dec = ".", quote = "",
      header = TRUE, encoding = "latin1", stringsAsFactors = TRUE)

    # Le référentiel local a 13 colonnes obligatoires :
    if (ncol(refesp.local) < 14){
      print(mltext("loadRefEspeces.new.error"))
    }else{
      # S'il est correct...
      # Renommage des 13, premières colonnes pour éviter les fautes de frappes :
      colnames(refesp.local)[1:14] <- c("species.code", "scient.name", "a.coeff",
        "b.coeff", "taxo.level.a.and.b", "Lmax", "L50", "mean.weight.small",
        "mean.weight.medium", "mean.weight.large", "max.length.small",
        "max.length.medium", "observed", "IUCN.loc.status")

      # Remplacement des -999 par NA :
      if (nrow(refesp.local) != 0){
        refesp.local[refesp.local == "-999"] <- NA
      }

      refesp <- priority.merge.f(first = refesp.local, second = refesp,
        by = "species.code", exclude = "scient.name")
    }
  }else{
    print(mltext("loadRefEspeces.new.no.local"))
  }

  return(refesp)
}


priority.merge.f <- function(first, second, by, exclude){

  ## Purpose: Merge de deux tables, avec fusion des colonnes communes selon
  ##          la priorité.
  ## ----------------------------------------------------------------------
  ## Arguments: first : table prioritaire.
  ##            second : table non-prioritaire.
  ##            by : colonne commune.
  ##            exclude : colonne(s) de la table prioritaire à exclure de
  ##                      la fusion.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 janv. 2013, 12:13


  # merge des colonnes non communes :
  res <- merge(second,
    first[ , c(by,
      colnames(first)[ ! is.element(colnames(first),
        colnames(second))])],
      by = by)

  # Colonnes communes :
  col.comm <- colnames(first)[is.element(colnames(first),
    colnames(second))]

  # ... retenues pour la fusion :
  col.fus <- col.comm[ ! is.element(col.comm, c(by, exclude))]

  # Écriture des valeurs prioritaires (hors NA) de first dans res :
  for (i in col.fus){
    res[ ! is.na(first[ , i]) , i] <- first[ ! is.na(first[ , i]) , i]
  }

  return(res)
}


calcTables.f <- function(obs, unitobs, refesp, dataEnv){

  ## Purpose: Lance le calcul des tables de métriques à divers niveaux
  ##          d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            refesp : référentiel espèces.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 déc. 2011, 10:33

  runLog.f(msg = c(mltext("logmsg.calcTables")))

  # Métriques par classe de taille par espèce par unité d'observation :
  unitSpSz <- calc.unitSpSz.f(obs = obs, unitobs = unitobs, refesp = refesp, dataEnv = dataEnv)

  # Métriques par espèce par unité d'observation :
  unitSp <- calc.unitSp.f(unitSpSz = unitSpSz, obs = obs, unitobs = unitobs, dataEnv = dataEnv)

  # Métriques par unité d'observation (dont biodiversité) :
  unit <- calc.unit.f(unitSp = unitSp, obs = obs, refesp = refesp, unitobs = unitobs, dataEnv = dataEnv)

  return(list(unitSpSz = unitSpSz, unitSp = unitSp, unit = unit))
}


calc.unitSpSz.f <- function(obs, unitobs, refesp, dataEnv){

  ## Purpose: Calcul de la table de métriques par  unité d'observation par
  ##          espèce par classe de taille.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            refesp : référentiel espèces.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 16 déc. 2011, 11:51

  runLog.f(msg = c(mltext("logmsg.calc.unitSpSz")))

  # Définition des types d'observation nécessitant les mêmes méthodes de calcul :
  casObsType <- c("SVR" = "SVR",
    "EMB" = "Fishing", "DEB" = "Fishing", "PSCI" = "Fishing", "PecRec" = "Fishing",
    "LIT" = "LIT",
    "PIT" = "PIT",
    "TRUVC" = "Transect", "UVC" = "Transect",
    "TRVID" = "Transect", "Q" = "Transect",
    "PFUVC" = "Fixe",
    "TRATO" = "TTracks",
    "OBSIND" = "OBSIND")

  # Calcul des métriques selon cas d'observation :
  if ( ! all(is.na(obs[ , "size.class"]))){

    factors <- c("observation.unit", "species.code", "size.class")

    unitSpSz <- switch(casObsType[getOption("P.obsType")],
      "SVR" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.SVR.f(obs = obs, dataEnv = dataEnv, factors = factors)
      },
      "Fishing" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.Fishing.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      "LIT" = {
        options(P.nbName = "colonies") # Nom du champ de nombres.
        # Pas calculé par classe de taille
      },
      "PIT" = {
        # Pas calculé par classe de taille
      },
      "Transect" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.Transect.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      "Fixe" = {
        # calc.unitSpSz.Fixe.f()
      },
      # Traces de tortues :
      "TTracks" = {
        options(P.nbName = "tracks.number") # Nom du champ de nombres.
        calc.tables.TurtleTracks.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      # OBSIND :
      "OBSIND" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.OBSIND.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      {
        stop(mltext("calc.unitSpSz.err.1"), getOption("P.obsType"),
          mltext("calc.unitSpSz.err.2"))
        NULL
      })
  }else{
    unitSpSz <- data.frame(
      "observation.unit" = NULL, "species.code" = NULL, "number" = NULL,
      "weight" = NULL, "mean.weight" = NULL, "density" = NULL,
      "pres.abs" = NULL, "site" = NULL, "biotop" = NULL,
      "year" = NULL, "protection.status" = NULL, stringsAsFactors = TRUE)
  }

  return(unitSpSz)
}


calc.tables.SVR.f <- function(obs, dataEnv,
  factors = c("observation.unit", "species.code", "size.class")){

  ## Purpose: Calcul générique d'une table de métriques pour les vidéos
  ##          rotatives.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            dataEnv : environnement des données.
  ##            factors : les facteurs d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 déc. 2011, 14:33

  # Calcul des statistiques de nombres :
  statRotations <- statRotations.f(facteurs = factors,
    obs = obs, dataEnv = dataEnv)

  # Moyenne pour les vidéos rotatives (habituellement 3 rotation) :
  nbr <- statRotations[["nombresMean"]]

  switch(is.element("size.class", factors),
    "TRUE" = print(paste(
      mltext("calc.tables.SVR.info.2"),
      mltext("calc.tables.SVR.info.3"),
      sep = "")),
    "FALSE" = print(paste(
      mltext("calc.tables.SVR.info.2"),
      mltext("calc.tables.SVR.info.4"),
      sep = "")))

  # Création de la data.frame de résultats (avec nombres, unitobs, ):
  res <- calc.numbers.f(nbr)

  # Statistiques sur les nombres :
  res$number.max <- as.vector(statRotations[["nombresMax"]])
  res$number.sd <- as.vector(statRotations[["nombresSD"]])

  # Tailles moyennes :
  res[ , "mean.length"] <- calc.meanSize.f(obs = obs, factors = factors)

  # Poids :
  res[ , "weight"] <- calc.weight.f(obs = obs, Data = res, factors = factors)

  # Poids moyen par individu :
  res[ , "mean.weight"] <- calc.meanWeight.f(Data = res)

  # Densité (+Max +SD) :
  res <- calc.density.SVR.f(Data = res, obs = obs,
    metric = c("density", "density.max", "density.sd"))

  # Biomass :
  res[ , "biomass"] <- calc.biomass.SVR.f(Data = res, obs = obs)

  # Biomass max+SD :
  res <- stat.biomass.SVR.f(Data = res, obs = obs,
    metric = c("biomass.max", "biomass.sd"))

  # Présence/absence :
  res[ , "pres.abs"] <- calc.presAbs.f(Data = res)

  if (is.element("size.class", factors)){
    # Proportions d'abondance par classe de taille :
    res[ , "abundance.prop.SC"] <- unitSpSz.propAb.f(unitSpSz = res, factors = factors)

    # Proportions de biomasse par classe de taille :
    res[ , "biomass.prop.SC"] <- unitSpSz.propBiom.f(unitSpSz = res, factors = factors)
  }else{}

  return(res)
}


statRotations.f <- function(facteurs, obs, dataEnv = .GlobalEnv){

  ## Purpose: Calcul des statistiques des abondances (max, sd) par rotation
  ##          en se basant sur des données déjà interpolées.
  ## ---------------------------------------------------------------------------
  ## Arguments: facteurs : vecteur des noms de facteurs d'agrégation
  ##                       (résolution à laquelle on travaille).
  ##            obs : données d'observation.
  ##            dataEnv : environnement des données (pour sauvegarde de
  ##                      résultats intermédiaires).
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 29 oct. 2012, 16:01

  # Identification des rotations valides :
  if (is.element("observation.unit", facteurs)){
    # Rotations valides (les vides doivent tout de même être renseignés) :
    rotations <- tapply(obs$rotation,
      as.list(obs[ , c("observation.unit", "rotation"), drop = FALSE]),
      function(x)length(x) > 0)

    # Les rotations non renseignés apparaissent en NA et on veut FALSE :
    rotations[is.na(rotations)] <- FALSE
  }else{
    stop(mltext("statRotations.err.1"))
  }

  # ###########################################################
  # Nombres par rotation avec le niveau d'agrégation souhaité :
  nombresR <- tapply(obs$number,
    as.list(obs[ , c(facteurs, "rotation"), drop = FALSE]),
    function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))},
    na.rm  =  TRUE)

  # Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
  nombresR <- sweep(nombresR,
    match(names(dimnames(rotations)), names(dimnames(nombresR)), nomatch = NULL),
    rotations,        # Tableau des secteurs valides (booléens).
    function(x, y){
      x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
      return(x)
    })

  # ##################################################
  # Statistiques :

  # Moyennes :
  nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
    function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm = TRUE)

  # Maxima :
  nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
    function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm = TRUE)

  # Déviation standard :
  nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
    function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm = TRUE)

  # Nombre de rotations valides :
  nombresRotations <- apply(rotations, 1, sum, na.rm = TRUE)

  if (is.element("size.class", facteurs)){
    # # Pour les calculs agrégés par unitobs :
    # tmpNombresSVR <- apply(nombresR,
    #                        which(names(dimnames(nombresR)) != "species.code"),
    #                        sum, na.rm = TRUE)

    # tmpNombresSVR[!rotations] <- NA

    # #### Densités brutes (pour agrégations) :
    # on réduit les facteurs (calcul de rayon par espèce) :
    factors2 <- facteurs[ ! is.element(facteurs, "size.class")]


    # rayons par espèce / unitobs :
    rayons <- as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
      as.list(obs[obs[ , "number"] > 0,
        factors2, drop = FALSE]),
      max, na.rm = TRUE))

    densitesR <- sweep(nombresR,
      match(names(dimnames(rayons)), names(dimnames(nombresR))),
      pi * rayons ^ 2,
      "/")

    # Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
    densitesR <- sweep(densitesR,
      match(names(dimnames(rotations)), names(dimnames(densitesR)), nomatch = NULL),
      rotations,        # Tableau des secteurs valides (booléens).
      function(x, y){
        x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
        return(x)
      })

    # Sauvegardes dans l'environnement des données :
    assign(".NombresSVR", nombresR, envir = dataEnv)
    assign(".DensitesSVR", densitesR, envir = dataEnv)
    assign(".Rotations", rotations, envir = dataEnv)
  }else{}

  # Retour des résultats sous forme de liste :
  return(list(nombresMean = nombresMean, nombresMax = nombresMax, nombresSD = nombresSD,
    nombresRotations = nombresRotations, nombresTot = nombresR))
}


calc.numbers.f <- function(nbr, nbName = "number"){

  ## Purpose: Produit la data.frame qui va servir de table, à partir du
  ##          tableau de nombres produit par calcNumber.default.f().
  ## ---------------------------------------------------------------------------
  ## Arguments: nbr : array de nombres avec autant de dimensions que de
  ##                  facteurs d'agrégations.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: une data.frame avec (nombre de facteurs d'agrégation + 1)
  ##         colonnes.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 13:46

  res <- as.data.frame(as.table(nbr), responseName = nbName, stringsAsFactor = TRUE)

  if (is.element("size.class", colnames(res))){
    res$size.class[res$size.class == ""] <- NA
  }else{}

  # Si les nombres sont des entiers, leur redonner la bonne classe :
  if (isTRUE(all.equal(res[ , nbName], as.integer(res[ , nbName])))){
    res[ , nbName] <- as.integer(res[ , nbName])
  }else{}

  return(res)
}


calc.meanSize.f <- function(obs,
  factors = c("observation.unit", "species.code", "size.class"),
  nbName = "number") {

  ## Purpose: Calcul des tailles moyennes pondérées (par le nombre
  ##          d'individus)
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            factors : les facteurs d'agrégation.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: vecteur des tailles moyennes.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 13:51

  return(as.vector(tapply(seq(length.out = nrow(obs)),
    as.list(obs[ , factors]),
    function(ii){
      weighted.mean(obs[ii, "length"], obs[ii, nbName])
    })))
}


calc.weight.f <- function(obs, Data,
  factors = c("observation.unit", "species.code", "size.class"),
  nbName = "number"){

  ## Purpose: Calcul des poids totaux.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            Data : la table de métrique (temporaire).
  ##            factors : les facteurs d'agrégation.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: vecteur des poids.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 14:12

  weight <- as.vector(tapply(obs[ , "weight"],
    as.list(obs[ , factors]),
      function(x){
        ifelse(all(is.na(x)),
          as.numeric(NA),
          sum(x, na.rm = TRUE))
  }))

  # Cohérence des zéros avec la calculabilité des poids par espèce :
  if (is.element("species.code", factors)){

    # Certains NAs correspondent à des vrai zéros :
    if (!all(is.na(obs[ , "weight"]))){
      weight[is.na(weight) & Data[ , nbName] == 0] <- 0
    }

    # Especes pour lesquelles aucune biomasse n'est calculée.
    noWeightSp <- tapply(weight, Data[ , "species.code"],
      function(x)all(is.na(x) | x == 0))

    noWeightSp <- names(noWeightSp)[noWeightSp]

    # Données non disponibles
    # (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
    weight[is.element(Data[ , "species.code"], noWeightSp)] <- NA
  }else{}

  return(weight)
}


calc.meanWeight.f <- function(Data, nbName = "number"){

  ## Purpose: Calcul des poids moyens (d'individus).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: vecteur des poids moyens.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 14:17

  if ( ! all(is.na(Data[ , "weight"]))){
    return(apply(Data[ , c(nbName, "weight")], 1,
      function(x){
        # Le poids moyen par individu est le poids total / le nombre d'individus :
        return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
          as.numeric(NA),
          x[2]/x[1])))
    }))
  }else{
    return(NA)
  }
}


calc.density.SVR.f <- function(Data, obs, metric = "density",
  factors = c("observation.unit", "species.code")){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 22 déc. 2011, 11:12

  # Nom de la colonne de nombre en fonction de la métrique de densité :
  nbMetric <- c(density = "number",
    density.max = "number.max",
    density.sd = "number.sd")

  if ( ! all(is.na(nbMetric[metric]))){     # la colonne de nombre doit être définie.
    metric <- metric[!is.na(nbMetric[metric])]

    # Calcul du rayon d'observation :
    Data <- merge(Data,
      # Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
      as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
        as.list(obs[obs[ , "number"] > 0, factors, drop = FALSE]),
        max, na.rm = TRUE)),
        responseName = "r"),
        stringsAsFactor = TRUE)

    # Calcul des différentes densités :
    res <- lapply(metric,
      function(x, Data, nbMetrics){
        density <- Data[ , nbMetric[x]] / (pi * (Data[ , "r"] ^ 2))

        # Vrais zéros :
        density[Data[ , nbMetric[x]] == 0 & !is.na(Data[ , nbMetric[x]])] <- 0

        return(density)
      },
      Data = Data, nbMetrics = nbMetrics)

    # Ajout des résultats à Data
    names(res) <- metric

    res <- as.data.frame(res, stringsAsFactor = TRUE)

    Data <- cbind(Data, res)
    Data$r <- NULL

    return(Data)
  }else{
    stop("Unknown metrics!")
  }
}


calc.biomass.SVR.f <- function(Data, obs,
  factors = c("observation.unit", "species.code")){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 22 déc. 2011, 12:02

  # Calcul du rayon d'observation :
  Data <- merge(Data,
    # Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
    as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
      as.list(obs[obs[ , "number"] > 0, factors, drop = FALSE]),
      max, na.rm = TRUE)),
    responseName = "r"),
    stringsAsFactor = TRUE)

  biomass <- Data[ , "weight"] / (pi * (Data[ , "r"] ^ 2))
  # Les poids ont été corrigés au préalable et tiennent compte des espèces pour
  # lesquelles ils ne peuvent être calculés.
  # Aucune correction n'est donc nécessaire.

  return(biomass)
}


stat.biomass.SVR.f <- function(Data, obs, metric,
  factors = c("observation.unit", "species.code")){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 22 déc. 2011, 12:10

  # Nom de la colonne de nombre en fonction de la métrique de densité :
  nbMetric <- c(biomass.max = "number.max", biomass.sd = "number.sd")

  if ( ! all(is.na(nbMetric[metric]))){    # la colonne de nombre doit être définie.

    metric <- metric[!is.na(nbMetric[metric])]

    # Calcul du rayon d'observation :
    Data <- merge(Data,
      # Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
      as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
        as.list(obs[obs[ , "number"] > 0, factors, drop = FALSE]),
        max, na.rm = TRUE)),
      responseName = "r"),
      stringsAsFactor = TRUE)

    # Calcul des différentes densités :
    res <- lapply(metric,
      function(x, Data, nbMetrics){
        return(Data[ , nbMetric[x]] * # métrique de nombre
          Data[ , "mean.weight"] /    # * poids moyens d'un individu.
          (pi * (Data[ , "r"] ^ 2)))
      },
      Data = Data, nbMetrics = nbMetrics)

    # Ajout des résultats à Data
    names(res) <- metric

    res <- as.data.frame(res, stringsAsFactor = TRUE)

    Data <- cbind(Data, res)
    Data$r <- NULL

    return(Data)
  }else{
    stop("Biomass metrics unknown!")
  }
}


calc.presAbs.f <- function(Data, nbName = "number"){

  ## Purpose: Calcul des présences/absences à partir des abondances.
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: vecteur des présences/absences (0 ou 1).
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 12:04

  # Presence - absence :
  presAbs <- integer(nrow(Data))
  presAbs[Data[ , nbName] > 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
  presAbs[Data[ , nbName] == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

  return(presAbs)
}


unitSpSz.propAb.f <- function(unitSpSz, factors){

  ## Purpose: Calcul des proportions d'abondance par classe de taille (au
  ##          sein des croisements d'autres facteurs que "size.class",
  ##          i.e. par unité d'observation par espèce en général).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            factors : les facteurs d'agrégation.
  ##
  ## Output: vecteur des proportions d'abondance.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 12:11

  # ##################################################
  # Proportion d'abondance par classe de taille :

  # Mise sous forme de tableau multi-dimensionel :
  abondance <- tapply(unitSpSz$density,
    as.list(unitSpSz[ , factors]),
    function(x){x}) # -> tableau à 3D.

  # Autres facteurs que "size.class" :
  factRed <- which(!is.element(factors, "size.class"))

  # Sommes d'abondances pour chaque unitobs pour chaque espèce :
  sumsCT <- apply(abondance, factRed, sum, na.rm = TRUE)

  # Calcul des proportions d'abondance -> tableau 3D :
  propAbondance <- sweep(abondance, factRed, sumsCT, FUN = "/")
  names(dimnames(propAbondance)) <- factors

  # Extraction des résultats et remise en ordre :
  tmp <- as.data.frame(as.table(propAbondance),
    responseName = "prop.abondance.SC",
    stringsAsFactors = FALSE)

  row.names(tmp) <- apply(tmp[ , factors], 1, paste, collapse = ":")

  # Même ordre que unitSpSz :
  tmp <- tmp[apply(unitSpSz[ , factors], 1, paste, collapse = ":") , ]

  # Mise au format colonne + % : ordre [OK]  [yr: 30/10/2012]
  return(100 * tmp$prop.abondance.SC)
}


unitSpSz.propBiom.f <- function(unitSpSz, factors){
  ## Purpose: Calcul des proportions de biomasse par classe de taille (au
  ##          sein des croisements d'autres facteurs que "size.class",
  ##          i.e. par unité d'observation par espèce en général).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            factors : les facteurs d'agrégation.
  ##
  ## Output: vecteur des proportions de biomasse.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 12:17

  if (!is.null(unitSpSz[ , "biomass"])){
    biomasses <- tapply(unitSpSz$biomass,
      as.list(unitSpSz[ , factors]),
      function(x){x}) # -> tableau à 3D.

    # Autres facteurs que "size.class" :
    factRed <- which(!is.element(factors, "size.class"))

    # Sommes de biomasses pour chaque unitobs pour chaque espèce :
    sumsCT <- apply(biomasses, factRed, sum, na.rm = TRUE)

    # Calcul des proportions de biomasse -> tableau 3D :
    propBiomass <- sweep(biomasses, factRed, sumsCT, FUN = "/")
    names(dimnames(propBiomass)) <- factors

    # Extraction des résultats et remise en ordre :
    tmp <- as.data.frame(as.table(propBiomass),
      responseName = "prop.biomass.SC",
      stringsAsFactors = FALSE)

    row.names(tmp) <- apply(tmp[ , factors], 1, paste, collapse = ":")

    # Même ordre que unitSpSz :
    tmp <- tmp[apply(unitSpSz[ , factors], 1, paste, collapse = ":") , ]

    # Mise au format colonne + % : ordre [OK]  [yr: 30/10/2012]
    return(100 * tmp$prop.biomass.SC)
  }else{
    return(NULL)
  }
}


calc.tables.Fishing.f <- function(obs, unitobs, dataEnv,
  factors = c("observation.unit", "species.code", "size.class")){

  ## Purpose: Calcul des métriques par unité d'observation, par espèce et
  ##          par classe de taille pour la pêche (seules des noms de
  ##          colonnes changent par rapport aux transects par defaut).
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ##            factors : les facteurs d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 déc. 2011, 13:20

  res <- calc.tables.Transect.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)

  # Renommage des colonnes de densité et biomasse :
  res[ , "CPUE"] <- res$density
  res$density <- NULL
  res[ , "CPUE.biomass"] <- res$biomass # Fonctionne même si biomasse n'existe pas.
  res$biomass <- NULL

  return(res)
}


calc.tables.Transect.f <- function(obs, unitobs, dataEnv,
  factors = c("observation.unit", "species.code", "size.class")){

  ## Purpose: Calcul de tables de métriques pour les transects par défaut.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ##            factors : les facteurs d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 12:18

  # Calcul des nombres par cl / espèces / unitobs :
  nbr <- calcNumber.default.f(obs, factors = factors)

  # Création de la data.frame de résultats (avec nombres, unitobs, ):
  res <- calc.numbers.f(nbr)

  # Tailles moyennes :
  res[ , "mean.length"] <- calc.meanSize.f(obs, factors = factors)

  # Poids :
  res[ , "weight"] <- calc.weight.f(obs = obs, Data = res, factors = factors)

  # Poids moyen par individu :
  res[ , "mean.weight"] <- calc.meanWeight.f(Data = res)

  # Densité :
  res[ , "density"] <- calc.density.f(Data = res, unitobs = unitobs)

  # Biomasse :
  res[ , "biomass"] <- calc.biomass.f(Data = res, unitobs = unitobs)

  # Présence/absence :
  res[ , "pres.abs"] <- calc.presAbs.f(Data = res)

  if (is.element("size.class", factors)){
    # Proportions d'abondance par classe de taille :
    res[ , "abundance.prop.SC"] <- unitSpSz.propAb.f(unitSpSz = res,
      factors = factors)

    # Proportions de biomasse par classe de taille :
    res[ , "biomass.prop.SC"] <- unitSpSz.propBiom.f(unitSpSz = res,
      factors = factors)
  }else{}

  return(res)
}


calcNumber.default.f <- function(obs,
  factors = c("observation.unit", "species.code", "size.class"),
  nbName = "number"){

  ## Purpose: Calcul des nombres au niveau d'agrégation souhaité.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            factors : les facteurs d'agrégation.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: un array avec autant de dimensions que de facteurs
  ##         d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 13:38

  # Somme des nombres d'individus :
  nbr <- tapply(obs[ , nbName],
    as.list(obs[ , factors]),
    sum, na.rm = TRUE)

  # Absences considérée comme "vrais zéros" :
  nbr[is.na(nbr)] <- 0

  return(nbr)
}


calc.density.f <- function(Data, unitobs){

  ## Purpose: Calcul générique des densités (également utilisable pour des
  ##          CPUE).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            unitobs : table des unités d'observation (data.frame).
  ##
  ## Output: vecteur des densités.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 déc. 2011, 15:47

  # Traitement spécial si la fraction est un facteur (on doit dabord passer par la classe "character") :
  if (is.factor(unitobs[ , "sampling.rate"])){
    unitobs[ , "sampling.rate"] <- as.numeric(as.character(unitobs[ , "sampling.rate"]))
  }else{}

  # Calculs :
  return(Data[ , "number"] /
    # Surface/unité d'effort :
    (unitobs[(idx <- match(Data[ , "observation.unit"],
      unitobs[ , "observation.unit"])) ,
      "obs.dim1"] *
      unitobs[idx, "obs.dim2"] *
      # ...qui tient compte de la fraction échantillonnée :
      ifelse(is.na(as.numeric(unitobs[idx , "sampling.rate"])),
        1,
        as.numeric(unitobs[idx , "sampling.rate"]))))
}


calc.biomass.f <- function(Data, unitobs){

  ## Purpose: Calcul généric des biomasses (également utilisable pour des
  ##          CPUE.biomass).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : la table de métrique (temporaire).
  ##            unitobs : table des unités d'observation (data.frame).
  ##
  ## Output: vecteur des biomasses.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 11:05

  if (!all(is.na(Data[ , "weight"]))){      # (length(unique(obs$biomass))>1)
    # ##################################################
    # poids :
    weight <- Data[ , "weight"]

    # Cohérence des zéros avec la calculabilité des poids par espèce
    # (en principe inutile car fait précédemment sur
    # le poids mais mis en sécurité si autre utilisation que l'originale) :
    if (is.element("species.code", colnames(Data))){
      # Especes pour lesquelles aucune biomasse n'est calculée.
      noBiomSp <- tapply(weight, Data[ , "species.code"],
        function(x)all(is.na(x) | x == 0))

      noBiomSp <- names(noBiomSp)[noBiomSp]

      # Données non disponibles
      # (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
      weight[is.element(Data[ , "species.code"], noBiomSp)] <- NA
    }else{}

    # Poids / surface ou unité d'effort :
    return(as.numeric(weight) /
      # Surface/unité d'effort :
      (unitobs[(idx <- match(Data[ , "observation.unit"],
        unitobs[ , "observation.unit"])) ,
        "obs.dim1"] *
        unitobs[idx, "obs.dim2"] *
        # ...qui tient compte de la fraction échantillonnée :
        ifelse(is.na(as.numeric(unitobs[idx , "sampling.rate"])),
          1,
          as.numeric(unitobs[idx , "sampling.rate"]))))
  }else{
    # alerte que les calculs de biomasse sont impossibles
    print(paste(
      mltext("calc.biomass.info.1"),
      mltext("calc.biomass.info.2"),
      sep = ""))

    return(NA)
  }
}


calc.tables.TurtleTracks.f <- function(obs, unitobs, dataEnv, factors){

  ## Purpose: Calcul de métriques à partir de traces de tortues.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ##            factors : les facteurs d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 janv. 2013, 19:29

  # Calcul des nombres par cl / espèces / unitobs :
  nbr <- calcNumber.default.f(obs, factors = factors, nbName = "tracks.number")

  # Création de la data.frame de résultats (avec nombres, unitobs, ):
  res <- calc.numbers.f(nbr, nbName = "tracks.number")

  # Calcul du succès de ponte (%) :
  res <- cbind(res,
               calc.nestingSuccess.f(obs = obs, Data = res, factors = factors, nbName = "tracks.number"))

  # Tailles moyennes :
  res[ , "mean.length"] <- calc.meanSize.f(obs, factors = factors, nbName = "tracks.number")

  # Poids :
  res[ , "weight"] <- calc.weight.f(obs = obs, Data = res, factors = factors, nbName = "tracks.number")

  # Poids moyen par individu :
  res[ , "mean.weight"] <- calc.meanWeight.f(Data = res, nbName = "tracks.number")

  # Présence/absence :
  res[ , "pres.abs"] <- calc.presAbs.f(Data = res, nbName = "tracks.number")

  return(res)
}


calc.nestingSuccess.f <- function(obs, Data,
  factors = c("observation.unit", "species.code", "size.class"),
  nbName = "number"){

  ## Purpose: Calcul du pourcentage de réussite de ponte.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            Data : la table de métrique (temporaire).
  ##            factors : les facteurs d'agrégation.
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output: vecteur des réussites de pontes (%).
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 janv. 2013, 15:57

  # Nombre de pontes (sûres + supposées) :
  pontes <- as.vector(tapply(
    subset(obs, grepl(paste0("^", "oui", "\\??$"),
      obs$ponte))[ , nbName],
    as.list(subset(obs, grepl(paste0("^", "oui", "\\??$"),
      obs$ponte))[ , factors]),
    FUN = function(x){
      ifelse(all(is.na(x)),
        as.numeric(NA),
        ifelse(all(is.element(na.omit(x), "NL")), # [ml?]
          0,
          sum(x, na.rm = TRUE)))
    }))


  # Correction de NAs à la place de 0 dans pontes lorsque aucune traces observées mais nombre valide (0) :
  pontes[is.na(pontes) & ! is.na(Data[ , nbName])] <- 0

  # Nombre de traces lisibles :
  traces.lisibles <- as.vector(tapply(subset(obs,
    grepl(paste0("^(", mltext("KW.yes"), "|",
      mltext("KW.no"), ")\\??$"), # "^(yes|no)\\??$"
      obs$ponte))[ , nbName],
      as.list(subset(obs,
        grepl(paste0("^(", mltext("KW.yes"), "|",
          mltext("KW.no"), ")\\??$"),
          obs$ponte))[ , factors]),
      FUN = function(x){
        ifelse(all(is.na(x)),
          as.numeric(NA),
          ifelse(all(is.element(na.omit(x), "NL")),
            0,
            sum(x, na.rm = TRUE)))
  }))

  # Correction de NAs à la place de 0 dans traces lisibles lorsque aucune traces observées mais nombre valide (0) :
  traces.lisibles[is.na(traces.lisibles) & ! is.na(Data[ , nbName])] <- 0

  return(data.frame("spawnings" = pontes, "readable.tracks" = traces.lisibles,
    "spawning.success" = 100 * pontes / traces.lisibles, stringsAsFactors = TRUE))
}


calc.tables.OBSIND.f <- function(obs, unitobs, dataEnv,
  factors = c("observation.unit", "species.code", "size.class")){

  ## Purpose: Calcul des métriques par unité d'observation, par espèce et
  ##          par classe de taille pour les observations individuelles
  ##          géoréférencées.
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ##            factors : les facteurs d'agrégation.
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 25 avril 2013, 18:41

  res <- calc.tables.Transect.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)

  ## Renommage des colonnes de densité et biomasse :
  res[ , "Density"] <- res$density
  res$density <- NULL
  res[ , "Biomass"] <- res$biomass # Fonctionne même si biomasse n'existe pas.
  res$biomass <- NULL

  return(res)
}


calc.unitSp.f <- function(unitSpSz, obs, unitobs, dataEnv){

  ## Purpose: Calcul de la table de métriques par unité d'observation par
  ##          espèce (choix de la méthode adéquate en fonction du
  ##          protocole).
  ## ---------------------------------------------------------------------------
  ## Arguments: unitSpSz : table de métriques par unité d'observation, par
  ##                       espèce par classe de taille.
  ##            obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 déc. 2011, 10:04

  ## Informations :
  print(mltext("calc.unitSp.info.1"))

  if (FALSE)  # (! is.null(unitSpSz) && nrow(unitSpSz))
  {
    # Note : désactivé car plus long que de recalculer la table.
    #        Conservé car pourrait redevenir intéressant avec de la parallelisation.

    unitSp <- switch(getOption("P.obsType"),
      SVR = calc.unitSp.SVR.f(unitSpSz = unitSpSz, obs = obs, dataEnv = dataEnv),
      LIT = {
        warning(paste("Trying to calculate metrics by size class",
          " for the benthos...!", sep = ""))

        calc.unitSp.LIT.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv)
      },
      calc.unitSp.default.f(unitSpSz = unitSpSz, dataEnv = dataEnv))
  }else{
    factors <- c("observation.unit", "species.code")

    casObsType <- c("SVR" = "SVR",
      "EMB" = "Fishing", "DEB" = "Fishing", "PSCI" = "Fishing", "PecRec" = "Fishing",
      "LIT" = "LIT",
      "PIT" = "PIT",
      "TRUVC" = "Transect", "UVC" = "Transect",
      "TRVID" = "Transect", "Q" = "Transect",
      "PFUVC" = "Fixe",
      "TRATO" = "TTracks",
      "OBSIND" = "OBSIND")

    unitSp <- switch(casObsType[getOption("P.obsType")],
      "SVR" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.SVR.f(obs = obs, dataEnv = dataEnv, factors = factors)
      },
      "Fishing" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.Fishing.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      "LIT" = {
        options(P.nbName = "colonies") # Nom du champ de nombres.
        calc.unitSp.LIT.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv)
      },
      "PIT" = {
        warning("PIT protocol not implemented yet!")
        # calc.unitSpSz.PIT.f()
      },
      "Transect" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.Transect.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      "Fixe" = {
        warning("\"Fix point\" protocol not implemented yet!")
        # calc.unitSpSz.Fixe.f()
      },
      # Traces de tortues :
      "TTracks" = {
        options(P.nbName = "tracks.number") # Nom du champ de nombres.
        calc.tables.TurtleTracks.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      # OBSIND :
      "OBSIND" = {
        options(P.nbName = "number") # Nom du champ de nombres.
        calc.tables.OBSIND.f(obs = obs, unitobs = unitobs, dataEnv = dataEnv, factors = factors)
      },
      {
        stop(mltext("calc.unitSp.err.1"), getOption("P.obsType"),
             mltext("calc.unitSp.err.2"))
        NULL
      })
  }

  return(unitSp)
}


calc.unitSp.SVR.f <- function(unitSpSz, obs, dataEnv){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 déc. 2011, 09:43

  # Agrégation pour les métriques par défaut (idem cas général) :
  unitSp <- calc.unitSp.default.f(unitSpSz, dataEnv = dataEnv)

  # Calcul à partir des données extrapolées brutes pour les statistiques :
  nbInterp <- get(".NombresSVR", envir = dataEnv)

  nbTmp <- apply(nbInterp,
    which( ! is.element(names(dimnames(nbInterp)), "size.class")),
      function(x,...){
        ifelse(all(is.na(x)), NA, sum(x,...))
      }, na.rm = TRUE)

  # nombres calculés... pour comparaison avec nombres agrégé uniquement :
  nbTest <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, mean(x,...))
    }, na.rm = TRUE)))

  if ( ! isTRUE(all.equal(unitSp$number, nbTest)))
    stop(mltext("calc.unitSp.SVR.err.1"))

  # nombre max :
  unitSp[ , "number.max"] <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, max(x,...))
    }, na.rm = TRUE)))

  # nombre SD :
  unitSp[ , "number.sd"] <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, sd(x,...))
    }, na.rm = TRUE)))

  # densité max :
  unitSp <- calc.density.SVR.f(Data = unitSp, obs = obs,
    metric = c("density.max", "density.sd"))

  # Biomass max :
  unitSp <- stat.biomass.SVR.f(Data = unitSp, obs = obs,
    metric = c("biomass.max", "biomass.sd"))

  return(unitSp)
}


calc.unitSp.default.f <- function(unitSpSz, dataEnv = .GlobalEnv){

  ## Purpose: Calcul de la table de métriques par unité d'observation par
  ##          espèce, cas général (à l'aide d'agrégations).
  ## ---------------------------------------------------------------------------
  ## Arguments: unitSpSz : table de métriques par unité d'observation, par
  ##                       espèce par classe de taille.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 15:31

  metrics <- c("number", "mean.length", "weight", "mean.weight", "density", "biomass",
    "pres.abs",
    "CPUE", "CPUE.biomass",
    "colonies", "coverage", "mean.size.colonies")

  usedMetrics <- metrics[is.element(metrics, colnames(unitSpSz))]

  return(agregations.generic.f(Data = unitSpSz,
    metrics = usedMetrics,
    factors = c("observation.unit", "species.code"),
    listFact = NULL,
    unitSpSz = unitSpSz,
    unitSp = NULL,
    dataEnv = dataEnv))
}


agregations.generic.f <- function(Data, metrics, factors, listFact = NULL, unitSpSz = NULL,
  unitSp = NULL, info = FALSE, dataEnv = .GlobalEnv, nbName = "number"){

  ## Purpose: Agréger les données selon un ou plusieurs facteurs.
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : Le jeu de données à agréger.
  ##            metrics : la métrique agrégée.
  ##            factors : les facteurs
  ##            listFact : noms des facteurs supplémentaires (agrégés et
  ##                       ajoutés à la table de sortie).
  ##            unitSpSz : Table de métriques par unitobs/esp/CT.
  ##            unitSp : Table de métriques par unitobs/esp
  ##            info : affichage des infos ?
  ##            nbName : nom de la colonne nombre.
  ##
  ## Output : une data.frame agrégée.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47

  # Informations (l'étape peut être longue) :
  if (info){
    print(mltext("agregation.info.f.info.1"))
  }

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
    "abundance.prop.SC" = "w.mean.prop", # Pas bon [!!!] ?
    "biomass.prop.SC" = "w.mean.prop.bio",  # Pas bon [!!!] ?
    ## Benthos :
    "colonies" = "sum",
    "coverage" = "sum",
    "mean.size.colonies" = "w.mean.colonies",
    ## SVR (expérimental) :
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
  if (any(casMetrique[metrics] == "%.nesting")){
    if (is.element("size.class", colnames(Data))){
      if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

      Data <- merge(Data,
        unitSpSz[ , c("species.code", "observation.unit", "size.class", "readable.tracks")],
        by = c("species.code", "observation.unit", "size.class"),
        suffixes = c("", ".y"))
    }else{
      if (is.null(unitSp)) stop("unitSp must be defined")

      Data <- merge(Data,
        unitSp[ , c("species.code", "observation.unit", "readable.tracks")],
        by = c("species.code", "observation.unit"),
        suffixes = c("", ".y"))
    }
  }else{}

  # Ajout du champ nombre pour le calcul des moyennes pondérées s'il est absent :
  if (any(casMetrique[metrics] == "w.mean" | casMetrique[metrics] == "w.mean.prop")){
    if (is.element("size.class", colnames(Data))){
      if (is.null(unitSpSz)) stop("unitSpSz must be defined")

      Data <- merge(Data,
        unitSpSz[ , c("species.code", "observation.unit", "size.class", nbName)],
        by = c("species.code", "observation.unit", "size.class"),
        suffixes = c("", ".y"))

      # Ajout de l'abondance totale /espèce/unité d'observation :
      nbTot <- tapply(unitSpSz[ , nbName],
        as.list(unitSpSz[ , c("species.code", "observation.unit")]),
        sum, na.rm = TRUE)

      Data <- merge(Data,
        as.data.frame(as.table(nbTot), responseName = "nombre.tot"), stringsAsFactor = TRUE)
    }else{
      if (is.null(unitSp)) stop("unitSp must be defined")

      Data <- merge(Data,
        unitSp[ , c("species.code", "observation.unit", nbName)], # [!!!] unitSpSz ?
        by = c("species.code", "observation.unit"),
        suffixes = c("", ".y"))
    }
  }else{}

  # Ajout du champ biomasse pour les proportions de biomasses par classe de taille :
  if (any(casMetrique[metrics] == "w.mean.prop.bio")){
    if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

    Data <- merge(Data,
      unitSpSz[ , c("species.code", "observation.unit", "size.class", "biomass")],
      by = c("species.code", "observation.unit", "size.class"),
      suffixes = c("", ".y"))

    # Ajout de la biomasse totale /espèce/unité d'observation :
    biomTot <- tapply(unitSpSz$biomass,
      as.list(unitSpSz[ , c("species.code", "observation.unit")]),
      function(x){
        ifelse(all(is.na(x)),
          NA,
          sum(x, na.rm = TRUE))
      })

    Data <- merge(Data,
      as.data.frame(as.table(biomTot), responseName = "tot.biomass"), stringsAsFactor = TRUE)
  }

  # Ajout du champ colonie pour le calcul des moyennes pondérées s'il est absent :
  if (any(casMetrique[metrics] == "w.mean.colonies" &
      ! is.element("colonies", colnames(Data)))){
    Data$colonies <- unitSp[match(
      apply(Data[ , c("species.code", "observation.unit")],
        1, paste, collapse = "*"),
      apply(unitSp[ , c("species.code", "observation.unit")],
        1, paste, collapse = "*")), "colonies"]
  }else{}


  # Agrégation de la métrique selon les facteurs :
  reslong <- betterCbind(dfList = lapply(metrics,   # sapply utilisé pour avoir les noms.
    agregation.f,
    Data = Data, factors = factors, casMetrique = casMetrique, dataEnv = dataEnv,
    nbName = nbName))

  # Agrégation et ajout des facteurs supplémentaires :
  if ( ! (is.null(listFact) || length(listFact) == 0)){
    reslong <- cbind(reslong,
      sapply(Data[ , listFact, drop = FALSE],
        function(fact){
          tapply(fact,
            as.list(Data[ , factors, drop = FALSE]),
            function(x){
              if (length(x) > 1 && length(unique(x)) > 1){
                # On doit n'avoir qu'une seule modalité...
                return(NULL)                  # ...sinon on retourne NULL
              }else{
                unique(as.character(x))
              }
          })
    }))
  }else{}

  # Si certains facteurs ne sont pas de classe facteur,
  # il faut les remettre dans leur classe d'origine :
  if (any(tmp <- sapply(reslong[ , listFact, drop = FALSE], class) !=
    sapply(Data[ , listFact, drop = FALSE], class))){
    for (i in which(tmp)){
      switch(sapply(Data[ , listFact, drop = FALSE], class)[i],
        "integer" = {
          reslong[ , listFact[i]] <- as.integer(as.character(reslong[ , listFact[i]]))
        },
        "numeric" = {
          reslong[ , listFact[i]] <- as.numeric(as.character(reslong[ , listFact[i]]))
        },
        reslong[ , listFact[i]] <- eval(call(
          paste("as", sapply(Data[ , listFact, drop = FALSE], class)[i], sep = "."),
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
    }, simplify = FALSE),
    stringsAsFactor = TRUE)

  # Vérification des facteurs supplémentaires agrégés.
  # Il ne doit pas y avoir d'élément nul (la fonction précédente renvoie NULL
  # si plusieurs niveaux de facteurs, i.e. le facteur est un sous ensemble
  # d'un des facteurs d'agrégation des observations) :
  if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))}))){
    warning(paste(mltext("agregations.generic.war.1"),
      mltext("agregations.generic.war.2"), sep = ""))
    return(NULL)
  }else{
    return(reslong)
  }
}


betterCbind <- function(..., dfList = NULL, deparse.level = 1){

  ## Purpose: Appliquer un cbind à des data.frames qui ont des colonnes
  ##          communes en supprimant les redondances (comme un merge mais
  ##          les lignes doivent être en mêmes nombres et
  ##          dans le même ordre)
  ## ---------------------------------------------------------------------------
  ## Arguments: ceux de cbind...
  ##            dfList : une liste de data.frames (evite un do.call
  ##                     supplémentaire).
  ##                     ... est utilisé à la place si NULL.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 janv. 2012, 21:10

  if (is.null(dfList)){
    dfList <- list(...)
  }else{}

  return(do.call(cbind,
    c(list(dfList[[1]][ , c(tail(colnames(dfList[[1]]), -1),
      head(colnames(dfList[[1]]), 1))]),
      lapply(dfList[-1],
        function(x, colDel){
          return(x[ , !is.element(colnames(x), colDel), drop = FALSE])
        },
        colDel = colnames(dfList[[1]])),
      deparse.level = deparse.level)))
}


agregation.f <- function(metric, Data, factors, casMetrique, dataEnv,
  nbName = "number"){

  ## Purpose: Agrégation d'une métrique.
  ## ---------------------------------------------------------------------------
  ## Arguments: metric: nom de la colonne de la métrique.
  ##            Data: table de données non-agrégées.
  ##            factors: vecteur des noms de facteurs d'agrégation.
  ##            casMetrique: vecteur nommé des types d'observation en
  ##                         fonction de la métrique choisie.
  ##            dataEnv: environnement des données.
  ##            nbName : nom de la colonne nombre.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 14:29

  switch(casMetrique[metric],
    "sum" = {
      res <- tapply(Data[ , metric],
        as.list(Data[ , factors, drop = FALSE]),
        function(x){
          ifelse(all(is.na(x)),
            NA,
            sum(x, na.rm = TRUE))
      })
    },
    "w.mean" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , factors, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metric])),
            NA,
            weighted.mean(Data[ii, metric],
              Data[ii, nbName],
              na.rm = TRUE))
      })
    },
    "w.mean.colonies" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , factors, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metric])),
            NA,
            weighted.mean(Data[ii, metric],
              Data[ii, "colonies"],
              na.rm = TRUE))
      })
    },
    "w.mean.prop" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , factors, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "nombre.tot"], na.rm = TRUE) == 0,
            NA,
            ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
              0,
              (sum(Data[ii, nbName][ !is.na(Data[ii, metric])], na.rm = TRUE) /
                sum(Data[ii, "nombre.tot"], na.rm = TRUE)) *
              # Correction si la classe de taille n'est pas un facteur d'agrégation
              # (sinon valeur divisée par le nombre de classes présentes) :
                ifelse(is.element("size.class", factors),
                  100,
                  100 * length(unique(Data$size.class)))))
      })

    },
    "w.mean.prop.bio" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , factors, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "tot.biomass"], na.rm = TRUE) == 0,
            NA,
            ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
              0,
              (sum(Data[ii, "biomass"][ !is.na(Data[ii, metric])], na.rm = TRUE) /
                sum(Data[ii, "tot.biomass"], na.rm = TRUE)) *
              # Correction si la classe de taille n'est pas un facteur d'agrégation
              # (sinon valeur divisée par le nombre de classes présentes) :
              ifelse(is.element("size.class", factors),
                100,
                100 * length(unique(Data$size.class)))))
      })

    },
    "pres" = {
      res <- tapply(Data[ , metric],
        as.list(Data[ , factors, drop = FALSE]),
        function(x){
          ifelse(all(is.na(x)), # Cas où il n'y a que des NAs.
            NA,
            ifelse(any(x > 0, na.rm = TRUE), # Sinon...
              1, # ...présence si au moins une observation dans le groupe.
              0))
      })
    },
    "nbMax" = {
      # Récupération des nombres brutes avec sélections :
      nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      nbTmp2 <- apply(nbTmp,
        which(is.element(names(dimnames(nbTmp)), c(factors, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
      })

      # Somme par croisement de facteur :
      res <- as.array(apply(nbTmp2,
        which(is.element(names(dimnames(nbTmp)), factors)),
          function(x){
            ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
      }))
    },
    "nbSD" = {
      # Récupération des nombres brutes avec sélections :
      nbTmp <- getReducedSVRdata.f(dataName = ".NombresSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      nbTmp2 <- apply(nbTmp,
        which(is.element(names(dimnames(nbTmp)), c(factors, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
      })

      # Somme par croisement de facteur :
      res <- as.array(apply(nbTmp2,
        which(is.element(names(dimnames(nbTmp)), factors)),
        function(x){
          ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
      }))
    },
    "densMax" = {
      # Récupération des nombres brutes avec sélections :
      densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      densTmp2 <- apply(densTmp,
        which(is.element(names(dimnames(densTmp)), c(factors, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
      })

      # Somme par croisement de facteur :
      res <- as.array(apply(densTmp2,
        which(is.element(names(dimnames(densTmp)), factors)),
        function(x){
          ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
      }))
    },
    "densSD" = {
      # Récupération des nombres brutes avec sélections :
      densTmp <- getReducedSVRdata.f(dataName = ".DensitesSVR", data = Data, dataEnv = dataEnv)

      # Somme par croisement de facteur / rotation :
      densTmp2 <- apply(densTmp,
        which(is.element(names(dimnames(densTmp)), c(factors, "rotation"))),
        function(x){
          ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
      })

      # Somme par croisement de facteur :
      res <- as.array(apply(densTmp2,
        which(is.element(names(dimnames(densTmp)), factors)),
        function(x){
          ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
      }))
    },
    "%.nesting" = {
      res <- tapply(1:nrow(Data),
        as.list(Data[ , factors, drop = FALSE]),
        function(ii){
          ifelse(all(is.na(Data[ii, metric])),
            NA,
            weighted.mean(Data[ii, metric],
              Data[ii, "readable.tracks"],
              na.rm = TRUE))
      })
    },
    stop("Not implemented!")
  )

  # Nom des dimensions
  names(dimnames(res)) <- c(factors)

  # Transformation vers format long :
  reslong <- as.data.frame(as.table(res), responseName = metric, stringsAsFactor = TRUE)
  reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

  return(reslong)
}


getReducedSVRdata.f <- function(dataName, data, dataEnv){

  ## Purpose: Récupérer des données brutes SVR (nombres, densités) réduites
  ##  aux sélections.
  ## ---------------------------------------------------------------------------
  ## Arguments: dataName : nom de tableau à récupérer.
  ##            data : données associées (avec sélections).
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 31 oct. 2012, 12:21

  res <- get(dataName, envir = dataEnv)

  # Limitations au classes de tailles, espèces et unité d'observations sélectionnées :
  if (is.element("species.code", colnames(data)) &&
      is.element("species.code", names(dimnames(res)))){

    species <- dimnames(res)[["species.code"]]
    res <- extract(res,
      indices = list(species[is.element(species, data[ , "species.code"])]),
      dims = which(is.element(names(dimnames(res)), "species.code")))

  }else{}

  if (is.element("observation.unit", colnames(data)) &&
      is.element("observation.unit", names(dimnames(res)))){

    unitObs <- dimnames(res)[["observation.unit"]]
    res <- extract(res,
      indices = list(unitObs[is.element(unitObs, data[ , "observation.unit"])]),
      dims = which(is.element(names(dimnames(res)), "observation.unit")))

  }else{}

  if (is.element("size.class", colnames(data)) &&
      is.element("size.class", names(dimnames(res)))){

    CL <- dimnames(res)[["size.class"]]
    res <- extract(res,
      indices = list(CL[is.element(CL, data[ , "size.class"])]),
      dims = which(is.element(names(dimnames(res)), "size.class")))

  }else{}

  return(res)
}


calc.unitSp.LIT.f <- function(obs, unitobs, dataEnv){

  ## Purpose: Calcul de la table de métrique par unité d'observation par
  ##          espèce pour le protocole benthos LIT
  ## ---------------------------------------------------------------------------
  ## Arguments: obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 déc. 2011, 18:22

  # Calcul des nombres par cl / espèces / unitobs :
  nbr <- calcNumber.default.f(obs,
    factors = c("observation.unit", "species.code"))

  # Création de la data.frame de résultats (avec nombres, unitobs, ):
  res <- calc.numbers.f(nbr)


  # Taille de transect :
  transectSz <- tapply(res[ , "number"], res[ , "observation.unit"], sum, na.rm = TRUE)

  # Pourcentage de recouvrement de chaque espèce/categorie pour les couvertures biotiques et abiotiques :
  res[ , "coverage"] <- as.vector(100 * res[ , "number"] /
    transectSz[match(res[ , "observation.unit"],
      rownames(transectSz))])
  rm(transectSz)

  # Nombre de colonies (longueurs de transition > 0) :
  obs$count <- ifelse(obs[ , "number"] > 0, 1, 0) # [???] isTRUE ?  [yr: 3/1/2012]

  res[ , "colonies"] <- as.vector(tapply(obs$count,
    as.list(obs[ , c("observation.unit", "species.code")]),
    sum, na.rm = TRUE))

  res[ , "colonies"][is.na(res[ , "colonies"])] <- 0 # [???]

  # Si les nombres sont des entiers, leur redonner la bonne classe :
  if (isTRUE(all.equal(res[ , "colonies"], as.integer(res[ , "colonies"])))){
    res[ , "colonies"] <- as.integer(res[ , "colonies"])
  }else{}


  res[ , "mean.size.colonies"] <- apply(res[ , c("number", "colonies")], 1,
    function(x){
      ifelse(x[2] == 0, NA, x[1] / x[2])
  })

  # Présence/absence :
  res[ , "pres.abs"] <- calc.presAbs.f(Data = res)

  return(res)
}


calc.unit.f <- function(unitSp, obs, refesp, unitobs, dataEnv){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ##            obs : table des observations (data.frame).
  ##            unitobs : table des unités d'observation (data.frame).
  ##            refesp : référentiel espèces.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 déc. 2011, 10:08

  #  # Informations :
  print(mltext("calc.unit.info.1"))

  casObsType <- c("SVR" = "SVR",
    "EMB" = "Fishing", "DEB" = "Fishing", "PSCI" = "Fishing", "PecRec" = "Fishing",
    "LIT" = "LIT",
    "PIT" = "PIT",
    "TRUVC" = "Transect", "UVC" = "Transect",
    "TRVID" = "Transect", "Q" = "Transect",
    "PFUVC" = "Fixe",
    "TRATO" = "TTracks")

  if ( ! is.null(unitSp) && nrow(unitSp)){
    unit <- switch(casObsType[getOption("P.obsType")],
      "SVR" = {
        calc.unit.SVR.f(unitSp = unitSp, obs = obs, refesp = refesp,
          unitobs = unitobs, dataEnv = dataEnv, colNombres = "number")
      },
      "LIT" = {        # Pour les types d'observation qui ont une colonne "colonie" à la place de
        # "number" :
        calc.unit.default.f(unitSp = unitSp, refesp = refesp, unitobs = unitobs,
          colNombres = "colonies", dataEnv = dataEnv)
      },
      "TTracks" = {    # Pour les types d'observation qui ont une colonne "tracks.number" à la
        # place de "number" :
        calc.unit.default.f(unitSp = unitSp, refesp = refesp, unitobs = unitobs,
          colNombres = "tracks.number", dataEnv = dataEnv, nbName = "tracks.number")
      },
      calc.unit.default.f(unitSp = unitSp, refesp = refesp, unitobs = unitobs,
          colNombres = "number", dataEnv = dataEnv))
  }else{
    unit <- NULL
  }

  return(unit)
}


calc.unit.SVR.f <- function(unitSp, obs, refesp, unitobs, dataEnv, colNombres = "number"){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 déc. 2011, 10:36

  # Agrégation des métriques par défaut :
  unit <- calc.unit.default.f(unitSp = unitSp, refesp = refesp, unitobs = unitobs,
    colNombres = "number", dataEnv = dataEnv)

  # Ajout des statistiques sur les rotations
  # (calcul à partir des données extrapolées brutes pour les statistiques) :
  nbInterp <- get(".NombresSVR", envir = dataEnv)

  # Réduction de la liste d'espèces si besoin (si sélection sur les espèces) :
  if (dim(nbInterp)[names(dimnames(nbInterp)) == "species.code"] > nlevels(unitSp[ , "species.code"])){
    species <- dimnames(nbInterp)[["species.code"]]

    nbInterp <- extract(nbInterp,
      indices = list(species[is.element(species,
        levels(unitSp[ , "species.code"]))]),
      dims = which(is.element(names(dimnames(nbInterp)), "species.code")))

  }else{}

  nbTmp <- apply(nbInterp,
    which( ! is.element(names(dimnames(nbInterp)), c("size.class", "species.code"))),
    function(x,...){
      ifelse(all(is.na(x)), NA, sum(x,...))
    }, na.rm = TRUE)

  # nombres calculés... pour comparaison avec nombres agrégé uniquement :
  nbTest <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, mean(x,...))
    }, na.rm = TRUE)))

  if ( ! isTRUE(all.equal(unit$number, nbTest))) stop(mltext("calc.unitSp.SVR.err.1"))

  # nombre max :
  unit[ , "number.max"] <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, max(x,...))
    }, na.rm = TRUE)))

  # nombre SD :
  unit[ , "number.sd"] <- as.vector(t(apply(nbTmp,
    which( ! is.element(names(dimnames(nbTmp)), "rotation")),
    function(x,...){
      ifelse(all(is.na(x)), NA, sd(x,...))
    }, na.rm = TRUE)))

  # Density max :
  unit <- calc.density.SVR.f(Data = unit, obs = obs,
    metric = c("density.max", "density.sd"),
    factors = "observation.unit")

  # Biomass max :
  unit <- stat.biomass.SVR.f(Data = unit, obs = obs,
    metric = c("biomass.max", "biomass.sd"),
    factors = "observation.unit")

  return(unit)
}


calc.unit.default.f <- function(unitSp, refesp, unitobs, colNombres = "number",
  dataEnv = .GlobalEnv, nbName = "number"){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 20 déc. 2011, 17:07

  metrics <- c("number", "mean.length", "weight", "mean.weight", "density", "biomass",
    "pres.abs",
    "CPUE", "CPUE.biomass",
    "colonies", "coverage", "mean.size.colonies",
    "spawnings", "readable.tracks", "spawning.success",
    "tracks.number")

  usedMetrics <- metrics[is.element(metrics, colnames(unitSp))]

  if ( ! is.null(unitSp)){
    unit <- agregations.generic.f(Data = unitSp,
      metrics = usedMetrics,
      factors = c("observation.unit"),
      listFact = NULL,
      unitSpSz = NULL,
      unitSp = unitSp,
      dataEnv = dataEnv,
      nbName = nbName)

    tmp <- do.call(rbind,
      lapply(unique(as.character(unitobs[ , getOption("P.MPAfield")])),
        function(MPA){
          calcBiodiv.f(Data = subset(unitSp,
            is.element(observation.unit,
              unitobs[unitobs[ , getOption("P.MPAfield")] == MPA ,
                "observation.unit"])),
            refesp = refesp,
            MPA = MPA,
            unitobs = "observation.unit",
            code.especes = "species.code", nombres = colNombres, indices = "all",
            printInfo = TRUE, global = TRUE,
            dataEnv = dataEnv)
    }))

    unit <- merge(unit, tmp[ , colnames(tmp) != colNombres],
      by.x = "observation.unit", by.y = "observation.unit")

    return(unit)
  }else{}
}


calcBiodiv.f <- function(Data, refesp, MPA, unitobs = "observation.unit",
  code.especes = "species.code", nombres = "number",
  indices = "all", global = FALSE, printInfo = FALSE, dataEnv = .GlobalEnv){

  ## Purpose: calcul des indices de biodiversité
  ## ----------------------------------------------------------------------
  ## Arguments: Data : les données à partir desquelles calculer les
  ##                   indices. Doivent comporter au minimum (colones) :
  ##                     * unités d'observations/sites
  ##                     * espèces présentes
  ##                     * nombre d'individus /espèce/unitobs.
  ##            refesp : le référentiel espèces.
  ##            MPA : l'AMP (chaîne de charactères).
  ##            unitobs : nom de la colone d'unités d'observation.
  ##            code.especes : nom de la colone d'espèces.
  ##            nombres : nom de la colone de nombres.
  ##            indices : liste des indices à calculer
  ##                      (vecteur de caractères)
  ##            global : est-ce que les résultats doivent être exportés
  ##                     globalement (booléen).
  ##            printInfo : affichage des infos (chargement) ? (booléen).
  ##            dataEnv : environnement des données
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 29 oct. 2010, 08:58

  # Unitobs appartenant a l'AMP courante:
  unitobsData <- get("unitobs", envir = dataEnv)

  Data <- subset(Data,
    is.element(Data[ , unitobs],
      unitobsData[unitobsData[ , getOption("P.MPAfield")] == MPA ,
        unitobs]))

  DataTmp <- Data

  # Supression de tout ce qui n'a pas d'espèce précisee
  # (peut être du non biotique ou identification >= genre) :
  if (! nrow(Data <- Data[(
    spTmp <- refesp$species[match(Data[ , code.especes], refesp$species.code)])
    != "sp." & !is.na(spTmp), ])){
    return(Data)

    if (printInfo){
      print(mltext("calcBiodiv.f.info.1"))
    }else{}

  }else{}

  # Suppression des niveaux de facteur inutilisés :
  Data <- dropLevels.f(df = Data)

  if (printInfo){
    if (nlevels(DataTmp[ , code.especes]) > nlevels(Data[ , code.especes])){
      nsup <- nlevels(DataTmp[ , code.especes]) - nlevels(Data[ , code.especes])

      print(paste(
        nsup, " \"species.code\" ",
        ifelse(nsup > 1 ,
          mltext("calcBiodiv.f.info.2.p"),
          mltext("calcBiodiv.f.info.2.s")),
        mltext("calcBiodiv.f.info.3"),
        ifelse(nsup > 1,
          mltext("calcBiodiv.f.info.4.p"),
          mltext("calcBiodiv.f.info.4.s")),
        mltext("calcBiodiv.f.info.5"),
        sep = ""))

    }else{}
  }else{}

  # Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
  if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes])))){
    Data <- agregations.generic.f(Data = Data, metrics = nombres,
      factors = c(unitobs, code.especes),
      listFact = NULL, dataEnv = dataEnv)
  }else{}

  df.biodiv <- as.data.frame(as.table(tapply(Data[ , nombres],
    Data[ , unitobs],
    sum, na.rm = TRUE)),
    stringsAsFactor = TRUE)

  colnames(df.biodiv) <- c(unitobs, nombres)

  # ##################################################
  # Richesse spécifique :
  Data$pres.abs <- presAbs.f(nombres = Data[ , nombres], logical = FALSE)

  df.biodiv$species.richness <- as.vector(tapply(
    Data$pres.abs, Data[ , unitobs], sum, na.rm = TRUE), "integer")
  # ... as.vector to avoid the class "array".

  # richesses specifiques relatives :

  # Phylum(s) présent(s) dans le jeux de données :
  phylums <- as.character(unique(na.omit(refesp$phylum[match(Data[ , code.especes],
    refesp$species.code)])))

  # RS relative par rapp. au nombre d'espèces du site :
  if (any(is.element(c("all", "relative.SR.site"), indices))){
    if (getOption("P.refesp.Coefs") == "new"){
      # Nouveau référentiel espèce ET fichier local chargé :
      if (is.element("observed", colnames(refesp))){
        df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
          nrow(subset(refesp,
            is.element(observed, c("oui", "O"))))) * 100
      }else{}
    }else{
      df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
        nrow(subset(refesp,
          is.element(eval(parse(text = paste("Obs", MPA, sep = ""))),
            c("oui", "O"))))) * 100
    }
  }

  # RS relative par rapp. au nombre d'espèces du site et
  # du(des) phylum(s) concerné(s) (jeu de données) :
  if (any(is.element(c("all", "relative.SR.site.phylum"), indices))){
    if (getOption("P.refesp.Coefs") == "new"){
      # Nouveau référentiel espèce ET fichier local chargé :
      if (is.element("observed", colnames(refesp))){
        df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
          nrow(subset(refesp,
            is.element(observed, c("oui", "O", "yes", "Y")) &
              is.element(phylum, phylums)))) * 100 # [ml?]
      }else{}
    }else{
      df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
        nrow(subset(refesp,
          is.element(eval(parse(text = paste("Obs", MPA, sep = ""))),
            c("oui", "O", "yes", "Y")) &
          is.element(phylum, phylums)))) * 100
    }
  }

  # RS relative par rapp. au nombre d'espèces des données :
  if (any(is.element(c("all", "relative.SR.data"), indices))){
    df.biodiv$relative.SR.data <- (df.biodiv$species.richness /
      nrow(subset(refesp,
        is.element(species.code, Data[ , code.especes])))) * 100
  }

  # # RS relative par rapp. au nombre d'espèces des données + des phyla présents :
  # Inutile : "RS.relative.donnees" est par définition limitée au phyla présents !

  # RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) :
  if (any(is.element(c("all", "relative.SR.region"), indices))){
    df.biodiv$relative.SR.region <- (df.biodiv$species.richness /
      nrow(refesp)) * 100
  }

  # RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) et
  # du(des) phylum(s) concerné(s) (jeu de données) :
  if (any(is.element(c("all", "relative.SR.region.phylum"), indices))){
    df.biodiv$relative.SR.region.phylum <- (df.biodiv$species.richness /
      nrow(subset(refesp, is.element(phylum, phylums)))) * 100
  }

  # ##################################################
  # Indices de Simpson et Shannon et dérivés :

  matNombres <- tapply(Data[ , nombres], # Matrice de nombres d'individus /espèce/unitobs.
    list(Data[ , unitobs], Data[ , code.especes]),
    sum, na.rm = TRUE)

  matNombres[is.na(matNombres)] <- 0  # Vrais zéros

  # Proportion d'individus de chaque espèce dans l'unitobs :
  propIndiv <- sweep(matNombres, 1,                           #
    apply(matNombres, 1, sum, na.rm = TRUE), # Nombre d'individus / unitobs ; équiv df.biodiv$nombre.
    FUN = "/")

  # Indices de Simpson :
  df.biodiv$simpson <- apply(propIndiv^2, 1, sum, na.rm = TRUE)

  if (any(is.element(c("all", "simpson.l"), indices))){
    df.biodiv$simpson.l <- 1 - df.biodiv$simpson
  }

  # calcul de l'indice de Shannon :
  df.biodiv$shannon <- -1 * apply(propIndiv * log(propIndiv), 1, sum, na.rm = TRUE)

  # calcul de l'indice de Pielou :
  if (any(is.element(c("all", "pielou"), indices))){
    df.biodiv$pielou <- df.biodiv$shannon / log(df.biodiv$species.richness)
  }

  # calcul de l'indice de Hill :
  if (any(is.element(c("all", "hill"), indices))){
    df.biodiv$hill <- (1 - df.biodiv$simpson) / exp(df.biodiv$shannon)
    # équiv df.biodiv$l.simpson / exp(df.biodiv$shannon)
  }

  # suppression de l'indice de shannon (non pertinent)
  df.biodiv$shannon <- NULL

  # ##################################################
  # Indices de biodiversité taxonomique :
  df.biodivTaxo <- calcBiodivTaxo.f(Data = Data,
    refesp = refesp,
    unitobs = unitobs, code.especes = code.especes, nombres = nombres,
    global = global, printInfo = printInfo,
    indices = indices,
    dataEnv = dataEnv)

  if (!is.null(dim(df.biodivTaxo))){
    df.biodiv <- cbind(df.biodiv,
      df.biodivTaxo[match(df.biodiv[ ,unitobs], row.names(df.biodivTaxo)), , drop = FALSE])
  }else{}

  for (ind in c("simpson", "shannon", "species.richness")){
    if (! any(is.element(c(ind, "all"), indices))){
      df.biodiv[ , ind] <- NULL
    }else{}
  }

  # # On retablit les niveaux de facteurs:
  # colFact <- colnames(df.biodiv)[is.element(sapply(df.biodiv, class), "factor")]

  # for (col in colFact)
  # {
  #     levels(df.biodiv[ , colFact]) <- levels(DataTmp[ , colFact])
  # }

  return(df.biodiv)
}


presAbs.f <- function(nombres, logical = FALSE){

  ## Purpose: Renvoie les présences/absences d'après les nombres.
  ## ----------------------------------------------------------------------
  ## Arguments: nombres : vecteur de nombre d'individus.
  ##            logical : faut-il renvoyer les résultats sous forme de
  ##                      booléens, ou 0/1 (booléen).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 29 oct. 2010, 10:20

  if (any(nombres < 0, na.rm = TRUE)){
    stop("Negative abundances!")
  }else{}

  if (logical){
    return(nombres > 0)
  }else{
    nombres[nombres > 0] <- 1
    return(nombres)
  }
}

#' @importFrom vegan taxondive taxa2dist

calcBiodivTaxo.f <- function(Data, refesp, unitobs = "observation.unit",
  code.especes = "species.code", nombres = "number",
  global = FALSE, printInfo = FALSE,
  indices = "all", dataEnv = .GlobalEnv){

  ## Purpose: Calcul des indices de biodiversité basés sur la taxonomie.
  ## ----------------------------------------------------------------------
  ## Arguments: Data : les données à partir desquelles calculer les
  ##                   indices. Doivent comporter au minimum (colones) :
  ##                     * unités d'observations/sites
  ##                     * espèces présentes
  ##                     * nombre d'individus /espèce/unitobs.
  ##            refesp : référentiel espèces.
  ##            unitobs : nom de la colone d'unités d'observation.
  ##            especes : nom de la colone d'espèces.
  ##            nombres : nom de la colone de nombres.
  ##            global : est-ce que les résultats doivent être exportés
  ##                     globalement (booléen).
  ##            printInfo : affichage des infos ? (booléen).
  ##            indices : liste des indices à calculer
  ##                      (vecteur de caractères), tous par défaut.
  ##            dataEnv : environnement des données
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 29 oct. 2010, 14:30

  # Indices proposés :
  proposed.indices <- c("D" = "Delta",
    "Dstar" = "DeltaStar",
    "Lambda" = "LambdaPlus",
    "Dplus" = "DeltaPlus",
    "SDplus" = "SDeltaPlus")

  # On sort de la fonction si elle n'a pas d'intéret :
  if (! any(is.element(c(proposed.indices, "all"), indices))){
    return(NULL)                    # Rien !
  }else{
    # Suppression de tout ce qui n'a pas de genre (peut être du non biotique) :
    Data <- Data[refesp$species[match(Data$species.code, refesp$species.code)] != "sp.", ]

    # Suppression des niveaux de facteur inutilisés :
    Data <- dropLevels.f(df = Data)

    # Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
    if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes])))){
      Data <- agregations.generic.f(Data = Data, metrics = nombres,
        factors = c(unitobs, code.especes),
        listFact = NULL, dataEnv = dataEnv)
    }else{}

    # Table de contingence unitobs-espèces :
    contingence <- tapply(Data[ , nombres],
      list(Data[ , unitobs], Data[ , code.especes]),
      sum, na.rm = TRUE)

    contingence[is.na(contingence)] <- 0 # Vrais zéros.

    # tableau avec genre, famille, etc.
    sp.taxon <- dropLevels.f(refesp[match(colnames(contingence),
      refesp$species.code, nomatch = NA, incomparables = FALSE),
      c("species", "genus", "family", "order", "class", "phylum")])

    # colnames(sp.taxon) <- c("genre", "famille", "ordre", "classe", "phylum")
    rownames(sp.taxon) <- colnames(contingence)

    # retrait des lignes ayant un niveau taxonomique manquant dans sp.taxon
    # et dans contingence (en colonnes) :
    manque.taxon <- apply(sp.taxon, 1, function(x){any(is.na(x))})
    sp.taxon <- sp.taxon[! manque.taxon, , drop = FALSE]
    contingence <- contingence[, ! manque.taxon, drop = FALSE]


    # Calcul des indices (librairie "vegan") :
    if (sum(sapply(sp.taxon, function(x)length(unique(x))) > 1) > 2){ # typiquement : une seule famille ou même genre.
      # Indices retenus :
      if (is.element("all", indices)){
        retained.indices <- proposed.indices
      }else{
        retained.indices <- proposed.indices[is.element(proposed.indices, indices)]
      }

      # calcul des distances taxonomiques entre les especes
      if (!is.null(taxdis <- tryCatch(vegan::taxa2dist(sp.taxon, varstep = TRUE, check = TRUE),
        error = function(e){
          errorLog.f(error = e, niv = -3)
          return(NULL)
        }))){
        # Function finds indices of taxonomic diversity and distinctiness, which are averaged
        # taxonomic distances among species or individuals in the community...
        divTaxo <- vegan::taxondive(contingence, taxdis)

        # mise de divTaxo sous forme de data.frame :
        df.biodivTaxo <- as.data.frame(divTaxo[names(retained.indices)], stringsAsFactor = TRUE)

        colnames(df.biodivTaxo) <- retained.indices # [!!!] "LambdaPlus" ? vraiment ? [???]

      }else{
        divTaxo <- NULL
        df.biodivTaxo <- NULL
      }

      # Résultats :
      if (global){
        # Création des objets dans l'environnement global
        assign("div", divTaxo, envir = .GlobalEnv)
        assign("taxdis", taxdis, envir = .GlobalEnv)
        assign("ind_div", df.biodivTaxo, envir = .GlobalEnv)
      }else{
        return(df.biodivTaxo)
      }
    }else{                              # nombre de genre < 2.
      switch(sum(sapply(sp.taxon, function(x)length(unique(x))) > 1),
        "1" = {
          warning(mltext("calcBiodivTaxo.f.Warn.1"))
        },
        "0" = {
          warning(mltext("calcBiodivTaxo.f.Warn.2"))
        })
    }
  }
}


listInEnv.f <- function(list, env){

  ## Purpose: Copie les éléments d'une liste (nommée) dans un environnement
  ##          (avec comme nom d'élément son nom dans la liste).
  ## ----------------------------------------------------------------------
  ## Arguments: list : la liste à copier.
  ##            env : l'environnement dans lequel enregistrer les
  ##                  éléments.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 15:38

  if (is.null(names(list))){
    listNames <- paste("obj", seq(length.out = length(list)), sep = "")

    warning("Unnamed list: the elements have been named \"obj1\", \"obj2\", etc.")
  }else{
    listNames <- names(list)
  }
  invisible(sapply(list,
    function(x, xN, env){
      # Numéro d'itération :
      i <- sys.call()[[2]][[3]]

      if (is.symbol(x = i)){ # for compatibility R > 3.1
        i <- eval(i, sys.frame(-1))
      }
      # Assignement :
      assign(x = xN[i], value = x, envir = env)
    },
    xN = listNames, env = env))
}


exportMetrics.f <- function(unitSpSz, unitSp, unit, obs, unitobs, refesp,
  filePathes, baseEnv){

  ## Purpose: Exporter
  ##            * les tables de métriques avec des colonnes supplémentaires
  ##              dans l'environnement global + les sauvegarder dans des
  ##              fichiers.
  ##            * exporter les tables de données dans l'environnement
  ##              global.
  ##          Les noms utilisés dans l'environnement global ne doivent
  ##          pas être les noms internes pour éviter des bugs
  ##          in-déboguables.
  ## ----------------------------------------------------------------------
  ## Arguments: unitSpSz : table de métriques /CT/esp/unitobs.
  ##            unitSp : table de métriques /esp/unitobs.
  ##            unit : table de métriques /unitobs.
  ##            obs : table des données d'observation.
  ##            unitobs : référentiel des unités d'observation.
  ##            refesp : référentiel espèces.
  ##            filePathes : chemins des fichiers/dossiers.
  ##            baseEnv : environnement de l'interface principale.
  ##
  ## Output: Rien !
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 20:08

  PlanEchantillonnageBasic.f(tabUnitobs = unitobs, tabObs = obs, filePathes = filePathes)

  # Certaines métriques (densités) sont ramenées à /100m² (avec ajout des colonnes par défaut) :
  unitSpSz <- scaleMetrics.f(Data = unitSpSz, unitobs = unitobs, refesp = refesp, scale = TRUE)
  unitSp <- scaleMetrics.f(Data = unitSp, unitobs = unitobs, refesp = refesp, scale = TRUE)
  unit <- scaleMetrics.f(Data = unit, unitobs = unitobs, refesp = refesp, scale = TRUE)

  # Export des principales tables dans l'environnement global :
  assign("TableUnitSpSz", unitSpSz, envir = .GlobalEnv)
  assign("TableUnitSp", unitSp, envir = .GlobalEnv)
  assign("TableUnit", unit, envir = .GlobalEnv)
  assign("DataObs", obs, envir = .GlobalEnv)
  assign("DataUnitobs", unitobs, envir = .GlobalEnv)
  assign("DataRefesp", refesp, envir = .GlobalEnv)

  print(paste(
    mltext("infoExport.1"),
    ifelse(! is.null(unitSpSz) && prod(dim(unitSpSz)),
      mltext("infoExport.2"),
      ""),
    mltext("infoExport.3"),
    mltext("infoExport.4"),
    mltext("infoExport.5"),
    sep = ""))

  # Sauvegardes dans des fichiers :
  if ( ! is.null(unitSpSz) &&
    prod(dim(unitSpSz))){            # i.e. nrow et ncol > 0.
    # Table unitSpSz si elle existe :
    fileNm <- paste(filePathes["results"],
      "UnitobsEspeceClassetailleMetriques",
      ifelse(getOption("P.selection"), "_selection", ""),
      ".csv", sep = "")
    tryCatch(write.csv(unitSpSz,
      file = fileNm,
      row.names = FALSE),
      error = function(e){
        warning(paste("Impossible d'écrire le fichier ", fileNm,
          ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""),
          call. = FALSE, immediate. = TRUE)
        errorLog.f(error = e, niv = -4)
    })
  }else{}                             # Sinon rien !

  # Table unitSp :
  fileNm <- paste(filePathes["results"],
    "UnitobsEspeceMetriques",
    ifelse(getOption("P.selection"), "_selection", ""),
    ".csv", sep = "")
  tryCatch(write.csv(unitSp,
    file = fileNm,
    row.names = FALSE),
    error = function(e){
      warning(paste("Impossible d'écrire le fichier ", fileNm,
        ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""),
        call. = FALSE, immediate. = TRUE)
      errorLog.f(error = e, niv = -4)
  })

  # Table unit :
  fileNm <- paste(filePathes["results"],
    "UnitobsMetriques",
    ifelse(getOption("P.selection"), "_selection", ""),
    ".csv", sep = "")
  tryCatch(write.csv(unit,
    file = fileNm,
    row.names = FALSE),
    error = function(e){
      warning(paste("Impossible d'écrire le fichier ", fileNm,
        ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""),
        call. = FALSE, immediate. = TRUE)
      errorLog.f(error = e, niv = -4)
  })
}


PlanEchantillonnageBasic.f <- function(tabUnitobs, tabObs, filePathes){

  ## Purpose: Écrire le plan d'échantillonnage basic dans un fichier.
  ## ----------------------------------------------------------------------
  ## Arguments: tabUnitobs : table des unités d'observation (data.frame).
  ##            tabObs : table des observations (data.frame).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 sept. 2011, 13:53

  PlanEchantillonnage <- with(dropLevels.f(tabUnitobs[is.element(tabUnitobs$observation.unit,
    levels(tabObs$observation.unit)), ]),
    table(year, protection.status, exclude = NA))

  attr(PlanEchantillonnage, "class") <- "array" # Pour un affichage en "tableau".

  write.csv(PlanEchantillonnage,
    file = paste(filePathes["results"],
      "PlanEchantillonnage_basique", # [ml?]
      ifelse(getOption("P.selection"), "_selection", ""),
      ".csv", sep = ""), row.names = TRUE)
}


scaleMetrics.f <- function(Data, unitobs, refesp,
  supl = c("year", "site", "protection.status", "biotop", "latitude", "longitude",
    "annee.campagne", "habitat1", "habitat2", "habitat3",
    "scient.name", "family", "genus", "species"),
  scale = TRUE){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 20:30

  if ( ! is.null(Data) &&
       prod(dim(Data))){                # i.e. ncol et nrow > 0.
    # Champs à ajouter par référentiel :
    suplUnitobs <- supl[is.element(supl, colnames(unitobs)) &
      ! is.element(supl, colnames(Data))]
    suplRefesp <- supl[is.element(supl, colnames(refesp)) &
      ! is.element(supl, colnames(Data))]

    # Ajout des champs supplémentaires des unitobs :
    if (length(suplUnitobs)){
      Data <- merge(Data,
        unitobs[ , unique(c("observation.unit", suplUnitobs)), drop = FALSE],
        by = c("observation.unit"))
    }else{}

    # Ajout des champs supplémentaires du référentiel espèces :
    if (length(suplRefesp) && is.element("species.code", colnames(Data))){
      Data <- merge(Data,
        refesp[ , unique(c("species.code", suplRefesp)), drop = FALSE],
        by = c("species.code"))
    }else{}

    # Scalling : certaines métriques (densités) doivent être ramenées à /100m² :
    if (scale &&
        any(is.element(colnames(Data),
          colTmp <- c("density", "density.max", "density.sd",
            "biomass", "biomass.max", "biomass.sd")))){
      Data[ , is.element(colnames(Data), colTmp)] <- sweep(
        Data[ , is.element(colnames(Data),colTmp), drop = FALSE],
        2, 100, "*")
    }else{}
  }else{}

  return(Data)
}


calcWeight.f <- function(Data){

  ## Purpose: Lance le calcul des poids pour chaque AMP et assemble les
  ##          résultat en une seule data.frame de type "observations".
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : une liste contenant 3 data.frames nommées :
  ##                   * obs : table des observations.
  ##                   * unitobs : référentiel d'unités d'observations.
  ##                   * refesp : le référentiel espèces.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 16 déc. 2011, 11:34

  print(mltext("calcWeight.info.1"))

  # Réassemblage de la liste des obs par cas d'étude avec les poids estimés.
  Data$obs <- do.call(rbind,
    lapply(unique(Data$unitobs[ , getOption("P.MPAfield")]),
      function(i, obs, unitobs, refesp) {
        print(mltext("calcWeight.info.2"))
        # Sélection des observations du cas d'étude i :
        obs <- obs[is.element(obs$observation.unit,
          unitobs[unitobs[ , getOption("P.MPAfield")] == i,
            "observation.unit"]), ]
        # Estimation des poids :
        return(calcWeightMPA.f(Data = obs, refesp = refesp, MPA = i))
      },
    obs = Data$obs, unitobs = Data$unitobs, refesp = Data$refesp))

  return(Data)
}


calcWeightMPA.f <- function(Data, refesp, MPA,
  vars = c(sp = "species.code", sz = "length", wg = "weight", szcl = "size.class",
    szmax = "Lmax", nb = "number")) {

  ## Purpose: Calculs des poids (manquants) pour un fichier de type
  ##          "observation" pour **une seule AMP**, avec par ordre
  ##          de priorité :
  ##           1) conservation des poids observés.
  ##           2) calcul des poids avec les relations taille-poids.
  ##           3) idem avec les tailles d'après les classes de tailles.
  ##           4) poids moyens d'après les classes de tailles (G, M, P).
  ##           5) rien sinon (NAs).
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : jeu de données ; doit contenir les colonnes
  ##                   décrites par vars (à l'exception de szcl qui
  ##                   est optionnelle).
  ##            refesp : référentiel espèces.
  ##            MPA : l'AMP où ont été collectées les données. En ne
  ##                  passant cet argument, on peut travailler sur une
  ##                  partie d'un jeu de données multi-sites.
  ##            vars : nom des colonnes de différents types dans Data.
  ##
  ## Output: Data avec les poids calculés dans la colonne "weight".
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  6 déc. 2010, 11:57

  runLog.f(msg = c(mltext("logmsg.weightMPA.calc")))

  # Identification des différents cas :
  casSite <- c("BA" = "Med", "BO" = "Med", "CB" = "Med", "CR" = "Med", "STM" = "Med",
    "MAY" = "MAY", "RUN" = "MAY")

  if (getOption("P.obsType") == "TRATO"){
    vars["nb"] <- "tracks.number"
  }

  # L'AMP est mise au format désiré (on ne traite qu'un élément à la fois) :
  MPA <- as.character(MPA[1])

  # Poids observés :
  res <- Data[ , "weight"]

  # Nombre d'obs totales, nombre de poids obs et nombre de tailles obs (sans poids obs) :
  nbObsType <- c("total" = length(res), "obs" = sum(!is.na(res)))

  # indices des tailles renseignées, pour lesquelles il n'y a pas de poids renseigné :
  idxTaille <- !is.na(Data[ , vars["sz"]]) & is.na(res)

  # ajout des tailles moyennes d'après la classe de taille :
  Data[ , vars["sz"]] <- addMeanSize.f(Data = Data, vars = vars)

  # indices des tailles ajoutées par cette méthode
  # pour lesquelles il n'y a pas de poids renseigné :
  idxTailleMoy <- !is.na(Data[ , vars["sz"]]) & ! idxTaille & is.na(res)

  # Calcul des poids à partir des relations taille-poids W = n*a*L^b :
  idxP <- is.na(res)                  # indices des poids à calculer.

  if (getOption("P.refesp.Coefs") == "new"){
    res[idxP] <- (Data[ , vars["nb"]] *
      refesp$a.coeff[match(Data[ , vars["sp"]], refesp[ , vars["sp"]])] *
      Data[ , vars["sz"]] ^ refesp$b.coeff[match(Data[ , vars["sp"]],
        refesp[ , vars["sp"]])])[idxP]
  }
  else{
    switch(casSite[MPA],
      # Méditerrannée :
      "Med" = {
      res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.Med[match(Data[
        , vars["sp"]], refesp[ , vars["sp"]])] *
        Data[ , vars["sz"]] ^ refesp$Coeff.b.Med[match(Data[ , vars["sp"]],
          refesp[ , vars["sp"]])])[idxP]
      },
      # Certains sites outre-mer :
      "MAY" = {
        res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.MAY[match(Data[
          , vars["sp"]], refesp[ , vars["sp"]])] *
           Data[ , vars["sz"]] ^ refesp$Coeff.b.MAY[match(Data[ , vars["sp"]],
             refesp[ , vars["sp"]])])[idxP]
      },
      # Autres (NC,...) :
      res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.NC[match(Data[
        , vars["sp"]], refesp[ , vars["sp"]])] *
        Data[ , vars["sz"]] ^ refesp$Coeff.b.NC[match(Data[ , vars["sp"]],
          refesp[ , vars["sp"]])])[idxP]
    )
  }

  # [!!!] Comptabiliser les tailles incalculables !
  # Nombre de poids ajoutés grâce à la méthode :
  nbObsType[c("length", "taille.moy")] <- c(sum(!is.na(res[idxTaille])),
    sum(!is.na(res[idxTailleMoy])))

  # Ajout des classes de taille (P, M, G) si nécessaire lorsque la taille est connue
  # (permet le calcul / poids moyen de classe si les coefs a et b sont inconnus) :
  Data <- sizeClasses.f(Data = Data, refesp = refesp, vars = vars)

  if ((getOption("P.refesp.Coefs") == "new" && # Nouveau référentiel avec fichier local chargé.
    all(is.element(c("mean.weight.small", "mean.weight.medium", "mean.weight.large"),
      colnames(refesp)))) ||
    isTRUE(MPA == "BO")){
    # Poids d'après les classes de taille lorsque la taille n'est pas renseignée :
    tmpNb <- sum(!is.na(res))           # nombre de poids disponibles avant.

    res[is.na(res)] <- (meanWeight.SzCl.f(Data = Data, refesp = refesp, vars = vars) *
      Data[ , vars["nb"]])[is.na(res)]

    nbObsType["poids.moy"] <- sum(!is.na(res)) - tmpNb # nombre de poids ajoutés.
  }

  # Stockage et retour des données :
  Data[ , "weight"] <- res
  return(Data)
}


addMeanSize.f <- function(Data, vars = c(sz = "length", szcl = "size.class")){

  ## Purpose: Calcul des tailles comme les moyennes de classes de taille,
  ##          si seules ces dernières sont renseignées.
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : les données d'observation.
  ##            vars : nom des colonnes de différents types dans Data.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 août 2010, 16:20

  runLog.f(msg = c(mltext("logmsg.meansize.szcl")))

  res <- Data[ , vars["sz"]]
  classes.taille <- Data[ , vars["szcl"]]

  # Calcul des tailles comme tailles moyennes à partir des classes de taille si nécessaire :
  if (any(is.na(res)[grep("^([[:digit:]]*)[-_]([[:digit:]]*)$", classes.taille)])){
    # Classes de tailles fermées :
    # classes de tailles qui correspondent aux motifs pris en compte...
    idxCT <- grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", classes.taille)
    idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
    res[idxCT] <- unlist(sapply(parse(text = sub("^([[:digit:]]+)[-_]([[:digit:]]+)$",
      "mean(c(\\1, \\2), na.rm = TRUE)", classes.taille[idxCT])),
      eval))

    # Classes de tailles ouvertes vers le bas (borne inférieure <- 0) :
    idxCT <- grep("^[-_]([[:digit:]]+)$", classes.taille)
    idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
    res[idxCT] <- unlist(sapply(parse(text = sub("^[-_]([[:digit:]]+)$",
      "mean(c(0, \\1), na.rm = TRUE)", classes.taille[idxCT])),
      eval))

    # Classes de tailles ouvertes vers le haut (pas de borne supérieur,
    # on fait l'hypothèse que la taille est le minimum de la gamme) :
    idxCT <- grep("^([[:digit:]]+)[-_]$", classes.taille)
    idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
    res[idxCT] <- unlist(sapply(parse(text = sub("^([[:digit:]]+)[-_]$",
      "mean(c(\\1), na.rm = TRUE)", classes.taille[idxCT])),
      eval))
  }
  else{}

  return(res)
}


sizeClasses.f <- function(Data, refesp,
  vars = c(sp = "species.code", sz = "length", szcl = "size.class", szmax = "Lmax")){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments: Data : table de données de type Obs.
  ##            refesp : référentiel espèces.
  ##            vars : nom des colonnes de différents types dans Data.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 15 déc. 2011, 11:33

  runLog.f(msg = c(mltext("logmsg.sizeclass")))

  if (any(idx <- (( ! is.na(Data[ , vars["sz"]])) &
    is.na(Data[ , vars["szcl"]])))){
    # On ne travaille que sur les lignes où la classe de taille peut être définie :
    df <- Data[idx, ]
    levels(df[ , vars["szcl"]]) <- c(levels(df[ , vars["szcl"]]), "P", "M", "G")

    # Premier tiers de la gamme de taille (d'après la taille maximale) -> "P" :
    idxP <- ((idxTmp <- df[ , vars["sz"]] < (refesp[match(df[ , vars["sp"]] ,
      refesp[ , vars["sp"]]), vars["szmax"]]/3)) &
      !is.na(idxTmp))

    df[idxP, vars["szcl"]] <- "P"

    # Second tiers de la gamme de taille (d'après la taille maximale) -> "M" :
    idxM <- ((idxTmp <- df[ , vars["sz"]] >= (refesp[match(df[ , vars["sp"]] ,
      refesp[ , vars["sp"]]), vars["szmax"]]/3) & # et
      df[ , vars["sz"]] < (2*(refesp[match(df[ , vars["sp"]] ,
        refesp[ , vars["sp"]]), vars["szmax"]]/3))) &
      ! is.na(idxTmp))

    df[idxM, vars["szcl"]] <- "M"

    # Troisième tiers de la gamme de taille (d'après la taille maximale) -> "G" :
    idxG <- ((idxTmp <- df[ , vars["sz"]] >= (2*(refesp[match(df[ , vars["sp"]] ,
      refesp[ , vars["sp"]]), vars["szmax"]]/3))) &
      ! is.na(idxTmp))

    df[idxG, vars["szcl"]] <- "G"

    # On rajoute les classes de tailles calculées à l'ensemble du jeu de données :
    if ( ! all(is.na(df[ , vars["szcl"]]))){
      levels(Data[ , vars["szcl"]]) <- levels(df[ , vars["szcl"]])
      Data[idx, vars["szcl"]] <- df[ , vars["szcl"]]
    }
    else{}

    # ct <- 1                         # en une ligne plutôt [yr: 27/07/2010]
    # assign("ct", ct, envir = .GlobalEnv)

  }else{
    # ct <- 2                         # en une ligne plutôt [yr: 27/07/2010]
    # assign("ct", ct, envir = .GlobalEnv)
  }

  return(Data)
}


meanWeight.SzCl.f <- function(Data, refesp,
  vars = c(sp = "species.code", szcl = "size.class")){

  ## Purpose: poids moyens d'après les classes de taille PMG du référentiel
  ##          espèces.
  ## ---------------------------------------------------------------------------
  ## Arguments: Data (type obs).
  ##            refesp : le référentiel espèces.
  ##            nom des colonnes de différents types dans Data.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 11 oct. 2010, 14:58

  runLog.f(msg = c(mltext("logmsg.meanweight.szcl")))

  refespTmp <- as.matrix(refesp[ , c("mean.weight.small", "mean.weight.medium", "mean.weight.large")])
  row.names(refespTmp) <- as.character(refesp[ , vars["sp"]] )

  classID <- c("P" = 1, "M" = 2, "G" = 3)

  res <- sapply(seq(length.out = nrow(Data)),
    function(i){
      ifelse(# Si l'espèce est dans le référentiel espèce...
        is.element(Data[i , vars["sp"]], row.names(refespTmp)),
        # ...poids moyen correspondant à l'espèce et la classe de taille :
        refespTmp[as.character(Data[i , vars["sp"]]),
        classID[as.character(Data[i , vars["szcl"]])]],
        # Sinon rien :
        NA)
  })

  return(res)
}

