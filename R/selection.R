#' Cette fonction permet de faire la selection et le recalcul des donnees.
#'
#' Elle prend en entree le type de critere selon lequel le recalcul sera fait
#' (champs du referentiel espece ou champs des unitobs).
#'
#' Elle prend egalement en entree deux environnements, celui de stockage des donnees
#' et l'environnement parent.
#'
#' Elle va ensuite faire la selection des donnees, en demandant a l'utilisateur
#' selon quel(s) critere(s) du champs choisi il veut la faire.
#'
#' Elle retourne la table des observations modifiee, celle des uniotobs, et celle du referentiel
#' spatial.


#' @title Selection des donnees
#'
#' @description Selection des donnees selon un champs du referentiel espece ou un
#' champs des unitobs.
#'
#' @param field : chr, unitobs ou refesp, type de champs choisi pour la selection.
#' @param dataEnv : environnement de stockage des donnees
#' @param baseEnv : environnement parent
#'
#' @return list, liste contenant les tables de donnees selon la selection.
#'  \itemize{unitobs}{ : chr, fichier unitobs}
#'  \itemize{obs}{ : chr, fichier d'observation}
#'  \itemize{refesp}{ : chr, referentiel especes}
#'
#' @examples
#' ws_path <- 'inst/example_data/'
#' unitobs_path <- 'inst/example_data/Data/UnitObs_Staviro_Example.txt'
#' obs_path <- 'inst/example_data/Data/Obs_Staviro_Example.txt'
#' refesp_path <- 'inst/example_data/Data/SpeciesReferenceTable_Example.txt'
#'
#' filePathes <- c(unitobs = unitobs_path, obs = obs_path, refesp =  refesp_path, ws = ws_path)
#'
#' data <- PAMPA::load_files.f(filePathes, dminMax = 5, .dataEnv, .baseEnv)
#'
#' data_sel <- PAMPA::selection.f('unitobs', .dataEnv, .baseEnv)
#'
#' @export

selection.f <- function(field, dataEnv, baseEnv){
  if(field == "refesp"){
    return(selectionOnRefesp.f(dataEnv, baseEnv))
  }
  else if(field == "unitobs"){
    return(selectionOnUnitobs.f(dataEnv, baseEnv))
  }
  else{
    print("Veuillez choisir le type de champs pour la sélection")
    print("'unitobs' ou 'refesp' uniquement !")
  }
}

#' @importFrom R.utils extract

selectionOnRefesp.f <- function(dataEnv, baseEnv){

  ## Purpose: Sélection des données (dans les observations) selon un
  ##          critère du référentiel espèces.
  ## ---------------------------------------------------------------------------
  ## Arguments: baseEnv : environnement de l'interface principale.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 14:54

  runLog.f(msg = c(mltext("logmsg.selectionOnRefesp")))

  # Récupération des données :
  obs <- get("obs", envir = dataEnv)
  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  filePathes <- get("filePathes", envir = dataEnv)

  if (exists(".NombresSVR", envir = dataEnv)){ # SVR !
    .NombresSVR <- get(".NombresSVR", envir = dataEnv)

    .DensitesSVR <- get(".DensitesSVR", envir = dataEnv)
  }

  if (exists("unitSpSz", envir = dataEnv)){
    unitSpSz <- get("unitSpSz", envir = dataEnv)
  }

  unitSp <- get("unitSp", envir = dataEnv)

  # Sélection des observations :
  selection <- selectionEsp.f(refesp = refesp, obs = obs)

  print(mltext("selectionOnRefesp.info.1"))

  if (!is.null(selection)){
    obs <- selection[["obs"]]

    keptEspeces <- as.character(refesp[is.element(refesp[ , selection[["facteur"]]],
      selection[["selection"]]),
      "species.code"])

    # Réduction des tables de données (aux espèces sélectionnées) :
    if (exists(".NombresSVR")){
      species <- dimnames(.NombresSVR)[["species.code"]]

      .NombresSVR <- R.utils::extract(.NombresSVR,
        indices = list(species[is.element(species, keptEspeces)]),
        dims = which(is.element(names(dimnames(.NombresSVR)), "species.code")))

      .DensitesSVR <- R.utils::extract(.DensitesSVR,
        indices = list(species[is.element(species, keptEspeces)]),
        dims = which(is.element(names(dimnames(.DensitesSVR)), "species.code")))
    }else{}

    if (exists("unitSpSz") && ncol(unitSpSz)){ # [!!!]  [yr: 4/1/2012]
      unitSpSz <- dropLevels.f(unitSpSz[is.element(unitSpSz[ , "species.code"],
        keptEspeces), , drop = FALSE],
        which = "species.code")

    }else{
      if ( ! exists("unitSpSz")) unitSpSz <- NULL
    }

    unitSp <- dropLevels.f(unitSp[is.element(unitSp[ , "species.code"],
      keptEspeces), , drop = FALSE],
      which = "species.code")

    # Recalcul des indices de biodiversité :
    unit <- calc.unit.f(unitSp = unitSp, obs = obs, refesp = refesp,
      unitobs = unitobs, dataEnv = dataEnv)

    # Sauvegarde des données recalculées dans l'environnement adéquat :
    if (exists(".NombresSVR")){
      listInEnv.f(list("obs" = obs,
        "unitSpSz" = unitSpSz,
        "unitSp" = unitSp,
        "unit" = unit,
        ".NombresSVR" = .NombresSVR,
        ".DensitesSVR" = .DensitesSVR),
        env = dataEnv)
    }else{
      listInEnv.f(list("obs" = obs,
        "unitSpSz" = unitSpSz,
        "unitSp" = unitSp,
        "unit" = unit),
        env = dataEnv)
    }

    # Plan d'échantillonnage basic :
    PlanEchantillonnageBasic.f(tabUnitobs = unitobs, tabObs = obs, filePathes = filePathes)

    # Export des tables (.GlobalEnv & fichiers):
    exportMetrics.f(unitSpSz = unitSpSz, unitSp = unitSp, unit = unit,
      obs = obs, unitobs = unitobs, refesp = refesp,
      filePathes = filePathes, baseEnv = baseEnv)

    print(paste(
      mltext("selectionOnRef.info.2"),
      mltext("selectionOnRefesp.info.3"),
      sep = ""))

  }else{
    print(mltext("selectionOnRef.info.4"))
  }

  return(list(obs = obs, unitobs = unitobs, refesp = refesp))
}

#' @importFrom svDialogs dlg_list

selectionEsp.f <- function(refesp, obs){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 17:38

  runLog.f(msg = c(mltext("logmsg.selectionEsp")))

  factSp <- chooseRefespField.f(refesp = refesp, obs = obs)

  if (length(factSp) == 0 || is.null(factSp)){
    selectfactSp <- NULL
  }else{
    obs[, factSp] <- refesp[match(obs[ , "species.code"],
      refesp[ , "species.code"]),
      factSp]

    levelsTmp <- levels(obs[ , "species.code"])

    selectfactSp <- selectModWindow.f(factSp, obs, selectmode = "extended")
  }

  if (!is.null(selectfactSp)){
    obs <- dropLevels.f(subset(obs, is.element(obs[, factSp], selectfactSp)),
      which = "species.code")

    # Réintégration des niveaux sélectionnés mais plus présents dans les données :
    levelsTmp <- levelsTmp[is.element(levelsTmp,
      refesp[is.element(refesp[ , factSp],
        selectfactSp) ,
        "species.code"])]

    obs[ , "species.code"] <- factor(obs[ , "species.code"], levels = levelsTmp)



    # On définit globalement que l'on travaille sur une sélection :
    options(P.selection = TRUE)

    return(list(facteur = factSp,
      selection = selectfactSp,
      obs = obs))
  }else{}
}



chooseRefespField.f <- function(refesp, obs){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments: refesp : référentiel espèces.
  ##            obs : la table des observations.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 17:47

  runLog.f(msg = c(mltext("logmsg.chooseRefespField")))

  # Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
  esptmp <- refesp[is.element(refesp[ , "species.code"],
    obs[ , "species.code"]), ] # sélection des lignes correspondant aux obs.
  esptmp <- esptmp[ , sapply(esptmp, function(x){
    !all(is.na(x)) # sélection des champs qui contiennent autre chose qu'uniquement des NAs.
  })]

  facts <- sort(names(esptmp))

  # ici, on liste les AMP qui ne correspondent pas au jeu de données :
  listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC") # [!!!] not very generic
  listeSiteExclus <- subset(listeSite,
    ! is.element(listeSite, getOption("P.MPA")))

  # On retire les champs contenant les lettres des sites exclus :
  for (k in (seq(along = listeSiteExclus))){ # On peut faire plus simple [yr: 03/08/2010]
    facts <- facts[ ! grepl(paste(listeSiteExclus[k], "$", sep = ""),
      facts)]
  }

  liste_choix <- c()
  # Ajout des facteur dans la liste :
  for (i in seq(along = facts)){
    # tcltk::tkinsert(LI.fields, "end", facts[i])
    liste_choix <- c(liste_choix, facts[i])
  }

  factesp <- svDialogs::dlg_list(liste_choix, multiple = FALSE,
    title = mltext("chooseRefespField.title"))$res

  # On retourne la sélection :
  if (exists("factesp") && length(factesp)){
    return(factesp)
  }else{
    return(NULL)
  }
}

#' @importFrom svDialogs dlg_list

selectModWindow.f <- function(champ, data, selectmode = "multiple", sort = TRUE,
  preselect = NULL, title = NULL, label = NULL){

  ## Purpose: Ouvre une fenêtre pour le choix des modalités d'un facteur
  ## ---------------------------------------------------------------------------
  ## Arguments: champ : le nom de la colonne du facteur
  ##            data : la table de données
  ##            selectmode : mode de sélection (parmi "single" et
  ##                         "multiple")
  ##            sort : ordonner les modalités ? (booléen)
  ##            preselect : un vecteur de modalités à présélectionner (pour
  ##                        des sélections persistantes).
  ##            title : titre de la fenêtre.
  ##            label : texte d'explication.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 août 2010, 09:38

  if(champ == ""){                     # condition d'arrêt (pas de champ sélectionné).
    return(NULL)
  }else{
    if (all(is.na(data[ , champ]))){   # Cas des champs vides (ajouter un message).
      return(NULL)
    }
  }
  selection <- NULL

  # Configuration de la liste :
  if (sort){
    listMod <- unique(as.character(sort(data[ , champ])))
  }else{
    listMod <- unique(as.character(data[ , champ]))
  }

  liste_choix <- c()
  for (i in seq(along = listMod)){
    liste_choix <- c(liste_choix, listMod[i])
  }

  selection <- svDialogs::dlg_list(liste_choix, multiple = TRUE,
    title = paste(mltext("selectModWindow.f.title"), aliases(champ), sep = ""))$res
  if (exists("selection") && length(selection)){
    return(selection)
  }else{
    return(NULL)
  }
}


#' @importFrom R.utils extract

selectionOnUnitobs.f <- function(dataEnv, baseEnv){

  ## Purpose: Sélection des données (dans les observations) selon un
  ##          critère du référentiel d'unités d'observation.
  ## ---------------------------------------------------------------------------
  ## Arguments: baseEnv : environnement de l'interface principale.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 janv. 2012, 21:02

  runLog.f(msg = c(mltext("logmsg.selectionOnUnitobs")))

  # Récupération des données :
  obs <- get("obs", envir = dataEnv)
  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

  filePathes <- get("filePathes", envir = dataEnv)

  if (exists(".NombresSVR", envir = dataEnv)){ # SVR !
    .NombresSVR <- get(".NombresSVR", envir = dataEnv)

    .DensitesSVR <- get(".DensitesSVR", envir = dataEnv)
  }

  # ...et des tables de métriques :
  if (exists("unitSpSz", envir = dataEnv)){
    unitSpSz <- get("unitSpSz", envir = dataEnv)
  }

  unitSp <- get("unitSp", envir = dataEnv)
  unit <- get("unit", envir = dataEnv)

  selection <- selectionUnitobs.f(unitobs = unitobs, obs = obs)

  if (!is.null(selection)){
    obs <- selection[["obs"]]

    keptUnitobs <- as.character(unitobs[is.element(unitobs[ , selection[["facteur"]]],
      selection[["selection"]]),
      "observation.unit"])

    # Réduction des tables de données (aux unitobs sélectionnées) :
    if (exists(".NombresSVR")){
      unitObs <- dimnames(.NombresSVR)[["observation.unit"]]

      .NombresSVR <- R.utils::extract(.NombresSVR,
        indices = list(unitObs[is.element(unitObs, keptUnitobs)]),
        dims = which(is.element(names(dimnames(.NombresSVR)), "observation.unit")))

      .DensitesSVR <- R.utils::extract(.DensitesSVR,
        indices = list(unitObs[is.element(unitObs, keptUnitobs)]),
        dims = which(is.element(names(dimnames(.DensitesSVR)), "observation.unit")))
    }else{}

    if (exists("unitSpSz") && ncol(unitSpSz)){
      unitSpSz <- dropLevels.f(unitSpSz[is.element(unitSpSz[ , "observation.unit"],
        keptUnitobs),
        , drop = FALSE],
        which = "observation.unit")
    }else{}

    unitSp <- dropLevels.f(unitSp[is.element(unitSp[ , "observation.unit"],
      keptUnitobs),
      , drop = FALSE],
       which = "observation.unit")

    unit <- dropLevels.f(unit[is.element(unit[ , "observation.unit"],
      keptUnitobs),
      , drop = FALSE],
      which = "observation.unit")

    # Sauvegarde des données recalculées dans l'environnement adéquat :
    if (exists(".NombresSVR")){
      listInEnv.f(list("obs" = obs,
        "unitSpSz" = unitSpSz,
        "unitSp" = unitSp,
        "unit" = unit,
        ".NombresSVR" = .NombresSVR,
        ".DensitesSVR" = .DensitesSVR),
        env = dataEnv)
    }else{
      listInEnv.f(list("obs" = obs,
        "unitSpSz" = unitSpSz,
        "unitSp" = unitSp,
        "unit" = unit),
        env = dataEnv)
    }

    # Plan d'échantillonnage basic :
    PlanEchantillonnageBasic.f(tabUnitobs = unitobs, tabObs = obs, filePathes = filePathes)

    # Export des tables (.GlobalEnv & fichiers):
    exportMetrics.f(unitSpSz = unitSpSz, unitSp = unitSp, unit = unit,
      obs = obs, unitobs = unitobs, refesp = refesp,
      filePathes = filePathes, baseEnv = baseEnv)

    # Information de l'utilisateur :
    print(paste(
      mltext("selectionOnRef.info.2"),
      mltext("selectionOnUnitobs.info.3"),
      sep = ""))

  }else{
    print(mltext("selectionOnRef.info.4"))
  }
  return(list(obs = obs, unitobs = unitobs, refesp = refesp))
}


selectionUnitobs.f <- function(unitobs, obs){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 janv. 2012, 21:11

  runLog.f(msg = c(mltext("logmsg.selectionUnitobs")))

  factunitobs <- chooseUnitobsField.f(unitobs = unitobs, obs = obs)

  if (length(factunitobs) == 0 || is.null(factunitobs)){
    selectfactunitobs <- NULL
  }else{
    obs[, factunitobs] <- unitobs[match(obs[ , "observation.unit"],
      unitobs[ , "observation.unit"]),
      factunitobs]

    levelsTmp <- levels(obs[ , "observation.unit"])

    selectfactunitobs <- selectModWindow.f(factunitobs, obs, selectmode = "extended")
  }

  if ( ! is.null(selectfactunitobs)){
    obs <- dropLevels.f(subset(obs,
      is.element(obs[, factunitobs],
        selectfactunitobs)),
      which = "observation.unit") # Vérifier si c'est correct [!!!]

    # Réintégration des niveaux sélectionnés mais plus présents dans les données :
    levelsTmp <- levelsTmp[is.element(levelsTmp,
      unitobs[is.element(unitobs[ , factunitobs],
        selectfactunitobs),
        "observation.unit"])]

    obs[ , "observation.unit"] <- factor(obs[ , "observation.unit"],
      levels = levelsTmp)

    # On définit globalement que l'on travaille sur une sélection :
    options(P.selection = TRUE)

    return(list(facteur = factunitobs,
      selection = selectfactunitobs,
      obs = obs))
  }else{}
}

#' @importFrom svDialogs dlg_list

chooseUnitobsField.f <- function(unitobs, obs){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------

  runLog.f(msg = c("Choix d'un Facteur dans le référentiel des unités d'observation :"))

  # Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
  # sélection des lignes correspondant aux obs :
  uobstmp <- unitobs[is.element(unitobs$observation.unit, obs$observation.unit), ]

  # sélection des champs qui contiennent autre chose qu'uniquement des NAs :
  uobstmp <- uobstmp[ , sapply(uobstmp, function(x){!all(is.na(x))})]

  facts <- sort(names(uobstmp))

  # On remplit la liste de choix :
  liste_choix <- c()
  for (i in (seq(along = facts))){
    liste_choix <- c(liste_choix, facts[i])
  }

  factunitobs <- svDialogs::dlg_list(liste_choix, multiple = FALSE,
    title = mltext("chooseUnitobsField.title"))$res

  # On retourne la sélection :
  if (exists("factunitobs") && length(factunitobs)){
    return(factunitobs)
  }else{
    return(NULL)
  }
}
