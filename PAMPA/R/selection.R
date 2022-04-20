#' Cette fonction permet de faire la selection et le recalcul des donnees.
#'
#' Elle prend en entree le type de critere selon lequel le recalcul sera fait
#' (champs du referentiel espece ou champs des unitobs).
#'
#' Elle prend egalemnt en entree deux environnements, celui ou stocker les donnees,
#' et l'environnement parent.
#'
#' Elle va ensuite faire la selection des donnees, en demandant a l'utilisateur
#' selon quel(s) critere(s) du champs choisi il veut la faire.
#'
#' Elle ne retourne rien, les donnees sont stockees dans l'environnement .dataEnv
#' (pas forcement tres pratique, a changer plus tard ?)
#'
#' A changer plus tard pour supprimer les lignes de code en doublon dans \code{selectionOnRefesp.f}
#' (et dependances) et \code{selectionOnUnitobs.f} (et dependances) ?


#' @importFrom svDialogs dlg_list
#' @import tcltk
#' @import R.utils


#' @title Selection des donnees
#'
#' @description Selection des donnees selon un champs du referentiel espece ou un
#' champs des unitobs.
#'
#' @param field : chr, unitobs ou refesp, type de champs choisi pour la selection.
#' @param baseEnv : environnement parent
#' @param dataEnv : environnement de stockage des donnees
#'
#' @return list, liste contenant les tables de donnees selon la selection.
#'  \itemize{unitobs}{ : chr, fichier unitobs}
#'  \itemize{obs}{ : chr, fichier d'observation}
#'  \itemize{refesp}{ : chr, referentiel especes}
#'
#' @examples
#' ws_path <- system.file('example_data/COTE BLEUE/Data/', package = 'PAMPA')
#' unitobs_path <- system.file('example_data/COTE BLEUE/Data/UnitObs_Staviro_CB191110_280720.txt', package = 'PAMPA')
#' obs_path <- system.file('example_data/COTE BLEUE/Data/Obs_Staviro_CB191110_AllIdentifiedSpecies_280720.txt', package = 'PAMPA')
#' refesp_path <- system.file('example_data/COTE BLEUE/Data/refEspecesMed_general_270720 - Int peche est intCH_CB.txt', package = 'PAMPA')
#'
#' filePathes <- c(unitobs = unitobs_path, obs = obs_path, refesp =  refesp_path, refspa = NULL, ws = ws_path)
#'
#' data <- load_and_calc.f(filePathes, .dataEnv, .baseEnv)
#'
#' selection.f('unitobs', .baseEnv, .dataEnv)
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

selectionOnRefesp.f <- function(dataEnv, baseEnv){

  ## Purpose: Sélection des données (dans les observations) selon un
  ##          critère du référentiel espèces.
  ## ---------------------------------------------------------------------------
  ## Arguments: baseEnv : environnement de l'interface principale.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 janv. 2012, 14:54

#  on.exit(winRaise.f(get("W.main", envir = baseEnv)))

  runLog.f(msg = c(mltext("logmsg.selectionOnRefesp")))

  # Récupération des données :
  obs <- get("obs", envir = dataEnv)
  unitobs <- get("unitobs", envir = dataEnv)
  refesp <- get("refesp", envir = dataEnv)

#  filePathes <- get("filePathes", envir = dataEnv)

  if (exists(".NombresSVR", envir = dataEnv)){ # SVR !
    .NombresSVR <- get(".NombresSVR", envir = dataEnv)

    .DensitesSVR <- get(".DensitesSVR", envir = dataEnv)
  }

  if (exists("unitSpSz", envir = dataEnv)){
    unitSpSz <- get("unitSpSz", envir = dataEnv)
  }

  unitSp <- get("unitSp", envir = dataEnv)

#  ## Objets tcltk :
#  W.main <- get("W.main", envir = baseEnv)

  # Sélection des observations :
  selection <- selectionEsp.f(refesp = refesp, obs = obs)

#  infoGeneral.f(msg = mltext("selectionOnRefesp.info.1"),
#    waitCursor = TRUE,
#    font = tcltk::tkfont.create(weight = "bold", size = 9), foreground = "darkred")

  print(mltext("selectionOnRefesp.info.1"))

  if (!is.null(selection)){
    # assign("obs", obs <- selection[["obs"]], envir = .GlobalEnv)
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

#    # Information de l'utilisateur :
#    infoLoading.f(msg = paste(
#      mltext("selectionOnRef.info.2"),
#      mltext("selectionOnRefesp.info.3"),
#      sep = ""), icon = "info", font = tcltk::tkfont.create(weight = "bold", size = 9))
    print(paste(
      mltext("selectionOnRef.info.2"),
      mltext("selectionOnRefesp.info.3"),
      sep = ""))


#    # Recréation des tables de calcul :
#    # creationTablesCalcul.f()
#    updateInterface.select.f(criterion = paste(selection[["facteur"]], ":",
#      paste(selection[["selection"]], collapse = ", ")),
#      tabObs = obs,
#      baseEnv = baseEnv)

#    # Ajout d'info dans le log de sélection :
#    add.logFrame.f(msgID = "selection", env  =  baseEnv,
#      facteur = selection[["facteur"]], selection = selection[["selection"]],
#      results = filePathes["results"], referentiel = "unitobs",
#      has.SzCl = ( ! is.null(unitSpSz) && prod(dim(unitSpSz))))

#    gestionMSGaide.f("etapeselected", env = baseEnv)

#    infoLoading.f(button = TRUE, WinRaise = W.main)
  }else{
#    infoLoading.f(msg = mltext("selectionOnRef.info.4"))
#    infoLoading.f(button = TRUE, WinRaise = W.main)
    print(mltext("selectionOnRef.info.4"))
  }

  return(list(obs = obs, unitobs = unitobs, refesp = refesp))
}


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
    # assign("selectfactSp", selectfactSp, envir = .GlobalEnv)
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

#  Done <- tcltk::tclVar("0")                 # Variable de statut d'exécution.
#
#  W.selRef <- tcltk::tktoplevel()
#  tcltk::tkwm.title(W.selRef, mltext("chooseRefespField.title"))
#
#  # Ascenceur :
#  SCR <- tcltk::tkscrollbar(W.selRef, repeatinterval = 5,
#    command = function(...)tcltk::tkyview(LI.fields, ...))
#
#  LI.fields <- tcltk::tklistbox(W.selRef, height = 20, width = 50, selectmode = "single",
#    yscrollcommand = function(...)tcltk::tkset(SCR, ...),
#    background = "white")
#
#  # Placement des éléments :
#  tcltk::tkgrid(tcltk::tklabel(W.selRef, text = mltext("chooseRefespField.LB.1")))
#
#  tcltk::tkgrid(LI.fields, SCR)
#  tcltk::tkgrid.configure(SCR, rowspan = 4, sticky = "ensw")
#  tcltk::tkgrid.configure(LI.fields, rowspan = 4, sticky = "ensw")

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
#  # Frame pour les boutons :
#  F.button <- tcltk::tkframe(W.selRef)
#
#  # Bouton OK :
#  B.OK <- tcltk::tkbutton(F.button, text = mltext("OK.button"),
#    command = function(){
#      assign("factesp",
#        facts[as.numeric(tcltk::tkcurselection(LI.fields))+1],
#        parent.env(environment()))
#
#      tcltk::tclvalue(Done) <- 1
#  })
#
#  # Bouton d'annulation :
#  B.Cancel <- tcltk::tkbutton(F.button, text = mltext("Cancel.button"),
#    command = function(){tcltk::tclvalue(Done) <- 2})
#
#  tcltk::tkgrid(B.OK, tcltk::tklabel(F.button, text = "\t"), B.Cancel, padx = 10)
#  tcltk::tkgrid(F.button, pady = 5, ## sticky = "we",
#    columnspan = 2)
#
#  tcltk::tkbind(W.selRef, "<Destroy>", function(){tcltk::tclvalue(Done) <- 2})
#
#  winSmartPlace.f(W.selRef)
#  tcltk::tkfocus(LI.fields)
#
#  # Attente d'une action de l'utilisateur :
#  tcltk::tkwait.variable(Done)

  # On retourne la sélection :
  if (exists("factesp") && length(factesp)){
    return(factesp)
  }else{
    return(NULL)
  }
}


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

  #  # ########## Définition des éléments graphiques ##########
  #  winfac <- tcltk::tktoplevel()   # (width = 80)
  #
  #  if (is.null(title)){
  #    tcltk::tkwm.title(winfac, paste(mltext("selectModWindow.f.title"), aliases(champ), sep = ""))
  #  }else{
  #    tcltk::tkwm.title(winfac, title)
  #  }
  #
  #  # Assenceur vertical :
  #  SCR.y <- tcltk::tkscrollbar(winfac, repeatinterval = 5, command = function(...){tcltk::tkyview(LB, ...)})
  #
  #  # List Box de sélection :
  #  LB <- tcltk::tklistbox(winfac, height = 20, width = 50, selectmode = selectmode,
  #    yscrollcommand = function(...)tcltk::tkset(SCR.y, ...), background = "white")
  #
  #  # Boutons :
  #  FrameB <- tcltk::tkframe(winfac)
  #  B.OK <- tcltk::tkbutton(FrameB, text = mltext("OK.button"), command = function(){
  #    assign("selection", listMod[as.numeric(tcltk::tkcurselection(LB))+1], parent.env(environment()))
  #    # assign("tmptk", tcltk::tkcurselection(LB), envir = .GlobalEnv)
  #    tcltk::tkdestroy(winfac)
  #  })
  #  B.Cancel <- tcltk::tkbutton(FrameB, text = mltext("Cancel.button"), command = function(){
  #    assign("selection", NULL, parent.env(environment()))
  #    tcltk::tkdestroy(winfac)
  #  })

  #  # ########## Placement des éléments sur la grille ##########
  #  # Explications :
  #  if (is.null(label)){
  #    tcltk::tkgrid(tcltk::tklabel(winfac, text = paste(mltext("selectModWindow.f.CB.1"), aliases(champ),
  #      mltext("selectModWindow.f.CB.2"),
  #      sep = "")), columnspan = 2)
  #  }else{
  #    tcltk::tkgrid(tcltk::tklabel(winfac, text = label), columnspan = 2)
  #  }
  #
  #  # Avertissement 'plusieurs sélections possibles' :
  #  if (is.element(selectmode, c("extended", "multiple"))){
  #    tcltk::tkgrid(tcltk::tklabel(winfac, text = mltext("selectModWindow.f.CB.3")), columnspan = 2)
  #  }else{}
  #
  #  # Avertissement mode de sélection étendu :
  #  if (selectmode == "extended"){
  #    tcltk::tkgrid(tcltk::tklabel(winfac,
  #      text = paste(mltext("selectModWindow.f.info.1"),
  #        mltext("selectModWindow.f.info.2"),
  #        mltext("selectModWindow.f.info.3"), sep = ""),
  #        fg = "red"), columnspan = 2, rowspan = 2)
  #  }else{}
  #
  #  tcltk::tkgrid(LB, SCR.y)
  #  tcltk::tkgrid.configure(SCR.y, rowspan = 4, sticky = "nsw")
  #  tcltk::tkgrid(FrameB, columnspan = 2, sticky = "")
  #  tcltk::tkgrid(B.OK, tcltk::tklabel(FrameB, text = "        "), B.Cancel, sticky = "", pady = 5)

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
  #  invisible(sapply(listMod, function(x){tcltk::tkinsert(LB, "end", x)}))
  #
  #  # Sélections persistantes :
  #  if (!is.null(preselect)){
  #    sapply(which(is.element(listMod, preselect)) - 1,
  #           function(i){tcltk::tkselection.set(LB, i)})
  #  }
  #  # tcltk::tkselection.set(LB, 0)
  #
  #  tcltk::tkbind(winfac, "<Control-a>",       # Tout sélectionner
  #    function(){
  #      sapply(seq(from = 0, length.out = length(listMod)),
  #        function(i) {tcltk::tkselection.set(LB, i)})
  #    })
  #
  #  # Affichage/attente :
  #  tcltk::tkfocus(LB)
  #
  #  tcl("update")
  #  winSmartPlace.f(winfac, xoffset = 50, yoffset = -100)
  #
  #  tcltk::tkwait.window(winfac)
  if (exists("selection") && length(selection)){
    return(selection)
  }else{
    return(NULL)
  }
}

# listInEnv.f <- function(list, env){
#
#  ## Purpose: Copie les éléments d'une liste (nommée) dans un environnement
#  ##          (avec comme nom d'élément son nom dans la liste).
#  ## ---------------------------------------------------------------------------
#  ## Arguments: list : la liste à copier.
#  ##            env : l'environnement dans lequel enregistrer les
#  ##                  éléments.
#  ## ---------------------------------------------------------------------------
#  ## Author: Yves Reecht, Date:  4 janv. 2012, 15:38
#
#  if (is.null(names(list))){
#    listNames <- paste("obj", seq(length.out = length(list)), sep = "")
#
#    warning("Unnamed list: the elements have been named \"obj1\", \"obj2\", etc.")
#  }else{
#    listNames <- names(list)
#  }
#
#  invisible(sapply(list,
#    function(x, xN, env){
#      # Numéro d'itération :
#      i <- sys.call()[[2]][[3]]
#
#      if (is.symbol(x = i)){ # for compatibility R > 3.1
#        i <- eval(i, sys.frame(-1))
#      }
#      # Assignement :
#      assign(x = xN[i], value = x, envir = env)
#    },
#    xN = listNames, env = env))
#}


#PlanEchantillonnageBasic.f <- function(tabUnitobs, tabObs, filePathes){
#
#  ## Purpose: Écrire le plan d'échantillonnage basic dans un fichier.
#  ## ---------------------------------------------------------------------------
#  ## Arguments: tabUnitobs : table des unités d'observation (data.frame).
#  ##            tabObs : table des observations (data.frame).
#  ## ---------------------------------------------------------------------------
#  ## Author: Yves Reecht, Date: 21 sept. 2011, 13:53
#
#  PlanEchantillonnage <- with(
#    dropLevels.f(tabUnitobs[is.element(tabUnitobs$observation.unit,
#      levels(tabObs$observation.unit)), ]),
#    table(year, protection.status, exclude = NA))
#
#  attr(PlanEchantillonnage, "class") <- "array" # Pour un affichage en "tableau".
#
#  write.csv(PlanEchantillonnage,
#    file = paste(filePathes["results"],
#      "PlanEchantillonnage_basique", # [ml?]
#      ifelse(getOption("P.selection"), "_selection", ""),
#      ".csv", sep = ""), row.names = TRUE)
#}


#exportMetrics.f <- function(unitSpSz, unitSp, unit, obs, unitobs, refesp, filePathes, baseEnv){
#
#  ## Purpose: Exporter
#  ##            * les tables de métriques avec des colonnes supplémentaires
#  ##              dans l'environnement global + les sauvegarder dans des
#  ##              fichiers.
#  ##            * exporter les tables de données dans l'environnement
#  ##              global.
#  ##          Les noms utilisés dans l'environnement global ne doivent
#  ##          pas être les noms internes pour éviter des bugs
#  ##          in-déboguables.
#  ## ---------------------------------------------------------------------------
#  ## Arguments: unitSpSz : table de métriques /CT/esp/unitobs.
#  ##            unitSp : table de métriques /esp/unitobs.
#  ##            unit : table de métriques /unitobs.
#  ##            obs : table des données d'observation.
#  ##            unitobs : référentiel des unités d'observation.
#  ##            refesp : référentiel espèces.
#  ##            filePathes : chemins des fichiers/dossiers.
#  ##            baseEnv : environnement de l'interface principale.
#  ##
#  ## Output: Rien !
#  ## ---------------------------------------------------------------------------
#  ## Author: Yves Reecht, Date:  4 janv. 2012, 20:08
#
#  PlanEchantillonnageBasic.f(tabUnitobs = unitobs, tabObs = obs, filePathes = filePathes)
#
#  # Certaines métriques (densités) sont ramenées à /100m² (avec ajout des colonnes par défaut) :
#  unitSpSz <- scaleMetrics.f(Data = unitSpSz, unitobs = unitobs, refesp = refesp, scale = TRUE)
#  unitSp <- scaleMetrics.f(Data = unitSp, unitobs = unitobs, refesp = refesp, scale = TRUE)
#  unit <- scaleMetrics.f(Data = unit, unitobs = unitobs, refesp = refesp, scale = TRUE)
#
#  # Export des principales tables dans l'environnement global :
#  assign("TableUnitSpSz", unitSpSz, envir = .GlobalEnv)
#  assign("TableUnitSp", unitSp, envir = .GlobalEnv)
#  assign("TableUnit", unit, envir = .GlobalEnv)
#  assign("DataObs", obs, envir = .GlobalEnv)
#  assign("DataUnitobs", unitobs, envir = .GlobalEnv)
#  assign("DataRefesp", refesp, envir = .GlobalEnv)
#
#  # Sauvegardes dans des fichiers :
#  if ( ! is.null(unitSpSz) &&
#       prod(dim(unitSpSz))){            # i.e. nrow et ncol > 0.
#    # Table unitSpSz si elle existe :
#    tryCatch(write.csv(unitSpSz,
#      file = (fileNm <- paste(filePathes["results"],
#        "UnitobsEspeceClassetailleMetriques",
#        ifelse(getOption("P.selection"), "_selection", ""),
#        ".csv", sep = "")),
#      row.names = FALSE),
#      error = function(e){
#        print(paste("Impossible d'écrire le fichier ", fileNm,
#          ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""))
#    })
#  }else{}                            # Sinon rien !
#
#  # Table unitSp :
#  tryCatch(write.csv(unitSp,
#    file = (fileNm <- paste(filePathes["results"],
#      "UnitobsEspeceMetriques",
#      ifelse(getOption("P.selection"), "_selection", ""),
#      ".csv", sep = "")),
#    row.names = FALSE),
#    error = function(e){
#      print(paste("Impossible d'écrire le fichier ", fileNm,
#        ".\nIl est possible qu'il soit ouvert par une autre application", sep = ""))
#    })
#
#  # Table unit :
#  tryCatch(write.csv(unit,
#    file = (fileNm <- paste(filePathes["results"],
#      "UnitobsMetriques",
#      ifelse(getOption("P.selection"), "_selection", ""),
#      ".csv", sep = "")),
#    row.names = FALSE),
#    error = function(e){
#      print(paste("Impossible d'écrire le fichier ", fileNm,
#        "\nIl est possible qu'il soit ouvert par une autre application", sep = ""))
#    })
#}


#scaleMetrics.f <- function(Data, unitobs, refesp,
#  supl = c("year", "site", "protection.status", "biotop", "latitude", "longitude",
#    "annee.campagne", "habitat1", "habitat2", "habitat3",
#    "scient.name", "family", "genus", "species"),
#  scale = TRUE){
#
#  ## Purpose:
#  ## ---------------------------------------------------------------------------
#  ## Arguments:
#  ## ---------------------------------------------------------------------------
#
#  if ( ! is.null(Data) &&
#       prod(dim(Data))){                # i.e. ncol et nrow > 0.
#    # Champs à ajouter par référentiel :
#    suplUnitobs <- supl[is.element(supl, colnames(unitobs)) &
#      ! is.element(supl, colnames(Data))]
#    suplRefesp <- supl[is.element(supl, colnames(refesp)) &
#      ! is.element(supl, colnames(Data))]
#
#    # Ajout des champs supplémentaires des unitobs :
#    if (length(suplUnitobs)){
#      Data <- merge(Data,
#        unitobs[ , unique(c("observation.unit", suplUnitobs)), drop = FALSE],
#        by = c("observation.unit"))
#    }else{}
#
#    # Ajout des champs supplémentaires du référentiel espèces :
#    if (length(suplRefesp) && is.element("species.code", colnames(Data))){
#      Data <- merge(Data,
#        refesp[ , unique(c("species.code", suplRefesp)), drop = FALSE],
#        by = c("species.code"))
#    }else{}
#
#    # Scalling : certaines métriques (densités) doivent être ramenées à /100m² :
#    if (scale &&
#      any(is.element(colnames(Data),
#        colTmp <- c("density", "density.max", "density.sd",
#          "biomass", "biomass.max", "biomass.sd")))){
#            Data[ , is.element(colnames(Data),
#              colTmp)] <- sweep(Data[ , is.element(colnames(Data), colTmp),
#              drop = FALSE],
#              2, 100, "*")
#    }else{}
#  }else{}
#
#  return(Data)
#}


selectionOnUnitobs.f <- function(dataEnv, baseEnv){

  ## Purpose: Sélection des données (dans les observations) selon un
  ##          critère du référentiel d'unités d'observation.
  ## ---------------------------------------------------------------------------
  ## Arguments: baseEnv : environnement de l'interface principale.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  5 janv. 2012, 21:02

#  on.exit(winRaise.f(get("W.main", envir = baseEnv)))

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

#  ## Objets tcltk :
#  W.main <- get("W.main", envir = baseEnv)

  selection <- selectionUnitobs.f(unitobs = unitobs, obs = obs)

#  infoGeneral.f(msg = mltext("selectionOnUnitobs.info.1"),
#    waitCursor = TRUE,
#    font = tcltk::tkfont.create(weight = "bold", size = 9), foreground = "darkred")

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
#    infoLoading.f(msg = paste(
#      mltext("selectionOnRef.info.2"),
#      mltext("selectionOnUnitobs.info.3"),
#      sep = ""), icon = "info", font = tcltk::tkfont.create(weight = "bold", size = 9))
    print(paste(
      mltext("selectionOnRef.info.2"),
      mltext("selectionOnUnitobs.info.3"),
      sep = ""))

#    updateInterface.select.f(criterion = paste(selection[["facteur"]], ":",
#      paste(selection[["selection"]], collapse = ", ")),
#      tabObs = obs, baseEnv = baseEnv)

#    # Ajout d'info dans le log de sélection :
#    add.logFrame.f(msgID = "selection", env = baseEnv,
#      facteur = selection[["facteur"]], selection = selection[["selection"]],
#      results = filePathes["results"], referentiel = "unitobs",
#      has.SzCl = ( ! is.null(unitSpSz) && prod(dim(unitSpSz))))

#    gestionMSGaide.f("etapeselected", env = baseEnv)

#    infoLoading.f(button = TRUE, WinRaise = W.main)
  }else{
#    infoLoading.f(msg = mltext("selectionOnRef.info.4"))
#    infoLoading.f(button = TRUE, WinRaise = W.main)
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


chooseUnitobsField.f <- function(unitobs, obs){

  ## Purpose:
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## ---------------------------------------------------------------------------

  runLog.f(msg = c("Choix d'un Facteur dans le référentiel des unités d'observation :"))

#  Done <- tcltk::tclVar("0")                 # Variable de statut d'exécution.
#
#  W.select <- tcltk::tktoplevel()
#  tcltk::tkwm.title(W.select, "Sélection du facteur de groupement des unités d'observation")
#
#  SCR <- tcltk::tkscrollbar(W.select, repeatinterval = 5,
#                     command = function(...)tcltk::tkyview(LI.fields, ...))
#
#  LI.fields <- tcltk::tklistbox(W.select, height = 20, width = 50, selectmode = "single",
#                         yscrollcommand = function(...)tcltk::tkset(SCR, ...),
#                         background = "white")
#
#  tcltk::tkgrid(tcltk::tklabel(W.select, text = "Liste des facteurs de groupement"))
#  tcltk::tkgrid(LI.fields, SCR)
#  tcltk::tkgrid.configure(SCR, rowspan = 4, sticky = "ensw")
#  tcltk::tkgrid.configure(LI.fields, rowspan = 4, sticky = "ensw")

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

#  # Frame pour les boutons :
#  F.button <- tcltk::tkframe(W.select)
#
#  # Bouton OK :
#  B.OK <- tcltk::tkbutton(F.button, text = "OK",
#                   command = function()
#                   {
#                     assign("factunitobs",
#                            facts[as.numeric(tcltk::tkcurselection(LI.fields))+1],
#                            parent.env(environment()))
#
#                     tcltk::tclvalue(Done) <- 1
#                   })
#
#  # Bouton d'annulation :
#  B.Cancel <- tcltk::tkbutton(F.button, text = "Annuler",
#                       command = function(){tcltk::tclvalue(Done) <- 2})
#
#  tcltk::tkgrid(B.OK, tcltk::tklabel(F.button, text = "\t"), B.Cancel, padx = 10)
#  tcltk::tkgrid(F.button, pady = 5, ## sticky = "we",
#         columnspan = 2)
#
#  tcltk::tkbind(W.select, "<Destroy>", function(){tcltk::tclvalue(Done) <- 2})
#
#  winSmartPlace.f(W.select)
#  tcltk::tkfocus(LI.fields)
#
#  # Attente d'une action de l'utilisateur :
#  tcltk::tkwait.variable(Done)

  # On retourne la sélection :
  if (exists("factunitobs") && length(factunitobs)){
    return(factunitobs)
  }else{
    return(NULL)
  }
}
