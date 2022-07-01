#' Fonction pour calculer des modeles lineaires.
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
#' \code{factAnaSel} peut prendre differentes valeurs en fonction de celle de \code{factAna}. Ce
#' parametre est facultatif. S'il est egal a NA, toutes les modalites du facteur seront
#' selectionnees.
#'
#' Il est deconseille de mettre plus de deux facteurs pour \code{listFact}, et il n'est pas possible
#' d'en mettre plus de trois.
#'
#' \code{listFactSel} peut prendre differentes valeurs en fonction de celle de \code{listFact}. Ce
#' parametre est facultatif. S'il est egal a NA, alors toutes les modalites du ou des facteur(s) de
#' regroupement selectionne(s) seront prises en compte.


#' @import tcltk


#' @title Modele lineaire
#'
#' @description Calcul de modeles lineaires selon les parametres fournis
#'
#' @param agregation chr, type d'agregation, par espece (espece) ou par groupe d'espece (unitobs)
#' @param metrique chr, metrique choisie
#' @param factAna chr, facteur de separation des graphiques
#' @param factAnaSel chr, selection de modalites du facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalite(s) selectionnee(s) pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param new_window bool, affichage des resultats dans une nouvelle fenetre ?
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @examples
#' lm.f(
#'   agregation = "espece",
#'   metrique = "density",
#'   factAna = "family",
#'   factAnaSel = "Acanthuridae",
#'   listFact = c("year", "protection.status"),
#'   listFactSel = NA,
#'   tableMetrique = "unitSp",
#'   new_window = TRUE,
#'   dataEnv = .dataEnv, baseEnv = .baseEnv)
#'
#' @export

lm.f <- function(agregation, metrique, factAna, factAnaSel = NA, listFact, listFactSel = NA,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  tableMetrique_possible <- c("unit", "unitSp", "unitSpSz")
  nextStep <- switch(agregation,
    "espece" = "modele_lineaire",
    "unitobs" = "modele_lineaire.unitobs",
    stop(
      "Veuillez choisir une valeur de 'agregation' parmi 'espece' ou 'unitobs' (groupe d'especes).",
      "Please, choose an agregation between 'espece' and 'unitobs'."
    )
  )
 if (is.null(factAna)){
   factAna <- ""
 }

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
  if (factAna != ""){
    if (agregation == 'espece'){
      factAna_possible_refesp <- spRefFields.aliases(site = getOption("P.MPA"), dataEnv = dataEnv,
        ordered = FALSE, tableMetrique = tableMetrique)
      factAna_possible_unitobs <- UnitobsFields.aliases(ordered = FALSE, dataEnv = dataEnv,
        tableMetrique = tableMetrique)

      if (!is.element(factAna, factAna_possible_refesp) & !is.element(factAna, factAna_possible_unitobs)){
        stop(
          paste("La valeur '", factAna, "' du paramètre 'factAna' n'est pas valide.\n", sep = ""),
          paste("Veuillez choisir parmi :\n"),
          paste(factAna_possible_refesp, collapse = ", "),
          paste ("\n ou :\n"),
          paste(factAna_possible_unitobs, collapse = ", "),
          paste("The value '", factAna, "' for the 'factAna' parameter isn't correct.\n", sep = ""),
          paste("Please, choose one in the following list :\n"),
          paste(factAna_possible_refesp, collapse = ", "),
          paste ("\n or :\n"),
          paste(factAna_possible_unitobs, collapse = ", ")
        )
      }
    }
    else{
      factAna_possible_refesp <- spRefFields.aliases(site = getOption("P.MPA"), dataEnv = dataEnv,
        ordered = FALSE, tableMetrique = tableMetrique)
      if (!is.element(factAna, factAna_possible_refesp)){
        stop(
          paste("La valeur '", factAna, "' du paramètre 'factAna' n'est pas valide.\n", sep = ""),
          paste("Veuillez choisir parmi :\n"),
          paste(factAna_possible_refesp, collapse = ", "),
          paste("The value '", factAna, "' of the 'factAna' parameter isn't correct.\n", sep = ""),
          paste("please, choose one in the following list :\n"),
          paste(factAna_possible_refesp, collapse = ", ")
        )
      }
    }
  }

  # ...des modalites du facteur de separation des graphiques :
  # ...the modalities of the factor for the  graphic separation :
  if (factAna != ""){
    factAnaSel_possible <- unique(selectModalites.f(tableMetrique = tableMetrique,
      facts = factAna, selections = append(list(NA), NA), metrique = metrique,
      nextStep = nextStep, dataEnv, level = 0)[, factAna])
    if (!is.na(factAnaSel) && !is.element(factAnaSel, factAnaSel_possible)){
      stop(
        paste("La valeur '", factAnaSel, "' du paramètre 'factAnaSel' n'est pas valide.\n", sep = ""),
        paste("Veillez choisir parmi :\n"),
        paste(factAnaSel_possible, collapse = ", "),
        paste("The value '", factAnaSel, "' of the 'factAnaSel' parameter isn't correct.\n", sep = ""),
        paste("Please, choose one in the following list :\n"),
        paste(factAnaSel_possible, collapse = ", ")
      )
    }
  }

  # ...des facteurs explicatifs :
  # ...the explanatory factors :
  listFact_possible <- refTablesFields.aliases(nomTable = tableMetrique, dataEnv = dataEnv)
  if (length(listFact) == 3){
    warning(
      "A partir de 3 facteurs de regroupement, les résultats deviennent difficiles à exploiter.",
      "Préférez les analyses à 1 ou 2 facteur(s).",
      "From 3 grouping factors, the results become difficult to exploit.",
      "Prefer analyzes with 1 or 2 factor(s).",
      immediate. = TRUE)

  }
  else if (length(listFact) > 3){
    stop(
      "Trop de facteurs de regroupement sélectionnés, les résultats seraient inexploitables.\n",
      "Veuillez ne sélectionner que 3 facteurs de regroupement au maximum.",
      "Too many grouping factors selected, the results would not be exploitable.",
      "Please, select 3 factors at most."
    )
  }
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
    facts = c(factAna, listFact), selections = append(list(factAnaSel), listFactSel),
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
    modeleLineaireWP2.esp.f(metrique, factAna, factAnaSel, listFact, listFactSel, tableMetrique,
      new_window = new_window, dataEnv, baseEnv)
  }
  else{
    modeleLineaireWP2.unitobs.f(metrique, factAna, factAnaSel, listFact, listFactSel, tableMetrique,
      new_window = new_window, dataEnv, baseEnv)
  }
}


#' @title Modele lineaire (donnees agregees par especes)
#'
#' @description Cette fonction permet de calculer des modeles lineaires sur les donnees agregees
#' par especes
#'
#' @param metrique chr, metrique choisie
#' @param factAna chr, facteur de separation des graphiques
#' @param factAnaSel chr, selection de modalites pour le factuer de selection des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel list, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param dataEnv environnement de stockage des donnnees
#' @param baseEnv environnement parent
#'
#' @noRd

modeleLineaireWP2.esp.f <- function(metrique, factAna, factAnaSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Gestions des différentes étapes des modèles linéaires.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factAna : le facteur de séparation des graphiques.
  ##            factAnaSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 août 2010, 15:59

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]

  listFact <- listFact[unlist(listFact) != ""]

  # Concaténation
  facteurs <- c(factAna, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factAnaSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

  # Données pour la série d'analyses :
  tmpData <- na.omit(subsetToutesTables.f(metrique = metrique, facteurs = facteurs,
    selections = selections, dataEnv = dataEnv, tableMetrique = tableMetrique,
    exclude = NULL, add = NULL))

  # Identification des différents lots d'analyses à faire:
  if (factAna == ""){                # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factAnaSel[1])){           # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factAna])))
    }else{                      # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factAnaSel[is.element(factAnaSel, tmpData[ , factAna])]
    }
  }

  # Formules pour différents modèles (avec ou sans transformation log) :
  exprML <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " * "))))
  logExprML <- eval(parse(text = paste("log(", metrique, ") ~", paste(listFact, collapse = " * "))))

  # Sauvegarde temporaire des données utilisées pour les analyses
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list()

  # Boucle sur les modalités de séparation des analyses :
  for (modSel in iFactGraphSel){
    # Préparation des données pour une analyse :
    if (modSel == ""){    # ...si pas de facteur de séparation des graphiques [!!!] changer 'Graph'
      tmpDataMod <- tmpData
    }else{                # ...sinon.
      tmpDataMod <- subset(tmpData, tmpData[ , factAna] == modSel) # Subset des données pour la modalité.
    }

    # Suppression des 'levels' non utilisés :
    tmpDataMod <- dropLevels.f(tmpDataMod)

    # Sauvegarde temporaire des données :
    DataBackup[[modSel]] <<- tmpDataMod

    # Aide au choix du type d'analyse :
    if (metrique == "pres.abs"){
      loiChoisie <- "BI"
    }else{
      loiChoisie <- choixDistri.f(metrique = metrique, Data = tmpDataMod[ , metrique, drop = FALSE])
    }

    if (!is.null(loiChoisie)){
      message(mltext("modeleLineaireWP2.esp.dist"), " = ", loiChoisie)

      if (is.element(loiChoisie, c("LOGNO"))){
        Log <- TRUE
        formule <- logExprML
      }else{
        Log <- FALSE
        formule <- exprML
      }

      res <- calcLM.f(loiChoisie = loiChoisie, formule = formule, metrique = metrique,
        Data = tmpDataMod)

      # Écriture des sorties :
      tryCatch(sortiesLM.f(
        objLM = res, formule = formule, metrique = metrique,
        factAna = factAna, modSel = modSel,
        listFact = listFact, listFactSel = listFactSel,
        Data = tmpDataMod, dataEnv = dataEnv, Log = Log,
        type = ifelse(tableMetrique == "unitSpSz",
          "CL_espece",
          "espece"),
        new_window = new_window,
        baseEnv = baseEnv),
        error = function(e) {
          errorLog.f()
          print("An error occurs")
        }
      )

      # Estimation des résidus "annormaux" :
      resid.out <- boxplot(residuals(res), plot = FALSE)$out

      if (length(resid.out)){
        suppr <- supprimeObs.f(residus = resid.out)

        if(!is.null(suppr)){
          if (!is.numeric(suppr)){ # conversion en numéros de lignes lorsque ce sont des noms :
            suppr <- which(is.element(row.names(tmpDataMod), suppr))
          }else{}

          tmpDataMod <- tmpDataMod[ - suppr, ]
          res.red <- calcLM.f(loiChoisie = loiChoisie, formule = formule, metrique = metrique,
            Data = tmpDataMod)

          resLM.red <<- res.red

          tryCatch(sortiesLM.f(objLM = res.red, formule = formule, metrique = metrique,
            factAna = factAna, modSel = modSel,
            listFact = listFact, listFactSel = listFactSel,
            Data = tmpDataMod, dataEnv = dataEnv, Log = Log, sufixe = "(red)",
            type = ifelse(tableMetrique == "unitSpSz",
              "CL_espece",
              "espece"),
            new_window = new_window,
            baseEnv = baseEnv),
            error = function(e){
              errorLog.f()
              print("An error occurs")
            }
          )
        }else{}
      }else{}
    }else{
      message(mltext("sortiesLM.Graph.cancelled"))
    }

  }
}


#' @import tcltk

choixDistri.f <- function(metrique, Data){

  ## Purpose: Aider l'utilisateur dans le choix d'une distribution de la
  ##          métrique et lancer les analyses adéquates.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : le nom de la métrique (variable dépendant)
  ##                       choisie.
  ##            Data : le jeu de données contenant la métrique.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 août 2010, 16:19

  # Systématiquement détruire la fenêtre en quitant :
  on.exit(tcltk::tkdestroy(WinDistri))
  # on.exit(print("WinDistri détruite !"), add = TRUE)


  # ##################################################
  # Variables :
  env <- environment()                # environnement courant.
  Done <- tcltk::tclVar(0)                   # État d'exécution.
  LoiChoisie <- tcltk::tclVar("NO")          # Variable pour le choix de distribution théorique.
  vscale <- 0.48                      # dimension verticale des graphiques.
  hscale <- 0.95                      # dimension horizontale des graphiques.
  pointsize <- 10                     # taille du point pour les graphiques
  distList <- list()                  # liste pour le stockage des AIC et autres.


  # ##################################################
  # Éléments graphiques :
  WinDistri <- tcltk::tktoplevel()           # Fenêtre principale.
  tcltk::tkwm.title(WinDistri, paste(mltext("choixDistri.WT")," '", metrique, "'", sep = ""))

  # tcltk::tkfocus(WinDistri)

  # Frame d'aide :
  FrameHelp <- tcltk::tkframe(WinDistri)
  T.help <- tcltk::tktext(FrameHelp, bg = "#fae18d", font = "arial", width = 100, height = 4,
    relief = "groove", borderwidth = 2)


  # Frame pour la loi Normale :
  FrameN <- tcltk::tkframe(WinDistri, borderwidth = 2, relief = "groove")
  Img.N <- tkrplot(FrameN,            # Création de l'image.
    fun = function(){
      plotDist.f(y = Data[ , metrique], family = "NO", metrique = metrique, env = env)
    },
    vscale = vscale, hscale = hscale, pointsize = pointsize)

  RB.N <- tcltk::tkradiobutton(FrameN, variable = LoiChoisie, value = "NO", # bouton de sélection.
    text = paste(mltext("choixDistri.dist.NO"),
      " (AIC=", round(distList[["NO"]]$aic, 0), "). ", sep = ""))


  # Frame pour la loi log-Normale :
  FrameLogN <- tcltk::tkframe(WinDistri, borderwidth = 2, relief = "groove")
  Img.LogN <- tkrplot(FrameLogN,  # Création de l'image.
    fun = function(){
      plotDist.f(y = Data[ , metrique], family = "LOGNO", metrique = metrique, env = env)
    },
    vscale = vscale, hscale = hscale, pointsize = pointsize)

  RB.LogN <- tcltk::tkradiobutton(FrameLogN, variable = LoiChoisie, value = "LOGNO", # bouton de sélection.
    text = paste(mltext("choixDistri.dist.LogNO"),
      " (AIC=", round(distList[["LOGNO"]]$aic, 0), "). ", sep = ""))

  # Frame pour la loi Gamma :
  FrameGa <- tcltk::tkframe(WinDistri, borderwidth = 2, relief = "groove")
  Img.Ga <- tkrplot(FrameGa,
    fun = function(){ # Création de l'image.
      plotDist.f(y = Data[ , metrique], family = "GA", metrique = metrique, env = env)
    },
    vscale = vscale, hscale = hscale, pointsize = pointsize)

  RB.Ga <- tcltk::tkradiobutton(FrameGa, variable = LoiChoisie, value = "GA", # bouton de sélection.
    text = paste(mltext("choixDistri.dist.G"),
      " (AIC=", round(distList[["GA"]]$aic, 0), "). ", sep = ""))

  if (is.integer(Data[ , metrique])){
    # # Frame pour la loi de Poisson :
    # FramePois <- tcltk::tkframe(WinDistri, borderwidth = 2, relief = "groove")
    # Img.Pois <- tkrplot(FramePois,  # Création de l'image.
    #   fun = function(){
    #     plotDist.f(y = Data[ , metrique], family = "PO", metrique = metrique, env = env)
    #   },
    #   vscale = vscale, hscale = hscale, pointsize = pointsize)

    # RB.Pois <- tcltk::tkradiobutton(FramePois, variable = LoiChoisie, value = "PO", # bouton de sélection.
    #   text = paste("loi de Poisson (AIC=", round(distList[["PO"]]$aic, 0), "). ", sep = ""))

    # Frame pour la loi bionomiale négative :
    FrameNBinom <- tcltk::tkframe(WinDistri, borderwidth = 2, relief = "groove")

    Img.NBinom <- tkrplot(FrameNBinom, # Création de l'image.
      fun = function(){
        plotDist.f(y = Data[ , metrique], family = "NBI", metrique = metrique, env = env)
      },
      vscale = vscale, hscale = hscale, pointsize = pointsize)

    RB.NBinom <- tcltk::tkradiobutton(FrameNBinom, variable = LoiChoisie, value = "NBI", # bouton de sélection.
      text = paste(mltext("choixDistri.dist.NB"),
        " (AIC=", round(distList[["NBI"]]$aic, 0), "). ", sep = ""))
  }else{}

  # Boutons :
  FrameB <- tcltk::tkframe(WinDistri)
  B.OK <- tcltk::tkbutton(FrameB, text = mltext("OK.button"), command = function(){
    tcltk::tclvalue(Done) <- "1"
  })
  B.Cancel <- tcltk::tkbutton(FrameB, text = mltext("Cancel.button"), command = function(){
    tcltk::tclvalue(Done) <- "2"
  })

  # ##################################################
  # Placement des éléments sur la grille :

  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))
  tcltk::tkinsert(T.help, "end", paste(mltext("choixDistri.dist.Help.1"), # texte de l'aide.
    mltext("choixDistri.dist.Help.2"),
    mltext("choixDistri.dist.Help.3"),
    mltext("choixDistri.dist.Help.4"),
    mltext("choixDistri.dist.Help.5"),
    mltext("choixDistri.dist.Help.6"), sep = ""))
  tcltk::tkgrid(T.help)
  tcltk::tkgrid(FrameHelp, column = 1, columnspan = 3)

  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))
  tcltk::tkgrid(Img.N, columnspan = 2)
  tcltk::tkgrid(RB.N, row = 1, sticky = "e")
  tcltk::tkgrid(tcltk::tklabel(FrameN, text = mltext("choixDistri.model.dist.NO"), fg = "red"),
    row = 1, column = 1, sticky = "w")

  tcltk::tkgrid(Img.LogN, columnspan = 2)
  tcltk::tkgrid(RB.LogN, sticky = "e")
  tcltk::tkgrid(tcltk::tklabel(FrameLogN, text = mltext("choixDistri.model.dist.LogNO"), fg = "red"),
    row = 1, column = 1, sticky = "w")
  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "), FrameN,
    tcltk::tklabel(WinDistri, text = " "), FrameLogN, tcltk::tklabel(WinDistri, text = "
  "),
  sticky = "ew")

  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))

  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "), FrameN,
    tcltk::tklabel(WinDistri, text = " "), FrameLogN,
    tcltk::tklabel(WinDistri, text = " "), sticky = "ew")

  tcltk::tkgrid(Img.Ga, columnspan = 2)
  tcltk::tkgrid(RB.Ga, sticky = "e")
  tcltk::tkgrid(tcltk::tklabel(FrameGa, text = mltext("choixDistri.model.dist.G"), fg = "red"),
    row = 1, column = 1, sticky = "w")


  # Évènements : sélections en cliquant sur les graphiques :
  tcltk::tkbind(Img.N, "<Button-1>", function(){
    tcltk::tclvalue(LoiChoisie) <- "NO"
  })
  tcltk::tkbind(Img.LogN, "<Button-1>", function(){
    tcltk::tclvalue(LoiChoisie) <- "LOGNO"
  })
  tcltk::tkbind(Img.Ga, "<Button-1>", function(){
    tcltk::tclvalue(LoiChoisie) <- "GA"
  })

  # Pour les données entières seulement :
  if (is.integer(Data[ , metrique])){
    tcltk::tkgrid(Img.NBinom, columnspan = 2)
    tcltk::tkgrid(RB.NBinom, sticky = "e")
    tcltk::tkgrid(tcltk::tklabel(FrameNBinom, text = mltext("choixDistri.model.dist.NB"), fg = "red"),
      row = 1, column = 1, sticky = "w")
    tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "), FrameGa, # FramePois,
      tcltk::tklabel(WinDistri, text = " "), FrameNBinom,
      tcltk::tklabel(WinDistri, text = " "), sticky = "ew")
    tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))

    # Évènements : sélections en cliquant sur les graphiques :
    tcltk::tkbind(Img.NBinom, "<Button-1>", function(){
      tcltk::tclvalue(LoiChoisie) <- "NBI"
    })
  }else{
    tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "), FrameGa, # FramePois,
      tcltk::tklabel(WinDistri, text = " "), tcltk::tklabel(WinDistri, text = " "),
      tcltk::tklabel(WinDistri, text = " "), sticky = "ew")
    tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))
  }

  # Boutons :
  tcltk::tkgrid(FrameB, column = 1, columnspan = 3)
  tcltk::tkgrid(B.OK, tcltk::tklabel(FrameB, text = "                         "), B.Cancel)
  tcltk::tkgrid(tcltk::tklabel(WinDistri, text = " "))

  # ##################################################
  # Autres évènements :
  tcltk::tkbind(WinDistri, "<Destroy>", function(){
    tcltk::tclvalue(Done) <- "2" # en cas de destruction de la fenêtre.
  })

  # Présélection de la distribution avec le plus petit AIC :
  tcltk::tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]

  tcltk::tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

  if (tcltk::tclvalue(Done) == "1"){
    return(tcltk::tclvalue(LoiChoisie))
  }else{
    return(NULL)
  }

}

#' @importFrom gamlss.dist  as.gamlss.family
#' @importFrom gamlss gamlss

plotDist.f <- function(y, family, metrique, env = NULL, ...){

  ## Purpose: Représenter l'ajustement de distribution et retourner l'objet
  ##          (contient notamment l'AIC du modèle).
  ## ----------------------------------------------------------------------
  ## Arguments: y : les données (numeric ou integer).
  ##            family : la loi de distribution, telle que défini dans
  ##                     'gamlss.family'.
  ##            metrique : nom de la métrique.
  ##            env : l'environnement de la fonction appelante.
  ##            ... : autres arguments à passer comme
  ##                  paramètres graphiques.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 août 2010, 16:46

  # Seed (je ne sais pas si c'est vraiment nécessaire) :
  set.seed(as.integer(runif(1, min = 1, max = 512)))

  # Noms et fonction de densité de la loi pour ajouter les titres
  # ainsi qu'ajuster et représenter la distribution :
  loi <- switch(family,
    NO = list(name = mltext("plotDist.title.NO"), densfunName = "normal",
      densfun = "dnorm", start = NULL),
    LOGNO = list(name = mltext("plotDist.title.LogNO"), densfunName = "log-normal",
      densfun = "dlnorm", start = NULL),
    PO = list(name = "de Poisson", densfunName = "poisson",
      densfun = "dpois", start = NULL),
    NBI = list(name = mltext("plotDist.title.NB"), densfunName = "negative binomial",
      densfun = "dnbinom", start = NULL),
    GA = list(name = mltext("plotDist.title.G"), densfunName = "gamma",
      densfun = "dgamma", start = list(shape = 1, scale = 2)))

  # Traitement des zéros pour la loi Log-Normale :
  if (is.element(family, c("LOGNO", "GA")) & sum(y == 0, na.rm = TRUE)){
    y <- y + ((min(y, na.rm = TRUE) + 1) / 1000)
  }else{}

  # abscisses pour la distribution théorique.
  if (is.element(family, c("PO", "NBI"))){
    xi <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
  }else{
    xi <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE), length.out = 5000)
  }

  # browser(condition = (family == "NBI"))  ## [!!!] attention, il arrive que les calculs bloquent
  # ici lors du premier lancement. (origine inconnue)
  # On ajuste la distribution :

  coefLoi <- tryCatch(fitdistr(na.omit(y), densfun = loi$densfunName, start = loi$start),
    error = function(e){
      return(NULL)
  })

  # Calcul des points théoriques à représenter :
  if (! is.null(coefLoi)){            # Uniquement si coefLoi peut être estimé...
    expr <- parse(text = paste(loi$densfun, "(xi, ",       # points à représenter.
      paste(names(coefLoi$estimate), coefLoi$estimate, # coefs estimés.
        sep = "=", collapse = ", "),
      ")", sep = ""))

    yi <- eval(expr)                # valeurs pour la loi de distribution théorique ajustée.
  }else{
    yi <- 0                         # ...sinon, yi neutre dans les max !
  }

  # Représentation graphique :
  nbreaks <- 60                       # Nombre de barres.

  histTmp <- hist(y, breaks = nbreaks, plot = FALSE) # pour connaitre la fréquence maximale de la distribution observée.

  par(mar = c(3.4, 3.4, 2.5, 0.1), mgp = c(2.0, 0.7, 0), bg = "white", cex = 0.8, # Paramètres graphiques.
    ...)

  hist(y, breaks = nbreaks, freq = FALSE, # histogramme (distribution observée).
    ylim = c(0, ifelse(max(yi, na.rm = TRUE) > 3 * max(histTmp$density, na.rm = TRUE),
      3 * max(histTmp$density, na.rm = TRUE),
      1.05 * max(c(histTmp$density, yi), na.rm = TRUE))),
    xlim = c(min(y, na.rm = TRUE), max(y, na.rm = TRUE)),
    main = paste("", loi$name, sep = ""),
    xlab = Capitalize.f(varNames[metrique, "nom"]),
    ylab = mltext("plotDist.ylab"),
    col = "lightgray")

  if (! is.null(coefLoi)){
    lines(xi, yi, lwd = 2, col = "red")     # courbe (distribution théorique).
  }else{
    text(x = mean(c(min(y, na.rm = TRUE), max(y, na.rm = TRUE))),
      y = mean(c(0, ifelse(max(yi, na.rm = TRUE) > 3 * max(histTmp$density, na.rm = TRUE),
        3 * max(histTmp$density, na.rm = TRUE),
        1.05 * max(c(histTmp$density, yi), na.rm = TRUE)))),
        labels = mltext("plotDist.failure"), col = "red")
  }

  # Calcul d'AIC (entre autres) :
  FA <- gamlss.dist::as.gamlss.family(family)      # On procède comme dans la fonction histDist.
  fname <- FA$family[1]

  res <- gamlss::gamlss(y ~ 1, family = fname)

  # Si un environnement est précisé, la valeur est sauvegardée dans une liste 'distList' :
  if (!is.null(env)){
    eval(substitute(evalq(distList[[family]] <- res, envir = env),
      list(family = eval(family), res = eval(res))))
  }else{}

  # Retourne le résultat :
  return(res)
}


calcLM.f <- function(loiChoisie, formule, metrique, Data){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 sept. 2010, 14:49

  switch(loiChoisie,
    # Modèle linéaire :
    NO = {
      res <- lm(formule, data = Data)
    },
    # Modèle linéaire, données log-transformées :
    LOGNO = {
    # Ajout d'une constante à la métrique si contient des zéros :
      if (sum(Data[ , metrique] == 0, na.rm = TRUE)){
        Data[ , metrique] <- Data[ , metrique] + ((min(Data[ , metrique], na.rm = TRUE) + 1) / 1000)
      }else{}
      res <- lm(formule, data = Data)
      },
    # GLM, distribution Gamma :
    GA = {
    # Ajout d'une constante à la métrique si contient des zéros :
    if (sum(Data[ , metrique] == 0, na.rm = TRUE)){
      Data[ , metrique] <- Data[ , metrique] + ((min(Data[ , metrique], na.rm = TRUE) + 1) / 1000)
    }else{}
    res <- glm(formule, data = Data, family = "Gamma")
    },
    # GLM, distribution de Poisson :
    PO = {
      res <- glm(formule, data = Data, family = "poisson")
    },
    # GLM, distribution binomiale négative :
    NBI = {
      res <- glm.nb(formule, data = Data)
    },
    # GLM, distribution binomiale (présences/absences) :
    BI = {
      res <- glm(formule, data = Data, family = "binomial")
    },
  )
  return(res)
}

#' @importFrom   grDevices dev.set dev.off


sortiesLM.f <- function(objLM, formule, metrique, factAna, modSel, listFact, listFactSel, Data,
  dataEnv, Log = FALSE, sufixe = NULL, type = "espece", new_window = TRUE, baseEnv = .GlobalEnv){

  ## Purpose: Formater les résultats de lm et les écrire dans un fichier
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe lm
  ##            formule : la formule utilisée (pas lisible dans le call).
  ##            metrique : la métrique choisie.
  ##            factAna : le facteur de séparation des analyses.
  ##            modSel : la modalité courante.
  ##            listFact : liste du (des) facteur(s) de regroupement.
  ##            Data : les données utilisées.
  ##            Log : données log-transformées ou non (booléen).
  ##            sufixe : un sufixe pour le nom de fichier.
  ##            type : type d'analyse, pour traitement conditionnel des
  ##                   titres et noms de fichiers.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 25 août 2010, 16:19

  # longueur des lignes pour les sorties textes :
  oOpt <- options()
  on.exit(options(oOpt))

  options(width = 120)

  # Fonction de création de fenêtre :
  if (.Platform$OS.type == "windows"){
    winFUN <- "windows"
  }else{
    winFUN <- "X11"
  }

  # Ajout d'une constante si des zéros dans la métrique + transformation 'log' :
  if (sum(Data[ , metrique] == 0, na.rm = TRUE) & Log){
    Data[ , metrique] <- Data[ , metrique] +
      ((min(Data[ , metrique], na.rm = TRUE) + 1) / 1000)
  }else{}

  # Formule de modèle lisible:
  objLM$call$formula <- formule
  formule <<- formule
  resLM <<- objLM

  # Chemin et nom de fichier :
  resFile <- resFileLM.f(objLM = objLM, metrique = metrique, factAna = factAna, modSel = modSel,
    listFact = listFact, dataEnv = dataEnv, Log = Log, sufixe = sufixe, type = type)
  on.exit(tryCatch(close(resFile), error = function(e){}), add = TRUE)


  # Informations et statistiques globales sur le modèle :
  infoStatLM.f(objLM = objLM, resFile = resFile)


  # Anova globale du modèle + significativité des coefficients :
  signifParamLM.f(objLM = objLM, resFile = resFile)


  # ##################################################
  # Valeurs prédites par le modèle :
  valPreditesLM.f(objLM = objLM, Data = Data, listFact = listFact, resFile = resFile)

  # ##################################################
  # Comparaisons multiples :

  if (length(listFact) == 2){
    WinInfo <- tcltk::tktoplevel()
    on.exit(tcltk::tkdestroy(WinInfo))
    tcltk::tkwm.title(WinInfo, mltext("sortiesLM.W.Title"))

    tcltk::tkgrid(tcltk::tklabel(WinInfo, text = "\t "),
      tcltk::tklabel(WinInfo, text = paste0("\n", mltext("sortiesLM.Wminfo.Info.1"), "\n")),
      tcltk::tklabel(WinInfo, text = "\t "),
      sticky = "w")

    tcltk::tkgrid(tcltk::tklabel(WinInfo, text = "\t "),
      tcltk::tklabel(WinInfo,
        text = paste(mltext("sortiesLM.Wminfo.Info.2"),
          mltext("sortiesLM.Wminfo.Info.3"),
          "\n", sep = "")),
      sticky = "w")

    tcltk::tkfocus(WinInfo)

    compMultiplesLM.f(objLM = objLM, Data = Data, fact1 = listFact[1], fact2 = listFact[2],
      resFile = resFile, exclude = factAna, Log = Log)

    # Représentation des interactions :
    mainTitle <- graphTitle.f(metrique = metrique, modGraphSel = modSel, factGraph = factAna,
      listFact = listFact, model = mltext("sortiesLM.Graph.Title", language = getOption("P.lang")),
      type = type)

    eval(call(winFUN, pointsize = ifelse(isTRUE(getOption("P.graphPaper")), 14, 12)))
    par(mar = c(5, 4,
      ifelse(isTRUE(getOption("P.graphPaper")), 2, 5),
      2) + 0.1)
    with(Data,
      if (Log){   # Les sens de variations peuvent changer en log (sur les moyennes) =>
                  # besoin d'un graphique adapté :
        eval(parse(text = paste("interaction.plot(", listFact[1], ", ", listFact[2],
          ", log(", metrique, "), ylab=\"",
          paste(mltext("sortiesLM.Graph.ylab.pfx", language = getOption("P.lang")),
            "log(", Capitalize.f(varNames[metrique, "nom"]), ")",
            mltext("sortiesLM.Graph.ylab.sfx", language = getOption("P.lang")), sep = ""),
          "\", xlab=\"", Capitalize.f(varNames[listFact[1], "nom"]),
          "\", main=\"",
          ifelse((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title")), mainTitle, ""),
          "\", trace.label=\"", Capitalize.f(varNames[listFact[2], "nom"]),
          "\", cex.main=0.9)", sep = "")))

      }else{
        eval(parse(text = paste("interaction.plot(", listFact[1], ", ", listFact[2],
          ", ", metrique, ", ylab=\"",
          paste(mltext("sortiesLM.Graph.ylab.pfx", language = getOption("P.lang")),
            Capitalize.f(varNames[metrique, "nom"]),
            mltext("sortiesLM.Graph.ylab.sfx", language = getOption("P.lang")),
            switch(varNames[metrique, "genre"],
              "f" = , # Double the consonnant in French!
              "fp" = mltext("sortiesLM.Graph.ylab.sfxDblCons", language = getOption("P.lang")),
              ""),
            switch(varNames[metrique, "genre"],
              "f" = mltext("graphTitle.f", language = getOption("P.lang")),
              "fp" = mltext("graphTitle.fp", language = getOption("P.lang")),
              "mp" = mltext("graphTitle.mp", language = getOption("P.lang")),
              ""), # "moyen", moyens,
                   # "moyenne" ou "moyennes" selon le genre.
            sep = ""),
          "\", xlab=\"", Capitalize.f(varNames[listFact[1], "nom"]),
          "\", main=\"", ifelse((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title")),
            mainTitle, ""),
          "\", trace.label=\"", Capitalize.f(varNames[listFact[2], "nom"]),
          "\", cex.main=0.9)", sep = "")))
         })

    tcltk::tkdestroy(WinInfo)
  }else{
    if (length(listFact) == 1){
      compSimplesLM.f(objLM = objLM, Data = Data, fact = listFact, resFile = resFile, Log = Log)
    }else{}
  }

  subTitle <- graphTitle.f(metrique = metrique, modGraphSel = modSel, factGraph = factAna,
    listFact = listFact, model = modelType.f(objLM = objLM, Log = Log), type = type)

  grDevices::dev.set(1)
  grDevices::dev.off()

  if (new_window) eval(call(winFUN, width = 45, height = 35))
  par(mfrow = c(2, 2), oma = c(0, 0, 4.7, 0))
  hist(objLM$residuals,
    xlab = mltext("sortiesLM.Graph.hist.xlab", language = getOption("P.lang")),
    ylab = mltext("sortiesLM.Graph.hist.ylab", language = getOption("P.lang")),
    main = NULL)
  mtext(mltext("sortiesLM.Graph.hist.title", language = getOption("P.lang")), side = 3, cex = 0.8)

  # Titre général :
  mtext(mltext("sortiesLM.Graph.diag.title", language = getOption("P.lang")),
    side = 3, outer = TRUE, line = 3.4, cex = 1.2)
  mtext(subTitle, side = 3, outer = TRUE, line = -2.4, cex = 1.1)

  # Essayer glm.diag.plots('glm')...
  plot.lm.ml(objLM, which = 2, cex.caption = 0.8)
  plot.lm.ml(objLM, which = c(1, 4), cex.caption = 0.8)

  # ##################################################
  # Sauvegarde des données :
  filename <- summary(resFile)$description

  close(resFile)                      # Maintenant seulement on peut fermer ce fichier.

  if (getOption("P.saveData") &&  ! isTRUE(sufixe == "(red)")){
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


resFileLM.f <- function(objLM, metrique, factAna, modSel, listFact, dataEnv,
  Log = FALSE,  prefix = NULL, ext = "txt", sufixe = NULL, type = "espece"){

  ## Purpose: Définit les noms du fichiers pour les résultats des modèles
  ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
  ##          mais par défaut, c'est le fichier de sorties texte qui est
  ##          créé.
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
  ##            metrique : nom de la métrique analysée.
  ##            factAna : nom du facteur de séprataion des analyses.
  ##            modSel : modalité de factAna sélectionnée.
  ##            listFact : vecteur des noms de facteurs de l'analyse.
  ##            Log : Est-ce que les données sont log-transformées.
  ##            prefix : préfixe du nom de fichier.
  ##            sufixe : un sufixe pour le nom de fichier.
  ##            ext : extension du fichier.
  ##            type : type de modèle (traitement conditionnel).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 15:48

  # si pas de préfix fourni :
  if (is.null(prefix)){
    prefix <- modelType.f(objLM = objLM, Log = Log)
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


modelType.f <- function(objLM, Log){

  ## Purpose: Fournir un prefix décrivant le modèle utilisé.
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe LM ou GLM.
  ##            Log : log-transformation des données (boolean).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 14 oct. 2010, 16:29

  return(ifelse(length(grep("^lm\\(", deparse(objLM$call), perl = TRUE)) > 0,
    paste("LM", ifelse(Log, "-log", ""), sep = ""),
    ifelse(length(grep("^glm\\.nb", deparse(objLM$call), perl = TRUE)) > 0,
      "GLM-NB",
      ifelse(length(grep("^glm.*poisson", deparse(objLM$call), perl = TRUE)) > 0,
        "GLM-P",
        ifelse(length(grep("^glm.*\"binomial\"", deparse(objLM$call), perl = TRUE)) > 0,
          "GLM-B",
          ifelse(length(grep("family[[:blank:]]*=[[:blank:]]*\"Gamma\"", deparse(objLM$call), perl = TRUE)) > 0,
            "GLM-Ga",
            "Unknown-model"))))))
}


infoStatLM.f <- function(objLM, resFile){

  ## Purpose: Écrit les informations sur le modèle insi que les
  ##          statistiques globale dans un fichier résultat
  ## ----------------------------------------------------------------------
  ## Arguments: objLM un objet de classe 'lm' ou 'glm'.
  ##            resFile : une connection pour les sorties.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 16:57

  # [!!!] Attention, il arrive que les calculs bloquent ici lors du premier lancement (origine inconnue)
  sumLM <- switch(class(objLM)[1],
    lm = summary.lm(objLM),
    glm = summary.glm(objLM),
    negbin = MASS:::summary.negbin(objLM),
    summary(objLM))

  # Informations sur le modèle :
  cat(mltext("infoStatLM.Info.model"), file = resFile, fill = 1)
  cat("\t", deparse(objLM$call), "\n\n\n", file = resFile, sep = "")

  # Stats globales :
  if (length(grep("^glm", objLM$call)) == 0){
    cat(mltext("infoStatLM.Info.Fheader"), "\n\n", file = resFile)
    cat("\t", mltext("infoStatLM.Info.R2mult"), " ", format(sumLM$r.squared, digits = 3),
      mltext("semicolon"),
      "\t", mltext("infoStatLM.Info.R2adj"), " ", format(sumLM$adj.r.squared, digits = 3),
      "\n", file = resFile, sep = "")

    cat("\t", mltext("infoStatLM.Info.Fstat"),
      paste(sapply(sumLM$fstatistic, format, digits = 4, nsmall = 0),
        mltext(c("infoStatLM.Info.over", "infoStatLM.Info.and", "infoStatLM.Info.DF")), sep = ""),
      "\t", mltext("infoStatLM.Info.Pval"), " ",
      format.pval(pf(sumLM$fstatistic[1L], sumLM$fstatistic[2L], sumLM$fstatistic[3L],
        lower.tail = FALSE), digits = 4),
      "\n\n\n", file = resFile, sep = "")
  }else{
  }
}


signifParamLM.f <- function(objLM, resFile){

  ## Purpose: Écrire les résultats de l'anova globale du modèle et
  ##          l'estimation de significativités des coefficients du modèle.
  ## ----------------------------------------------------------------------
  ## Arguments: objLM un objet de classe 'lm' ou 'glm'.
  ##            resFile : une connection pour les sorties.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 17:07

  # Anovas et résumés :
  if (length(grep("^glm", objLM$call)) > 0){   # Pour les GLMs.
    anovaLM <- anova(objLM, test = "Chisq")      # Pour les LMs.
  }else{
    anovaLM <- anova(objLM)
  }
  sumLM <- summary(objLM)

  # Anova globale du modèle :
  capture.output(print.anova.ml(anovaLM), file = resFile)

  # Significativités des paramètres :
  cat("\n\n", mltext("signifParamLM.Info.1"), " \n",
    mltext("signifParamLM.Info.2"), "\n\n",
    file = resFile)

  capture.output(printCoefmat.red(sumLM$coef, anovaLM = anovaLM, objLM = objLM), file = resFile)
}


print.anova.ml <- function(x, digits = max(getOption("digits") - 2, 3),
  signif.stars = getOption("show.signif.stars"), ...){

  ## Purpose: Hack de la méthode print.anova pour franciser les sorties et
  ##          supprimer les infos inutiles.
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de print.anova
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 26 août 2010, 11:36

  attr(x, "row.names")[attr(x, "row.names") == "Residuals"] <- mltext("print.anova.ml.KW.resid")

  # Françisation des en-têtes (gsub itératif) :
  attr(x, "heading") <- iter.gsub(
    pattern = c("Analysis of Deviance Table",
      "Analysis of Variance Table",
      "Model:",
      "Negative Binomial",
      "binomial",
      "Terms added sequentially \\(first to last\\)",
      "Response:",
      "link:"),
    replacement = c(
      paste0("\n--------------------------------------",
        "-------------------------------------\n",
        c(mltext("print.anova.ml.KW.devTab"),
        mltext("print.anova.ml.KW.varTab"))),
      mltext("print.anova.ml.KW.family"),
      mltext("print.anova.ml.KW.NB"),
      mltext("print.anova.ml.KW.B"),
      mltext("print.anova.ml.KW.termSeq"),
      mltext("print.anova.ml.KW.response"),
      mltext("print.anova.ml.KW.link")),
    x = attr(x, "heading"), fixed = TRUE)

  # Définitions issues de la fonction originale :
  if (!is.null(heading <- attr(x, "heading"))){
    cat(heading, sep = "\n")
  }else{}

  nc <- dim(x)[2L]
  if (is.null(cn <- colnames(x))){
    stop("'anova' object must have colnames")
  }else{}
  has.P <- grepl("^(P|Pr)\\(", cn[nc])
  zap.i <- 1L:(if (has.P)
  {
    nc - 1
  }else{
    nc
  })
  i <- which(substr(cn, 2, 7) == " value")
  i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
  if (length(i)){
    zap.i <- zap.i[!(zap.i %in% i)]
  }else{}

  tst.i <- i
  if (length(i <- grep("Df$", cn))){
    zap.i <- zap.i[!(zap.i %in% i)]
  }else{}

  printCoefmat(x, digits = digits, signif.stars = signif.stars,
    signif.legend = FALSE,
    has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
    zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
  invisible(x)
}


iter.gsub <- function(pattern, replacement, x, ...){

  if (length(pattern) > 0){
    return(gsub(pattern = pattern[1],
      replacement = replacement[1],
      x = iter.gsub(pattern = pattern[-1], replacement = replacement[-1], x = x),
      ...))
  }else{
    return(x)
  }
}


printCoefmat.red <- function(x, digits = max(3, getOption("digits") - 2),
  signif.stars = getOption("show.signif.stars"), signif.legend = signif.stars,
  dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
  P.values = NULL, has.Pvalue = nc >= 4 && substr(colnames(x)[nc], 1, 3) == "Pr(",
  eps.Pvalue = .Machine$double.eps, na.print = "NA", anovaLM = NULL, objLM = NULL, ...){

  ## Purpose: Modification de printCoefmat pour n'afficher que les z-values
  ##          et p-values, et pour les facteurs significatife uniquement.
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de printCoefmat
  ##            + anovaLM : résultat d'anova globale du modèle (pour les
  ##                        facteurs et intéractions significatifs).
  ##            objLM : objet de classe 'lm' ou 'glm'
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 31 août 2010, 10:46

  # Sélection des coefficients à montrer (pour effets/interactions significatifs) :
  x <- x[selRowCoefmat(x, anovaLM, objLM), , drop = FALSE]

  # Définitions issues de la fonction originale :
  if (is.null(d <- dim(x)) || length(d) != 2L)
    stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  if (is.null(P.values)) {
    scp <- getOption("show.coef.Pvalues")
    if (!is.logical(scp) || is.na(scp)) {
      warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
      scp <- TRUE
    }
    P.values <- has.Pvalue && scp
  }
  else if (P.values && !has.Pvalue)
    stop("'P.values' is TRUE, but 'has.Pvalue' is not")
  if (has.Pvalue && !P.values) {
    d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
    nc <- nc - 1
    has.Pvalue <- FALSE
  }
  else xm <- data.matrix(x)
  k <- nc - has.Pvalue - (if (missing(tst.ind))
    1
    else length(tst.ind))
  if (!missing(cs.ind) && length(cs.ind) > k)
    stop("wrong k / cs.ind")
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 0]))
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- format(round(coef.se, max(1, digits - digmin)), digits = digits)
    }
  }
  if (length(tst.ind))
    Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
                            digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc))))
    for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
  okP <- if (has.Pvalue)
    ok[, -nc]
  else ok
  x1 <- Cf[okP]
  dec <- getOption("OutDec")
  if (dec != ".")
    x1 <- chartr(dec, ".", x1)
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1, digits - 1))
  }
  if (any(ina))
    Cf[ina] <- na.print
  if (P.values) {
    if (!is.logical(signif.stars) || is.na(signif.stars)) {
      warning("option \"show.signif.stars\" is invalid: assuming TRUE")
      signif.stars <- TRUE
    }
    if (any(okP <- ok[, nc])) {
      pv <- as.vector(xm[, nc])
      Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, eps = eps.Pvalue)
      signif.stars <- signif.stars && any(pv[okP] < 0.1)
      if (signif.stars) {
        Signif <- symnum(pv, corr = FALSE, na = FALSE,
          cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
          symbols = c("***", "**", "*", ".", " "))
        Cf <- cbind(Cf, format(Signif))
      }
    }
    else signif.stars <- FALSE
  }
  else signif.stars <- FALSE

  # Sélection de colonnes :
  Cf <- Cf[ , ncol(Cf) - c(2:0)]

  print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, ...)
  if (signif.stars && signif.legend)
    cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
  invisible(x)
}


selRowCoefmat <- function(coefsMat, anovaLM, objLM){

  ## Purpose: Retourne un vecteur de booléen donnant les indices de ligne
  ##          de la matrice de coefs correspondant à des facteurs ou
  ##          intéractions significatifs (les autres coefs n'ont pas
  ##          d'intéret).
  ## ----------------------------------------------------------------------
  ## Arguments: coefsMat : matrice de coefficients.
  ##            anovaLM : objet correspondant de classe 'anova.lm'.
  ##            objLM : l'objet de classe 'lm' (nécessaire pour traiter
  ##                    les cas de NAs)
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 31 août 2010, 14:31

  if (!is.null(anovaLM)){

    # Facteurs et intéractions dont les coefs doivent être imprimés :
    selectedFactInt <- attr(anovaLM, "row.names")[
      which(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]] < 0.05)]

    # Tous les facteurs :
    facts <- attr(anovaLM, "row.names")[!is.na(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]])]

    # indices des intéractions dans "selectedFactInt" :
    has.interactions <- TRUE
    interactions <- grep(":", facts, fixed = TRUE)
    if (length(interactions) == 0){
      interactions <- length(facts) + 1
      has.interactions <- FALSE
    }else{}

    # Coefficients par facteur (sans compter les intéractions) :
    if (length(facts) == 1){
      factsRows <- list(grep(paste("^", facts, sep = ""), row.names(coefsMat), value = TRUE))
      names(factsRows) <- facts

    }else{
      factsRows <- # sapply(
        sapply(facts[-c(interactions)],
          function(fact){
            grep(paste("^", fact, sep = ""), row.names(coefsMat), value = TRUE)[! grepl(":",
                grep(paste("^", fact, sep = ""), row.names(coefsMat), value = TRUE),
                fixed = TRUE)]
          }, simplify = FALSE)# , as.vector)
    }



    # type de coef (facteur et intéractions) par ligne de la matrice de coef :
    rows <- c("(Intercept)",
      # facteurs :
      unlist(sapply(1:length(factsRows), function(i){
        rep(names(factsRows)[i], length(factsRows[[i]]))
    })))

    # intéractions :
    if (has.interactions){
      # nombre de répétitions par type d'intéraction :

      # liste des nombres de modalités pour chaque facteur d'une intéraction :
      nmod <- sapply(strsplit(facts[interactions], ":"),
        function(fa) sapply(fa, function(i)length(factsRows[[i]])))

      if (!is.list(nmod)){         # corrige un bug lorsqu'uniquement 1 type d'intéraction).
        nmod <- list(as.vector(nmod))
      }else{}

      # Nombres de répétitions :
      nrep <- sapply(nmod, prod)

      # Ajout des types d'intéractions :
      rows <- c(rows,
        unlist(sapply(1:length(nrep), function(i){
          rep(facts[interactions][i], nrep[i])
      })))
    }else{}

    # Lignes conservées :
    return(is.element(rows, c("(Intercept)", selectedFactInt))[!is.na(objLM$coefficients)])
  }else{
    return(rep(TRUE, nrow(coefsMat)))
  }
}


valPreditesLM.f <- function(objLM, Data, listFact, resFile){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : objet de classe 'lm' ou 'glm'.
  ##            Data : les données utilisées pour ajuster le modèle.
  ##            listFact : un vecteur donnant la liste des noms de
  ##                       facteurs.
  ##            resFile : la connection au fichier résultat
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 16:12


  # ##################################################
  # Valeurs prédites :
  OrdreNivFact <- sapply(unique(Data[ , listFact]), as.numeric)

  if (!is.matrix(OrdreNivFact)){     # Si un seul facteur, on transforme le vecteur d'ordre
                                     # des niveaux en matrice.
    OrdreNivFact <- matrix(OrdreNivFact, ncol = 1, dimnames = list(NULL, listFact))
  }else{}

  # Valeurs prédites pour chaque combinaison réalisée des facteurs :
  if (length(grep("^glm", objLM$call)) > 0){
    valPredites <- predict(objLM, newdata = unique(Data[ , listFact, drop = FALSE]),
      type = "response")
  }else{
    valPredites <- predict(objLM, newdata = unique(Data[ , listFact, drop = FALSE]))
  }

  # Noms des valeurs prédites (combinaisons des différents niveaux de facteurs) :
  nomCoefs <- unique(apply(Data[ , listFact, drop = FALSE], 1, paste, collapse = ":"))
  names(valPredites) <- nomCoefs

  # On remet les modalités en ordre :
  valPredites <- valPredites[eval(parse(text = paste("order(",
    paste("OrdreNivFact[ , ", 1:ncol(OrdreNivFact), "]", sep = "", collapse = ", "),
      ")", sep = "")))]

  # Écriture de l'en-tête :
  cat("\n\n\n---------------------------------------------------------------------------",
      "\n", mltext("valPreditesLM.header"), "\n\n",
      file = resFile)

  # Écriture du résultat :
  capture.output(print(valPredites), file = resFile)

}

#' @importFrom multcomp glht

compMultiplesLM.f <- function(objLM, Data, fact1, fact2, resFile, exclude, Log = FALSE){

  ## Purpose: Calculer et écrire les résultats des comparaisons multiples.
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : objet de classe 'lm' ou 'glm'.
  ##            Data : les données utilisées pour ajuster le modèle.
  ##            fact1 : le nom du premier facteur utilisé pour les
  ##                    comparaisons multiples.
  ##            fact2 : le nom du second facteur utilisé pour les
  ##                    comparaisons multiples.
  ##            resFile : la connection pour les sorties textes.
  ##            exclude : facteur non analysé.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 oct. 2010, 09:54

  facts <- c(fact1, fact2)

  # écriture des en-têtes :
  cat("\n\n\n---------------------------------------------------------------------------",
    "\n", mltext("compMultiplesLM.header"),
    file = resFile)

  # Avertissement concernant les estimations de différences :
  compMultiplesAvertissement.f(objLM = objLM, Log = Log, resFile = resFile)

  # Test si un facteur est temporel :
  tempFact <- is.temporal.f(facts, unitobs)

  # Calculs des matrices de différences :
  for (i in seq(along = facts)){
    if (tempFact[i]){                # Si le facteur inclus est temporel :
      assign(paste("diff", i, sep = ""),
        diffTemporelles.f(objLM = objLM,
          factSpatial = facts[-i],
          factTemp = facts[i],
          Data = Data,
          exclude = exclude))
    }else{                           # ... sinon :
      difftmp <- diffSpatiales.f(objLM = objLM,
        factSpatial = facts[i],
        factTemp = facts[-i],
        Data = Data,
        exclude = exclude)
      # On réordonne d'après le second facteur (plus lisible) :
      assign(paste("diff", i, sep = ""),
        difftmp[order(sub("^([^:]+) :.+$", "\\1", row.names(difftmp))), ])
      rm(difftmp)
    }
  }

  # Si des coefs n'ont pu être calculés, glht plante... à moins que :
  if (any(is.na(coef(objLM)))){
    # Avertissement :
    cat("\n\n\t",
      mltext("compMultiplesLM.W.1"),
      mltext("compMultiplesLM.W.2"),
      mltext("compMultiplesLM.W.3"), "\n",
      file = resFile)

    # Réduction des matrices de différences :
    diff1 <- diff1[ , !is.na(coef(objLM))]
    diff2 <- diff2[ , !is.na(coef(objLM))]

    objLM$coefficients <- objLM$coefficients[!is.na(coef(objLM))]
  }

  for (i in seq(along = facts)){
    # Résultats des comparaisons spatiales/de statut :
    cat(paste("\n\n", mltext("compMultiplesLM.Info.1"), " '", varNames[facts[i], "nom"], "' ",
      ifelse(tempFact[i],
        paste0("(", mltext("KW.temporal"), ") "),
        ""),
      "par '", varNames[facts[-i], "nom"], "' ",
      ifelse(tempFact[-i],
        paste0("(", mltext("KW.temporal"), ") "),
        ""),
      ":\n", sep = ""),
      file = resFile)

    capture.output(print.summary.glht.red(summary(multcomp::glht(objLM,
      linfct = get(paste("diff", i, sep = "")), alternative = "two.sided"))),
      file = resFile)
  }

}


compMultiplesAvertissement.f <- function(objLM, Log, resFile){

  ## Purpose: Afficher un avertissement concernant les différences (dans la
  ##          fonction de lien).
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : objet de classe (G)LM.
  ##            Log : booléen indiquant la log-transfomation des données.
  ##            resFile : fichier de sortie.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 31 janv. 2011, 14:11

  cat("\n",
    switch(modelType.f(objLM = objLM, Log = Log),
      "LM" = {
        ""
      },
      "LM-log" = {
        paste("\t", mltext("multCompWarn.logLM"),
          "\n\t(log(A) - log(B))", sep = "")
      },
      "GLM-NB" = {
        paste("\t", mltext("multCompWarn.link"), mltext("multCompWarn.logL"),
          "\n\tlog(A) - log(B)", sep = "")
      },
      "GLM-P" = {
        paste("\t", mltext("multCompWarn.link"), mltext("multCompWarn.logL"),
          "\n\tlog(A) - log(B)", sep = "")
      },
      "GLM-B" = {
        paste("\t", mltext("multCompWarn.link"), mltext("multCompWarn.logitL"),
          "", sep = "")
      },
      "GLM-Ga" = {
        paste("\t", mltext("multCompWarn.link"), mltext("multCompWarn.invL"),
          "\n\t(1/A) - (1/B)\t=>\t*", mltext("multCompWarn.invSign"),"*", sep = "")
      },
      ""),
    file = resFile)
}


diffTemporelles.f <- function(objLM, factSpatial, factTemp, Data, exclude){

  ## Purpose: Calcule et retourne la matrice de diff?rences temporelles par
  ##          statut(pour une utilisation avec la fonction 'glht').
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
  ##            factSpatial : nom du facteur spatial.
  ##            factTemp : nom du facteur temporel.
  ##            Data : donn?es utilis?es pour ajuster le mod?le.
  ##            exclude : facteur non analys?.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 sept. 2010, 11:11

  # Coefficients :
  theta <- coef(objLM)

  tDiff <- paste(c(head(rev(levels(Data[, factTemp])), 1), head(rev(levels(Data[, factTemp])), -1)),
    c(tail(rev(levels(Data[, factTemp])), 1), tail(rev(levels(Data[, factTemp])), -1)),
    sep = " - ")

  # Matrice pour construire les diff?rences entre coefficients :
  Dtemp <- matrix(0,
    # Il y a autant de différences temporelles que de niveaux pour la variable temporelle
    # (en raison de la différence supplémentaire final - initial) :
    nrow = nlevels(Data[ , factTemp]) * nlevels(Data[ , factSpatial]),
    ncol = length(theta))

  # Noms des colonnes (pas obligatoire mais utile pour v?rification) :
  row.names(Dtemp) <- paste(rep(levels(Data[ , factSpatial]), each = nlevels(Data[ , factTemp])),
    tDiff,
    sep = " : ")


  colnames(Dtemp) <- names(theta)

  # Noms de colonnes ordonn?s :
  namesCol <- c(colnames(Data)[1], colnames(objLM$model[ , -1]))

  namesCol <- namesCol[! is.element(namesCol, exclude)] # - colonne exclue

  # Calculs des nombres de colonnes des facteurs et int?raction :
  nlev <- combn(sapply(Data[ , namesCol],
    function(x){
      ifelse(is.factor(x),
        nlevels(x) - 1,
        1)
    }),
    2)

  # Nombre de colonnes par type de facteur/interaction :
  nCol <- apply(nlev, 2, prod)

  # Position de la premi?re colonne
  premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

  # Position des facteurs d'intérêt et leur interaction,
  # dans l'ordre de l'ensemble des facteurs et interactions :
  facts <- c(factSpatial, factTemp)
  posTemp <- which(attr(objLM$terms, "term.labels") == factTemp)
  posSpatial <- which(attr(objLM$terms, "term.labels") == factSpatial)
  posInteraction <- which(is.element(attr(objLM$terms, "term.labels"),
    paste(facts, rev(facts), sep = ":")))

  # Différences sur l'effet temporel seul :
  d1 <- rbind(c(-1, rep(0, nCol[posTemp] - 1), 1),
    cbind(0, diag(1, nCol[posTemp])[ , seq(nCol[posTemp], 1)]) +
      cbind(diag(-1, nCol[posTemp])[ , seq(nCol[posTemp], 1)], 0))[ , -1]

  Dtemp[ , seq(from = premiereCol[posTemp],
    length.out = nCol[posTemp])] <- sapply(as.data.frame(d1), rep, nlevels(Data[ , factSpatial]))


  # Différences sur les interactions :
  d2 <- Dtemp[ , seq(from = premiereCol[posInteraction],
    length.out = nCol[posInteraction]), drop = FALSE]

  l <- nlevels(Data[ , factTemp]) + 1
  for (i in seq(from = 0, length.out = nCol[posSpatial])){
    if (posSpatial > posTemp){       # traitement différent selon l'imbrication des facteurs :
                                     # Cas où le facteur temporel est en premier :
      d2[seq(from = l, length.out = nlevels(Data[ , factTemp])) ,
         seq(from = 1, length.out = nCol[posTemp]) + i * nCol[posTemp]] <- d1
    }else{                          #... cas où il est en second :
      d2[seq(from = l, length.out = nlevels(Data[ , factTemp])) ,
         seq(from = 1 + i, by = nCol[posSpatial], length.out = nCol[posTemp])] <- d1
    }

    l <- l + nlevels(Data[ , factTemp])
  }

  Dtemp[ , seq(from = premiereCol[posInteraction], length.out = nCol[posInteraction])] <- d2

  return(Dtemp)

}


diffSpatiales.f <- function(objLM, factSpatial, factTemp, Data, exclude){

  ## Purpose: Calcule et retourne la matrice de différences spatiales par
  ##          année (pour une utilisation avec la fonction 'glht').
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
  ##            factSpatial : nom du facteur spatial.
  ##            factTemp : nom du facteur temporel.
  ##            Data : données utilisées pour ajuster le modèle.
  ##            exclude : facteur non analysé.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  7 sept. 2010, 16:15

  # Coefficients :
  theta <- coef(objLM)

  # Nom des différences spatiales (statut de protection) :
  sDiff <- apply(combn(levels(Data[ , factSpatial]), 2), 2,
    function(x){
      paste(rev(x), collapse = " - ")
    })

  # Matrice pour construire les différences entre coefficients :
  Dspat <- matrix(0,
    nrow = nlevels(Data[ , factTemp]) * choose(nlevels(Data[ , factSpatial]), 2),
    ncol = length(theta))

  # Noms des colonnes (pas obligatoire mais utile pour vérification) :
  row.names(Dspat) <- paste(levels(Data[ , factTemp]),
    rep(sDiff, each = nlevels(Data[ , factTemp])), sep = " : ")
  colnames(Dspat) <- names(theta)

  # Noms de colonnes ordonnés :
  namesCol <- c(colnames(Data)[1], colnames(objLM$model[ , -1]))

  namesCol <- namesCol[! is.element(namesCol, exclude)] # - colonne exclue

  # Calculs des nombres de colonnes des facteurs et intéraction :
  nlev <- combn(sapply(Data[ , namesCol],
    function(x){
      ifelse(is.factor(x),
        nlevels(x) - 1,
        1)
    }),
    2)

  # Nombre de colonnes par type de facteur/interaction :
  nCol <- apply(nlev, 2, prod)

  # Position de la première colonne
  premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

  # Position des facteurs d'intérêt et leur interaction,
  # dans l'ordre de l'ensemble des facteurs et interactions :
  facts <- c(factSpatial, factTemp)
  posTemp <- which(attr(objLM$terms, "term.labels") == factTemp)
  posSpatial <- which(attr(objLM$terms, "term.labels") == factSpatial)
  posInteraction <- which(is.element(attr(objLM$terms, "term.labels"),
    paste(facts, rev(facts), sep = ":")))

  # Différences entres les effets statuts (sans intéraction temporelles) :
  tmp <- sapply(as.data.frame(combn(1:nlevels(Data[ , factSpatial]), 2)),
    function(x){
      m <- matrix(0,
        ncol = nlevels(Data[ , factSpatial]),
        nrow = nlevels(Data[ , factTemp]))
      m[ , x] <- matrix(c(-1, 1),
        nrow = nlevels(Data[ , factTemp]),
        ncol = 2,
        byrow = TRUE)
      return(m)
    }, simplify = FALSE)

  m <- tmp[[1]][NULL, ]
  for(i in 1:length(tmp)){
    m <- rbind(m, tmp[[i]])
  }

  Dspat[ , premiereCol[posSpatial] - 1 + 1:nCol[posSpatial]] <- m[ , -1]

  # Ajout des intéractions :
  tmp2 <- Dspat[ , seq(from = premiereCol[posInteraction], length.out = nCol[posInteraction]),
    drop = FALSE]

  l <- 1
  for (i in as.data.frame(combn(0:nCol[posSpatial], 2))){ # pour chaque combinaison de statut :
    if(i[1] != 0){
      d1 <- rbind(0, diag(-1, nrow = nCol[posTemp]))
      if (posSpatial > posTemp){   # facteur spatial après le facteur temporel...
        tmp2[seq(from = l, length.out = nlevels(Data[ , factTemp])),
          seq(from = (i[1] - 1) * nCol[posTemp] + 1, length.out = nCol[posTemp])] <- d1
      }else{                       # ... avant le facteur temporel.
        tmp2[seq(from = l, length.out = nlevels(Data[ , factTemp])),
          seq(from = i[1], by = nCol[posSpatial] , length.out = nCol[posTemp])] <- d1
      }
    }else{}

    d2 <- rbind(0, diag(1, nrow = nCol[posTemp]))

    if (posSpatial > posTemp){       # facteur spatial après le facteur temporel...
      tmp2[seq(from = l, length.out = nlevels(Data[ , factTemp])),
           seq(from = (i[2] - 1) * nCol[posTemp] + 1, length.out = nCol[posTemp])] <- d2
    }else{                           # ... avant le facteur temporel.
      tmp2[seq(from = l, length.out = nlevels(Data[ , factTemp])),
        seq(from = i[2], by = nCol[posSpatial], length.out = nCol[posTemp])] <- d2
    }

    l <- l + nlevels(Data[ , factTemp])
  }

  # Stockage des différences d'interactions :
  Dspat[ , seq(from = premiereCol[posInteraction], length.out = nCol[posInteraction])] <- tmp2

  return(Dspat)
}


print.summary.glht.red <- function (x, digits = max(3, getOption("digits") - 3), ...){

  if (!is.null(x$observation.type))
    cat(mltext("print.summary.glht.red.KW.multComp"), x$observation.type,
      mltext("print.summary.glht.red.KW.contrast"), "\n\n\n")
  call <- if (isS4(x$model)){
    x$model@call
  }else{
    x$model$call
  }
  cat("\n")
  pq <- x$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ,
    less = paste("Pr(<", ifelse(x$df == 0, "z", "t"), ")", sep = ""),
    greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""),
    two.sided = paste("Pr(>|", ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error",
    ifelse(x$df == 0, "z value", "t value"), pname)
  type <- ifelse(is.null(pq$observation.type) & ! is.null(pq$type), pq$type, pq$observation.type)
  if (!is.null(error) && error > .Machine$double.eps){
    sig <- which.min(abs(1/error - (10^(1:10))))
    sig <- 1/(10^sig)
  }else{
    sig <- .Machine$double.eps
  }
  cat(mltext("print.summary.glht.red.KW.linH"), "\n")
  alt <- switch(x$alternative,
    two.sided = "==",
    less = ">=",
    greater = "<=")
  rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
  printCoefmat(mtests, digits = digits, has.Pvalue = TRUE, P.values = TRUE, eps.Pvalue = sig)
  switch(type,
    univariate = cat(mltext("print.summary.glht.red.KW.pValU")),
    `single-step` = cat(mltext("print.summary.glht.red.KW.pValST")),
    Shaffer = cat(mltext("print.summary.glht.red.KW.pValS")),
    Westfall = cat(mltext("print.summary.glht.red.KW.pValW")),
    cat(mltext("print.summary.glht.red.KW.pValDef"), type, "method)"))
  cat("\n\n")
  invisible(x)
}


compSimplesLM.f <- function(objLM, Data, fact, resFile, Log = FALSE){

  ## Purpose: Calculer et ?crire les r?sultats des comparaisons simples.
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : objet de classe 'lm' ou 'glm'.
  ##            Data : les donn?es utilis?es pour ajuster le mod?le.
  ##            fact : le nom du facteur utilis?.
  ##            resFile : la connection pour les sorties textes.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 oct. 2010, 15:51

  # Ecriture des en-têtes :
  cat("\n\n\n---------------------------------------------------------------------------",
      "\n", mltext("compSimplesLM.header"),
      file = resFile)

  # Avertissement concernant les estimations de diff?rences :
  compMultiplesAvertissement.f(objLM = objLM, Log = Log, resFile = resFile)

  if (is.temporal.f(fact, unitobs)){
    # Suite en-tête :
    cat(paste("\n\n\t", mltext("compSimplesLM.KW.factor"),
      " '", varNames[fact, "nom"], "' (",
      mltext("KW.temporal"),
      ") :\n", sep = ""),
      file = resFile)

    # Comparaisons temporelles :
    compSimple <- glht(objLM,
      linfct = diffTempSimples.f(objLM = objLM, fact = fact, Data = Data),
      alternative = "two.sided")

    # Ecriture des résultats :
    capture.output(print.summary.glht.red(summary(compSimple)), file = resFile)

  }else{
    # Suite en-tête :
    cat(paste("\n\n", mltext("compSimplesLM.KW.factor"),
      " '", varNames[fact, "nom"], "' :\n", sep = ""),
      file = resFile)

    # Comparaisons de toutes les paires ("Tukey") :
    compSimple <- glht(objLM,
      linfct = eval(parse(text = paste("mcp(", fact, "=\"Tukey\")"))),
      alternative = "two.sided")

    # Ecriture des résultats :
    capture.output(print.summary.glht.red(summary(compSimple)), file = resFile)
  }
}


diffTempSimples.f <- function(objLM, fact, Data){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
  ##            factSpatial : nom du facteur spatial.
  ##            factTemp : nom du facteur temporel.
  ##            Data : donn?es utilis?es pour ajuster le mod?le.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  4 oct. 2010, 16:28

  tDiff <- paste(c(head(rev(levels(Data[ , fact])), 1), head(rev(levels(Data[ , fact])),  - 1)),
    c(tail(rev(levels(Data[ , fact])), 1), tail(rev(levels(Data[ , fact])),  - 1)),
    sep = " - ")

  diffDim <- length(coef(objLM))

  diffMat <- matrix(0, ncol = diffDim, nrow = diffDim,
    dimnames = list(tDiff, names(coef(objLM))))


  # Différences sur l'effet temporel seul :
  diffMat[ , -1] <- rbind(c(-1, rep(0, diffDim - 2), 1),
    cbind(0, diag(1, diffDim - 1)[ , seq(diffDim - 1, 1)]) +
      cbind(diag(-1, diffDim - 1)[ , seq(diffDim - 1, 1)], 0))[ , -1]

  return(diffMat)
}


plot.lm.ml <- function (x, which = c(1L:3L, 5L),
  caption = list(
    mltext("plot.lm.ml.title.ResVsVal", language = getOption("P.lang")),
    mltext("plot.lm.ml.title.NQQ", language = getOption("P.lang")),
    mltext("plot.lm.ml.title.SL", language = getOption("P.lang")),
    mltext("plot.lm.ml.title.Cooks", language = getOption("P.lang")),
    mltext("plot.lm.ml.title.ResVsLev", language = getOption("P.lang")),
    substitute(expression(title1 * h[ii]/(1 - h[ii])),
      list(
       title1 = paste0(mltext("plot.lm.ml.title.CooksVsLev", language = getOption("P.lang")), "  ")
    ))),
    panel = if (add.smooth) panel.smooth else points, sub.caption = NULL,
    main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(),
    ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
    qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"),
    label.pos = c(4, 2), cex.caption = 1,
    print.sub.caption = FALSE){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##           + print.sub.caption : Imprimer le sub.caption automatique.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 sept. 2010, 09:46

  dropInf <- function(x, h) {
    if (any(isInf <- h >= 1)) {
      warning("Not plotting observations with leverage one:\n  ",
        paste(which(isInf), collapse = ", "), call. = FALSE)
      x[isInf] <- NaN
    }
    x
  }
  if (!inherits(x, "lm"))
    stop("use only with \"lm\" objects")
  if (!is.numeric(which) || any(which < 1) || any(which > 6))
    stop("'which' must be in 1:6")
  isGlm <- inherits(x, "glm")
  show <- rep(FALSE, 6)
  show[which] <- TRUE
  r <- residuals(x)
  yh <- predict(x)
  w <- weights(x)
  if (!is.null(w)) {
    wind <- w != 0
    r <- r[wind]
    yh <- yh[wind]
    w <- w[wind]
    labels.id <- labels.id[wind]
  }
  n <- length(r)
  if (any(show[2L:6L])) {
    s <- if (inherits(x, "rlm"))
      x$s
    else if (isGlm)
      sqrt(summary(x)$dispersion)
    else sqrt(deviance(x)/df.residual(x))
    hii <- lm.influence(x, do.coef = FALSE)$hat
    if (any(show[4L:6L])) {
      cook <- if (isGlm)
        cooks.distance(x)
      else cooks.distance(x, sd = s, res = r)
    }
  }
  if (any(show[2L:3L])) {
    ylab23 <- if (isGlm)
      mltext("plot.lm.ml.lab.devRes", language = getOption("P.lang"))      # [!!!]
    else mltext("plot.lm.ml.lab.stdRes", language = getOption("P.lang"))
    r.w <- if (is.null(w))
      r
    else sqrt(w) * r
    rs <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
  }
  if (any(show[5L:6L])) {
    r.hat <- range(hii, na.rm = TRUE)
    isConst.hat <- all(r.hat == 0) || diff(r.hat) < 1e-10 *
      mean(hii, na.rm = TRUE)
  }
  if (any(show[c(1L, 3L)]))
    l.fit <- if (isGlm)
      mltext("plot.lm.ml.lab.pred", language = getOption("P.lang"))
  else mltext("plot.lm.ml.lab.adj", language = getOption("P.lang"))
  if (is.null(id.n))
    id.n <- 0
  else {
    id.n <- as.integer(id.n)
    if (id.n < 0L || id.n > n)
      stop(gettextf("'id.n' must be in {1,..,%d}", n), domain = NA)
  }
  if (id.n > 0L) {
    if (is.null(labels.id))
      labels.id <- paste(1L:n)
    iid <- 1L:id.n
    show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
    if (any(show[2L:3L]))
      show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
    text.id <- function(x, y, ind, adj.x = TRUE) {
      labpos <- if (adj.x)
        label.pos[1 + as.numeric(x > mean(range(x)))]
      else 3
      text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
        pos = labpos, offset = 0.25)
    }
  }
  getCaption <- function(k) if (length(caption) < k)
    NA_character_
  else as.graphicsAnnot(caption[[k]])
  if (is.null(sub.caption)) {
    cal <- x$call
    if (!is.na(m.f <- match("formula", names(cal)))) {
      cal <- cal[c(1, m.f)]
      names(cal)[2L] <- ""
    }
    cc <- deparse(cal, 80)
    nc <- nchar(cc[1L], "c")
    abbr <- length(cc) > 1 || nc > 75
    sub.caption <- if (abbr)
      paste(substr(cc[1L], 1L, min(75L, nc)), "...")
    else cc[1L]
  }
  one.fig <- prod(par("mfcol")) == 1
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  if (show[1L]) {
    ylim <- range(r, na.rm = TRUE)
    if (id.n > 0)
      ylim <- extendrange(r = ylim, f = 0.08)
    plot(yh, r,
      xlab = l.fit, ylab = mltext("plot.lm.ml.lab.res", language = getOption("P.lang")),
      main = main,
      ylim = ylim, type = "n", ...)
    panel(yh, r, ...)
    if (one.fig)
      title(sub = sub.caption, ...)
    mtext(getCaption(1), 3, 0.25, cex = cex.caption)
    if (id.n > 0) {
      y.id <- r[show.r]
      y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
      text.id(yh[show.r], y.id, show.r)
    }
    abline(h = 0, lty = 3, col = "gray")
  }
  if (show[2L]) {
    ylim <- range(rs, na.rm = TRUE)
    ylim[2L] <- ylim[2L] + diff(ylim) * 0.075
    qq <- qqnorm(rs, main = main, ylab = ylab23, ylim = ylim,
      xlab = mltext("plot.lm.ml.lab.theoQ", language = getOption("P.lang")),
      ...)
    if (qqline)
      qqline(rs, lty = 3, col = "gray50")
    if (one.fig)
      title(sub = sub.caption, ...)
    mtext(getCaption(2), 3, 0.25, cex = cex.caption)
    if (id.n > 0)
      text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
  }
  if (show[3L]) {
    sqrtabsr <- sqrt(abs(rs))
    ylim <- c(0, max(sqrtabsr, na.rm = TRUE))
    yl <- as.expression(substitute(sqrt(abs(YL)), list(YL = as.name(ylab23))))
    yhn0 <- if (is.null(w))
      yh
    else yh[w != 0]
    plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main, ylim = ylim, type = "n", ...)
    panel(yhn0, sqrtabsr, ...)
    if (one.fig)
      title(sub = sub.caption, ...)
    mtext(getCaption(3), 3, 0.25, cex = cex.caption)
    if (id.n > 0)
      text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
  }
  if (show[4L]) {
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      ymx <- cook[show.r[1L]] * 1.075
    }
    else ymx <- max(cook, na.rm = TRUE)
    plot(cook, type = "h", ylim = c(0, ymx), main = main,
      xlab = mltext("plot.lm.ml.lab.obsN", language = getOption("P.lang")),
      ylab = mltext("plot.lm.ml.lab.CooksD", language = getOption("P.lang")),
      ...)
    if (one.fig)
      title(sub = sub.caption, ...)
    mtext(getCaption(4), 3, 0.25, cex = cex.caption)
    if (id.n > 0)
      text.id(show.r, cook[show.r], show.r, adj.x = FALSE)
  }
  if (show[5L]) {
    ylab5 <- if (isGlm)
      mltext("plot.lm.ml.lab.stdResP", language = getOption("P.lang"))
    else mltext("plot.lm.ml.lab.stdRes", language = getOption("P.lang"))
    r.w <- residuals(x, "pearson")
    if (!is.null(w))
      r.w <- r.w[wind]
    rsp <- dropInf(r.w/(s * sqrt(1 - hii)), hii)
    ylim <- range(rsp, na.rm = TRUE)
    if (id.n > 0) {
      ylim <- extendrange(r = ylim, f = 0.08)
      show.rsp <- order(-cook)[iid]
    }
    do.plot <- TRUE
    if (isConst.hat) {
      if (missing(caption))
        caption[[5L]] <- mltext("plot.lm.ml.lab.cstLev", language = getOption("P.lang"))
      aterms <- attributes(terms(x))
      dcl <- aterms$dataClasses[-aterms$response]
      facvars <- names(dcl)[dcl %in% c("factor", "ordered")]
      mf <- model.frame(x)[facvars]
      if (ncol(mf) > 0) {
        effM <- mf
        for (j in seq_len(ncol(mf))) effM[, j] <- sapply(split(yh, mf[, j]), mean)[mf[, j]]
        ord <- do.call(order, effM)
        dm <- data.matrix(mf)[ord, , drop = FALSE]
        nf <- length(nlev <- unlist(unname(lapply(x$xlevels, length))))
        ff <- if (nf == 1)
          1
        else rev(cumprod(c(1, nlev[nf:2])))
        facval <- ((dm - 1) %*% ff)
        facval[ord] <- facval
        xx <- facval
        plot(facval, rsp,
          xlim = c(-1/2, sum((nlev - 1) * ff) + 1/2), ylim = ylim,
          xaxt = "n", main = main,
          xlab = mltext("plot.lm.ml.lab.levComb", language = getOption("P.lang")),
          ylab = ylab5,
          type = "n", ...)
        axis(1, at = ff[1L] * (1L:nlev[1L] - 1/2) - 1/2,
           labels = x$xlevels[[1L]][order(sapply(split(yh, mf[, 1]), mean))])
        mtext(paste(facvars[1L], ":"), side = 1, line = 0.25, adj = -0.05)
        abline(v = ff[1L] * (0:nlev[1L]) - 1/2, col = "gray", lty = "F4")
        panel(facval, rsp, ...)
        abline(h = 0, lty = 3, col = "gray")
      }
      else {
        message("hat values (leverages) are all = ",
          format(mean(r.hat)), "\n and there are no factor predictors; no plot no. 5")
        frame()
        do.plot <- FALSE
      }
    }
    else {
      xx <- hii
      xx[xx >= 1] <- NA
      plot(xx, rsp, xlim = c(0, max(xx, na.rm = TRUE)),
        ylim = ylim, main = main,
        xlab = mltext("plot.lm.ml.lab.Lev", language = getOption("P.lang")),
        ylab = ylab5, type = "n", ...)
      panel(xx, rsp, ...)
      abline(h = 0, v = 0, lty = 3, col = "gray")
      if (one.fig)
        title(sub = sub.caption, ...)
      if (length(cook.levels)) {
        p <- length(coef(x))
        usr <- par("usr")
        hh <- seq.int(min(r.hat[1L], r.hat[2L]/100), usr[2L], length.out = 101)
        for (crit in cook.levels) {
          cl.h <- sqrt(crit * p * (1 - hh)/hh)
          lines(hh, cl.h, lty = 2, col = 2)
          lines(hh, -cl.h, lty = 2, col = 2)
        }
        legend("bottomleft",
          legend = mltext("plot.lm.ml.lab.CooksD", language = getOption("P.lang")),
          lty = 2, col = 2, bty = "n")
        xmax <- min(0.99, usr[2L])
        ymult <- sqrt(p * (1 - xmax)/xmax)
        aty <- c(-sqrt(rev(cook.levels)) * ymult, sqrt(cook.levels) * ymult)
        axis(4, at = aty, labels = paste(c(rev(cook.levels), cook.levels)),
          mgp = c(0.25, 0.25, 0), las = 2,
          tck = 0, cex.axis = cex.id, col.axis = 2)
      }
    }
    if (do.plot) {
      mtext(getCaption(5), 3, 0.25, cex = cex.caption)
      if (id.n > 0) {
        y.id <- rsp[show.rsp]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(xx[show.rsp], y.id, show.rsp)
      }
    }
  }
  if (show[6L]) {
    g <- dropInf(hii/(1 - hii), hii)
    ymx <- max(cook, na.rm = TRUE) * 1.025
    plot(g, cook, xlim = c(0, max(g, na.rm = TRUE)), ylim = c(0, ymx), main = main,
      ylab = mltext("plot.lm.ml.lab.CooksD", language = getOption("P.lang")),
      xlab = substitute(expression(title1 * h[ii]),
        list(title1 = paste0(mltext("plot.lm.ml.lab.Lev", language = getOption("P.lang")),
          "  "))),
      xaxt = "n", type = "n", ...)
    panel(g, cook, ...)
    athat <- pretty(hii)
    axis(1, at = athat/(1 - athat), labels = paste(athat))
    if (one.fig)
      title(sub = sub.caption, ...)
    p <- length(coef(x))
    bval <- pretty(sqrt(p * cook/g), 5)
    usr <- par("usr")
    xmax <- usr[2L]
    ymax <- usr[4L]
    for (i in seq_along(bval)) {
      bi2 <- bval[i]^2
      if (ymax > bi2 * xmax) {
        xi <- xmax + strwidth(" ")/3
        yi <- bi2 * xi
        abline(0, bi2, lty = 2)
        text(xi, yi, paste(bval[i]), adj = 0, xpd = TRUE)
      }
      else {
        yi <- ymax - 1.5 * strheight(" ")
        xi <- yi/bi2
        lines(c(0, xi), c(0, yi), lty = 2)
        text(xi, ymax - 0.8 * strheight(" "), paste(bval[i]), adj = 0.5, xpd = TRUE)
      }
    }
    mtext(getCaption(6), 3, 0.25, cex = cex.caption)
    if (id.n > 0) {
      show.r <- order(-cook)[iid]
      text.id(g[show.r], cook[show.r], show.r)
    }
  }
  if (!one.fig && par("oma")[3L] >= 1 && print.sub.caption)
    mtext(sub.caption, outer = TRUE, cex = 1.25)
  invisible()
}


supprimeObs.f <- function(residus){

  ## Purpose: Choisir des observations à supprimer d'après leur résidus.
  ## ----------------------------------------------------------------------
  ## Arguments: residus : un vecteur de résidus avec les numéros
  ##                        d'observations en noms (obtenus par la fonction
  ##                        'boxplot(...)$out' par exemple).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 sept. 2010, 13:52

  Done <- tcltk::tclVar("0")
  res <- NULL

  WinSuppr <- tcltk::tktoplevel()
  tcltk::tkwm.title(WinSuppr, mltext("supprimeObs.WT"))

  FrameB <- tcltk::tkframe(WinSuppr)
  B.Oui <- tcltk::tkbutton(FrameB, text = paste0("    ", mltext("KW.yes"), "    "),
    command = function(){
      tcltk::tclvalue(Done) <- "1"
  })
  B.Non <- tcltk::tkbutton(FrameB, text = paste0("    ", mltext("KW.no"), "    "),
    command = function(){
      tcltk::tclvalue(Done) <- "2"
  })

  tcltk::tkgrid(tcltk::tklabel(WinSuppr, text = "\t"),
    tcltk::tklabel(WinSuppr, text = paste("\n",
      mltext("supprimeObs.L.1"), "\n",
      mltext("supprimeObs.L.2"), "\n",
      sep = "")),
    tcltk::tklabel(WinSuppr, text = "\t"),
    sticky = "w")

  tcltk::tkgrid(FrameB, column = 1)
  tcltk::tkgrid(B.Oui, tcltk::tklabel(FrameB, text = "\t\t\n"), B.Non)


  tcltk::tkbind(WinSuppr, "<Destroy>", function(){
    tcltk::tclvalue(Done) <- "2"
  })

  tcltk::tkfocus(WinSuppr)

  tcltk::tkwait.variable(Done)

  if (tcltk::tclvalue(Done) == "1"){
    tcltk::tkdestroy(WinSuppr)
    # Sélection des observations par l'utilisateur :
    select <- selectModWindow.f("residus",
      data = if (any(is.na(tryCatch(as.integer(names(residus)), warning = function(w){NA})))){
      # Si les noms de lignes correspondent à des noms d'unitobs...
        data.frame(residus = paste("Unitobs. ",
          names(sort(abs(residus), decreasing = TRUE)),
          "  (",
          format(residus[order(abs(residus), decreasing = TRUE)], digits = 3),
          ")",
          sep = ""))
      }else{ # ...si ce sont des numéros :
        data.frame(residus = paste("Obs. ",
          format(as.integer(names(sort(abs(residus), decreasing = TRUE))),
            width = ceiling(log(max(as.integer(names(residus))), 10)),
            justify = "right"),
          "  (",
          format(residus[order(abs(residus), decreasing = TRUE)], digits = 3),
          ")",
          sep = ""))
      },
      sort = FALSE, selectmode = "extended",
      title = mltext("supprimeObs.selectModWindow.WT"),
      label = paste(mltext("supprimeObs.selectModWindow.L.1"),
        mltext("supprimeObs.selectModWindow.L.2"), sep = ""))

    if (length(select)){
      if (any(is.na(tryCatch(as.integer(names(residus)), warning = function(w){NA})))){
        # Si les noms de lignes correspondent à des noms d'unitobs...
        res <- sub("^Unitobs.[[:blank:]]+([^[:blank:]]+)[[:blank:]]+.*", "\\1", select, perl = TRUE)
      }else{
        # ...sinon, numéro des observations à supprimer : [!!!] on pourrait s'en passer (à régler plus tard).
        res <- as.integer(sub("^Obs.[[:blank:]]+([[:digit:]]+)[[:blank:]]+.*", "\\1",
          select, perl = TRUE))
      }
    }else{}
  }else{
    tcltk::tkdestroy(WinSuppr)
  }

  return(res)
}


#' @title Modele lineaire (donnees agregees par unites d'observation)
#'
#' @description Cette fonction permet de faire des modeles lineaires sur des donnees agregees
#' par unites d'observation
#'
#' @param metrique chr, metrique choisie
#' @param factAna chr, facteur de separation des graphiques
#' @param factAnaSel chr, selection de modalites pour le facteur de separation des graphiques
#' @param listFact chr, facteur(s) de regroupement
#' @param listFactSel chr, modalites selectionnees pour le(s) facteur(s) de regroupement
#' @param tableMetrique chr, nom de la table de metrique
#' @param dataEnv environnement de stockage des donnees
#' @param baseEnv environnement parent
#'
#' @noRd

modeleLineaireWP2.unitobs.f <- function(metrique, factAna, factAnaSel, listFact, listFactSel,
  tableMetrique, new_window = TRUE, dataEnv, baseEnv = .GlobalEnv){

  ## Purpose: Gestions des différentes étapes des modèles linéaires.
  ## ----------------------------------------------------------------------
  ## Arguments: metrique : la métrique choisie.
  ##            factAna : le facteur de séparation des graphiques.
  ##            factAnaSel : la sélection de modalités pour ce dernier
  ##            listFact : liste du (des) facteur(s) de regroupement
  ##            listFactSel : liste des modalités sélectionnées pour ce(s)
  ##                          dernier(s)
  ##            tableMetrique : nom de la table de métriques.
  ##            dataEnv : environnement de stockage des données.
  ##            baseEnv : environnement de l'interface.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 18 août 2010, 15:59

  # Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
  listFactSel <- listFactSel[unlist(listFact) != ""]

  listFact <- listFact[unlist(listFact) != ""]
  # Concaténation
  facteurs <- c(factAna, unlist(listFact)) # Concaténation des facteurs

  selections <- c(list(factAnaSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

  # Données pour la série d'analyses :
  if (tableMetrique == "unit"){
    # Pour les indices de biodiversité, il faut travailler sur les nombres... :
    tmpData <- subsetToutesTables.f(metrique = getOption("P.nbName"), facteurs = facteurs,
      selections = selections, dataEnv = dataEnv, tableMetrique = "unitSp",
      exclude = NULL, add = c("observation.unit", "species.code"))
  }else{
    # ...sinon sur la métrique choisie :
    tmpData <- subsetToutesTables.f(metrique = metrique, facteurs = facteurs, selections = selections,
      dataEnv = dataEnv, tableMetrique = tableMetrique, exclude = NULL,
      add = c("observation.unit", "species.code"))
  }

  # Identification des différents lots d'analyses à faire:
  if (factAna == ""){                # Pas de facteur de séparation des graphiques.
    iFactGraphSel <- ""
  }else{
    if (is.na(factAnaSel[1])){       # Toutes les modalités.
      iFactGraphSel <- unique(as.character(sort(tmpData[ , factAna])))
    }else{                      # Modalités sélectionnées (et présentes parmi les données retenues).
      iFactGraphSel <- factAnaSel[is.element(factAnaSel, tmpData[ , factAna])]
    }
  }
  # Formules pour différents modèles (avec ou sans transformation log) :
  exprML <- eval(parse(text = paste(metrique, "~", paste(listFact, collapse = " * "))))
  logExprML <- eval(parse(text = paste("log(", metrique, ") ~", paste(listFact, collapse = " * "))))

  # Agrégation des observations / unité d'observation :
  if (tableMetrique == "unitSpSz" && factAna != "size.class"){
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
              unitobs = "observation.unit",
              code.especes = "species.code",
              nombres = getOption("P.nbName"),
              indices = metrique,
              dataEnv = dataEnv)
          }))

      # On rajoute les anciennes colonnes :
      tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
        tmpData[match(tmp$observation.unit, tmpData$observation.unit),
          !is.element(colnames(tmpData),
            c(colnames(tmp), getOption("P.nbName"), "species.code")), drop = FALSE])

      # On garde le strict minimum :
      tmpData <- tmpData[ , is.element(colnames(tmpData), c(metrique, facteurs))]
    }else{
      tmpData <- na.omit(
        agregationTableParCritere.f(Data = tmpData,
          metrique = metrique,
          facteurs = c("observation.unit"),
          dataEnv = dataEnv,
          listFact = listFact))
    }
  }

  # Sauvegarde temporaire des données utilisées pour les analyses
  # (attention : écrasée à chaque nouvelle série de graphiques) :
  DataBackup <<- list(tmpData)


  # Suppression des 'levels' non utilisés :
  tmpData <- dropLevels.f(tmpData)

  # Aide au choix du type d'analyse :
  if (metrique == "pres.abs"){
    loiChoisie <- "BI"
  }else{
    loiChoisie <- choixDistri.f(metrique = metrique, Data = tmpData[ , metrique, drop = FALSE])
  }

  if (!is.null(loiChoisie)){
    message(mltext("modeleLineaireWP2.esp.dist"), " = ", loiChoisie)

    if (is.element(loiChoisie, c("LOGNO"))){
      Log <- TRUE
      formule <- logExprML
    }else{
      Log <- FALSE
      formule <- exprML
    }

    res <- calcLM.f(loiChoisie = loiChoisie, formule = formule, metrique = metrique, Data = tmpData)

    # Écriture des résultats formatés dans un fichier :
    tryCatch(
      sortiesLM.f(objLM = res, formule = formule, metrique = metrique,
        factAna = factAna, modSel = iFactGraphSel,
        listFact = listFact, listFactSel = listFactSel,
        Data = tmpData, dataEnv = dataEnv, Log = Log,
        type = ifelse(tableMetrique == "unitSpSz" && factAna != "size.class",
          "CL_unitobs",
          "unitobs"),
        new_window = new_window,
        baseEnv = baseEnv),
      error = function(e){
        errorLog.f()
        print("error 1")
      }
    )

    resid.out <- boxplot(residuals(res), plot = FALSE)$out

    if (length(resid.out)){
      suppr <- supprimeObs.f(residus = resid.out)

      if(!is.null(suppr)){
        if (!is.numeric(suppr)){ # conversion en numéros de lignes lorsque ce sont des noms :

          suppr <- which(is.element(row.names(tmpData), suppr))
        }else{}

        tmpData <- tmpData[ - suppr, ]
        res.red <- calcLM.f(loiChoisie = loiChoisie, formule = formule,
          metrique = metrique, Data = tmpData)

        resLM.red <<- res.red

        tryCatch(
          sortiesLM.f(objLM = res.red, formule = formule, metrique = metrique,
            factAna = factAna, modSel = iFactGraphSel,
            listFact = listFact, listFactSel = listFactSel,
            Data = tmpData, Log = Log, sufixe = "(red)",
            type = ifelse(tableMetrique == "unitSpSz" && factAna != "size.class",
              "CL_unitobs",
              "unitobs"),
            new_window = new_window,
            dataEnv = dataEnv, baseEnv = baseEnv),
          error = function(e){
            errorLog.f()
            print("error 2")
          }
        )

      }else{}

    }else{}

  }else{
    message("Annulé !")
  }
}
