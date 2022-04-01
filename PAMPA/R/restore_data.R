#' Fonction pour la restauration des donnees apres la selection et le recalcul.
#' Vraiment utile ? Un loadData.f suffit pour ca...
#' Et si on enlève les environnement, sera tocké dans une variable...


#' @title Selection et recalcul
#'
#' @description Permet la selection des donnees d'observation, selon un critere du
#' referentiel d'unites d'observation
#'
#' @param baseEnv : environnement parent
#' @param dataEnv : environnement de stockage des donnees
#'
#' @return none
#'
#' @examples
#' restoreData.f(.baseEnv, .dataEnv)
#'
#' @export

restoreData.f <- function(dataEnv, baseEnv){

  ## Purpose: Restauration des données originales (avant sélection selon un
  ##          ou plusieurs critères).
  ## ---------------------------------------------------------------------------
  ## Arguments: baseEnv : environnement de l'interface principale.
  ##            dataEnv : environnement des données.
  ## ---------------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  6 janv. 2012, 15:44

  listInEnv.f(list = get("backup", envir = dataEnv), env = dataEnv)

#  updateInterface.restore.f(criterion = "Tout",
#    tabObs = get("obs", envir = dataEnv),
#    baseEnv = baseEnv)

  options(P.selection = FALSE)

#  add.logFrame.f(msgID = "restauration", env = baseEnv)

#  tcltk::tkmessageBox(message = paste0(mltext("restoreData.info")# , dim(obs)[1],
#    # "enregistrements dans la table des observations"
#    ))

  print(mltext("restoreData.info"))
  return(get("backup", envir = dataEnv))

#  gestionMSGaide.f("SelectionOuTraitement", env = baseEnv

}


#listInEnv.f <- function(list, env){
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

