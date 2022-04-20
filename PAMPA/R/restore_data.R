#' Fonction pour la restauration des donnees apres la selection et le recalcul.
#'
#' Permet de faire la restauration des donnees. Cela revient à re-charger les donnees, mais en plus
#' rapide.


#' @title Restauration des donnees
#'
#' @description Permet la restauration des donnees.
#'
#' @param dataEnv : environnement de stockage des donnees
#' @param baseEnv : environnement parent
#'
#' @return list, liste contenant les tables de donnees selon la selection.
#'  \itemize{unitobs}{ : chr, fichier unitobs}
#'  \itemize{obs}{ : chr, fichier d'observation}
#'  \itemize{refesp}{ : chr, referentiel especes}
#'
#' @examples
#' restoreData.f(.dataEnv, .baseEnv)
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

