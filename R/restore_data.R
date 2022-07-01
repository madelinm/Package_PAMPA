#' Fonction pour la restauration des donnees apres la selection et le recalcul.
#'
#' Permet de faire la restauration des donnees. Cela revient a re-charger les donnees, mais en plus
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

  options(P.selection = FALSE)

  print(mltext("restoreData.info"))

  return(get("backup", envir = dataEnv))
}
