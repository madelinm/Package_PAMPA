# Liste de fonctions appelées à différents endroits dans le code.
# Ces fonctions concernent la traduction du package (anglais/français), la typographie(majuscules /
# minuscules), la modification de fonctions déjà existantes, les fichiers log...


aliases <- function(fieldID, language = tolower(getOption("P.GUIlang")), reverse = FALSE){

  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 Dec 2018, 23:19

  aliases <- read.csv(system.file("file_translation/Field_aliases.csv", package = "PAMPA"),
    stringsAsFactor = FALSE, row.names = 1)
  colnames(aliases) <- tolower(colnames(aliases))

  lang <- ifelse(language %in% colnames(aliases),
    language,
    "en")

  als <- sapply(fieldID,
    function(i){
      res <- if (! i %in% row.names(aliases)){
        i
      }else{
        ifelse(test = nchar(aliases[i, lang]),
          yes = aliases[i, lang],
          no = ifelse(test = nchar(aliases[i, "en"]),
            yes = aliases[i, "en"],
            no = i))
      }
      return(res)
  })

  if (isTRUE(reverse)){
    alsTmp <- als
    als <- names(alsTmp)
    names(als) <- alsTmp
  }

  return(als)
}


Capitalize.f <- function(x, words = FALSE){

  ## Purpose: Mettre en majuscule la première lettre de chaque mot
  ## ----------------------------------------------------------------------
  ## Arguments: x : une chaîne de caractères
  ##            words : tous les mots (TRUE), ou juste le premier.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  9 août 2010, 21:08

  if (words){
    s <- strsplit(x, " ")[[1]]
  }else{
    s <- x
  }

  return(paste(toupper(substring(s, 1,1)), substring(s, 2),
    sep = "", collapse = " "))
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


has.no.pres.abs <- function(nextStep, tableMetrique, dataEnv){

  ## Purpose: si les présences/absences doivent ne pas être affichées,
  ##          renvoie "pres.abs", NULL sinon
  ## ----------------------------------------------------------------------
  ## Arguments: nextStep : l'identifiant de l'étape suivante
  ##            tableMetrique : la table de métrique.
  ##            dataEnv : l'environnement des données.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 12 oct. 2010, 09:07
  if (is.element(nextStep, c("boxplot.esp", "boxplot.unitobs",
    "barplot.esp", "barplot.unitobs")) |     # pas proposé si on fait des boxplots.
    length(unique(na.omit(get(tableMetrique, # "            " une seule modalité.
      envir = dataEnv)$pres.abs))) < 2)
  {
    return("pres.abs")                       # fonctionnement "inversé" !
  }else{
    return(NULL)
  }
}


is.benthos.f <- function(){

  ## Purpose: Raccourci pour tester s'il s'agit d'un jeu de données
  ##          benthos.
  ##          Remarque : liste des types "benthiques" susceptible
  ##          d'évoluer.
  ## ----------------------------------------------------------------------
  ## Arguments: Aucun
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 17 août 2010, 16:19

  benthTypes <- c("LIT")
  return(all(is.element(getOption("P.obsType"), benthTypes)))
}


is.peche.f <- function(){

  ## Purpose: Définir s'il s'agit d'un jeu de données "pêche".
  ## ----------------------------------------------------------------------
  ## Arguments: aucun
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 19 oct. 2010, 15:45

  if (length(getOption("P.obsType")) > 1){
    stop("Several observation types")
  }else{
    return(is.element(as.character(getOption("P.obsType")),
      c("EMB", "DEB", "PSCI", "PecRec")))
  }
}


mltext <- function(msgid, language = tolower(getOption("P.GUIlang"))){

  ## Purpose: Fetching translation of GUI text in the current language
  ##          (option). Uses English if language/msg not defined
  ## ----------------------------------------------------------------------
  ## Arguments: msgid: unique identifier of the text message to fetch
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  9 Jul 2018, 15:38

  transl <- read.csv(system.file("file_translation/Translations.csv", package = "PAMPA"),
    stringsAsFactor = FALSE, row.names = 1)
  colnames(transl) <- tolower(colnames(transl))

  lang <- ifelse(isTRUE(language %in% colnames(transl)),
    language,
    "en")

  msg <- sapply(msgid,
    function(i){
      res <- if (! i %in% row.names(transl)){
        " !! No Text !! "
      }else{
        ifelse(test = nchar(transl[i, lang]),
          yes = gsub("\\\\n", "\n",
            gsub("\\\\t", "\t",
              transl[i, lang])),
          # If not defined in the desired language, tests if mandatory:
          no = ifelse(test = (nchar(transl[i, "en"]) && transl[i, "mandatory"]),
            # English used if missing and mandatory, and defined in English:
            yes = gsub("\\\\n", "\n",
              gsub("\\\\t", "\t",
                transl[i, "en"])),
            no = ifelse(transl[i, "mandatory"],
              # ...generic text if mandatory
              #    AND not defined in English:
              " !! No Text !! ",
              ""))) # Kept empty if not mandatory!
      }
      return(res)
    })

    return(unname(msg))
}


varNames.f <- function(fields, info = "name", quote = TRUE){

  ## Purpose: revoyer les informations (en particulier nom) sur le nom
  ##          "d'usage" d'un ou plusieurs champ(s).
  ## ----------------------------------------------------------------------
  ## Arguments: fields : champ(s) recherché(s).
  ##            info : type d'info ("name", "article", "gender", "unit")
  ##            quote : faut-il mettre des guillemets pour les noms de
  ##                    champs tels-quels (pas de nom d'usage défini).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 21 févr. 2013, 18:21

  info <- info[1]

  if (is.element(info, c("nom", "name"))){
    # S'il n'est pas définit, le nom d'usage est remplacé par le nom de champ plutôt que par NA :
    res <- ifelse(is.na(tmp <- varNames[fields, "nom"]),
      paste(ifelse(quote, "\"", ""),
        fields,
        ifelse(quote, "\"", ""), sep = ""),
      tmp)
  }else{
    # Possibilité de nommer les infos en français et anglais:
    res <- ifelse(is.na(varNames[fields, info]),
      "",
      varNames[fields,
        switch(info,
          "article" = "article",
          "genre" = ,
          "gender" = "genre",
          "unite" = ,
          "unit" = "unite",
          "nom")])
  }

  return(res)
}

#' @import tkrplot

tkrplot <- function(parent, fun, hscale = 1, vscale = 1, ...){

  ## Purpose: écraser la définition de tkrplot du packages tkrplot pour
  ##          permettre de passer des options supplémentaires au
  ##          périphérique graphique.
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de tkrplot original + ...
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 août 2010, 12:46

  image <- paste("Rplot",
    ifelse(exists(".make.tkindex"),
      .make.tkindex(),
      tkrplot:::.make.tkindex()), # nécessaire si ".make.tkindex" n'a pas été exportée.
    sep = "")

  # Périphérique graphique :
  .my.tkdev(hscale, vscale, ...)

  invisible(try(fun()))
  .Tcl(paste("image create Rplot", image))

  lab <- tklabel(parent, image = image)
  tkbind(lab, "<Destroy>", function() {
    .Tcl(paste("image delete", image))
  })
  lab$image <- image
  lab$fun <- fun
  lab$hscale <- hscale
  lab$vscale <- vscale
  return(lab)
}


.my.tkdev <- function (hscale = 1, vscale = 1, ...){

  ## Purpose: écraser la définition de .my.tkdev du packages tkrplot pour
  ##          permettre de passer des options supplémentaires au
  ##          périphérique graphique + gestion des différents systèmes
  ##          d'exploitation/versions de R.
  ## ----------------------------------------------------------------------
  ## Arguments: ceux de .my.tkdev original + ...
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 23 août 2010, 12:41

  if (Sys.info()["sysname"] == "Windows"){            # Système Windows
    if(R.version$major == 2 && R.version$minor < 3){
      win.metafile(width = 4 * hscale, height = 4 * vscale, ...)
    }else{
      win.metafile(width = 4 * hscale, height = 4 * vscale, restoreConsole = FALSE, ...)
    }
  }else{                                              # Systèmes Unix(-like).
    if (exists("X11", env = .GlobalEnv)){
      X11("XImage", 480*hscale, 480*vscale, ...)
    }else{
      stop("tkrplot only supports Windows and X11")
    }
  }
}


runLog.f <- function(msg, niv = -1){
  ## Purpose: écrire les appels de fonctions dans un fichier log, si et
  ##          seulement si l'option est activée.
  ## ----------------------------------------------------------------------
  ## Arguments: msg : message.
  ##            niv : niveau de l'appel pour retrouver la fonction
  ##                  appelante () ; si NULL, seul le message est
  ##                  écrit.
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date:  8 févr. 2011, 14:14

  on.exit(if (exists("logFile") &&
    tryCatch(isOpen(logFile),
      error = function(e) return(FALSE))) close(logFile))

  logDir <- file.path(PAMPAhome, "logs")

  # Test d'existance et éventuelle création du dossier de logs:
  if (! dir.exists(logDir)){
    dir.create(logDir)
  }

  # Test d'existance et éventuelle création du fichier de log du jour :
  logFileName <- paste("Runs_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep = "")

  if (!file.exists(file.path(logDir, logFileName)) ||
      isTRUE(file.info(file.path(logDir, logFileName))$isdir))
  {
    file.create(file.path(logDir, logFileName))
  }

  logFile <- file(description = file.path(logDir, logFileName),
                  open = "a", encoding = "latin1")

  callingFct <- ifelse(is.null(niv),
    "",
    deparse(sys.call(niv)))

  cat(paste("\n", format(Sys.time(), "[%H:%M:%S] : "),
    paste(msg, collapse = "\n\t"), "\n",
    paste(callingFct, collapse = "\n\t"), "\n", sep = ""),
    file = logFile)

  close(logFile)
}


errorLog.f <- function(error, niv = -3){
  ## Purpose: écrire les erreurs dans un fichier log + avertissement de
  ##          l'utilisateur
  ## ----------------------------------------------------------------------
  ## Arguments: error : erreur (récupérée par la fonction tryCatch).
  ##            niv : niveau de l'appel pour retrouver la fonction
  ##                  appelante (-3 par défaut pour tryCatch).
  ## ----------------------------------------------------------------------
  ## Author: Yves Reecht, Date: 22 déc. 2010, 11:54

  on.exit(if (exists("logFile") &&
    tryCatch(isOpen(logFile),
      error = function(e) return(FALSE))) close(logFile))

  logDir <- file.path(PAMPAhome, "logs")

  # Test d'existance et éventuelle création du dossier de logs :
  if (! dir.exists(logDir)){
    dir.create(logDir)
  }

  # Test d'existance et éventuelle création du fichier de log du jour :
  logFileName <- paste("Errors_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep = "")

  if (!file.exists(file.path(logDir, logFileName)) ||
      isTRUE(file.info(file.path(logDir, logFileName))$isdir))
  {
    file.create(file.path(logDir, logFileName))
  }

  logFile <- file(description = file.path(logDir, logFileName), open = "a", encoding = "latin1")

  callingFct <- sys.call(niv)

  cat(paste("\n", format(Sys.time(), "[%H:%M:%S]"), "\n",
    paste(deparse(callingFct), collapse = "\n\t"), " :\n", sep = ""),
    file = logFile)
  capture.output(print(error), file = logFile)
  cat("\n", file = logFile)

  close(logFile)

  message(mltext("errorLog.f.msg1"), mltext("errorLog.f.msg2"), logFileName, "\n")
}
