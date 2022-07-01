# Création du repertoire PAMPA_package qui contient les logs
# Creation of the directory PAMPA_package which contains logs

PAMPAhome <- ifelse(Sys.getenv("PAMPA_HOME") == "",
  normalizePath(file.path(Sys.getenv("HOME"), "PAMPA_package", fsep = "/"),
    winslash = "/", mustWork = FALSE),
  normalizePath(Sys.getenv("PAMPA_HOME"),
    winslash = "/", mustWork = FALSE))
if (!dir.exists(PAMPAhome)){
  dir.create(PAMPAhome)
  print(paste("The directory 'PAMPA' was created at '", PAMPAhome, "'", sep = ""))
}


# Liste des options nécessaires au bon fonctionnement de l'application
# Avant chaque option est précisé le fichier et/ou la fonction qui l'utilise.

# Ensemble du package
options(
  versionPAMPA = "3.1-beta3",
  defaultLang = "en"
)

# base_function.R - VarNames.f
options(
  P.lang = ifelse(test = ! is.null(getOption("P.lang")),
    yes = tolower(getOption("P.lang")),
    no = ifelse(test = ( ! is.null(getOption("P.GUIlang")) && tolower(getOption("P.GUIlang")) == "fr"),
      yes = "fr",
      no = "en")),                        # Langage for the variable labels ("fr" or "en").
  P.GUIlang = ifelse(test = ( ! is.null(getOption("defaultLang")) && is.null(getOption("P.GUIlang"))),
    yes = tolower(getOption("defaultLang")),
    no = "en")                            # Language for the GUI.
)

# load_files.R & load_data.R & generic_graphic.R
options(P.MPAfield = "study.area")


# generic_graphic.R
options(
  PAMPAdummy = TRUE,                      # Sert à savoir si les options ont déjà été définies.
  P.maxExclu = FALSE,                     # Suppressions des données supérieures à une certaine proportion du
                                            # maximum ?
  P.GraphPartMax = 0.95,                  # Proportion du maximum à conserver si P.maxExclu == TRUE
  P.NbObs = TRUE,                         # Affichage sur le graphique du nombre d'observations
                                            # par boite à moustache.
  P.NbObsCol = "orange",                  # Couleur d'affichage des nombres d'observations.
  P.pointMoyenne = FALSE,                 # Affichage des moyennes (points) sur le graphique.
  P.pointMoyenneCol = "blue",             # Couleur d'affichage des moyennes (points).
  P.valMoyenne = TRUE,                    # Affichage des moyennes (valeurs) sur le graphique.
  P.valMoyenneCol = "blue",               # Couleur d'affichage des moyennes (valeurs).
  P.MinNbObs = 1,                         # ??
  P.sepGroupes = TRUE,                    # Séparateurs du premier niveau de regroupements sur un même
                                            # graphique ?
  P.sepGroupesCol = "red",                # Couleur des séparateurs de groupes.
  P.graphPDF = FALSE,                     # Sorties graphiques en pdf ?
  P.graphPNG = FALSE,                     # Sorties graphiques en png ?
  P.plusieursGraphPage = FALSE,           # Plusieurs graphiques par page/fenêtre ?
  P.ncolGraph = 2,                        # Nombres de colonnes de graphiques
                                            # (si P.plusieursGraphPage est TRUE).
  P.nrowGraph = 2,                        # Nombres de lignes de graphiques (si P.plusieursGraphPage est TRUE).
  P.PDFunFichierPage = FALSE,             # Créer un fichier par page pour les sorties PDF ?
  P.NbDecimal = 2,                        # Nombre de décimales à afficher sur les graphiques
  P.legendeCouleurs = TRUE,               # Afficher la légende pour le facteur identifié
                                            # par une des couleurs ?
  P.colPalette = ifelse(( ! is.null(getOption("P.GUIlang")) &&
    tolower(getOption("P.GUIlang")) == "fr"),
    "défaut", "default"),                 # Type de palette de couleur.
  P.statusOrder = c("RI", "RE", "IN",     # Ordre des niveaux de protection pour les graphiques et analyses.
    "Z1", "I1",
    "PP", "RP",
    "Z2", "I2",
    "Z3", "I3",
    "HR", "OUT",
    "Z4"),
  P.interestOrder = c("TR", "AR",         # Ordre des modalités d'intérêts (pour des types de pêche).
    "CA", "NC"),
  P.mobilityOrder = c("TM", "MO", "SE"),  # Ordre des modalités de mobilité.
  P.positionOrder = c("surface",          # Ordre des modalités de la position dans la colonne d'eau.
    "milieu/surface",
    "P", "milieu",
    "D", "benthique",
    "B"),
  P.tideOrder = c("MM", "HM",             # Ordre des modalités des phases de marée.
    "MD", "BM"),
  P.moonOrder = c("NJ", "PC", "PQ",       # Ordre des modalités des phases lunaires.
    "LM", "PL", "LD",
    "DQ", "DC"),
  P.depthOrder = c("peu profond",         # Ordre des modalités de profondeur.
    "variable",
    "profond"),
  P.protection2Order = c("RNI", "RN",     # Ordre des modalités du statut de protection
    "RSF", "AGDR", # (classification alternative).
    "PMP", "HR"),
  P.sizeOrder = c("P", "M", "G"),         # Ordre des modalités de classes de taille.
  P.graphPaper = FALSE,                   # Graphiques adaptés pour la publication (pas de titre, format plus
                                            # petit,...) ?
  P.warnings = TRUE,                      # Affichage des avertissement (graph tronqué, petits effectifs) ?
  P.pointMoyenneCex = 1,                  # Taille des points pour affichage de la moyenne.
  P.pointMoyennePch = 18,                 # Type de point pour affichage de la moyenne.
  P.cex = 1,                              # Taille générale des caractères.
  P.graphWMF = FALSE,                     # Sauvegarde des graphiques affichés à l'écran en WMF (Windows) ?
  P.pdfEmbedFonts = TRUE,                 # Inclusion des polices dans les pdfs ?
  P.barplotStat = "mean",                 # Statistique des barplots
                                          # ("mean", "moyenne", "médiane" ou "median").
  P.barplotErrorBar = TRUE,               # Doit-on afficher les barres d'erreur
                                          # (sd/quantiles) sur les barplots?
  P.saveData = TRUE,                      # Sauvegarde des données de graphiques et analyses ?
  P.saveStats = TRUE,                     # Sauvegarde des informations sur les données (stats incluses)?
  P.axesLabels = TRUE,                    # Affichage des noms d'axes ?
  P.title = TRUE,                         # Affichage des titres ?
  # Carto :
  P.colorLegendCarto = TRUE,              # Afficher la légende des couleurs (facteur de second niveau).
  P.zonesPalette = "carto1",              # Palette de couleurs pour différentier les zones.
  P.symbMaxIn = 0.5,                      # Taille maximale des symboles de taille variable (inches).
  P.symbColor = "red",                    # Couleur des symboles de taille variable.
  P.symbScale = TRUE,                     # Affichage de l'échelle des symboles.
  # ############################################################################################
 # Classe des options (pour conversion depuis les variables tcl) :
 P.optionsClass = c(P.maxExclu = "logical", P.NbObs = "logical", P.NbObsCol = "character",
   P.pointMoyenne = "logical", P.pointMoyenneCol = "character",
   P.valMoyenne = "logical",
   P.valMoyenneCol = "character", "P.GraphPartMax" = "numeric",
   P.MinNbObs = "integer", P.sepGroupes = "logical", P.sepGroupesCol = "character",
   P.graphPDF = "logical", P.graphPNG = "logical", P.plusieursGraphPage = "logical",
   P.ncolGraph = "integer",
   P.nrowGraph = "integer", P.PDFunFichierPage = "logical", P.NbDecimal = "integer",
   P.legendeCouleurs = "logical", P.colPalette = "character",
   P.statusOrder = "character",
   P.interestOrder = "character", P.mobilityOrder = "character",
   P.positionOrder = "character",
   P.tideOrder = "character", P.moonOrder = "character", P.depthOrder = "character",
   P.protection2Order = "character", P.sizeOrder = "character",
   P.graphPaper = "logical", P.warnings = "logical",
   P.pointMoyenneCex = "numeric", P.pointMoyennePch = "integer", P.cex = "numeric",
   P.graphWMF = "logical", P.pdfEmbedFonts = "logical",
   P.lang = "character", P.GUIlang = "character",
   P.barplotStat = "character",  P.barplotErrorBar = "logical",
   P.saveData = "logical",
   P.saveStats = "logical", P.axesLabels = "logical", P.title = "logical",
   P.zonesPalette = "character", P.colorLegendCarto = "logical",
   P.symbMaxIn = "numeric", P.symbColor = "character", P.symbScale = "logical")
)

# Variables de fichiers requises:
options(
  P.requiredVar = c(unitobs = "fileNameUnitobs",
    obs = "fileNameObs",
    refesp = "fileNameRefesp",
    ws = "nameWorkspace")
)

# Options du référentiel spatial :
options(P.linkUnitobs = "site",
  P.linkRefspa = "CODE.SITE",
#  P.shapefileEncoding = "latin1",         # changé pour UTF-8... utilisé pour chargement shapefile
  P.shapefileEncoding = "UTF-8",
  P.landField = "HABITAT1",               # Champs du référentiel spatial permettant d'identifier la terre...
  P.landMods = c("terre", "ilot"),        # ...modalités de ce champs correspondant à la terre.
  P.landCols = c(terre = "chocolate3",  mer = "powderblue"), # couleurs terre/mer.
  P.pinSubplot = c(2.0, 1.8)               # dimensions (en pouces/inches) des sous-graphiques pour représentation
)


# load_files.R / selectLink.interface.f
.fileimageLink <- system.file("img/tableLink.GIF", package = "PAMPA")
assign(".fileimageLink", .fileimageLink, envir=.GlobalEnv)
