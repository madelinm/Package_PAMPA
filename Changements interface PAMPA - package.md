# Liste des modifications faites lors de la création du package


### Modification de fonctions :

 - de nombreuses fonctions ont été modifiées, mais pour la plupart il s'agit de la suppression des lignes faisant appel aux fonctions permettant la création de l'interface.

 - modification de la fonction *selectModalites.f* afin qu'elle retourne les listes des modalités disponibles afin de vérifier que celle(s) entrée(s) par l'utilisateur est (sont) correcte(s) (dans les fonctions degraphiques notament)

 - modification des fonctions de cartographie suivantes afin d'avoir des cartes intéractives (avec zoom, sélection des zones, ...) : *subplotCarto.esp.f*, *boxplotCarto.generic.f*, *barplotCarto.generic.f*, *subplotCarto.unitobs.f*, *symbColCarto.esp.f*, *symbColCarto.unitobs.f*, *selectModalitesSpatiales.f*.


### Création de fonctions :

 - création d'une fonction *load_files.f* regroupant le chargement des données, les calculs des poids et des métriques et les créations de fichier.

 - création de fonction regroupant les couples de fonctions unitobs/esp (par exemple, sélection sur unitobs et sélection sur refesp, boxplot sur unitobs et boxplot sur esp, ...)

 - création d'une fonction permettant d'avoir les fréquences de familles. Elle reprend celle pour les fréquences d'occurrences mais y apporte des modifications.

 - création d'une fonction permettant de faire une permanova.

 - création de fonctions permettant de répondre au besoin de statistiques descriptives de l'outil de Tendances Vigie Nature.
