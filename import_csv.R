PATH = "C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/2021_11/bd/"
DOSSIER = c('proposal', 'project')

import_csv <- function(path, dossier){
  path_dos = paste0(path, dossier)
  # print(path_dos)
  liste_csv <- list.files(path_dos, full.names = TRUE)
  # print(liste_csv)
  for (i in liste_csv){
    print(i)
    che =strsplit(i, '/')
    nom = lapply(che, function(x) {x[length(x)]})
    nom = lapply(nom, function(x) {gsub("\\.csv", "", x)})
    dos = lapply(che, function(x) {x[length(x)-1]})
    print(dos)
    print(nom)
    for (j in nom){
    assign(paste(dos, nom, sep="_"),
      read.csv(file = i, na.strings = "", encoding = "UTF-8"), envir = .GlobalEnv)
      }
  }
}

import_csv(PATH, DOSSIER)



path_dos = paste0(PATH,DOSSIER)
liste_csv <- list.files(path_dos, full.names = TRUE)
for (i in liste_csv){
  che =strsplit(i, '/')
  nom = lapply(che, function(x) {x[length(x)]})
  nom = lapply(nom, function(x) {gsub("\\.csv", "", x)})
  dos = lapply(che, function(x) {x[length(x)-1]})
  for (j in nom){
    assign(paste(dos, j, sep="_"),
      read.csv(file = i, na.strings = "", encoding = "UTF-8"))
  }
}