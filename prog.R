livraison = "2021_11"
date_pub = "4 octobre 2021"

chemin = paste0("C:/Users/zfriant/Documents/OneDrive/PCRI/traitement/");
chemin_donnees = paste0(chemin, livraison, "/donnees/")
chemin_tab = paste0(chemin, "tableau/connexion/")
chemin_bd = paste0(chemin, livraison, "/bd/")
chemin_open_data = paste0(chemin, livraison, "/open_data/")

source(file = "fonction.R")
source(file = "importation.R")

# 1ERE ETAPE A FAIRE QU'UNE SEULE FOIS A CHAQUE ACTUALISATION
#IMPORTATION TABLES SOURCES
#avant importation à partir d'access les transformer en txt
creation_dossier()

# Import des tables brutes et sauvegarde; lancer le programme renommage pour réimporter des tables R
importation_data(chemin_bd)

# importation_nomenclatures()

#chargement des tables RData après importation des tables sources et renommage des variables

- renommage

#controle des valeurs vides
controle_NA(proposal$APPEL)
controle_NA(proposal$ELIGIBLE)
controle_NA(proposal$STATUT_EVAL)
controle_NA(proposal$CODE_ACT)
#controle_NA(proposal$CODE_DG)
controle_NA(proposal$CD_THEMA)
controle_NA(proposal$CD_TOPIC)
controle_NA(applicant$ROLE_PART)
controle_NA(applicant$CODE_PAYS)

controle_NA(project$APPEL)
controle_NA(project$STADE_PROJET)
controle_NA(project$CODE_ACT)
#controle_NA(project$CODE_DG)
controle_NA(project$CD_THEMA)
controle_NA(participant$ROLE_PART)
controle_NA(participant$CODE_PAYS)
controle_NA(participant$TYPE_ORG)


#s'il manque des donnees : ouvrir le prog ci-dessous pour corriger
- corrections_donnees 

#maj les call continu : surtout pour le traitement de l'année de call
call_continu_detect()
call_continu = c("H2020-Adhoc-2014-20", "EURATOM-Adhoc-2014-20")


##################Traitements particuliers :
## -année d'exercice
## -statut : verifier qu'il n'y a pas une nouvelle modalité à prendre en compte
## -role : vérifie que tous les projets ont un coordinateur, à développer si dans prochaine actualisation
#ce n'est pas le cas (voire le programme SAS qui règle le problème)
## -suppression :
# des projets COST
# des contributions pour ESA (999644559), CERN (999988133), GEANT LIMITED (999740589)

- nettoyage 
- nettoyage_autres_bases
# load(paste0(chemin_donnees,"autres_prog.Rdata"))

rm(verif_annee_project,verif_annee_proposal)
gc()

save( "applicant", "proposal", "participant", "project", file=paste0(chemin_bd, "bases_tempory.Rdata"))
#############################################################
# applicant <- load_one_obj(rdata=paste0(chemin_bd, "bases_tempory.Rdata"), "applicant")
load(file=paste0(chemin_bd, "bases_tempory.Rdata"))

- pays
rm(list=ls(pattern = "^(pays|COUNTRY|temp)"))
gc()

- topic
rm(list=ls(pattern = "^(topic|TOPIC|comp)"))
gc()

- theme
rm(list=ls(pattern = "^(THEM|them|mln)"))
gc()

- panel
rm(list=ls(pattern = "^(pan|verif|temp)"))
gc()

- type_org
rm(list=ls(pattern = "^(typ)"))
gc()

- actions
rm(list=ls(pattern = "^(act|ACT)"))
gc()


verif_na <- proposal[apply(proposal, 2, function(x) any(is.na(x)))]
verif_na <- project[apply(project, 2, function(x) any(is.na(x)))]

#sauvagerde des deux tables nettoyées et détaillées
save("proposal", "project", "applicant", "participant", file = paste0(chemin_donnees,"datas.RData"))

######################################################################################################
# penser à réouvrir une session en 64 bits avant de continuer le traitement
######################################################################################################

rm(list=c("verif_na", "eit", "eit_activities", "eit_participant", "art", "art_part", 
          "art_proj", "art_project"))
gc()

# réimporter les rdatas de datas.RData
load(paste0(chemin_donnees,"datas.RData"))
autres_prog <- load_one_obj(rdata=paste0(chemin_donnees,"autres_prog.RData"), "tab")

gc()

###############################################################################################
# création tables finales pour Tableau- proposition
#######################################################
# si traitement R en plusieurs parties, chargement des deux tables nettoyées sauvées ci-dessus
#load(paste0(chemin_donnees,"datas.RData"))

date_pub <- as.character(date_pub)
date_extraction <- as.data.frame(date_pub)
write.csv2(date_extraction, file = str_c(chemin_tab, "date_extraction.csv"), row.names = F)
save(date_extraction, file = str_c(chemin_tab, "date_extraction.rda")) 

applicant <- applicant %>%
  mutate(subv_source = SUBV,
    SUBV = ifelse(CODE_ORG %in% c(999644559, 999988133, 999740589), NA_real_, SUBV),
    SUBV = ifelse(NUM_PROJET == 633053 & ROLE_PART == "coordinator", NA_real_, SUBV))

proposal <- proposal %>% 
  inner_join(applicant, by = "NUM_PROJET") %>% 
  select(-starts_with("DT_LAST_UPDATE")) %>%
  dplyr::filter(!(str_detect(CD_TOPIC, "(COST)"))) 
# pic cost association 948759523


# proposal <- inner_join(proposal, applicant, by = "NUM_PROJET") %>% 
#   unique()
  
verif_na <- proposal[apply(proposal, 2, function(x) any(is.na(x)))]

participant <- participant %>%
  mutate(subv_source = SUBV, subv_source_net = SUBV_NET,
    SUBV = ifelse(CODE_ORG %in% c(999644559, 999988133, 999740589), NA_real_, SUBV),
    SUBV_NET = ifelse(CODE_ORG %in% c(999644559, 999988133, 999740589), NA_real_, SUBV_NET),
    SUBV = ifelse(NUM_PROJET == 633053 & ROLE_PART == "coordinator", NA_real_, SUBV),
    SUBV_NET = ifelse(NUM_PROJET == 633053 & ROLE_PART == "coordinator", NA_real_, SUBV_NET))

project <- participant %>% 
  inner_join(project, by = "NUM_PROJET") %>% 
  dplyr::filter(!(str_detect(CD_TOPIC, "(COST)"))) %>% 
  select(-starts_with("DT_LAST_UPDATE")) %>%
  unique()

verif_na <- project[apply(project, 2, function(x) any(is.na(x)))]

save("proposal", "project", "autres_prog", file = paste0(chemin_donnees,"datas2.RData"))
load(file = paste0(chemin_donnees,"datas2.RData"))

#réintegration des subv eurofusion pour l'Allemagne
#SUBV_NET = ifelse(NUM_PROJET == 633053 & ROLE_PART == "coordinator", SUBV_NET/2, SUBV_NET))  
# temp <- project %>% subset(NUM_PROJET == 633053 & ORDRE_PART == 1)
# subv_de <- round(sum(temp$SUBV_NET), digits = 2)
# project <- project %>%  
#   mutate(SUBV = ifelse(NUM_PROJET == 633053 & ROLE_PART == "coordinator", subv_de, SUBV),
#          SUBV = round(SUBV, digits = 2))  

#export Mialy
# write.csv2(project, file = paste0(chemin_donnees, "projets_", livraison,".csv"), row.names = FALSE)

# suppression tables bases pour faire de la place
# rm(list=c("eit_participant", "eit_activities", "eit", "art_proj", "art_part", "art_project", "art"))
# gc()

#rm(list = setdiff(ls(), lsf.str()))

########################################################################################################•
# AVANT de lancer prog : verifier les bits et passer en 64 !

#proposal <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "proposal")
- propositions
sauve_fin(proposition,"proposition")
sauve_fin(proposition_part,"proposition_part")
rm(list=ls(pattern = "^(prop)"))
gc()

#project <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "project")
- contrats
sauve_fin(contract,"contract")
sauve_fin(contract_pme,"contract_pme")
sauve_fin(contract_org,"contract_org")
sauve_fin(contract_part,"contract_part")
sauve_fin(contract_benef,"contract_benef")
#sauve_fin(contract_synthese,"contract_synthese")
#write.csv2(contract_synthese, file = paste0(chemin_tab, "contract_synthese.csv"), row.names = FALSE)

sauve_fin(contrats_public,"contrats_public")
sauve_fin(contrats_rappel,"contrats_rappel")
#contrats_rappel <- load(paste0(chemin_donnees,"contrats_rappel.RData"))
#write.csv2(tab, file = paste0(chemin_open_data, "contrats_rappel.csv"), row.names = FALSE)

- contrib_ue
sauve_fin(contrib, "contrib")


write.csv2(contrats_projets, file = paste0(chemin_open_data, "contrats_projets.csv"), row.names = FALSE, na="")
write.csv2(contrats_participants, file = paste0(chemin_open_data, "contrats_participants.csv"), row.names = FALSE, na="")
write.csv2(contrats_pays, file = paste0(chemin_open_data, "contrats_pays.csv"), row.names = FALSE, na="")
