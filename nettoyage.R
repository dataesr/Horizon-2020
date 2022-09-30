##################################################################
# ANNEE D'EXERCICE
##################################################################

#1-extraction de l'année dans le libellé de l'appel à projets
#3-creation de "DATE" sans les champs à années multiples qui sont remplacées par 'null'
#4-creation de "DATE_SUBM" pour l'extraction de l'année de la var "DT_SUBMISSION_DATE"
#5-creation de "ANNEE", si "DATE" = 'null' alors on prend "DATE_SUBM"
#6-si call continu, utilisation de l'année de "DATE_SUBM"
#7-changer 2013 en 2014
#8-ramener les années ultérieures à maintenant


proposal <- proposal %>%  
              mutate(#DATE = annee_call(proposal),
               DATE = format(dmy(DATE_CALL), "%Y"),  
               DATE_SUBM = format(as.Date(DT_SUBMISSION_DATE, "%d/%m/%Y"), "%Y"),
               ANNEE = ifelse(is.na(DATE), DATE_SUBM, DATE),
               ANNEE = ifelse(APPEL %in% call_continu, DATE_SUBM, ANNEE),
               ANNEE = ifelse(ANNEE == "2013", "2014", 
                              ifelse(ANNEE >= "2021", "2020", ANNEE)),
               ANNEE = ifelse(ANNEE > year(now()), year(now()), ANNEE),
               ANNEE = as.character(ANNEE)) %>% 
               select(-DATE, -DATE_SUBM, -DT_SUBMISSION_DATE)
  
verif_annee_proposal <- proposal %>% select(NUM_PROJET, ANNEE) %>% 
  unique(.) %>% dplyr::filter(is.na(ANNEE))
if (length(is.na(verif_annee_proposal$ANNEE)) > 0) {
  print ("Manque une année d'exercice, voir table verif_annee_proposal")
  } else{
  print("tout va bien")
}


#4-creation de "DATE2" pour extraction de l'année de la "DATE_SIGNE"
#5-creation de "ANNEE", si "DATE" = 'null' alors on prend "DATE_START"
#6-si call continu, utilisation de l'année de "DATE2"
#7-si stade projet est "UNDER_PREPARATION" et sans "DATE2" alors prendre l'année du projet dans PROPOSAL
project <- project %>% 
               mutate(#DATE = annee_call(project),
                ANNEE = format(dmy(DATE_CALL), "%Y"),
                ANNEE = ifelse(ANNEE >= "2021", "2020", ANNEE))
               #ANNEE = year(dmy_hms(DATE_SIGNE)),
               #DATE2 = format(as.Date(DATE_SIGNE, "%d-%m-%Y"), "%Y"),
               #ANNEE = ifelse(DATE == "null", DATE2, DATE),
               #ANNEE = ifelse(is.na(DATE2), DATE, DATE2),
               #ANNEE = ifelse(APPEL %in% call_continu, DATE2, ANNEE),
               #ANNEE = as.character(ANNEE))


# project <- proposal %>% 
#   select(NUM_PROJET, ANNEE) %>% 
#   left_join(project, ., by = "NUM_PROJET")

        
verif_annee_project <- project %>% 
  select(NUM_PROJET, ANNEE, STADE_PROJET) %>% 
  unique(.) %>% 
  dplyr::filter(is.na(ANNEE), STADE_PROJET != "REJECTED")
if (length(is.na(verif_annee_project$ANNEE)) > 0) {
  print ("Manque année d'exercice, voir table verif_annee_project")
} else {
  print("tout va bien")  
  }



##############################################################
# STATUT
##############################################################

#1-suppression des projets "terminés" et "rejetés"
#2-regroupement des projets "clos" et "signes" sous le statut "SIGNED"
#3-regroupement des projets "suspendus" et "en preparation" sous le statut "UNDER_PREPARATION"

unique(project$STADE_PROJET)

project <- project %>% 
  dplyr::filter(!(STADE_PROJET %in% c("TERMINATED", "REJECTED", "SUSPENDED"))) %>% 
  mutate(STADE_PROJET_ORIG = STADE_PROJET,
         STADE_PROJET = ifelse(STADE_PROJET == "CLOSED", "SIGNED", STADE_PROJET))

#1-jointure avec PROJECT pour mettre à jour les propositions de PROPOSAL en "MAINLIST" et "ELIGIBLE"
#2-correction de "ELIGIBLE" pour les propositions en "MAINLIST"
#3-si "ELIGIBLE" est vide alors ineligible
#4-suppression des propositions ineligibles

unique(proposal$STATUT_EVAL)

proposal <- project %>% 
  select(NUM_PROJET, STADE_PROJET) %>% 
  left_join(proposal, . , by = "NUM_PROJET") %>% 
  mutate(ELIGIBLE = ifelse(!is.na(STADE_PROJET), "ELIGIBLE", ELIGIBLE),
         STATUT_EVAL = ifelse(!is.na(STADE_PROJET), "MAIN", STATUT_EVAL),
         ELIGIBLE = ifelse(STATUT_EVAL == "MAIN", "ELIGIBLE", ELIGIBLE),
         ELIGIBLE = ifelse(is.na(ELIGIBLE), "INELIGIBLE", ELIGIBLE)) %>% 
  dplyr::filter(ELIGIBLE != "INELIGIBLE") %>% 
  select(-STADE_PROJET)

unique(proposal$STATUT_EVAL)

################################################################
# ROLE
################################################################

#verification que les vars ROLE_PART et PME ne sont pas manquantes
controle_NA(applicant$ROLE_PART)
unique(applicant$ROLE_PART)
controle_NA(applicant$PME)
unique(applicant$PME)


#transformation de "Partner" en "participant" , ROLE_PART en minuscule 
#si PME est vide mettre non = "N"

sum(temp %>% select(SUBV), na.rm=T)#708 677 570 918

# 2021 -> 15 912 693 459
# 2020 -> 89 267 459 723 = 105 180 153 182


applicant <- 
  applicant %>% 
  mutate(ROLE_PART = tolower(ROLE_PART),
         TYPE_PART = ifelse(ROLE_PART %in% c("partner", "coordinator"), "beneficiary", ROLE_PART),
         TYPE_PART = ifelse(TYPE_PART == "associatedpartner", "partnerassociated", TYPE_PART),
         TYPE_PART = ifelse(TYPE_PART == "partner organisation", "partnerorganization", TYPE_PART),
         TYPE_PART = ifelse(TYPE_PART == "host", "host", TYPE_PART),
         ROLE_PART = ifelse(ROLE_PART != "coordinator", "partner", ROLE_PART),
         PME = ifelse(is.na(PME)| PME == "Missing", "N", PME)) %>% 
         unique()

unique(applicant$ROLE_PART)
unique(applicant$TYPE_PART)

#verification que toutes les propositions ont un coordinateur
verif_coord <- role_coordinator(applicant) %>% 
  mutate(NUM_PROJET = as.numeric(as.character(NUM_PROJET))) %>% 
  left_join(proposal, by = "NUM_PROJET") %>% 
  dplyr::filter(ELIGIBLE != "INELIGIBLE") %>% 
  select(NUM_PROJET, Freq)

if (nrow(verif_coord) > 0) {
  verif_coord <- verif_coord %>% 
    left_join(applicant, by = "NUM_PROJET") %>% 
    mutate(prem = ifelse(ORDRE_PART == 1, 1, 0)) %>% 
    group_by(NUM_PROJET, prem) %>% 
    dplyr::summarise(nb = n()) %>% 
    ungroup()
} else {
  print("tout va bien")  
}

#si le participant est unique, le "coordoniser"
# temp <- applicant %>% dplyr::filter(ORDRE_PART == 1) %>% inner_join(verif_coord, by = "NUM_PROJET")
temp <- verif_coord %>% 
  dplyr::filter(nb == 1)
applicant <- applicant %>% 
  mutate(ROLE_PART = ifelse(NUM_PROJET %in% temp$NUM_PROJET, "coordinator", ROLE_PART))

temp <- verif_coord %>% 
  inner_join(applicant, by = "NUM_PROJET") %>% 
  dplyr::filter(ORDRE_PART == 1)


#verification que les vars "ROLE_PART" et "PME" ne sont pas manquantes
controle_NA(participant$ROLE_PART)
controle_NA(participant$PME)

#ROLE_PART en minuscule 
#si PME est vide mettre non = "N"
participant <- participant %>% 
        mutate(ROLE_PART = tolower(ROLE_PART),
               ROLE_PART = ifelse(ROLE_PART=="participant", "partner", ROLE_PART),
               TYPE_PART = tolower(TYPE_PART),
               TYPE_PART = ifelse(TYPE_PART == "associatedpartner", "partnerassociated", TYPE_PART),
               ROLE_PART = ifelse(TYPE_PART != "beneficiary", "partner", ROLE_PART),
               PME = ifelse(is.na(PME)| PME == "Missing", "N", PME)) %>% 
               unique()


unique(participant$ROLE_PART)
unique(participant$TYPE_PART)

coord_trop <- role_coordinator(participant) %>% 
  mutate(NUM_PROJET = as.numeric(as.character(NUM_PROJET)))
if(nrow(coord_trop)>0){
  write.table(coord_trop, file=paste0(chemin, livraison, "/projets_+coord.csv"))
}

coord_trop <- coord_trop %>% 
  left_join(participant, by=c("NUM_PROJET", "ROLE_PART")) %>% 
  group_by(NUM_PROJET) %>% 
  mutate(ROLE_PART=ifelse(SUBV>0, 'coordinator', 'partner'), nb=ifelse(ROLE_PART=='coordinator', 1, 0))

verif_coord <- coord_trop %>% 
  group_by(NUM_PROJET) %>% 
  mutate(nb=sum(nb)) %>% 
  select(NUM_PROJET, nb) %>% 
  unique() %>% 
  dplyr::filter(nb>1)

if (nrow(verif_coord)>0){
  print('attention verifier encore les coordinations soit doublon ou manquant')
} else{print('ok')}

coord_trop <- coord_trop %>% 
  dplyr::filter(ROLE_PART=="partner") %>% 
  select(NUM_PROJET, ORDRE_PART) %>% 
  unique()

participant <- participant %>% 
  mutate(ROLE_PART = ifelse(NUM_PROJET %in% coord_trop$NUM_PROJET & ORDRE_PART %in% coord_trop$ORDRE_PART & SUBV==0, 
                            "partner", ROLE_PART))


################################################################
# ORDRE_PART et type de participation
################################################################

# selection des type_part non bénéficiaires et dont le numero d'ordre > 100 car composé du num du bénéficiaire
# et d'un num commençant par "0" ex : 101 -> beneficiaire = 1 et ordre = 01
applicant <- ordre_type_part(applicant)

participant <- ordre_type_part(participant)

