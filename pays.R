########################################################################
# PAYS
#######################################################################

#identifier si des structures ne sont pas localisées et essayer de corriger
#mettre à jour la requête si réponse aux "detect" dans la console
temp_prop <- detect_pays_zz(applicant)

if(!(is.null(temp_prop))){
  temp_prop <- 
    temp_prop %>% left_join(applicant, by = c("CODE_ORG")) %>%
    select(CODE_ORG, pays_temp = CODE_PAYS) %>%
    unique() %>% 
    dplyr::filter(pays_temp !="ZZ") %>% 
    group_by(CODE_ORG) %>% mutate(n=n()) %>% 
    dplyr::filter(n<2) 
  applicant <- 
    applicant %>% left_join(temp_prop, by = c("CODE_ORG")) %>% 
    mutate(CODE_PAYS = if_else(!is.na(pays_temp), pays_temp, CODE_PAYS)) %>% 
    select(-pays_temp) %>% unique() %>% group_by()
  temp_prop <- detect_pays_zz(applicant) %>% 
    left_join(participant, by = "CODE_ORG") %>% dplyr::filter(CODE_PAYS != "ZZ") %>% 
    select(CODE_ORG, pays_temp = CODE_PAYS) %>% unique()
  if (nrow(temp_prop) > 0){
    applicant <- applicant %>% left_join(temp_prop, by = "CODE_ORG") %>% 
      mutate(CODE_PAYS=if_else(!is.na(pays_temp), pays_temp, CODE_PAYS))
  } else{
    print("pas de code_org")
  }
  
} else {
  rm(temp_prop)
}
  
#recherche du code_pays manquant dans la base des propositions
temp_cont <- detect_pays_zz(participant) 

if(!(is.null(temp_cont ))){
temp_cont <- temp_cont %>% left_join(applicant, by = c("CODE_ORG", "PIC_ID2")) %>%
             select(CODE_ORG, PIC_ID2, pays_temp = CODE_PAYS) %>%  unique()
participant <- participant %>% left_join(temp_cont, by = c("CODE_ORG", "PIC_ID2")) %>% 
  mutate(CODE_PAYS = ifelse(CODE_PAYS == "ZZ", pays_temp, CODE_PAYS)) %>% 
  select(-pays_temp)
} else {
  rm(temp_cont)
}

#verifier à la main à quel pays peut-être rattaché les participations de la table  
temp_cont <- detect_pays_zz(participant)   
- corrections_donnees.R


# table de réference des pays dans les bases h2020
# COUNTRY_PROP <- data.frame()
# sql <- "SELECT * FROM H20_REF_COUNTRY"
# access_sql_32(sql, "COUNTRY_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
# 
# COUNTRY_PROJ <- data.frame()
# sql <- "SELECT * FROM H20_REF_COUNTRY"
# access_sql_32(sql, "COUNTRY_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))

COUNTRY_PROP <- 
  read.csv(file = paste0(chemin_bd, "proposal/h20_ref_country.csv"), na.strings = "", encoding = "UTF-8")
  # COUNTRY_PROP %>% mutate(CD_COUNTRY = ifelse( LB_COUNTRY == "Namibia", "NA", CD_COUNTRY))


COUNTRY_PROJ <- 
  read.csv(file = paste0(chemin_bd, "project/h20_ref_country.csv"), na.strings = "", encoding = "UTF-8")
   # COUNTRY_PROJ %>% mutate(CD_COUNTRY = ifelse( LB_COUNTRY == "Namibia", "NA", CD_COUNTRY))

#charger nomenclatures pays avec libellés propres en français
pays_fr <- load_one_obj("nomenclature/pays.rdata", "pays_fr")
pays_statut <- load_one_obj("nomenclature/pays.rdata", "pays_statut")
pays_statut_gen <- load_one_obj("nomenclature/pays.rdata", "pays_statut_gen")

#comparaison des deux tables ref pays, contrôle des diff (les champs des deux tables doivent être les mêmes)
#création d'une table avec les données pays de la dernière actualisation
comparaison_df(COUNTRY_PROP,COUNTRY_PROJ)
pays_ref <- dplyr::union(COUNTRY_PROP,COUNTRY_PROJ) %>% 
            mutate(CD_STATUS = ifelse(CD_COUNTRY == "ZZ", "UNDEF", CD_STATUS))

###################################
#vérification des territoires d'outre-mer
pays_tom <- pays_ref %>% dplyr::filter(CD_STATUS == "TIERS-INCO-OCT")

pays_tom_verif <- pays_tom %>% left_join(pays_fr, by = c("CD_COUNTRY" = "CODE_PAYS")) %>% 
                         dplyr::filter(CD_COUNTRY == CODE_PAYS_RGP)

if (nrow(pays_tom_verif) > 0) {
  print("Ajout du code regroupement pays manquant dans la table de nomenclature pays_fr, ouvrir prog nomenclatures_modif.R")
} else{
  print("tout va bien")
}

###################################
#vérification du statut des pays et arrêt s'il en manque dans la table pays_statut et pays_statut_gen
pays_statut_verif <- pays_ref %>% left_join(pays_statut_gen, by = c("CD_STATUS" = "CODE_STATUT")) %>% 
                dplyr::filter(CD_COUNTRY != "ZZ", is.na(gen_4_pays)) %>% select(CD_STATUS) %>% unique()

if (nrow(pays_statut_verif) > 0) {
  print("Ajout de statuts de pays dans tables nomenclature pays_statut et pays_statut_gen, ouvrir prog nomenclatures_modif.R")
} else{
  print("tout va bien")
}


###########################
#pays présents dans bases
pays_base <- applicant %>% 
             select(CODE_PAYS) %>% 
             unique() %>% 
             full_join(participant, by = "CODE_PAYS") %>% 
             select(CODE_PAYS) %>% 
             unique()


#comparaison pays entre tables refs et nomenclatures existantes
pays <- left_join(pays_base, pays_fr, by = "CODE_PAYS")
        
if (nrow(pays) != nrow(pays_base)) {
  print("il manque un pays dans la table nomenclature pays_fr")
} else{
  print("tout va bien")
}

#ajout du statut et recuperation du libellé en français
pays <- pays %>% 
        left_join(pays_ref, by = c("CODE_PAYS_RGP" = "CD_COUNTRY")) %>% 
        select(CODE_PAYS_RGP, CODE_PAYS, NOM_PAYS, CODE_STATUT = CD_STATUS) %>% 
        left_join(pays_statut, by = "CODE_STATUT")

if (is.na(pays$NOM_STATUT) && pays$CODE_PAYS_RGP != "ZZ") {
  print("il manque un statut dans la table nomenclature pays_statut")
} else{
  print("tout va bien")
}



# UPDATE
# maj du code_pays des tables avec le code_pays_regroupement
# proposal <- maj_pays(proposal) %>% select(-CODE_PAYS_RGP)
# project <- maj_pays(project) %>% select(-CODE_PAYS_RGP)
applicant <- maj_pays(applicant) %>% select(-CODE_PAYS_RGP)
participant <- maj_pays(participant) %>% select(-CODE_PAYS_RGP)

#Ajout des différents niveaux de statuts pays
applicant <- statut_pays(applicant)
participant <- statut_pays(participant) 

controle_NA(applicant$CODE_PAYS)
controle_NA(participant$CODE_PAYS)




        



