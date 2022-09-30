#chargement des datas nettoyées
proposal <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "proposal")
project <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "project")

#ajustement dans libellé variables pour faire correspondre proposal et project + ajout variables à project
proposal <- proposal %>% dplyr::rename(status = STATUT_EVAL) %>% 
  mutate(stage = "proposal") %>% 
  select(-APPEL_MASTER, -ELIGIBLE, -programme, -area, -panel_2, -panel_1, -panel1_tri, -panel2_tri, -programme_tri, -area_tri, -pilier_tri,
         -action_1, -action_2, -action_3, -action2_tri, -action1_tri, -action3_tri, -pilier_abbr)

project <- project %>% dplyr::rename(status = STADE_PROJET) %>%
  mutate(stage = "project") %>%
  select(-APPEL_MASTER, -programme, -area, -panel_2, -panel_1, -panel1_tri, -panel2_tri, -programme_tri, -area_tri, -pilier_tri,
         -action_1, -action_2, -action_3, -action2_tri, -action1_tri, -action3_tri, -pilier_abbr)
project <- proposal %>% select(NUM_PROJET, DATE_CALL, KEY_WORDS) %>% unique() %>% 
           right_join(project, by = "NUM_PROJET")


#provisoire à supprimer à la prochaine actu : ajout du budget_financed
proposal <- proposal %>%  left_join(prop, by = "NUM_PROJET")
project <- project %>%  left_join(proj, by = "NUM_PROJET")

#pour récupérer l'url des fiches projest sur le site de la commission
cordis_h2020 <- read_csv2("http://cordis.europa.eu/data/cordis-h2020projects.csv", 
                          col_types = cols(id = col_integer())) %>% select(NUM_PROJET = id, rcn = 1)

#création des adresses web des fiches H2020 sur le site de la commission pour chaque projet
project <- left_join(project, cordis_h2020, by = "NUM_PROJET") %>%
  mutate(url = ifelse(!is.na(rcn), paste0("http://cordis.europa.eu/project/rcn/", rcn, "_en.html"), NA)) %>% 
  select(-rcn)


# Ajout du lib de Call
appels_h2020 <- fromJSON("http://ec.europa.eu/research/participants/portal/data/call/h2020/calls.json") %>% 
  .[["callData"]] %>%
  .[["Calls"]]


project <- tibble::data_frame(APPEL = appels_h2020$CallIdentifier$CallId,
                                   call_name = appels_h2020$Title) %>% 
                  right_join(project, by = "APPEL")


#création dataframe complet proposal + projets
projects <- bind_rows(project,proposal) %>% 
  mutate(type = "H2020",
         panels = if_else(programme_abbr == "ERC", panel2_id, ""),
         msca_code = if_else(programme_abbr == "MSCA", panel2_id, ""),
         msca_name = if_else(programme_abbr == "MSCA", panel2_lib, ""),
         DURATION = round(DURATION, digits = 0),
         NB_PART = as.integer(NB_PART)) %>% 
  dplyr::rename(id = NUM_PROJET, name = TITRE_PROJET, acronym = ACRONYM_PROJET, keywords_en = KEY_WORDS, year = ANNEE,
                signature_date = DATE_SIGNE, start_date = DATE_START, end_date = DATE_END, 
                budget_total = COUT_PROJET, budget_financed = COUT_PART,
                dg_code = CODE_DG, duration = DURATION, source_url = url, number_participant = NB_PART, call_code = APPEL,
                call_date = DATE_CALL, topic_code = CD_TOPIC, topic_name = LIB_TOPIC, description = ABSTRACT) %>% 
  select(-panel1_id, -panel2_lib, -panel2_id, -panel1_lib)
  
#provisoire : juste pour charger les projest dans scanr
projects <- projects %>% filter(stage == "project")
write.csv2(projects, file = paste0(chemin_open_data, "projects.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
save(projects, file = paste0(chemin_donnees,"python.RData"))

rm(list= c("appels_h2020", "cordis_h2020", "h2020_call", "project", "proj", "prop"))
gc()

#################################################################################################################
#PARTICIPANTS
#chargement des datas nettoyées
applicant <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "applicant") %>% 
  dplyr::rename(PIC_ID = CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT) %>%
  mutate(STAGE = "proposal", TYPE_PART = ifelse(TYPE_PART=="partnerorganisation", "partnerorganization",TYPE_PART))

participant <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "participant") %>% 
  dplyr::rename(PIC_ID = CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT) %>%
  mutate(STAGE = "project") %>%  select(-org_tri)

# applicant <- applicant %>% dplyr::rename(PIC_ID = CODE_ORG) %>%
#   mutate(STAGE = "proposal", TYPE_PART = ifelse(TYPE_PART=="partnerorganisation", "partnerorganization",TYPE_PART))

# participant <- participant %>% dplyr::rename(PIC_ID = CODE_ORG) %>%
#   mutate(STAGE = "project") %>%  select(-org_tri)

#traitement sur le siren_corda à voir comment gérer
temp <- participant %>% select(PIC_ID, name_source ,CODE_PAYS, SIREN_CORDA) %>% unique() %>%
  mutate(siren0 = str_replace_all(SIREN_CORDA, " +", ""),
         siren0 = str_split(siren0, "/"),
         siren1 = str_extract(siren0, "[0-9]+"),
         siren2 = if_else(str_detect(siren1, "^\\d+$"), siren1, ""),
         siren3 = if_else(str_length(siren2) > 8, siren2, ""),
         siren3 = if_else(str_trim(siren3) == "", SIREN_CORDA, siren3)) %>%
  filter(!is.na(siren3), siren3 != "") %>%
  select(CODE_PAYS, PIC_ID, source_siren = siren3)

participant <- participant %>% left_join(temp, by = c("CODE_PAYS", "PIC_ID")) %>% select(-SIREN_CORDA)

rm(temp)
gc()
######################################################################################################
#ajout grid surtout pour les etrangers
grid <- read.csv2(file = "D:/pcrdt_project/etranger/grid_pic_link.txt") %>% 
  dplyr::rename(grid=Grid) %>% 
  mutate(grid = if_else(grid == "x" & grid_parent != "", grid_parent, grid)) %>% 
  filter(!(grid %in% c("","x"))) %>% select(grid, PIC_ID, CODE_PAYS)

# part_grid <- inner_join(part, grid, by = c("pic_id" = "PIC")) %>% 
#   select(project_id, pic_id, participant_order, acronym, name, country_code, Grid) %>% unique()

# 
#applicant <- left_join(applicant, grid, by = "PIC_ID")
 
#participant <- left_join(participant, grid, by = "PIC_ID")


####################################################################################
#recuperation des identifiants ajoutés suite au recodage mesri
date_recodage = "2018_06"
part_recode <- importer_table_sas(paste0("D:/reperage/", date_recodage, "/pcrdt/participants.sas7bdat")) %>% 
  filter(rattachement == 1) %>% 
  select(id_ref, part_order) %>% 
  mutate(id_ref = if_else(part_order == "776661_18", 5083, id_ref)) %>% 
  unique()

# Importation table REF, création de participant_id_ref qui correspond à un identifiant unique prioritaire pour SCANR
ref <- importer_table_sas(paste0("D:/reperage/", date_recodage, "/pcrdt/referentiel_complet.sas7bdat")) %>% 
  mutate(participant_id_ref = toupper(num_nat_struct),
         participant_id_ref = ifelse(is.na(participant_id_ref) & !is.na(siret), siret, participant_id_ref),
         participant_id_ref = ifelse(orig_ref == "bce" & !is.na(numero_uai), toupper(numero_uai), participant_id_ref),
         libelle1 = if_else(is.na(libelle1), nom_entier, libelle1))

lib <- read.csv2(paste0("D:/reperage/nettoyage/libelle_nettoye.csv")) %>% 
  filter(libelle != "0") %>% mutate(sigle = if_else(sigle == "0", NA_character_, sigle))
# Création table avec les variables utiles et modif du part_order
# part_id <- inner_join(part_recode, ref, by = "id_ref") %>% 
#   mutate(part_order=str_replace(part_order,"_","-")) %>% 
#   select(orig_ref, id_ref, name, acronym = sigle, part_order, participant_id_0, adresse, code_postal=code_postal_clean, ville=ville_tag) %>%  
#   unique()

# Récupération du niveau supérieur de participant_id_ref hors structure RNSR
ref_niv <- importer_table_sas(paste0("D:/reperage/", date_recodage, "/pcrdt/niveau.sas7bdat")) %>% 
  #•filter(orig_ref_1 != "rnsr") %>% 
  select(orig_ref_1, id_ref_niv_1, id_ref_niv_6) %>% 
  mutate(id_ref = if_else(orig_ref_1 == "rnsr", id_ref_niv_1, id_ref_niv_6)) %>% 
  left_join(ref, by = c("id_ref_niv_1" = "id_ref")) %>%
  select(orig_ref_1, id_ref_niv_1, id_ref_niv_6, participant_id_0 = participant_id_ref, id_ref) %>% 
  unique() %>% 
  left_join(ref, by = "id_ref") %>%
  select(id_ref, id_ref_niv_1, participant_id = participant_id_ref, participant_id_0, name_2 = libelle1, acronym_2 = sigle, siege, orig_ref_1, adresse, code_postal=code_postal_clean, ville=ville_tag) %>% 
  unique() %>% 
  left_join(lib, by = c("id_ref" = "id_ref_niv_6")) %>% 
  mutate(name_2 = if_else(!is.na(libelle), libelle, name_2),
         acronym_2 = if_else(!is.na(sigle), sigle, acronym_2)) %>% 
  select(-sigle, -libelle, -id_ref_car)

part_id <- part_recode %>% right_join(ref_niv, by = c("id_ref" = "id_ref_niv_1")) %>% 
  mutate(part_order=str_replace(part_order,"_","-")) %>%
  #ajout bout de code pour name et acronym
  unique()

  #mutate(NUM_PROJET = as.integer(str_match(part_order, "^(\\d+)_")[, 2]),
  #        ORDRE_PART = str_match(part_order, "_(\\d+)$")[, 2] %>% as.integer(),
  #        CODE_PAYS = "FR") 

id_non_enregistre <- part_id %>% filter(str_replace_all(paste0(num_nat_struct, numero_uai, siret), "NA", "") == "") %>% 
                        select(name_2, acronym_2) %>% unique()

#prochaine actu revoir le part_order qui n'aura plus besoin d'être modifié puisque l'on prendra le ORDE_ALL pour tous
app <- applicant %>% 
  mutate(part_order = if_else(TYPE_PART=="beneficiary", paste(sep="-", NUM_PROJET, ORDRE_PART), paste(sep="-", NUM_PROJET, ORDRE_ALL)))
app <- app %>% 
  left_join(part_id[,c("part_order", "name_2", "acronym_2","participant_id_0", "participant_id", "adresse", "code_postal", "ville")], 
            by = "part_order") %>%
  unique()

#suppression du part_order pour créer un id
part_app <- app %>% #gather(part_id, key=id2, value=participant_id, num_nat_struct:numero_finess) %>%
           select(STAGE, name_source, acronym_source, name = name_2, acronym = acronym_2, NUM_PROJET, PIC_ID, PIC_ID2, 
                  ORDRE_ALL, participant_id, participant_id_0, adresse, code_postal, ville, CODE_PAYS) %>% 
           unique() 


part1 <- part_app %>% select(NUM_PROJET, name, acronym, PIC_ID, PIC_ID2, participant_id, participant_id_0, adresse, code_postal, ville) %>% 
            right_join(participant, by = c("NUM_PROJET", "PIC_ID", "PIC_ID2")) %>% 
            select(STAGE, name_source, acronym_source, name, acronym, NUM_PROJET, ORDRE_ALL, PIC_ID, PIC_ID2, participant_id, participant_id_0, adresse, code_postal, ville, CODE_PAYS) %>% 
            unique()


participants_id <-  bind_rows(part_app,part1) %>% 
            left_join(grid, by = c("PIC_ID", "CODE_PAYS")) %>% unique() %>% 
  group_by(STAGE, NUM_PROJET, ORDRE_ALL) %>% 
  mutate(id = paste(NUM_PROJET, ORDRE_ALL, seq_len(n()),  sep ="-")) %>% 
  ungroup() %>% unique()


applicant <- participants_id %>% filter(STAGE == "proposal") %>% 
             select(-STAGE, -name_source, -acronym_source, -CODE_PAYS) %>%
             right_join(applicant, by = c("NUM_PROJET", "PIC_ID", "PIC_ID2", "ORDRE_ALL")) %>% 
             mutate(ADRESS=if_else(!is.na(adresse), adresse, ADRESS),
              CP=if_else(!is.na(code_postal), code_postal, CP),
              CITY=if_else(!is.na(ville), ville, CITY)) %>% 
             select(-adresse, -code_postal, -ville) %>% unique()

participant <- participants_id %>% filter(STAGE == "project") %>% 
               select(-STAGE, -name_source, -acronym_source, -CODE_PAYS) %>% 
               right_join(participant, by = c("NUM_PROJET", "PIC_ID", "PIC_ID2", "ORDRE_ALL")) %>% 
               mutate(ADRESS=if_else(!is.na(adresse), adresse, ADRESS),
                CP=if_else(!is.na(code_postal), code_postal, CP),
                CITY=if_else(!is.na(ville), ville, CITY)) %>% 
               select(-adresse, -code_postal, -ville) %>% unique()

participants_id <- participants_id %>% 
  filter(str_replace_all(paste0(participant_id, grid), "NA", "") != "")

###############################################################################
#création de la table id par participants
#A REFAIRE EN AJOUTANT NAME ACRONYM

  # participant_id <- left_join(part_id, part_grid[,c("project_id", "pic_id", "participant_order", "Grid")], 
 #                             by=c("project_id", "pic_id", "participant_order")) %>% 
 #   unique()
 # 
 # participants_id <- anti_join(part_grid, part_id,by=c("project_id", "pic_id", "participant_order")) %>% 
 #   bind_rows(participant_id)


#création dataframe avec tous participants + participant_id

participants <- bind_rows(applicant,participant) %>%
  mutate(project_type = "H2020",
         participant_id = toupper(participant_id),
         participant_id = if_else(is.na(participant_id) & !is.na(grid), grid, participant_id),
         sies_id = if_else(!is.na(participant_id), 1, 0),#créa var sies_id avant d'utiliser var source_siren
         participant_id = if_else(is.na(participant_id) & CODE_PAYS == "FR" & nchar(source_siren) >= 9, source_siren, participant_id),
         participant_id_0 = toupper(participant_id_0)) %>% 
  select(project_type, id, project_id = NUM_PROJET, pic_id = PIC_ID, pic_id_2 = PIC_ID2, ORDRE_PART, participant_order = ORDRE_ALL,
         vat_id = source_siren, pme = PME, stage = STAGE, acronym, name, name_source, acronym_source,
         participates_as = TYPE_PART, global_costs = COUT_TOT_PART, funding = SUBV, funding_share = SUBV_NET,
         role = ROLE_PART, status = STATUT_PART, participant_type_code = org_abbr, participant_type_name = org_typ,
         website = URL_PART, ADRESS, city = CITY, post_code = CP, country = pays_lib, country_code = CODE_PAYS,
         country_level_2 = pays_statut_2, country_level_1 = pays_statut_1, participant_id, participant_id_0, sies_id) %>% 
  unique()

# participants <- participants %>%     
#       group_by(project_id, participant_order) %>% 
#       mutate(id = paste(project_id, participant_order, seq_len(n()),  sep ="-")) %>% 
#       ungroup()  

#probleme : les structures identifiées mais sans label(uai siret ou autre) disparaissent de la base
#si ce sont des labos non rnsr, ils sont rattachés à leur tutelle 
#################################################################################################################
#traitement infos complémenatires participants
APPL_DEPT <- read.csv2(paste0(chemin_bd,"/APPLICANTS_DEPT.txt")) %>% 
  mutate(stage = "proposal")
PART_DEPT <- read.csv2(paste0(chemin_bd,"/PARTICIPANTS_DEPT.txt")) %>% 
  mutate(stage = "project")

applicant_add <- applicant %>% select(NUM_PROJET, PIC_ID, PIC_ID2, ORDRE_PART, ORDRE_ALL) %>% 
                  inner_join(APPL_DEPT, 
                   by = c("NUM_PROJET"="CD_PROP_ID", "PIC_ID"="CD_PART_PIC", "PIC_ID2"="CD_APPL_PIC")) %>% 
  mutate(LB_DEPT_NAME = str_replace(str_trim(LB_DEPT_NAME), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         LB_DEPT_ADRS = str_replace(str_trim(LB_DEPT_ADRS), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         CD_DEPT_COUNTRY = str_replace(str_trim(CD_DEPT_COUNTRY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         LB_DEPT_CITY = str_replace(str_trim(LB_DEPT_CITY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         CD_DEPT_POST = str_replace(str_trim(CD_DEPT_POST), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         loc = str_length(paste0(str_trim(LB_DEPT_NAME), str_trim(LB_DEPT_ADRS)))) %>%
      filter(CD_DEPT_COUNTRY != "", loc > 0) %>% 
  select(project_id = NUM_PROJET, pic_id = PIC_ID, pic_id_2 = PIC_ID2, ORDRE_PART, participant_order = ORDRE_ALL, name = LB_DEPT_NAME,
         address = LB_DEPT_ADRS, city = LB_DEPT_CITY, post_code = CD_DEPT_POST, country = CD_DEPT_COUNTRY, stage)

participant_add <- participant %>% select(NUM_PROJET, PIC_ID, PIC_ID2, ORDRE_PART, ORDRE_ALL) %>% 
  inner_join(PART_DEPT, 
                  by = c("NUM_PROJET"="CD_PROJ_ID", "PIC_ID"="CD_PART_PIC", "PIC_ID2"="CD_APPL_PIC")) %>% 
  mutate(LB_DEPT_NAME = str_replace(str_trim(LB_DEPT_NAME), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         LB_DEPT_ADRS = str_replace(str_trim(LB_DEPT_ADRS), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         CD_DEPT_COUNTRY = str_replace(str_trim(CD_DEPT_COUNTRY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         LB_DEPT_CITY = str_replace(str_trim(LB_DEPT_CITY), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         CD_DEPT_POST = str_replace(str_trim(CD_DEPT_POST), "^((n/a|N/A|N/a|[:punct:])+( )*[:punct:]*( )*[:punct:]*)", " " ),
         loc = str_length(paste0(str_trim(LB_DEPT_NAME), str_trim(LB_DEPT_ADRS)))) %>% 
  filter(str_trim(CD_DEPT_COUNTRY) != "", loc > 0) %>% 
  select(project_id = NUM_PROJET, pic_id = PIC_ID, pic_id_2 = PIC_ID2, ORDRE_PART, participant_order = ORDRE_ALL, name = LB_DEPT_NAME,
         address = LB_DEPT_ADRS, city = LB_DEPT_CITY, post_code = CD_DEPT_POST, country = CD_DEPT_COUNTRY, stage)


participants_add <- bind_rows(participant_add, applicant_add)

####################################################################################################################
#correction 28-10-2019
participants <- participants %>% 
  mutate(participant_id = if_else(pic_id == 955970406, "48870221800022", participant_id),
         participant_id = if_else(pic_id == 995923930, "48536113300021", participant_id),
         participant_id = if_else(pic_id == 936099083, "18130008800661", participant_id),
         participant_id = if_else(pic_id == 928736589, "53813803300047", participant_id),
         participant_id = if_else(toupper(city) == "CAEN" & pic_id == 999957481, "198318600W", participant_id),
         participant_id = if_else(pic_id == 906475671, "13000750300019", participant_id),
         participant_id = if_else(pic_id == 998711613, "13000250400020", participant_id),
         participant_id = if_else(pic_id %in% c(936710668,998779222), " ", participant_id),
         participant_id = if_else(pic_id == 994819973, "19971585500011", participant_id))




options(scipen = 999)
write.csv2(participants_id, file = paste0(chemin_open_data, "participants_id.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv2(participants, file = paste0(chemin_open_data, "participants.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
write.csv2(participants_add, file = paste0(chemin_open_data, "participants_add.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
save("participants", "participants_id", "id_non_enregistre", "grid", "participants_add", file = paste0(chemin_donnees,"python.RData"))

