#chargement des datas nettoyées
# appdep <- charger_rdata(paste0(chemin_bd,"bases.RData"), "appl_dept")
# partdep <- charger_rdata(paste0(chemin_bd,"bases.RData"), "part_dept") %>% 
#   select(NUM_PROJET = CD_PROJ_ID, PIC_ID = CD_PART_PIC) %>% 
#   group_by(NUM_PROJET, PIC_ID) %>% mutate(nb = n()) %>% 
#   unique()
# 
# temp <- project %>% select(NUM_PROJET, ORDRE_ALL, PIC_ID) %>% unique()
# 
# temp <- inner_join(temp,partdep, by = c("NUM_PROJET","PIC_ID"))

###################################################################################################
#recuperation des identifiants ajoutés suite au recodage mesri
date_recodage = "2017_06"
part_recode <- importer_table_sas(paste0("D:/reperage/", date_recodage, "/pcrdt/participants.sas7bdat")) %>% 
  filter(rattachement == 1)
ref <- importer_table_sas(paste0("D:/reperage/", date_recodage, "/pcrdt/referentiel_complet.sas7bdat")) 

participants_id <- inner_join(part_recode, ref, by = "id_ref") %>% 
  select(NOM_LONG = libelle1, NOM_COURT = sigle, part_order, num_nat_struct, numero_uai, siret, numero_finess) %>% 
  mutate(NUM_PROJET = as.integer(str_match(part_order, "^(\\d+)_")[, 2]),
       ORDRE_PART = str_match(part_order, "_(\\d+)$")[, 2] %>% as.integer(),
       CODE_PAYS = "FR") %>%  unique() %>% 
  left_join(applicant[,c("NUM_PROJET", "ORDRE_PART", "PIC_ID","PIC_ID2")], 
            by = c("NUM_PROJET", "ORDRE_PART")) %>% 
  select(-part_order)

######################################################################################################

# grid <- read.csv2(file = paste0(chemin_bd, "grid_pic_link.txt"))
# 
# applicant <- left_join(participants, grid, by = c("PIC_ID" = "PIC")) %>% 
#   dplyr::rename(participant_id = grid)
# participant <- left_join(participant, grid, by = c("PIC_ID" = "PIC")) %>% 
#   dplyr::rename(participant_id = grid)


# etranger <- function(table) {
#   temp <- table %>% 
#     filter(CODE_PAYS != "FR") %>% 
#     select(CODE_PAYS, NUM_PROJET, ORDRE_PART, PIC_ID, PIC_ID2, NOM_LONG,NOM_COURT) %>% 
#     unique() %>% 
#   #provisoire
#     inner_join(grid, by = c("PIC_ID" = "PIC"))
# 
#   if ("table" == "applicant") {
#     partenaires <-  temp
#   } else {
#     partenaires <- bind_rows(partenaires, temp)
#   }
#   
#   return(partenaires)
# }
# 
# etranger(applicant)
############################################################################################

participants_id <- bind_rows(participants_id, partenaires)
write.csv2(participants_id, file = paste0(chemin_open_data, "participants_id.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
save(participants_id, file = paste0(chemin_donnees,"python.RData"))
