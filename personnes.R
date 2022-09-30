#traitement PERSONNES
PERSO_PROP <- read.csv2(paste0(chemin_bd,"/PROPOSAL_CONTACT_PERS.txt")) %>% 
  mutate(perso_type = "contact", stage = "proposal")
MSCA_PERSO <- read.csv2(paste0(chemin_bd,"/PROPOSAL_MSCA_RES.txt")) %>% 
  mutate(perso_type = "msca", stage = "proposal")
ERC_PERSO <- read.csv2(paste0(chemin_bd,"/PROPOSAL_ERC_PI.txt")) %>% 
  mutate(perso_type = "PI", stage = "proposal")

# PERSO_CONT <- read.csv2(paste0(chemin_bd,"/PROJECT_PERSO.txt")) %>% 
#   mutate(perso_type = "contact", stage = "project")
# MSCA_CONT <- read.csv2(paste0(chemin_bd,"/PROJECT_MSCA_RES.txt")) %>% 
#   mutate(perso_type = "msca", stage = "project")
# ERC_CONT <- read.csv2(paste0(chemin_bd,"/PROJECT_ERC_PI.txt")) %>% 
#   mutate(perso_type = "PI", stage = "project")



personnes <- union_all(PERSO_PROP,MSCA_PERSO,ERC_PERSO) %>% 
  select(NUM_PROJET = CD_PROP_ID, PIC_ID = CD_PART_PIC, PIC_ID2 = CD_APPL_PIC, ROLE = CD_ROLE, TITLE_PERSO = TITLE, FIRST_NAME, LAST_NAME,
         GENDER, PHONE = CD_PHONE, EMAIL, NATIONALITY = CD_MAIN_NATIONALITY, COUNTRY_ORIGINE = CD_ORIGIN_COUNTRY, 
         COUNTRY_RESIDENCE = CD_RESIDENCE_COUNTRY, perso_type, STAGE = stage)

temp <- participants %>% select(STAGE, NUM_PROJET, PIC_ID, PIC_ID2, ORDRE_PART) %>%  unique()

personnes <- left_join(personnes, temp, by = c("NUM_PROJET", "PIC_ID2", "PIC_ID", "STAGE")) %>% 
             select(-PIC_ID2)

#personnes[, ] <- lapply(personnes[, ], as.character)

write.csv2(personnes, file = paste0(chemin_open_data, "personnes.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
save(personnes, file = paste0(chemin_donnees,"python.RData"))
