
library(devtools)
#install_github("biostatmatt/sas7bdat.parso")
library(sas7bdat.parso)

s7b2csv("D:/reperage/2018_06/pcrdt/referentiel.sas7bdat", "D:/reperage/2018_06/pcrdt/referentiel.csv") 
ref<-read.csv("D:/reperage/2018_06/pcrdt/referentiel.csv", colClasses = "character")

s7b2csv("D:/reperage/2018_06/pcrdt/part_ref.sas7bdat", "D:/reperage/2018_06/pcrdt/part_ref.csv") 
part_ref<-read.csv("D:/reperage/2018_06/pcrdt/part_ref.csv", colClasses = "character")

s7b2csv("D:/reperage/2018_06/pcrdt/niveau.sas7bdat", "D:/reperage/2018_06/pcrdt/niveau.csv") 
niveau<-read.csv("D:/reperage/2018_06/pcrdt/niveau.csv", colClasses = "character")


date_recodage = "2018_06"

part <- part_ref %>% 
  select(part_order, id_ref_niv_6) %>% 
  mutate(NUM_PROJET = substring(part_order,1,6), ORDER_PART = substring(part_order,8), id = id_ref_niv_6)

temp <- participant %>% filter(TYPE_PART == "thirdparty", CODE_PAYS == "FR") %>% 
  select(NUM_PROJET, ORDRE_PART) %>% unique()

temp2 <- temp %>% left_join(participant, by = c("NUM_PROJET", "ORDRE_PART")) %>% 
  select(NUM_PROJET, ORDRE_PART, ORDRE_ALL, CODE_ORG, PIC_ID2, TYPE_PART, NOM_LONG, NOM_COURT, SUBV, SUBV_NET)

part_ <- charger_rdata(paste0(chemin_donnees,"python.RData"), nom_objet = "participant_id")
part_id <- part_ %>% filter(STAGE == "project") %>%  select(NUM_PROJET, ORDRE_ALL, PIC_ID, PIC_ID2, name, acronym,
                                                            num_nat_struct,siret,numero_uai)

temp3 <- temp2 %>% left_join(part_id, by = c("NUM_PROJET", "ORDRE_ALL"))
