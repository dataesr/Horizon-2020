#PARTICIPANTS

# PARTICIPANTS FR #######################################################################################################
# Programme extraction des participants FR pour vérifier le code REG_ID et sireniser les PIC
library(devtools)
#install_github("biostatmatt/sas7bdat.parso")
library(sas7bdat.parso)

s7b2csv("D:/reperage/2018_06/pcrdt/referentiel.sas7bdat", "D:/reperage/2018_06/pcrdt/referentiel.csv") 
ref<-read.csv("D:/reperage/2018_06/pcrdt/referentiel.csv", colClasses = "character")


#traitement sur le siren_corda à voir comment gérer
temp <- participant %>% select(PIC_ID, name_source, CODE_PAYS, CD_REG_ID) %>% unique() %>%
  mutate(id0 = str_replace_all(CD_REG_ID, "N/A|n/a", NA_character_),
         id0 = str_replace_all(id0, "^[[:punct:]]+$", NA_character_)) %>% 
  tidyr::separate(id0, into=c("id1", "id2"), sep="/") %>% 
  mutate(id3 = if_else(str_detect(id1, "^W"), id1, str_replace_all(id1, "\\D+", "")),
         id3 = str_replace_all(id3, " +", ""),
         id4 = if_else(nchar(id3) < 9, NA_character_, if_else(nchar(id3) > 9, substr(id3,1,9), id3)),
         CD_REG_VAT_ID = str_replace_all(CD_REG_VAT_ID, " +", ""),
         id4 = if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,-9), "\\d+") & nchar(CD_REG_VAT_ID) == 13, 
                       str_sub(CD_REG_VAT_ID, -9), 
                       if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,-9), "\\d+") & nchar(CD_REG_VAT_ID) == 16,
                               str_sub(CD_REG_VAT_ID,3,11), 
                               if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID,1,2), "^FR") & nchar(CD_REG_VAT_ID) == 14,
                                       str_sub(CD_REG_VAT_ID,-9),
                                       if_else(is.na(id4) & str_detect(str_sub(CD_REG_VAT_ID), "^\\d+$") & nchar(CD_REG_VAT_ID) == 14,
                                               str_sub(CD_REG_VAT_ID,1,9),                       
                                               id4))))) %>% 
  select(-id1, -id3)

ref_t <- ref %>% select(id_ref, numero_uai, num_nat_struct, siren, an_fermeture_ref) %>% unique()

fr_pic <- temp %>% 
  left_join(ref_t, by = c("id4"="siren")) %>% 
  mutate(ok = if_else(!is.na(id_ref), "ok", NA_character_))

write_csv(fr_pic, "D:/pcrdt_project/participants/2019_11/FR_PIC_SIREN.xlsx", na="")
write.csv2(ref, "C:/Users/zfriant/OneDrive - Ministère de l'Éducation Nationale, de l'Enseignement Supérieur et de la Recherche/PCRI/participations/referentiel.csv", na="")




# PARTICIPANTS ETRANGERS #######################################################################################################
#chargement des datas nettoyées
applicant <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "applicant") %>% 
  dplyr::rename(PIC_ID = CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT) %>% 
  select(NUM_PROJET,PIC_ID,PIC_ID2, name_source, acronym_source, CODE_PAYS, ADRESS, CP, CITY, URL_PART, pays_lib) %>% 
  unique()
  
participant <- charger_rdata(paste0(chemin_donnees,"datas.RData"), "participant") %>% 
  dplyr::rename(PIC_ID = CODE_ORG, name_source=NOM_LONG, acronym_source=NOM_COURT) %>% 
  select(NUM_PROJET,PIC_ID,PIC_ID2, name_source, acronym_source, CODE_PAYS, ADRESS, CP, CITY, URL_PART, pays_lib) %>% 
  unique()


part <- left_join(participant, 
                  applicant[,c("NUM_PROJET", "PIC_ID", "PIC_ID2", "CODE_PAYS")], 
                  by = c("NUM_PROJET", "PIC_ID", "PIC_ID2", "CODE_PAYS")) %>% unique() %>% 
  mutate(stage="proj")


app <- anti_join(applicant, part[,c("NUM_PROJET", "PIC_ID", "PIC_ID2")],  
                 by = c("NUM_PROJET", "PIC_ID", "PIC_ID2")) %>% unique() %>% 
  mutate(stage="prop")

part <- bind_rows(part, app) 

write.csv2(part, file=paste0(chemin_donnees, "part_etrangers.csv"), na="")
