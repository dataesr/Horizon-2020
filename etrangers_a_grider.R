#PARTICIPANTS
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
