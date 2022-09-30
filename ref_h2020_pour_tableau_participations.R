load(file="D:/pcrdt_project/H2020/2018_06/donnees/datas.RData")
options("digits"=15)

temp <- proposal%>% 
  select(-ABSTRACT, -TITRE_PROJET, -DATE_CALL) %>% filter(CODE_PAYS == "FR")

write.csv2(temp, file="D:/pcrdt_project/H2020/2018_06/donnees/proposal_applicants_det.csv", na="", row.names = FALSE,
           fileEncoding = "UTF-8", quote = TRUE)


temp <- proposal %>% group_by(programme_lib, pilier, NUM_PROJET, CODE_PAYS, STATUT_EVAL, ELIGIBLE) %>% 
  mutate(subv=sum(SUBV), Statut = "EVA") %>% 
  select(programme = programme_lib, pilier, NUM_PROJET, CODE_PAYS, STATUT_EVAL, ELIGIBLE, Statut, subv) %>% 
  unique()

temp1 <- temp %>%  filter(STATUT_EVAL == "MAIN") %>% 
  mutate(Statut = "RET") 

temp <- bind_rows(temp, temp1) %>% mutate(subv = if_else(is.na(subv), 0, subv))



all <- temp %>% group_by(Statut) %>% 
  mutate(code_pays = "ALL", pilier = "ALL", programme = "ALL", contrib=as.numeric(sum(subv)), nb_proj =  n_distinct(NUM_PROJET)) %>% 
  select(Statut, code_pays, pilier, programme, contrib, nb_proj) %>% unique()

prog <- temp %>% group_by(Statut, pilier, programme) %>% 
  mutate(code_pays = "ALL", contrib=sum(subv), nb_proj =  n_distinct(NUM_PROJET)) %>% 
  select(Statut, code_pays, pilier, programme, contrib, nb_proj) %>% unique()

fr <- temp %>% filter(CODE_PAYS == "FR") %>% 
  group_by(Statut, pilier, programme, code_pays = "FR") %>% 
  mutate(contrib=sum(subv), nb_proj =  n_distinct(NUM_PROJET)) %>% 
  select(Statut, code_pays, pilier, programme, contrib, nb_proj) %>% unique()

ref_h2020 <- bind_rows(all, prog, fr)
write.table(ref_h2020, file="D:/pcrdt_project/H2020/2018_06/donnees/ref_h2020.csv", na="", sep = ";", row.names = FALSE, dec=".")
