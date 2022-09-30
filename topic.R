##############################################################
# TOPICS
#############################################################

TOPIC_PROP <- 
  read.csv(file = paste0(chemin_bd, "proposal/h20_ref_topic.csv"), na.strings = "", encoding = "UTF-8")
#   data.frame()
# sql <- "SELECT * FROM H20_REF_TOPIC"
# access_sql_32(sql, "TOPIC_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
TOPIC_PROJ <- 
  read.csv(file = paste0(chemin_bd, "project/h20_ref_topic.csv"), na.strings = "", encoding = "UTF-8")
  # data.frame()
# sql <- "SELECT * FROM H20_REF_TOPIC"
# access_sql_32(sql, "TOPIC_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))

comp1 <- setequal(TOPIC_PROP, TOPIC_PROJ)
comp2 <- setequal(TOPIC_PROJ, TOPIC_PROP)
comp <- union(comp1,comp2)


#contrôle des deux tables thema referentes des bases H2020
comparaison_df(TOPIC_PROP,TOPIC_PROJ)
topic_ref <- dplyr::union(TOPIC_PROP,TOPIC_PROJ)

#topic <- charger_rdata("nomenclature/topic.rdata", "topic") 

topic <- topic_ref %>% select(CD_TOPIC, LIB_TOPIC = LB_TOPIC_DESC) %>% unique()

proposal <- left_join(proposal, topic, by = "CD_TOPIC")
project <- left_join(project, topic, by = "CD_TOPIC")

controle_NA(proposal$CD_TOPIC)
controle_NA(project$CD_TOPIC)
