pays_fr <- read.csv2("data_access_txt/pays.txt", na.strings = "") 
pays_fr[pays_fr == ""]<- NA_character_

pays_statut <- pays_fr %>% 
               filter(is.na(CODE_PAYS_RGP)) %>% 
               select(CODE_STATUT = CODE_PAYS, NOM_STATUT = NOM_PAYS)

pays_fr <- pays_fr %>% 
        filter(!is.na(CODE_PAYS_RGP)) %>% 
        select(-ARTICLE_PAYS)


pays_statut_gen <- read.csv2("data_access_txt/pays_STATUT.txt", na.strings = "") 
pays_statut_gen[pays_statut_gen == ""]<- NA_character_
pays_statut_gen <- pays_statut_gen %>% reshape::rename(., c(STATUT="CODE_STATUT"))

save(list = c("pays_fr", "pays_statut", "pays_statut_gen"), file = "nomenclature/pays.RData")
#####################################################################################################################
themes <- read.csv2("data_access_txt/THEMES.txt", na.strings = "")
          #select(-CODE_THEME_old, -CODE_TEMP, CD_THEMA = CD_DIVNAME, abbr = CD_ABBR, theme = LB_DESC)
themes[themes == ""]<- NA_character_

#themes <- themes %>% mutate(CD_THEMA = ifelse(CODE_THEME %in% c("ABAH","ABBE", "ABF", "ACCI"), NA_character_ , CD_THEMA))

save("themes", file = "nomenclature/theme.RData")
##############################################################################################################
panels <- read.csv2("data_access_txt/PANEL.txt", na.strings = "")
panels[panels == ""]<- NA_character_

save("panels", file = "nomenclature/panels.RData")
###############################################################################################################
actions <- read.csv2("data_access_txt/ACTION_TYPE.txt", na.strings = "") %>% select(-ACTION_PARENT)
actions[actions == ""]<- NA_character_

save("actions", file = "nomenclature/actions.RData")
###############################################################################################################
type_org <- read.csv2("data_access_txt/TYPE_ORG.txt", na.strings = "")
type_org[type_org == ""]<- NA_character_
save("type_org", file = "nomenclature/type_org.RData")
################################################################################################################
contrib_ue <- read.csv2("data_access_txt/CONTRIB_UE.csv", na.strings = "")
save("contrib_ue", file = "contrib_ue.RData")
################################################################################################################
topic <- read.csv2("data_access_txt/TOPIC.txt", na.strings = "")
save("topic", file = "nomenclature/topic.RData")
