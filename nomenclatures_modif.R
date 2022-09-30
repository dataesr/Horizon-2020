####################################################################################################################
#   PAYS
#################################################

#changement de statut pour la Turquie
#pays_ref <- pays_ref %>% mutate(CD_STATUS = ifelse(CD_COUNTRY == "TR", "CANDIDATE", CD_STATUS))

#ajout de curacao aux pays-bas 09/11/2017
pays_fr <- pays_fr %>% mutate(CODE_PAYS_RGP = ifelse(CODE_PAYS == "CW", "NL", CODE_PAYS_RGP))
resave(c("pays_fr"), file = "nomenclature/pays.rdata")

####################################################
#statut pays ajout et libellé en français 02/10/2019
y <- c("CANDIDATE", "CANDIDATE", "CANDIDATE", "ASSO_CAND")
pays_statut_gen <- rbind(pays_statut_gen, y)
resave(c("pays_statut_gen"), file = "nomenclature/pays.rdata")


#statut pays ajout et libellé en français 09/11/2017
print(statut_verif)
x <- c("ASSOCIATE-CANDIDATE-INCO-WESTERNBALKAN", "Pays associés Ouest-Balkans (potentiellement candidats)")
y <- c("ASSOCIATE-CANDIDATE", "Pays associés (potentiellement candidats)")
pays_statut <- rbind(pays_statut, x, y)
resave(c("pays_statut"), file = "nomenclature/pays.rdata")

x <- c("ASSOCIATE-CANDIDATE-INCO-WESTERNBALKAN", "ASSOCIATE-CANDIDATE-INCO-WESTERNBALKAN", "ASSOCIATE-CANDIDATE", "ASSO_CAND")
y <- c("ASSOCIATE-CANDIDATE", "ASSOCIATE-CANDIDATE", "ASSOCIATE-CANDIDATE", "ASSO_CAND")
pays_statut_gen <- rbind(pays_statut_gen, x, y)
resave(c("pays_statut_gen"), file = "nomenclature/pays.rdata")

#####################################################################################################################

 ##############################################################################################################
# THEMES
############################################################################################################
#charger nomenclatures themes avec harmonisation des codes à utiliser
themes <- load_one_obj("nomenclature/theme.rdata", "themes")

temp <- proposal %>% select(area_lib, area_tri, area) %>%  unique() %>% 
  group_by(area_tri) %>%summarize(nb = n()) %>%  filter(nb > 1) %>% 
  left_join(proposal, by = "area_tri") %>% 
  select(area_lib, area_tri, area) %>%  unique()

theme <- read.csv2("nomenclature/themes.csv")

#ajout EIT -> AG
temp <- eit_activities %>% select(abbr = LB_KIC_NAME) %>% unique() 
CODE_THEME <- c("AGA", "AGB", "AGC", "AGD", "AGE")
y <- c("Climate", "Digital", "Health", "InnoEnergy", "Raw Materials")

temp <- temp %>% cbind(.,CODE_THEME) %>% 
  mutate(theme = abbr, NEW_PARENT = "AG", LEVEL_NEW = 3)

themes <- rbind(themes,temp)


# 24/11/2021
themes <- themes %>% 
  mutate(CODE_THEME = ifelse(CD_THEMA == "EU.2.1.", "ABG", CODE_THEME),
         abbr = ifelse(CD_THEMA == "EU.2.1.", "LEIT", abbr),
         NEW_PARENT = ifelse(CD_THEMA == "EU.2.1.", "AB", NEW_PARENT),
         LEVEL_NEW = ifelse(CD_THEMA == "EU.2.1.", 3, LEVEL_NEW))


#05-01-2021

themes <- themes %>% 
  mutate(CODE_THEME = 
          ifelse(str_detect(CODE_THEME, "^AA"), paste0("A_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AB"), paste0("B_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AC"), paste0("D_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AT"), paste0("E_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AG"), paste0("F_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AD"), paste0("G_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AE"), paste0("H_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AF"), paste0("K_", CODE_THEME),
          ifelse(str_detect(CODE_THEME, "^AZ"), paste0("J_", CODE_THEME),
          CODE_THEME))))))))))


######GREEN DEAL
temp <- project %>% 
  dplyr::filter(str_detect(APPEL, "GD")) %>% 
  select(CD_THEMA, APPEL, CD_TOPIC) %>% 
  unique()

x <- c('AA', '', 'GD', 'Green Deal', '', 'A', '2')

themes <- rbind(themes, x)


# reprise des themes et de leur code
themes$CODE_THEME <- substring(themes$CODE_THEME, 2)

themes <- themes %>% dplyr::filter(abbr != 'GD')

x <- c('AU', '', 'GD', 'Green Deal', '', 'A', '2')
y <- c('AUA', '', 'GD', 'Green Deal', '', 'AU', '3')
themes <- rbind(themes, x, y)



save(themes, file = "nomenclature/theme.RData")


#####################################################################################################################

##############################################################################################################
# INSTRUMENT
############################################################################################################
# ajout actions dans la table nomenclatures

# act_na <- actions_ref %>% right_join(act_na, by =c("CD_ACTION_TYPE"="CODE_ACT")) %>% 
#   mutate(CODE_ACTION = "OE", level = 2, TRI = "EE") %>% 
#   dplyr::rename(LB_ACTION_TYPE=LB_ACTION_TYPE_DESCR)
# actions <- rbind(actions, act_na)
# save(actions, file="nomenclature/actions.RData")

#ajout d'une ligne pour article 185

actions <- charger_rdata("nomenclature/actions.rdata", "actions")


a <- c(2, "CB", "CSA-LSP", "DB", "Coordination and support action Lump sum")
b <- c(2, "IIC", "EuroHPC-IA", "GC", "EuroHPC-IA")
# c <- c(2, "IIB", "EuroHPC-CSA", "GB", "EuroHPC-CSA")
# d <- c(2, "IIA", "EuroHPC-RIA", "GA", "EuroHPC-RIA")
# e <- c(2, "GC", "SME-2b", "TC", "SME Instrument (grant only and blended finance)")
actions <- rbind(actions, a,b)
save(actions, file="nomenclature/actions.RData")



x <- c(2, "AB", "RIA-LS", "QA", "Research and Innovation action Lump Sum")
actions <- rbind(actions, x)
save(actions, file="nomenclature/actions.RData")

x <- c(3, "PAA", "JTI-Shift2Rail-RIA-LS", "WBA", "Research and Innovation Action Lump-Sum")
y <- c(3, "PBA", "JTI-Shift2Rail-IA-LS", "WAA", "Innovation Action Lump-Sum")
actions <- rbind(actions, x, y)
save(actions, file="nomenclature/actions.RData")

x <- c(1, "Z1", "Article 185", "G", "Article 185")
y <- c(1, "Z2", "EIT", "U", "European Institute of Innovation & Technology")
actions <- rbind(actions, x, y)


x <- c(2, "EF", "MSCA-NIGHT", "OF", "European Researchers' Night")
actions <- rbind(actions, x)


actions <- actions %>% mutate(level = if_else(nchar(TRI)==3, "3", level))

actions <- actions %>% mutate(CODE_ACTION=if_else(CODE_ACTION=="HGA", "HDA", CODE_ACTION))

save(actions, file="nomenclature/actions.RData")
