########################################################################
# THEMES
#######################################################################

# table de réference des thema dans les bases h2020 déjà importées 

THEMA_PROP <- 
  read.csv(file = paste0(chemin_bd, "proposal/h20_ref_thema_all.csv"), na.strings = "", encoding = "UTF-8")
#   data.frame()
# sql <- "SELECT * FROM H20_REF_THEMA_ALL"
# access_sql_32(sql, "THEMA_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
THEMA_PROJ <- 
  read.csv(file = paste0(chemin_bd, "proposal/h20_ref_thema_all.csv"), na.strings = "", encoding = "UTF-8")
#   data.frame()
# sql <- "SELECT * FROM H20_REF_THEMA_ALL"
# access_sql_32(sql, "THEMA_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))

comp1 <- setequal(THEMA_PROP, THEMA_PROJ)
comp2 <- setequal(THEMA_PROJ, THEMA_PROP)
comp <- union(comp1,comp2)

#charger nomenclatures themes avec harmonisation des codes à utiliser
themes <- load_one_obj("nomenclature/theme.rdata", "themes")
          

#contrôle des deux tables thema referentes des bases H2020
comparaison_df(THEMA_PROJ,THEMA_PROP)
themes_ref <- dplyr::union(THEMA_PROP,THEMA_PROJ) %>% 
              select(CD_THEMA = CD_DIVNAME)


#thémes présentes dans actualisation
themes_base <- proposal %>% 
  select(CD_THEMA) %>% 
  unique() %>% 
  full_join(project, by = "CD_THEMA") %>% 
  select(CD_THEMA) %>% 
  unique()


#comparaison des themes de la table themes_base et de la table nomenclature avec nouveaux codes
theme <- left_join(themes_base, themes, by = "CD_THEMA") %>% 
  dplyr::filter(CODE_THEME != "" , !is.na(CODE_THEME))

if (nrow(theme)!=nrow(themes_base)) {
  print("il manque un theme dans la table nomenclature themes, voire table cor")
  cor <- left_join(themes_base, themes, by = "CD_THEMA") %>% 
    dplyr::filter(CODE_THEME == "" | is.na(CODE_THEME))
} else{
  print("tout va bien")
}
#faire programme quand il manquera un theme
theme <- left_join(themes_base, themes, by = "CD_THEMA") %>% 
  dplyr::filter(CODE_THEME != "" , !is.na(CODE_THEME))

#mise à jour des tables PROPOSAL et PROJECT avec les nouveaux codes
#mise à jour des tables PROPOSAL et PROJECT avec les nouveaux codes
maj_theme <- function(tab1) {
  theme %>% select(CD_THEMA, CODE_THEME) %>%
    right_join(tab1, by = "CD_THEMA") %>% 
    mutate(CODE_THEME = ifelse(str_detect(CODE_THEME,"^(AD|AE)") == TRUE, str_sub(CODE_THEME, 1, 2), CODE_THEME), 
           CODE_THEME = ifelse((CD_THEMA == "EU.2.0." & str_detect(CODE_DG, "CNECT") == TRUE), "ABAH", CODE_THEME),
           CODE_THEME = ifelse((CD_THEMA == "EU.2.0." & CODE_DG == "RTD"), "ABBE", CODE_THEME),
           CODE_THEME = ifelse((CD_THEMA == "EU.2.0." & CODE_DG == "EASME"), "ABF", CODE_THEME), 
           CODE_THEME = ifelse((CD_THEMA == "EU.3.3." & str_detect(APPEL, "FCH") == TRUE), "ACCI", CODE_THEME),
           CODE_THEME = ifelse(str_detect(APPEL, "-GD-"), "AUA", CODE_THEME))
}

proposal <- maj_theme(proposal)
project <- maj_theme(project)

unique(proposal$CODE_PANEL)

#verification qu'il n'y a pas de themes dans les panels
pan <- proposal %>% 
  dplyr::filter(!(is.na(CODE_PANEL) | CODE_PANEL== "null") & !(CD_THEMA %in% c("EU.1.1.", "EU.1.3."))) %>% 
  select(CD_THEMA, CODE_PANEL) %>% unique()
#ajouter la liste à la table THEMES si n'existe pas


# transforme certains panels en sous-themes
temp <- themes %>% select(code_temp = CODE_THEME, theme) 
temp <- left_join(pan, temp, by=c("CODE_PANEL"="theme")) %>% 
  dplyr::filter(!is.na(code_temp))

proposal <- proposal %>% 
  mutate(CODE_PANEL = ifelse(CODE_PANEL == "null", NA_character_, CODE_PANEL)) %>% 
  left_join(temp, by = c("CD_THEMA", "CODE_PANEL")) %>%
  # left_join(temp, by = c("CODE_PANEL" = "theme")) %>%
  mutate(CODE_THEME = ifelse(!is.na(code_temp), code_temp, CODE_THEME))

temp <- proposal %>% 
  dplyr::filter(!is.na(code_temp)) %>% 
  select(NUM_PROJET, code_temp) %>% 
  unique()

project <- project %>%  
  left_join(temp, by = "NUM_PROJET") %>%
  mutate(CODE_THEME = ifelse(!is.na(code_temp), code_temp, CODE_THEME)) %>%
  select(-code_temp)


#creation des generations themes
#max_level_new : les différents niveaux de la nomenclature themes
#programme qui ajoute à la table demandée tous les niveaux des themes
mln <- unique(theme$LEVEL_NEW) #niveau le plus élevé
mln <- mln[!is.na(mln)]
proposal <- generation_theme(proposal, mln)
project <- generation_theme(project, mln)

proposal <- proposal %>% 
  select(-CD_THEMA, -CODE_THEME, -code_temp) %>% 
  mutate(programme = ifelse(programme_abbr == toupper(programme_lib), programme_lib, paste0(programme_abbr," - ",programme_lib)), 
   area = ifelse(area_abbr == toupper(area_lib), area_lib,paste0(area_abbr," - ", area_lib)))

project <- project %>% 
  select(-CD_THEMA, -CODE_THEME)  %>% 
  mutate(programme = paste0(programme_abbr," - ",programme_lib), 
   area = paste0(area_abbr," - ", area_lib))

#pour l'instant supprimer les code_panels non ERC et MSCA; à voire si on les transforme en libellé thémes
proposal <- proposal %>% 
  mutate(CODE_PANEL = ifelse(!is.na(CODE_PANEL) & !(programme_abbr %in% c("ERC", "MSCA")), NA_character_, CODE_PANEL))

controle_NA(proposal$area_tri)
controle_NA(proposal$area_lib)
controle_NA(project$area_tri)
controle_NA(project$area_lib)

controle_NA(proposal$programme_abbr)
controle_NA(proposal$programme_lib)
controle_NA(project$programme_abbr)
controle_NA(project$programme_lib)

