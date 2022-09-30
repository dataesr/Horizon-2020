#########   ARTICLE 185


pays_fr <- load_one_obj("nomenclature/pays.rdata", "pays_fr")
themes <- load_one_obj("nomenclature/theme.rdata", "themes")

# ON CONSERVE LES PROJETS DONT LE LB8NETWORK : EMPIR et EUROSTARS 2
#AAL 2 => CD_THEMA = 3.1.         => ACA
#EDCTP 2 => CD_THEMA = 3.1.       => ACA
#EMPIR => CD_THEMA = 2.1.3.       => ABBB
#EUROSTARS 2 => CD_THEMA = 2.3.   => ABE

# "art_proj", "art_part", "eit_act", "eit_part"

verif_na <- art_proj[apply(art_proj, 2, function(x) any(is.na(x)))]

art_proj <- art_proj %>% 
  mutate(CODE_THEME = ifelse(APPEL == "EMPIR", "ABBB",
                      ifelse(APPEL == "Eurostars 2", "ABE", 
                      ifelse(APPEL == "AAL 2", "ACA", 
                             NA_character_)))) %>% 
  dplyr::filter(!is.na(CODE_THEME), CD_PROJ_ID != "7a3b6a48b3794a7c8d70d18f2a91afbc") %>% 
  mutate(ANNEE = "ND", STADE_PROJET = "SIGNED", CD_TOPIC = "Article 185", CODE_ACT = "G",
         DATE_DEBUT = format(dmy(DATE_DEBUT), "%d-%m-%Y"), DATE_FIN = format(dmy(DATE_FIN), "%d-%m-%Y"))

verif_na <- art_part[apply(art_part, 2, function(x) any(is.na(x)))]
  
art_part <- art_part %>% 
  group_by(CD_PROJ_ID) %>% 
  mutate(ligne= cur_group_id(),
    NUM_PROJET = paste0("art_", ligne), 
    role = ifelse(ROLE_PART %in% c("CO", NA_character_), "coordinator", "participant")) %>% 
  relocate(NUM_PROJET, role) %>% 
  arrange(NUM_PROJET, role) %>% 
  ungroup()

art_part$ORDRE_PART <- sequence(rle(art_part$CD_PROJ_ID)$lengths)

art_project <- art_proj %>% 
  left_join(art_part, by = "CD_PROJ_ID") %>% 
  select(-CD_PROJ_ID)
  
controle_NA(art_project$APPEL)
controle_NA(art_project$CODE_THEME)
controle_NA(art_project$ROLE_PART)
controle_NA(art_project$CODE_PAYS)
controle_NA(art_project$TYPE_ORG)
controle_NA(art_project$NNUM_PROJET)

art <- art_project %>%
  group_by(APPEL, CODE_THEME, NUM_PROJET, CODE_PAYS, role) %>% 
  mutate(part = n(), subv = sum(SUBV, na.rm = TRUE)) %>% 
  select(APPEL, CODE_THEME, NUM_PROJET, CODE_PAYS, role, part, subv) %>% 
  unique()

controle_NA(art$CODE_PAYS)

n_distinct(art$NUM_PROJET)
prettyNum(sum(art$subv, na.rm = TRUE), big.mark = " ")
prettyNum(sum(art_project$SUBV, na.rm = TRUE), big.mark = " ")

############################################################################################################
#EIT

#verification des cd_activités sans KIC : du coup non ventilation des activités et 
#regroupement sous EIT
temp <- full_join(eit_participant, eit_activities, by = "CD_ACTIVITY_ID") %>% 
  select(CD_ACTIVITY_ID, LB_KIC_NAME) %>%  
  unique()

verif_na <- temp[apply(temp, 2, function(x) any(is.na(x)))]
# eit_act <- eit_activities %>% 
#   select(LB_KIC_NAME, CD_ACTIVITY_ID) %>% unique() %>% 
#   arrange(CD_ACTIVITY_ID)
# eit_act <- eit_act %>% 
#   mutate(APPEL = "EIT", CODE_THEME = "AG",
#          #ANNEE = "ND", nb = as.character(1:nrow(eit_act)), 
#          # NUM_PROJET = ifelse(str_length(nb) == 1, paste0("00000", nb),
#          #              ifelse(str_length(nb) == 2, paste0("0000", nb),
#          #              ifelse(str_length(nb) == 3, paste0("000", nb),
#          #              ifelse(str_length(nb) == 4, paste0("00", nb),
#          #              ifelse(str_length(nb) == 5, paste0("0", nb), nb))))),
#          STADE_PROJET = "SIGNED", CD_TOPIC = "EIT", CODE_ACT = "U"
#          # CODE_THEME = ifelse(LB_KIC_NAME == "Climate", "AGA",
#          #               ifelse(LB_KIC_NAME == "Digital", "AGB",
#          #                ifelse(LB_KIC_NAME == "Health", "AGC",
#          #                 ifelse(LB_KIC_NAME == "InnoEnergy", "AGD",
#          #                  ifelse(LB_KIC_NAME == "Raw Materials", "AGE", NA_character_)))))
#          ) %>% 
#   unique()


# eit_part <- eit_participant %>% 
#   arrange(CD_ACTIVITY_ID) %>% 
#   mutate(APPEL = "EIT", CODE_THEME = "AG", ROLE_PART = "others") %>% 
#   group_by(CODE_THEME, CODE_ORG, PIC_ID2, ROLE_PART, CODE_PAYS, NOM_LONG, NOM_COURT, TYPE_ORG) %>% 
#   mutate(PME = ifelse(is.na(FL_SME)| FL_SME == "NO", "N", FL_SME), part = n(), SUBV = sum(AM_CONTRIB_EIT, na.rm = TRUE)) %>% 
#   select(APPEL, CODE_THEME, CODE_ORG, PIC_ID2, ROLE_PART, CODE_PAYS, NOM_LONG, NOM_COURT, TYPE_ORG, PME, part, SUBV) %>% 
#   unique()
#eit_part$ORDRE_PART <- unlist(lapply(rle(as.vector(eit_part$CD_ACTIVITY_ID))$lengths,function(x){1:x}))

# A REVOIR certainement des CD_ACTIVITY en plus dans les participants
# eit_project <- left_join(eit_act, eit_part, by = "CD_ACTIVITY_ID") %>% unique()
# 
# save(eit_project, file = str_c(chemin_donnees, "eit_project.RData"))

# doublons dans eit_project non résolu pour l'instant; on utilise à la place uniquement la table des particpants
eit <- eit_participant %>% 
  group_by(CD_ACTIVITY_ID) %>% 
  mutate(ligne= cur_group_id(),
         NUM_PROJET = paste0("eit_", ligne),
         role = ifelse(str_detect(tolower(ROLE_PART), "head|lead|coordinat") | is.na(ROLE_PART), 'coordinator', 'participant')) %>% 
  ungroup() %>% 
  group_by(CODE_PAYS, role, NUM_PROJET) %>% 
  mutate(subv = sum(AM_CONTRIB_EIT, na.rm = TRUE), part = n()) %>% 
  select(NUM_PROJET, CODE_PAYS, role, part, subv) %>% 
  unique() %>% 
  mutate(APPEL = "EIT", CODE_THEME = "AG") %>% 
  ungroup()


controle_NA(eit$CODE_PAYS)
controle_NA(eit$NUM_PROJET)

autres_prog <- as.data.frame(bind_rows(art,eit))
autres_prog <- autres_prog %>% 
  mutate(CODE_PAYS=if_else(CODE_PAYS=="GB", "UK", if_else(CODE_PAYS=="GR", "EL", CODE_PAYS))) %>% 
  left_join(pays_fr, by = "CODE_PAYS") %>% 
  select(-CODE_PAYS) %>% 
  dplyr::rename(CODE_PAYS = CODE_PAYS_RGP) %>% 
  group_by(CODE_THEME, CODE_PAYS, role, NUM_PROJET) %>% 
  mutate(subv = sum(subv), part = sum(part)) %>% 
  select(-APPEL) %>% unique() %>% ungroup()

#ajout des libelles et niveaux aux themes
theme <- autres_prog %>% left_join(themes, by = "CODE_THEME") %>% 
  select(CODE_THEME, LEVEL_NEW) %>% unique()
mln <- unique(theme$LEVEL_NEW) #niveau le plus élevé
autres_prog <- generation_theme(autres_prog, mln) %>% 
  dplyr::rename(pays_code = CODE_PAYS, pays_lib = NOM_PAYS, programme_code = programme_abbr,
                area_code = area_abbr) %>% 
  select(-CODE_THEME, -pilier_abbr)
#20/11/2019 - suppression du code... , area_code = area_abbr

autres_prog %>% group_by(pays_code, role) %>% 
  dplyr::summarize(pres=n_distinct(NUM_PROJET), part=sum(part), sub=sum(subv)) %>% 
  dplyr::filter(pays_code=="FR")


# autres_prog <- autres_prog %>% select(-CODE_THEME) %>% 
#   mutate(programme = ifelse(programme_abbr == toupper(programme_lib), programme_lib, paste0(programme_abbr," - ",programme_lib)), 
#          area = ifelse(area_abbr == toupper(area_lib), area_lib,paste0(area_abbr," - ", area_lib)))

controle_NA(autres_prog$pilier)
controle_NA(autres_prog$pays_code)
controle_NA(autres_prog$programme_code)

verif_na <- autres_prog[apply(autres_prog, 2, function(x) any(is.na(x)))]


# nombre de projets pour tableau public RAPPEL
prog_aut <- as.numeric(n_distinct(autres_prog$NUM_PROJET, na.rm = TRUE))

# prog_part <- as.numeric(sum(autres_prog$Part, na.rm=TRUE))
# prog_subv <- as.numeric(sum(autres_prog$Subv, na.rm=TRUE))


sauve_fin(autres_prog, "autres_prog")
save("art_project", "eit_participant", file = str_c(chemin_donnees,  "autres_prog_details.RData"))

# prog <- autres_prog %>% group_by(CODE_PAYS) %>% 
#   mutate(Subv = sum(subv), Part = sum(part), Proj = n_distinct(NUM_PROJET, na.rm = TRUE)) %>% 
#   select(-APPEL, -NUM_PROJET, -part, -subv) %>% unique()













