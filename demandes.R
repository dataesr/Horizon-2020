##############################################################################
#DEMANDE SAMI
##############################################################################

temp = proposal %>% select(NUM_PROJET, ACRONYM_PROJET, TITRE_PROJET, ABSTRACT, APPEL, 
                           CD_TOPIC, LB_TOPIC_DESC, pilier, programme_abbr, programme_lib, 
                           area_abbr, area_lib, panel_1, panel_2) %>% unique()
write.csv2(temp, file = "D:/Sami/depots_projets_03_2018.csv", row.names = FALSE)


##################################################
# création table theme par niveau pour les participants
####################################################


temp <- proposal %>% 
  select(pilier_tri, pilier, programme_tri, programme_abbr, programme_lib, area_tri, area_abbr, area_lib) %>% 
  unique()

temp2 <- project %>% 
  select(pilier_tri, pilier, programme_tri, programme_abbr, programme_lib, area_tri, area_abbr, area_lib) %>% 
  unique()


write.csv2(temp, file = paste0(chemin, "exe/nomenclature/themes_niv.csv"))
 
###########################################################################
# etranger collab avec france pour Emmanuel
###########################################################################

temp <- contract_part %>% filter(CODE_PAYS == "FR") %>%  select(NUM_PROJET) %>%  unique()
write.csv2(temp, file = paste0(chemin_bd, "num_proj_fr.csv"))

###########################################################################
#projet + themes pour scanr
###########################################################################

projets_scanr <- contract %>% select(NUM_PROJET, area_tri, area_lib, programme_tri, programme_lib, pilier_tri, pilier) %>% unique()
write.csv2(projets_scanr, file = paste0(chemin_bd, "projets_scanr.csv"))



