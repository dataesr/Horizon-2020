#############################################################################################
# CONTRIBUTIONS UE
##############################################################################################

###################################
# IMPORT si nouvelles données
#################################
# contrib_ue <- read.csv2(file = "D:/pcrdt_project/H2020/contrib_UE/contrib_2018.csv") %>% 
#   select(-lib_pays)
# save(contrib_ue, file = "contrib_ue.RData")

##############################################################################################
#ATTENTION refaire code avec les EIT et ART185
#FAire le lien avec lke fichier SYNTHESE
#provisoire: rajout du lib_pays oublié et remplacement des subv par ceux de la table synthèse
load(file = paste0(chemin_donnees,"contrib.RData"))
contrib <- tab %>% select(-subv, -subv_part)
load(file = paste0(chemin_donnees,"contract_synthese.RData"))
subv_tot <- sum(tab$subv)
contrib <- left_join(contrib, tab, by = c("CODE_PAYS"= "pays_code")) %>%
  select(CODE_PAYS, pays_lib, subv, contrib_ue) %>%
  mutate(subv_part = round(subv/subv_tot, 4))



options(digits = 8)

#chargement de la table Rdata : demander à Frédéric Laurent les nouvelles données chaque année
load(file = "contrib_ue.RData")


contrib <- contrib_ue %>%  group_by(CODE_PAYS) %>% 
                           mutate(contrib = sum(contrib_ue), div = n()) %>%  
                           select(-annee, -contrib_ue) %>% unique() %>%
                           mutate(contrib_ue = contrib/div) %>% 
                           select(-contrib, -div)



temp <- contract_synthese %>% 
                     select(pays_code, pays_lib, subv, subv_tot) %>% 
                     group_by(pays_code) %>% 
                     mutate(subv = sum(subv)) %>% 
                     filter(subv > 0) %>% 
                     unique() %>% 
                     mutate(subv_part = round(subv/subv_tot, 4))



# temp <- contract%>% 
#   select(CODE_PAYS, pays_lib, subv) %>% 
#   group_by(CODE_PAYS) %>% 
#   mutate(subv = sum(subv)) %>% 
#   filter(subv > 0) %>% 
#   unique() %>% 
#   mutate(subv_part = round(subv/subv_tot, 4))
#subv_tot <- sum(temp$subv)
#temp <- temp %>% mutate(subv_part = round(subv/subv_tot, 4))


contrib <- left_join(contrib, temp, by = c("CODE_PAYS"= "pays_code")) %>% select(-subv_tot)

contrib <- as.data.frame(contrib)
