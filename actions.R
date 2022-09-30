################################################################
# ACTIONS
###############################################################

ACT_PROP <- 
  read.csv(file = paste0(chemin_bd, "proposal/h20_ref_action_types.csv"), na.strings = "", encoding = "UTF-8")
#   data.frame()
# sql <- "SELECT * FROM H20_REF_ACTION_TYPES"
# access_sql_32(sql, "ACT_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
ACT_PROJ <- 
  read.csv(file = paste0(chemin_bd, "project/h20_ref_action_types.csv"), na.strings = "", encoding = "UTF-8")
#   data.frame()
# sql <- "SELECT * FROM H20_REF_ACTION_TYPES"
# access_sql_32(sql, "ACT_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))


#ACT_PROP <- read.csv2(paste0(chemin_bd, "ACTIONS_PROP.txt", na.strings = "") )
#ACT_PROJ <- read.csv2(paste0(chemin_bd, "ACTIONS_PROj.txt", na.strings = "") )

#charger nomenclatures themes avec harmonisation des codes à utiliser
actions <- load_one_obj("nomenclature/actions.rdata", "actions")
# actions <- readxl::read_xlsx("nomenclature/actions.xlsx")

#contrôle des deux tables thema referentes des bases H2020
comparaison_df(ACT_PROP, ACT_PROJ)
actions_ref <- dplyr::union(ACT_PROJ,ACT_PROP)

#actions présentes dans dernières actualisations
actions_base <- proposal %>% select(CODE_ACT) %>% 
  mutate(CODE_ACT=str_squish(CODE_ACT)) %>% 
  unique() %>% 
  full_join(project, by = "CODE_ACT") %>%  
  select(CODE_ACT) %>% 
  mutate(CODE_ACT=str_squish(CODE_ACT)) %>% 
  unique() %>% 
  left_join(actions, by = c("CODE_ACT" = "CD_ACTION_TYPE")) %>%  
  select(-level, -LB_ACTION_TYPE, -TRI)

# code à ajouter dans nomenclatures_modif.R
act_na <- actions_base %>% dplyr::filter(is.na(CODE_ACTION))
 if (nrow(act_na) > 0) {
   print("il manque un code action dans la table nomenclature, aller le chercher dans ref")
 } else{
   print("tout va bien")
 }  

# si regroupement de types d'action, modifier dans proposal et project
proposal <- group_action(proposal, "CSA-LSP", "CSA-LS")
project <- group_action(project, "CSA-LSP", "CSA-LS")

# MSCA modif des night csa
proposal <- proposal %>% dplyr::mutate(CODE_ACT=if_else(str_detect(APPEL, "NIGHT"), "MSCA-NIGHT", CODE_ACT))
project <- project %>% dplyr::mutate(CODE_ACT=if_else(str_detect(APPEL, "NIGHT"), "MSCA-NIGHT", CODE_ACT))

# actions dans les bases après modifs
actions_base <- proposal %>% select(CODE_ACT) %>% 
  mutate(CODE_ACT=str_squish(CODE_ACT)) %>% 
  unique() %>% 
  full_join(project, by = "CODE_ACT") %>%  
  select(CODE_ACT) %>% 
  mutate(CODE_ACT=str_squish(CODE_ACT)) %>% 
  unique() %>% 
  left_join(actions, by = c("CODE_ACT" = "CD_ACTION_TYPE")) %>%  
  select(-level, -LB_ACTION_TYPE, -TRI)




mla <- unique(actions$level) #niveau le plus élevé
mla <- sort(as.integer(mla), decreasing = T)
#ajoute  
generation_actions <- function(mla) {
  
  #dans la boucle, le prog va créer un niveau à la fois et lui attribuer les libellés correspondants
  for (i in mla){
    actions_base <- actions_base %>% 
      mutate(ta = decoupe_variable(CODE_ACTION, i)) %>% 
      left_join(actions, by = c("ta" = "CODE_ACTION")) %>% 
      mutate(temp = paste0(CD_ACTION_TYPE, " - ", LB_ACTION_TYPE)) %>% 
      select(-ta, -level)
 
    i=as.character(i)
      names(actions_base)[match("LB_ACTION_TYPE",names(actions_base))] <- paste0("action", i, "_lib")
      names(actions_base)[match("CD_ACTION_TYPE",names(actions_base))] <- paste0("action", i, "_id")
      names(actions_base)[match("temp",names(actions_base))] <- paste0("action_", i)
      names(actions_base)[match("TRI",names(actions_base))] <- paste0("action", i, "_tri")
  }
  
  return(actions_base)
} 

# act <- generation_actions(mla)
actions_base <- generation_actions(mla)

proposal <- merge(proposal, actions_base) %>% select(-CODE_ACTION, -CODE_ACT)
project <-  merge(project, actions_base) %>% select(-CODE_ACTION, -CODE_ACT)

unique(proposal$action4_id)
