####################################################################################
# Types de structures participantes
###################################################################################

#charger nomenclatures type org avec libellés propres
type_org <- load_one_obj("nomenclature/type_org.rdata", "type_org")

#type org de la dernière actualisation
type_base <- participant %>%
  select(CODE_TYPE_ORG = TYPE_ORG) %>%
  unique()


type_verif <- inner_join(type_base, type_org, by = "CODE_TYPE_ORG")

if (nrow(type_verif) != nrow(type_base)) {
  print("il manque un type org dans la table nomenclature type org")
} else{
  print("tout va bien")
}

participant <- participant %>% 
  left_join(type_org, by = c("TYPE_ORG" = "CODE_TYPE_ORG")) %>% 
  dplyr::rename(org_tri = ORDRE, org_typ = NOM_TYPE_ORG, org_abbr = ORG_alias) %>% 
  select(-TYPE_ORG)

controle_NA(participant$org_abbr)
controle_NA(participant$org_typ)
