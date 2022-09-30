#############################################################################################
# CONTRATS
##############################################################################################

project <- load_one_obj(rdata = paste0(chemin_donnees,"datas2.RData"), "project")
autres_prog <- load_one_obj(rdata=paste0(chemin_donnees,"autres_prog.RData"), "tab")

# peut etre supprimer pb réglé dans la partie nettoyage
# coord_trop <- role_coordinator(project) %>% 
#   mutate(NUM_PROJET = as.numeric(as.character(NUM_PROJET)))
# coord_trop <- coord_trop %>% 
#   left_join(project, by=c("NUM_PROJET", "ROLE_PART")) %>% 
#   group_by(NUM_PROJET) %>% 
#   mutate(ROLE_PART=ifelse(SUBV>0, 'coordinator', 'partner'), nb=ifelse(ROLE_PART=='coordinator', 1, 0)) %>% 
#   dplyr::filter(ROLE_PART=="partner") %>% 
#   select(NUM_PROJET, ORDRE_PART) %>% 
#   unique()
# project <- project %>% 
#   mutate(ROLE_PART = ifelse(NUM_PROJET %in% coord_trop$NUM_PROJET & ORDRE_PART %in% coord_trop$ORDRE_PART & SUBV==0, 
#                             "partner", ROLE_PART))



gc()

#Variable statut de projets pour les propositions
project <- project %>% 
  mutate(coord = ifelse(ROLE_PART == "coordinator" , 1, 0),
         statut_lib = ifelse(STADE_PROJET == "SIGNED", "contrat signé", "projet négocié")) %>%
  select(-ACRONYM_PROJET, -STADE_PROJET, -TITRE_PROJET, -ABSTRACT, -CODE_DG, -DATE_START, -DATE_END,
        -DATE_SIGNE, -COUT_PROJET, -NB_PART, -CODE_ORG, -NOM_LONG, -NOM_COURT, -STATUT_PART, -COUT_TOT_PART,
        -COUT_PART, -ADRESS, -CP, -CITY, -URL_PART, -CD_REG_ID, -CD_REG_VAT_ID, -PIC_ID2, -DURATION, -LAT, -LNG,
        -KEY_WORDS, -LIB_TOPIC, -STADE_PROJET_ORIG)
gc()

verif_na <- project[apply(project, 2, function(x) any(is.na(x)))]

  #verif stats
  project %>% group_by(statut_lib) %>% 
    mutate(proj_t = n_distinct(NUM_PROJET), part_t = n(), subv_t = sum(SUBV, na.rm = TRUE)) %>% unique() %>% 
    select(statut_lib, subv_t, proj_t, part_t) %>%  unique()
#H2020
# statut_lib              subv_t    proj_t  part_t
# 1  contrat signé 53 202 676 555  29 403   140 226
# 2 projet négocié    3 179 497 776     881     7 238
# statut_lib           subv_t proj_t part_t
# <chr>                 <dbl>  <int>  <int>
# 1 contrat signé  62 311 489 097.  32 319 161 737
# 2 projet négocié  313 927 9268.   2 245   8 547
# statut_lib           subv_t proj_t part_t
# <chr>                 <dbl>  <int>  <int>
# 1 contrat signé  66 869 027 502   34 937 174 020
# 2 projet négocié   221 783 325.     53    471

  project %>%  dplyr::filter(CODE_PAYS == "FR") %>% group_by(statut_lib) %>% 
    mutate(subv_t = sum(SUBV, na.rm = TRUE), proj_t = n_distinct(NUM_PROJET), part_t = n()) %>% 
    unique() %>% select(statut_lib, subv_t, proj_t, part_t) %>%  unique()
#FRANCE
# statut_lib            subv_t      proj_t   part_t
#1  contrat signé   5 981 920 151   6 472   13 595
#2 projet négocié     388 356 472     317    742
# statut_lib          subv_t proj_t part_t
# <chr>                <dbl>  <int>  <int>
# 1 contrat signé  6 972 029 428.   7 385  15 742
# 2 projet négocié  339 732 596.    415    781
# statut_lib          subv_t proj_t part_t
# <chr>                <dbl>  <int>  <int>
# 1 contrat signé  7 388 638 764.   7 869  16 803
# 2 projet négocié   54 836 688.     22     63

##########################################################################################
#partenariat
copy <- project %>% 
  select(NUM_PROJET, action1_id, action2_id, statut_lib, ORDRE_ALL_1 = ORDRE_ALL, pays_lib_1 = pays_lib,
         code_pays_part = CODE_PAYS, pays_statut_part_1 = pays_statut_1, pays_statut_part_2 = pays_statut_2)
contract_part <- project %>% 
  select(-starts_with("action"), -starts_with("org"), -starts_with("SUBV"), -PME,
        -panel_1, -panel1_tri, -panel_2, -panel2_tri) %>%
  merge(copy, by = c("NUM_PROJET", "statut_lib")) %>% 
  dplyr::filter(ORDRE_ALL_1 != ORDRE_ALL) %>% 
  select(-ORDRE_ALL_1 , -ORDRE_ALL) %>% 
  unique()

rm(copy)
gc()


######################################################################################
#PAYS regroup
pays_regroup <- project %>% 
  select(CODE_PAYS, pays_lib, pays_statut_2, pays_statut_1) %>% 
  unique()
##################

#aggrégation des données au niveau projet et pays

proj_1 <- project %>% group_by(NUM_PROJET, CODE_PAYS, statut_lib) %>%  
  mutate(subv = sum(SUBV, na.rm = TRUE), subv_source = sum(subv_source, na.rm=TRUE),
         part = n(), coord = sum(coord, na.rm = TRUE)) %>% 
  select(-subv_source_net, -SUBV, -SUBV_NET, -ROLE_PART, -ORDRE_PART, -TYPE_PART, -ORDRE_ALL, -PME, -org_typ, -org_abbr, 
         -org_tri) %>%
  unique()
verif_na <- proj_1[apply(proj_1, 2, function(x) any(is.na(x)))]
  
  #verif stat proj
  proj_1 %>%  dplyr::filter(CODE_PAYS == "FR") %>% group_by(statut_lib) %>% 
  mutate(subv_t = sum(subv, na.rm = TRUE), part_t = sum(part)) %>% unique() %>% 
    select(statut_lib, subv_t, part_t) %>%  unique()

proj_2 <- project %>% 
   # group_by(ANNEE, APPEL, area_tri, panel_2, action_3, CD_TOPIC, statut_lib) %>% 
   group_by(panel_1, panel1_tri, panel1_lib, panel1_id, panel_2, panel2_tri, panel2_lib, 
            panel2_id, APPEL, CD_TOPIC, ANNEE, programme_tri, 
            programme_abbr, programme_lib, pilier_tri, pilier_abbr, pilier, area_tri, 
            area_abbr, area_lib, programme, area, action2_tri, action2_id, action2_lib, 
            action_2, action1_tri, action1_id, action1_lib, action_1, action3_tri, action3_id, 
            action3_lib, action_3, action4_tri, action4_id, 
            action4_lib, action_4, statut_lib) %>% 
  mutate(subv_t = sum(SUBV, na.rm = TRUE), part_t = n(), pres_t = n_distinct(NUM_PROJET),
         subv_source_t = sum(subv_source, na.rm=TRUE)) %>% 
  select(panel_1, panel1_tri, panel1_lib, panel1_id, panel_2, panel2_tri, panel2_lib, 
         panel2_id, APPEL, CD_TOPIC, ANNEE, programme_tri, 
         programme_abbr, programme_lib, pilier_tri, pilier_abbr, pilier, area_tri, 
         area_abbr, area_lib, programme, area, action2_tri, action2_id, action2_lib, 
         action_2, action1_tri, action1_id, action1_lib, action_1, action3_tri, action3_id, 
         action3_lib, action_3, action4_tri, action4_id, action4_lib, action_4, 
         statut_lib, subv_t, subv_source_t, part_t, pres_t) %>% 
  # select(-NUM_PROJET, -SUBV, -SUBV_NET, -ROLE_PART, -ORDRE_PART, -TYPE_PART, -ORDRE_ALL, -PME, -org_typ, 
  #        -org_abbr, -org_tri, -CODE_PAYS, -pays_lib, -pays_statut_2, -pays_statut_1, -coord, -DATE_CALL) %>%
  unique()
verif_na <- proj_2[apply(proj_2, 2, function(x) any(is.na(x)))]
# proj_2 <- project %>% 
#   group_by(APPEL, area_lib, action_3, statut_lib) %>% 
#   summarize(count = n())
  
  #verif stat proj
  proj_2 %>%  group_by(statut_lib) %>% 
  mutate(subv_t2 = sum(subv_t, na.rm = TRUE), part_t2 = sum(part_t), pres_t2 = sum(pres_t)) %>% 
    unique() %>% 
    select(statut_lib, subv_t2, pres_t2, part_t2) %>% unique()

proj_2 <- merge(proj_2, pays_regroup)

verif_na <- proj_2[apply(proj_2, 2, function(x) any(is.na(x)))]
  #verif stat proj
  proj_2 %>%  dplyr::filter(CODE_PAYS == "FR") %>%  group_by(statut_lib) %>% 
    mutate(subv_t3 = sum(subv_t, na.rm = TRUE), part_t3 = sum(part_t)) %>% 
    unique() %>% select(statut_lib, subv_t3, part_t3) %>% unique()

contract <- bind_rows(proj_1, proj_2)  

verif_na <- contract[apply(contract, 2, function(x) any(is.na(x)))]
#rm(list=ls(pattern = "^(proj_)"))
gc()


##########################################################################################
#contract autre
################

#table total pour type et pme
proj_2 <- project %>% 
  group_by(APPEL, ANNEE, area, panel_2, action1_id, statut_lib) %>% 
  mutate(subv_t = sum(SUBV, na.rm = TRUE), subv_net_t = sum(SUBV_NET, na.rm = TRUE), part_t = n(), pres_t = n_distinct(NUM_PROJET)) %>% 
  select(APPEL, ANNEE, panel_2, pilier, area, area_abbr, area_lib, programme, programme_abbr, programme_lib, action1_id, statut_lib, subv_t, subv_net_t, part_t, pres_t) %>% 
  unique()
  #verif stat proj
  proj_2 %>%  group_by(statut_lib) %>% 
    mutate(subv_t2 = sum(subv_t, na.rm = TRUE), part_t2 = sum(part_t)) %>% unique() %>% 
    select(statut_lib, subv_t2, part_t2) %>%  unique()


#par type organisme
contract_org <- project %>% 
  group_by(NUM_PROJET, CODE_PAYS, org_abbr, statut_lib) %>% 
  mutate(subv = sum(SUBV, na.rm = TRUE), subv_net = sum(SUBV_NET, na.rm = TRUE), part = n(), coord = sum(coord, na.rm = TRUE)) %>% 
  select(APPEL, NUM_PROJET, ANNEE, pilier, area, area_abbr, area_lib, programme, programme_abbr, programme_lib, action1_id, statut_lib, 
         org_abbr, org_typ, CODE_PAYS, pays_statut_2, pays_statut_1, pays_lib, subv, subv_net, part, coord) %>% 
  unique()
  #verif stats
  contract_org %>%  group_by(statut_lib) %>% 
    mutate(subv_t2 = sum(subv, na.rm = TRUE), part_t2 = sum(part)) %>% unique() %>% 
    select(statut_lib, subv_t2, part_t2) %>%  unique()

proj_org <- project %>%  
  select(org_abbr, org_typ) %>%  
  unique()
# proj_typ <- project %>% select(TYPE_PART) %>%  unique()
proj_tot <- merge(proj_2, proj_org) %>% 
  merge(pays_regroup) 
# %>% merge(proj_typ)

contract_org <- bind_rows(contract_org, proj_tot)

verif_na <- contract_org[apply(contract_org, 2, function(x) any(is.na(x)))]

#PME
contract_pme <- project %>% 
  group_by(NUM_PROJET, CODE_PAYS, PME, statut_lib) %>% 
  mutate(subv = sum(SUBV, na.rm = TRUE), subv_net = sum(SUBV_NET, na.rm = TRUE), part = n(), coord = sum(coord, na.rm = TRUE)) %>% 
  select(NUM_PROJET, ANNEE, pilier, area, area_abbr, area_lib, programme, programme_abbr, programme_lib, action1_id, statut_lib, PME,
         CODE_PAYS, pays_statut_2, pays_statut_1, pays_lib, subv, subv_net, part, coord) %>% 
  unique()

proj_pme <- project %>% 
  select(PME) %>% 
  unique()
proj_tot <- merge(proj_2, proj_pme) %>% 
  merge(pays_regroup)
# %>% merge(proj_typ)

contract_pme <- bind_rows(contract_pme, proj_tot)

#type de participation
contract_benef <- project %>% 
  group_by(NUM_PROJET, CODE_PAYS, TYPE_PART, statut_lib) %>%
  mutate(subv = sum(SUBV, na.rm = TRUE), subv_net = sum(SUBV_NET, na.rm = TRUE), part = n(), coord = sum(coord, na.rm = TRUE)) %>%
  select(APPEL, NUM_PROJET, ANNEE, pilier, area, area_abbr, area_lib, programme, programme_abbr, programme_lib, action1_id, statut_lib, TYPE_PART,
         CODE_PAYS, pays_statut_2, pays_statut_1, pays_lib, subv, subv_net, part, coord) %>%
  unique() %>% 
  ungroup() 

proj_typ <- project %>% select(TYPE_PART) %>% 
  unique()
proj_tot <- merge(proj_2, proj_typ) %>% 
  merge(pays_regroup)
contract_benef <- bind_rows(contract_benef, proj_tot)

verif_na <- contract_benef[apply(contract_benef, 2, function(x) any(is.na(x)))]

#rm(list=ls(pattern = "^(proj_)"))
gc()

##################################################################################################################
##################################################################################################################
# Open-Data et Tableau site H2020

temp <- project %>% 
  dplyr::rename(pays_code = CODE_PAYS, zone_pays = pays_statut_1, org_code = org_abbr, org_lib = org_typ,
        area_code = area_abbr, programme_code = programme_abbr, panel = panel_2, panel_group = panel_1, action = action_3, action_group = action_1) %>% 
  unique()

################################################################################################################
#TABLEAU PUBLIC
public <- temp %>% 
  group_by(NUM_PROJET, area_lib, pays_code) %>% 
  mutate(subv_source = sum(subv_source, na.rm=TRUE), subv = sum(SUBV, na.rm = TRUE), part = n(), 
         coord = sum(coord, na.rm = TRUE)) %>% 
  select(ANNEE, NUM_PROJET, statut_lib, pilier, pilier_tri, programme_code, programme_lib, 
         programme_tri, area_code, area_lib, area_tri, pays_code, pays_lib, subv_source, subv, part, coord) %>% 
  unique() %>% 
  ungroup() %>% 
  mutate(NUM_PROJET=as.character(NUM_PROJET))
         
public %>% group_by(pays_code) %>% 
  mutate(pres_t = n_distinct(NUM_PROJET), part_t = sum(part), subv_t2 = sum(subv, na.rm = TRUE)) %>% 
  unique() %>% select(pres_t, part_t, subv_t2)
# tab <- tab %>% dplyr::rename(area_code = area_abbr)

autres <- autres_prog %>% 
  mutate(ANNEE = "NR", subv_source = subv, 
         coord = ifelse(role == "coordinator" , 1, 0)) %>%
  select(-role) %>% 
  unique()

public <- 
  bind_rows(public, autres) %>% 
  group_by(NUM_PROJET, pays_code, area_lib) %>% 
  mutate(subv_source = sum(subv_source, na.rm=TRUE), subv = sum(subv, na.rm = TRUE), part = sum(part, na.rm = T), coord = sum(coord, na.rm = TRUE),
         statut_lib = ifelse(is.na(statut_lib), "contrat signé", statut_lib)) %>% 
  ungroup()

verif_na <- public[apply(public, 2, function(x) any(is.na(x)))]

#verif stat proj
public %>% group_by(pays_code) %>% 
  mutate(part_t = sum(part), pres_t = n_distinct(NUM_PROJET), subv_t2 = sum(subv, na.rm = TRUE)) %>% unique() %>% 
  select(pays_code, part_t, pres_t, subv_t2) %>% dplyr::filter(pays_code == "FR") %>% unique()
public %>% #group_by(pays_code) %>% 
  summarize(pres_t = n_distinct(NUM_PROJET), part_t = sum(part), subv_t2 = sum(subv, na.rm = TRUE)) %>% unique() %>% 
  select(pres_t, part_t, subv_t2)

#table regroupement
public_total <- public %>% 
  group_by(ANNEE, programme_code, area_tri) %>%  
  mutate(part_t = sum(part), pres_t = n_distinct(NUM_PROJET, na.rm = T), subv_t = sum(subv, na.rm = T), subv_source_t = sum(subv_source, na.rm = T)) %>% 
  select(ANNEE, pilier, pilier_tri, programme_code, programme_lib, programme_tri, area_code, area_lib, area_tri, part_t, pres_t, subv_t, subv_source_t) %>% 
  ungroup() %>% 
  unique()

  public_total %>%
    summarize(pres_t2 = sum(pres_t, na.rm=T), part_t2 = sum(part_t, na.rm = TRUE), subv_t2 = sum(subv_t, na.rm = TRUE)) %>%
    select(pres_t2, part_t2, subv_t2) %>% unique() #38 342

  verif_na <- public_total[apply(public_total, 2, function(x) any(is.na(x)))]


temp2 <- temp %>% 
  select(pays_code, pays_lib) %>% 
  unique()

public_total <- merge(public_total, temp2) 

  public_total %>% dplyr::filter(pays_code=="BR") %>% 
    summarize(pres_t2 = sum(pres_t, na.rm=T), part_t2 = sum(part_t, na.rm = TRUE), subv_t2 = sum(subv_t, na.rm = TRUE)) %>%
    select(pres_t2, part_t2, subv_t2) %>% unique() 


# Table finale
contrats_public <- bind_rows(public, public_total)

verif_na <- contrats_public[apply(contrats_public, 2, function(x) any(is.na(x)))]

test <- contrats_public %>% dplyr::filter(subv>0, is.na(NUM_PROJET))
  
 
# cont_temp <- contract %>% 
#   dplyr::filter(!is.na(NUM_PROJET)) %>% 
#   group_by(statut_lib) %>% 
#   mutate(proj = as.double(n_distinct(NUM_PROJET))) %>% 
#   select(statut_lib, proj) %>% 
#   unique() %>% 
#   spread(statut_lib, proj) 

cont_temp <- public %>% 
  dplyr::filter(!is.na(NUM_PROJET)) %>% 
  group_by(statut_lib) %>% 
  mutate(proj = as.double(n_distinct(NUM_PROJET))) %>% 
  select(statut_lib, proj) %>% 
  unique() %>% 
  spread(statut_lib, proj)    


# nombre de projets pour tableau public RAPPEL
prog_aut <- as.numeric(n_distinct(autres_prog$NUM_PROJET, na.rm = TRUE))

contrats_rappel <- bind_cols(cont_temp, date_extraction, proj_ret = proj_ret$proj_ret)
contrats_rappel$proj_ret <- sum(as.double(contrats_rappel$proj_ret) + prog_aut)

rm(list=ls(pattern = "^(proj_|pays|cont_|public)"))
gc()

##################################################################################################
#Open-data

contrats_participants <- 
  temp %>% group_by(APPEL, area_lib, panel, action, pays_code, org_code) %>%  
  mutate(nb_participation = n(), montant_subvention = sum(SUBV, na.rm = TRUE), nb_coordination = sum(coord, na.rm = TRUE)) %>% 
  select(APPEL, pilier, programme_code, programme_lib, area_code, area_lib, panel, panel_group, action_group, action,
         zone_pays, pays_code, pays_lib, org_code, org_lib, nb_participation, montant_subvention, nb_coordination) %>% 
  unique()
#verif
contrats_participants %>% group_by(pays_code) %>% 
  mutate(part2 = sum(nb_participation, na.rm = TRUE)) %>% unique() %>% 
  select(pays_code, part2) %>% dplyr::filter(pays_code == "FR") %>% unique()  

contrats_projets <- 
  temp %>% group_by(APPEL, area_lib, panel, action, pays_code) %>%  
  mutate(nb_projet = n_distinct(NUM_PROJET)) %>% 
  select(APPEL, pilier, programme_code, programme_lib, area_code, area_lib, panel, panel_group, action_group, action,
         zone_pays, pays_code, pays_lib, nb_projet) %>% 
  unique()  
#verif
contrats_projets %>% group_by(pays_code) %>% select(pays_code, nb_projet) %>%
  mutate(proj_2 = sum(nb_projet, na.rm = TRUE)) %>%  unique() %>% 
  dplyr::filter(pays_code == "FR") %>% unique()  

contrats_pays <- temp %>% group_by(pays_code) %>%  
  mutate(nb_projet = n_distinct(NUM_PROJET), nb_participation = n(), montant_subvention = sum(SUBV, na.rm = TRUE), 
         nb_coordination = sum(coord, na.rm = TRUE)) %>% 
  select(zone_pays, pays_code, pays_lib, nb_projet, nb_participation, montant_subvention, nb_coordination) %>% 
  unique()  

####################################################################################################################
###################################################################################################################

#doublement des variables SUBVENTIONS pour résoudre le problème des NAN
contract_benef <- contract_benef %>% no_nan(.) %>% 
  mutate(CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS))
contract_org <- contract_org %>% ungroup(.) %>%  
  no_nan(.) %>% mutate(CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS))
contract_pme <- contract_pme %>% ungroup(.) %>%  
  no_nan(.) %>% mutate(CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS))

contract <- contract %>% 
  ungroup(.) %>% 
  mutate(subv = ifelse(is.na(subv), 0, subv),
    subv_t = ifelse(is.na(subv_t), 0, subv_t),
    CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS))
verif_na <- contract[apply(contract, 2, function(x) any(is.na(x)))]

contrats_public <- contrats_public %>%
  mutate(subv = ifelse(is.na(subv), 0, subv), subv_t = ifelse(is.na(subv_t), 0, subv_t),
         pays_code = ifelse(pays_code == "NA", "NAM", pays_code))


######################################################################################################################
# création d'un dataframe pour la synthèse qui agrège les données (inclus eit et art185) au niveau des pays et global
# proj_cont_tot <- as.numeric(sum(n_distinct(contract$NUM_PROJET, na.rm=TRUE), prog_proj_art))
# part_cont_tot <- as.numeric(sum(sum(contract$part, na.rm=TRUE), prog_part))
# subv_cont_tot <- as.numeric(sum(sum(contract$subv, na.rm=TRUE), prog_subv))
# 
 # contract_synthese <- left_join(contrats_pays, prog, by = c("pays_code" = "CODE_PAYS")) %>% 
 #   group_by(pays_code) %>% 
 #   mutate(part = sum(nb_participation, Part, na.rm = TRUE), subv = sum(montant_subvention, Subv, na.rm = TRUE), 
 #          proj = sum(nb_projet, Proj, na.rm = TRUE), proj_tot = proj_cont_tot, subv_tot = subv_cont_tot, part_tot = part_cont_tot) %>% 
 #   select(-nb_projet, -nb_participation, -montant_subvention, -Subv, -Part, -Proj, -zone_pays) %>% 
 #   ungroup() %>% 
 #   mutate(pays_code = ifelse(pays_code == "NA", "NAM", pays_code))
 # 
 # contract_synthese <- as.data.frame(contract_synthese)

#s'assurer que les tables à exporter pour tableau sont biens des data.frame
contract_part <- as.data.frame(contract_part)
contract_part <- net_var(contract_part)

contract <- as.data.frame(contract)
contract <- net_var(contract)
contract_org <- as.data.frame(contract_org)
contract_org <- net_var(contract_org)
contract_pme <- as.data.frame(contract_pme)
contract_pme <- net_var(contract_pme)
contract_benef <- as.data.frame(contract_benef)
contract_benef <- net_var(contract_benef)

verif_na <- contrats_public[apply(contrats_public, 2, function(x) any(is.na(x)))]
contrats_public <- as.data.frame(contrats_public)
# contrats_public <- net_var(contrats_public)
contrats_rappel <- as.data.frame(contrats_rappel)

contrats_projets <- as.data.frame(contrats_projets)
contrats_participants <- as.data.frame(contrats_participants)
contrats_pays <- as.data.frame(contrats_pays)




# contract %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(statut_lib) %>%
#   mutate(subv_3 = sum(subv2, na.rm = TRUE)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()
# contract %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(statut_lib) %>%
#    mutate(pres = n_distinct(NUM_PROJET), subv_3 = sum(subv)) %>% unique() %>% select(statut_lib, subv_3, pres) %>% unique()
# contract %>% dplyr::filter(is.na(NUM_PROJET), CODE_PAYS == "FR") %>% group_by(statut_lib) %>%
#    mutate(subv_3 = sum(subv_t2, na.rm = TRUE)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()
# contract %>% dplyr::filter(is.na(NUM_PROJET), CODE_PAYS == "FR") %>% group_by() %>%
#    mutate(pres_3 = sum(pres_t, na.rm = TRUE), subv_3 = sum(subv_t)) %>% select(subv_3, pres_3) %>% unique()
# 
# contrats_public %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(pays_code) %>%
#   mutate(subv_3 = sum(subv2, na.rm = TRUE)) %>% unique() %>% select(pays_code, subv_3) %>% unique()
# contrats_public %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(pays_code) %>%
#   mutate(subv_3 = sum(subv)) %>% unique() %>% select(pays_code, subv_3) %>% unique()
