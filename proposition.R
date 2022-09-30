#############################################################################################
# PROPOSITIONS
##############################################################################################
# proposal <- load_one_obj(rdata = paste0(chemin_donnees,"datas2.RData"), "proposal")

#Variable statut de projets pour les propositions
proposal <- proposal %>% 
  mutate(statut = "EVA", coord = ifelse(ROLE_PART == "coordinator", 1, 0))
prop_temp <- proposal %>% 
  dplyr::filter(STATUT_EVAL == "MAIN") %>% 
  mutate(statut = "RET")
proposal <- bind_rows(proposal, prop_temp) %>% 
  mutate(statut_lib = ifelse(statut == "RET", "projet retenu", "projet évalué")) %>%
  select(-STATUT_EVAL, -ELIGIBLE, -ACRONYM_PROJET, -TITRE_PROJET, -ABSTRACT, -CODE_DG,  
         -KEY_WORDS, -NB_PART, -COUT_PROJET, -CODE_ORG, -PIC_ID2, -NOM_LONG, -NOM_COURT, 
         -PME, -COUT_TOT_PART,  -LIB_TOPIC,
         -COUT_PART, -ADRESS, -CP, -CITY, -URL_PART, -DURATION, -DATE_CALL, -LAT, -LNG) 

verif_na <- proposal[apply(proposal, 2, function(x) any(is.na(x)))]
#verif stat
#H2020
proposal %>% group_by(statut_lib) %>% 
  mutate(subv_t = sum(SUBV, na.rm = TRUE), part_t = n(), proj_t = n_distinct(NUM_PROJET)) %>% 
  unique() %>% 
  select(statut_lib, subv_t, part_t, proj_t) %>% unique()

#statut_lib       subv_t            part_t  proj_t
# <chr>        <dbl>                <int>   <int>
# 1 projet évalué 455 005 062 359   892 851  254 325
# 2 projet retenu  59 193 073 379   137 798   31 182
# statut_lib           subv_t  part_t proj_t
# <chr>                 <dbl>   <int>  <int>
# 1 projet évalué 547 613 056 036. 1 066 292 294 871
# 2 projet retenu  68 899 072 700.  161 082  35 601
# statut_lib           subv_t  part_t proj_t
# <chr>                 <dbl>   <int>  <int>
# 1 projet évalué 557 039 012 699. 1 100 796 299 419
# 2 projet retenu  69 716 543 556.  163 951  36 037
# statut_lib           subv_t  part_t proj_t
# <chr>                 <dbl>   <int>  <int>
# 1 projet évalué 556 219 140 988. 1 100 829 299 427
# 2 projet retenu  69 092 989 366.  164 055  36 074

proposal %>% dplyr::filter(CODE_PAYS == "FR") %>% group_by(statut_lib) %>% 
  mutate(subv_t = sum(SUBV, na.rm = TRUE), part_t = n(), proj_t = n_distinct(NUM_PROJET), coord_t = sum(coord, na.rm = TRUE)) %>% 
  unique() %>% select(statut_lib, subv_t, part_t, proj_t, coord_t) %>% unique()

#FRANCE
# statut_lib         subv_t       part_t    proj_t  coord_t
# 1 projet évalué 39 777 314 489  69 075    44 259  20 547
# 2 projet retenu  6 656 990 879  12 400     6 872  2 969
# statut_lib          subv_t part_t proj_t coord_t
# <chr>                <dbl>  <int>  <int>   <dbl>
# 1 projet évalué 47 945 702 769.  82 386  52 103   23 886
# 2 projet retenu  7 701 279 530.  14 421   7 935    3 410
# statut_lib          subv_t part_t proj_t coord_t
# <chr>                <dbl>  <int>  <int>   <dbl>
# 1 projet évalué 47 796 481 779.  84 642  53 176   24 285
# 2 projet retenu  7 590 359 887.  14 644   8 032    3 447
# statut_lib          subv_t part_t proj_t coord_t
# <chr>                <dbl>  <int>  <int>   <dbl>
# 1 projet évalué 47 781 209 661.  84 645  53 178   24 285
# 2 projet retenu  7 586 798 684.  14 652   8 038    3 450

rm(prop_temp)
gc()

#########################################################################################################
#partenariat

proposition_part <- proposal %>% 
  select(NUM_PROJET, statut_lib, ORDRE_ALL_1 = ORDRE_ALL, code_pays_part = CODE_PAYS)
proposition_part <- proposal %>% 
  select(NUM_PROJET, statut_lib, ORDRE_ALL, CODE_PAYS, ROLE_PART) %>% 
  merge(proposition_part, by = c("NUM_PROJET", "statut_lib")) %>% 
  dplyr::filter(ORDRE_ALL_1 != ORDRE_ALL) %>% 
  select(-ORDRE_ALL_1, -ORDRE_ALL) %>% 
  unique()

gc()

temp <- proposal %>% 
  select(CODE_PAYS, pays_lib, pays_statut_1, pays_statut_2) %>% 
  unique()
proposition_part <- left_join(proposition_part, temp, by = "CODE_PAYS")
proposition_part <- temp %>% 
  select(code_pays_part=CODE_PAYS, pays_lib_1=pays_lib, pays_statut_part_1=pays_statut_1, pays_statut_part_2=pays_statut_2) %>% 
  right_join(proposition_part, by = "code_pays_part")

gc()

temp <- proposal %>% 
  select(NUM_PROJET, APPEL, ANNEE, CD_TOPIC, pilier_tri, pilier, programme_tri, action2_id, action1_id,
         programme_abbr, programme_lib, programme, area_tri, area_abbr, area_lib, area) %>% 
  unique()

proposition_part  <- temp %>% 
  right_join(proposition_part, by = "NUM_PROJET")

verif_na <- proposition_part[apply(proposition_part, 2, function(x) any(is.na(x)))]

rm(temp)
gc()

########################################################################################################
#aggrégation des données au niveau projet et pays


prop_1 <- proposal %>% 
  group_by(NUM_PROJET, CODE_PAYS, statut) %>%
  mutate(subv_source = sum(subv_source, na.rm = TRUE), subv = sum(SUBV, na.rm = TRUE), 
         part = n(), coord = sum(coord, na.rm = TRUE)) %>% 
  select(-SUBV, -ROLE_PART, -ORDRE_PART, -ORDRE_ALL, -TYPE_PART) %>% unique()

verif_na <- prop_1[apply(prop_1, 2, function(x) any(is.na(x)))]  

  #verif stat
  prop_1 %>% dplyr::filter(CODE_PAYS == "FR") %>% 
    group_by(statut_lib) %>% 
    mutate(subv_t = sum(subv, na.rm = TRUE), part_t = sum(part), proj_t = n_distinct(NUM_PROJET), coord_t = sum(coord)) %>% 
    select(statut_lib, subv_t, part_t, proj_t, coord_t) %>% 
    unique()


prop_2 <- proposal %>% 
  group_by(APPEL, ANNEE, area_tri, panel2_tri, action4_tri, CD_TOPIC, statut) %>% 
  mutate(subv_source_t = sum(subv_source, na.rm = TRUE), subv_t = sum(SUBV, na.rm = TRUE), part_t = n(), pres_t = n_distinct(NUM_PROJET)) %>% 
  select(-subv_source, -SUBV, -coord, -ROLE_PART, -NUM_PROJET, -ORDRE_PART, -ORDRE_ALL, -TYPE_PART, 
         -CODE_PAYS, -pays_lib, -pays_statut_2, -pays_statut_1) %>% 
  unique()

verif_na <- prop_2[apply(prop_2, 2, function(x) any(is.na(x)))]  

#verif stat
  prop_2 %>% group_by(statut_lib) %>% 
    mutate(subv_t2 = sum(subv_t, na.rm = TRUE), part_t2 = sum(part_t), proj_t = sum(pres_t)) %>% 
    select(statut_lib, subv_t2, part_t2, proj_t) %>% unique()

prop_pays <- proposal %>% 
  select(CODE_PAYS, pays_lib, pays_statut_2, pays_statut_1) %>% 
  unique()

prop_2 <- merge(prop_2, prop_pays)

proposition <- bind_rows(prop_1, prop_2)  

verif_na <- proposition[apply(proposition, 2, function(x) any(is.na(x)))]  

proposition <- proposition %>% 
  mutate(subv2 = subv, subv = ifelse(is.na(NUM_PROJET), 0, subv),
      subv_t2 = subv_t, subv_t = ifelse(!is.na(NUM_PROJET), 0, subv_t))


  proposition %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(statut_lib) %>%
    mutate(subv_3 = sum(subv2, na.rm = TRUE)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()
  proposition %>% dplyr::filter(!is.na(NUM_PROJET)) %>% group_by(statut_lib) %>%
    mutate(subv_3 = sum(subv)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()
  proposition %>% dplyr::filter(is.na(NUM_PROJET), CODE_PAYS == "FR") %>% group_by(statut_lib) %>%
    mutate(subv_3 = sum(subv_t2, na.rm = TRUE)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()
  proposition %>% dplyr::filter(is.na(NUM_PROJET), CODE_PAYS == "FR") %>% group_by(statut_lib) %>%
    mutate(subv_3 = sum(subv_t)) %>% unique() %>% select(statut_lib, subv_3) %>% unique()

  
#nombre de projets retenus pour alimenter le tableau public H2020. A conserver pour les contrats
proj_ret <- proposal %>% dplyr::filter(statut == "RET") %>% 
  mutate(proj_ret = n_distinct(NUM_PROJET)) %>%  
  select(proj_ret) %>% unique()


rm(list=ls(pattern = "^(prop_|proposal)"))
gc()

verif_na <- proposition[apply(proposition, 2, function(x) any(is.na(x)))]

#s'assurer que les tables à exporter pour tableau sont bien des data.frame
proposition <-  as.data.frame(proposition)
proposition <- net_var(proposition)
proposition <- proposition %>% 
  mutate(CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS))
proposition_part <- as.data.frame(proposition_part)
proposition_part <- net_var(proposition_part)  
proposition_part <- proposition_part %>% 
  mutate(CODE_PAYS = ifelse(CODE_PAYS == "NA", "NAM", CODE_PAYS),
    code_pays_part = ifelse(code_pays_part == "NA", "NAM", code_pays_part))
