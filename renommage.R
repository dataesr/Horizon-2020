#renommage des datas pour traitements classiques

#PROPOSAL
proposal <- load_one_obj(paste0(chemin_bd, "bases.RData"), "proposal") %>% 
  select( NUM_PROJET = CD_PROP_ID, 
          ACRONYM_PROJET = CD_ACRONYM, 
          TITRE_PROJET = LB_TITLE, 
          ABSTRACT = LB_ABSTRACT,
          APPEL = CD_CALL_ID,
          DATE_CALL = DT_CALL_CLOSUREDATE,
          CODE_ACT = CD_ACTION_TYPE,
          CODE_PANEL = CD_PANEL,
          CODE_DG = CD_BUDG_GROUP_DG,
          STATUT_EVAL = CD_EC_DEC_STATUS,
          ELIGIBLE = CD_EC_ELIGIBLE,
          COUT_PROJET = AM_TOT_COST,
          COUT_PART = AM_CONTRIB_RQ,
          NB_PART = QT_PARTNERS,
          DURATION = QT_DURATION,
          KEY_WORDS = LB_FRE_KEYW,
          DT_SUBMISSION_DATE, CD_THEMA, CD_TOPIC, CD_PANEL, DT_LAST_UPDATE) 


#PROPOSAL_APPLICANTS   
applicant <- load_one_obj(paste0(chemin_bd, "bases.RData"), "proposal_applicants") %>% 
  select(NUM_PROJET = CD_PROP_ID,
         ORDRE_PART = CD_PART_ORDER,
         CODE_ORG = CD_PART_PIC,
         PIC_ID2 = CD_APPL_PIC,
         NOM_LONG = LB_PART_LEG_NAM, 
         NOM_COURT = LB_PART_SHT_NAM,
         ROLE_PART = CD_ROLE,
         CODE_PAYS = CD_PART_COUNTRY,
         PME = FL_SME,
         SUBV = AM_PART_CONTRIB_RQ,
         COUT_TOT_PART = AM_PART_TOT_COST,
         URL_PART = LB_PART_WEB_PAGE,
         ADRESS = LB_PART_ADRS,
         CP = CD_PART_POST,
         CITY = LB_PART_TOWN, 
         LAT=CD_LATITUDE,
         LNG=CD_LONGITUDE,
         DT_LAST_UPDATE)


#PROJECT 
project <- load_one_obj(paste0(chemin_bd, "bases.RData"), "projects") %>% 
  select (NUM_PROJET = CD_PROJ_ID,
          APPEL = CD_CALL_ID,
          DATE_CALL = DT_CALL_CLOSUREDATE,
          CODE_ACT = CD_ACTION_TYPE,
          ACRONYM_PROJET = CD_ACRONYM,
          TITRE_PROJET = LB_TITLE,
          ABSTRACT = LB_ABSTRACT,
          STADE_PROJET = LB_STATUS_DESCR,
          CODE_DG  = CD_DG,  
          DATE_START = DT_PROJECT_START,
          DATE_END = DT_PROJECT_END,
          DATE_SIGNE = DT_CONTRACT_SIGNATURE,
          COUT_PROJET = AM_TOT_COST,
          COUT_PART = AM_EC_CONTRIBUTION,
          NB_PART = QT_NBR_OF_PARTICIPANTS,
          DURATION = QT_DURATION,
          KEY_WORDS = LB_FRE_KEYW,
          CD_THEMA, CD_TOPIC, DT_LAST_UPDATE)


#PROJECT_PARTICIPANT
participant <- load_one_obj(paste0(chemin_bd, "bases.RData"), "participants") %>% 
  select(NUM_PROJET = CD_PROJ_ID,
         ORDRE_PART = CD_PART_NR,
         CODE_ORG = CD_PART_PIC,
         PIC_ID2 = CD_APPL_PIC,
         TYPE_PART = CD_TYPE,
         ROLE_PART  = CD_ROLE,
         NOM_LONG = LB_LEGAL_NAME,
         NOM_COURT = CD_SHORT_NAME,
         COUT_TOT_PART = AM_COST,
         SUBV = AM_EC_CONTRIB,
         SUBV_NET = AM_NET_CONTRIB,
         STATUT_PART = CD_STATUS,
         CODE_PAYS = CD_ORG_COUNTRY,
         TYPE_ORG = CD_PROJ_ORG_TYPE,
         PME = FL_SME,
         URL_PART = LB_ORG_WEB_PAGE,
         ADRESS = LB_ORG_ADDRESS,
         CP = CD_ORG_POSTAL_CODE,
         CITY = LB_ORG_CITY,
         LAT=CD_LATITUDE,
         LNG=CD_LONGITUDE,
         CD_REG_ID, CD_REG_VAT_ID, DT_LAST_UPDATE)


# ARTICLE 185 projet
art_proj <- load_one_obj(paste0(chemin_bd, "bases.RData"), "p2p_projects") %>% 
    select (CD_PROJ_ID,
            APPEL = LB_NETWORK,
            ACRONYM_PROJET = LB_ACRONYM,
            TITRE_PROJET = LB_TITLE,
            DATE_DEBUT = DT_START_DATE,
            DATE_FIN = DT_END_DATE)

art_proj[art_proj == ""]<- NA_character_
  
art_part <- load_one_obj(paste0(chemin_bd, "bases.RData"), "p2p_participants") %>% 
      select(CD_PROJ_ID,
           CODE_ORG = CD_PART_PIC,
           ROLE_PART  = CD_ROLE,
           NOM_LONG = LB_TITLE,
           SUBV = AM_TOTAL_REQ_GRANT,
           CODE_PAYS = CD_COUNTRY,
           LAT=CD_LATITUDE,
           LNG=CD_LONGITUDE,
           TYPE_ORG = CD_ORG_TYPE)

art_part[art_part == ""]<- NA_character_


  # #import EIT_KAVA
eit_activities <- load_one_obj(paste0(chemin_bd, "bases.RData"), "eit_activities")
eit_activities[eit_activities == ""]<- NA_character_

eit_participant <- load_one_obj(paste0(chemin_bd, "bases.RData"), "eit_kava_participants") %>% 
  dplyr::rename(CODE_ORG = CD_PART_PIC,
                PIC_ID2 = CD_APPL_PIC,
                ROLE_PART  = CD_ROLE,
                NOM_LONG = LB_PART_LEG_NAM, 
                NOM_COURT = LB_PART_SHT_NAM,
                TYPE_ORG = CD_ORG_TYPE,
                CODE_PAYS = CD_PART_COUNTRY,
                LAT=LATITUDE,
                LNG=LONGITUDE)

eit_participant[eit_participant == ""]<- NA_character_

