# 2020_04
applicant <- applicant %>% 
  filter(!(NUM_PROJET==676392 & ORDRE_PART==9))




#panels
proposal <- proposal %>% mutate(CODE_PANEL = stringr::str_replace_all(CODE_PANEL, "^(null|\\d)", NA_character_))


#correction des valeurs manquantes à la main

# ROLE 

#table des participants
unique(participant$ROLE_PART)
temp <- participant %>% filter(is.na(ROLE_PART))
temp <- participant %>% filter(NUM_PROJET == 769255)
participant <- participant %>% 
               mutate(ROLE_PART = ifelse(NUM_PROJET == 769255 & CODE_ORG == 914694287, "PARTICIPANT", ROLE_PART))



#PAYS
unique(participant$CODE_PAYS)
participant <- participant %>% 
  mutate(CODE_PAYS = ifelse(CODE_ORG %in% c(888888210, 888892565, 888892558, 888888702), "CZ", CODE_PAYS),
       CODE_PAYS = ifelse(CODE_ORG %in% c(888889506, 888890380), "NL", CODE_PAYS),
       CODE_PAYS = ifelse(CODE_ORG %in% c(888895890, 888892691), "CH", CODE_PAYS),
       CODE_PAYS = ifelse(CODE_ORG %in% c(888888893), "BE", CODE_PAYS),
       CODE_PAYS = ifelse(CODE_ORG %in% c(888890502), "DE", CODE_PAYS))



# contrôle du nbre de participants dans les projets comparaison propositions-contrats

temp <- proposal %>% group_by(NUM_PROJET) %>% 
                     summarise(nb_prop = n())

temp2 <- project %>% group_by(NUM_PROJET) %>% 
                     summarise(nb_cont = n())

temp3 <- temp2 %>% left_join(temp, by = "NUM_PROJET") %>% 
                  mutate(diff = (nb_prop - nb_cont))


