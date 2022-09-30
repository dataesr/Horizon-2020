# Commission envoie des structures françaises
part_com <- participants %>% filter(country_code=="FR", stage=="project", !is.na(participant_id)) %>% 
  select(id, project_id, pic_id, pic_id_2, ORDRE_PART, participant_order, name_source, role, participates_as, sies_id, participant_id, participant_id_0, name, acronym, ADRESS, post_code, city) %>% 
  mutate(id_temp = if_else(nchar(participant_id) == 9, participant_id,
                        if_else(nchar(participant_id) == 14, substr(participant_id, 1, 9),
                        if_else(nchar(participant_id) < 8, " ", participant_id))))



# extrait REF pour lien avec part_com
temp <- ref %>% 
  #mutate(id_temp = if_else(nchar(participant_id) == 14, substr(participant_id, 1, 9), participant_id)) %>% 
  select(orig_ref, num_nat_struct, numero_uai, siren, siege) %>% unique() %>% 
  gather(key = type, value = id_temp, num_nat_struct:siren) %>% 
  mutate(id_temp = toupper(id_temp)) %>% 
  filter(!is.na(id_temp), !(siege == "0" & type == "siren"), !(orig_ref == "bce" & nchar(id_temp) == 9)) %>% 
  unique() %>% 
  left_join(ref_niv, by = c("id_temp" = "participant_id")) %>% 
  select(id_temp, name_2, acronym_2) %>% 
  unique()


part_com <- part_com %>% 
  left_join(temp, by = "id_temp") %>% 
  mutate(name = if_else(is.na(name), name_2, name),
         acronym = if_else(is.na(acronym), acronym_2, acronym)) %>% 
  select(-name_2, -acronym_2)

temp <- ref %>% 
  #mutate(id_temp = if_else(nchar(participant_id) == 14, substr(participant_id, 1, 9), participant_id)) %>% 
  select(orig_ref, num_nat_struct, numero_uai, siren, libelle1, sigle, siege) %>% unique() %>% 
  gather(key = type, value = id_temp, num_nat_struct:siren) %>% 
  mutate(id_temp = toupper(id_temp)) %>% 
  filter(!is.na(id_temp), !(siege == "0"), orig_ref == "sirene") %>% unique()


x <- part_com %>% filter((is.na(name))) %>% 
  left_join(temp, by = "id_temp") %>% 
  select(id, libelle1, sigle) %>% unique() %>%  filter(!(is.na(libelle1)))
  
part_com <- part_com %>% left_join(x, by = "id") %>% 
  mutate(name=if_else(is.na(name), libelle1, name), acronym = if_else(is.na(acronym), sigle, acronym)) %>% 
  select(-libelle1, -sigle) %>% unique()


temp <- ref %>% 
  select(orig_ref, num_nat_struct, numero_uai, siren, libelle1, sigle, siege) %>% unique() %>% 
  gather(key = type, value = id_temp, num_nat_struct:siren) %>% 
  mutate(id_temp = toupper(id_temp)) %>% 
  filter(!is.na(id_temp), !(siege == "0")) %>% unique()

x <- part_com %>% filter((is.na(name))) %>% 
  left_join(temp, by = "id_temp") %>% 
  select(id, libelle1, sigle) %>% unique() %>%  filter(!(is.na(libelle1)))


part_com <- part_com %>% left_join(x, by = "id") %>% 
  mutate(name=if_else(is.na(name), libelle1, name), acronym = if_else(is.na(acronym), sigle, acronym)) %>% 
  select(-libelle1, -sigle) %>% unique()



write.csv2(part_com, file=paste0(chemin_open_data, "Tab_UE_participations_departement_FR_02122019.csv"), na = "", row.names = FALSE)


