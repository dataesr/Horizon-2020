########################################################################
# PANELS
#######################################################################

panels <- load_one_obj("nomenclature/panels.rdata", "panels")


#panels <- panels %>%  dplyr::filter(!is.na(old_CODE_PANEL))

#liste des panels dans les bases
panel_base <- proposal %>% 
  select(CODE_PANEL) %>% 
  dplyr::filter(!is.na(CODE_PANEL)) %>% 
  unique()

pan_comp <- panel_base %>% 
  anti_join(panels, by = "CODE_PANEL") %>% 
  dplyr::filter(str_detect(CODE_PANEL, "^[0-9]+", negate=TRUE))
if (nrow(pan_comp) == 0) {
  print("tout va bien")
} else{
  print("il manque un/plusieurs code_panel dans la table nomenclature panels")
  pan_comp <- pan_comp %>% 
    left_join(proposal[,c("CODE_PANEL", "programme_abbr", "CODE_ACT")], by = "CODE_PANEL") %>% 
    unique()
}


proposal <- proposal %>% 
  mutate(CODE_PANEL = ifelse(is.na(CODE_PANEL) & CODE_ACT == "ERC-POC", "PC1", CODE_PANEL),
         CODE_PANEL = ifelse(str_detect(CODE_PANEL, "^[0-9]+$") & CODE_ACT == "ERC-POC", "PC1", CODE_PANEL),
         CODE_PANEL = ifelse(is.na(CODE_PANEL) & programme_abbr == "ERC", "SP1", CODE_PANEL),
         CODE_PANEL = ifelse(is.na(CODE_PANEL) & programme_abbr == "MSCA", "SAP", CODE_PANEL)) 
                                
#verification que tous les ERC et MSCA ont un panel
panel_verif <- proposal %>% 
  dplyr::filter(programme_abbr %in% c("ERC", "MSCA") & is.na(CODE_PANEL))

#création des variables "panel"
proposal <- proposal %>% 
        left_join(panels, by = c("CODE_PANEL", "programme_abbr" = "GEN_3_PANEL")) %>% 
        select(-old_CODE_PANEL, -LEVEL) %>% 
        dplyr::rename(panel_lib = LIB_PANEL, panel2_tri = TRI) %>% 
        left_join(panels[, c("CODE_PANEL", "LIB_PANEL", "GEN_3_PANEL", "TRI")], by = c("GEN_2_PANEL" = "CODE_PANEL", "programme_abbr" = "GEN_3_PANEL"))


proposal <- proposal %>%  
        mutate(CODE_PANEL = ifelse(CODE_PANEL == "PC1", "PC", CODE_PANEL),
        panel_2 = ifelse(CODE_PANEL %in% c("SP1", "SAP"), panel_lib, paste0(CODE_PANEL, " - ", panel_lib)),
        panel_1 = ifelse(GEN_2_PANEL %in% c("SP", "SAP"), LIB_PANEL, paste0(GEN_2_PANEL, " - ", LIB_PANEL)),
        panel_2 = ifelse(programme_abbr %in% c("ERC", "MSCA"), panel_2, NA_character_),
        panel_1 = ifelse(is.na(panel_2), NA_character_, panel_1)) %>% 
        dplyr::rename(panel1_tri = TRI, panel2_lib = panel_lib, panel1_lib = LIB_PANEL,
                      panel1_id = GEN_2_PANEL, panel2_id = CODE_PANEL)


#ajout des panels aux contrats

project <- proposal %>% 
  select(NUM_PROJET, panel_1, panel1_tri, panel1_lib, panel1_id, panel_2, panel2_tri, panel2_lib, panel2_id) %>%  unique() %>% 
  right_join(project, by = "NUM_PROJET")

