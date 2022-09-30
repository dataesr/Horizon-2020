part <- participant %>% mutate(ordre_origine = ORDRE_PART)
part <- ordre_type_part(part)




part <- ordre_type_part(part)
part2 <- part %>% 
  select(NUM_PROJET, PIC_ID=CODE_ORG, PIC_ID2, ordre_origine, ORDRE_PART, ORDRE_ALL, TYPE_PART, CD_REG_ID, CD_REG_VAT_ID)
write.csv2(part2, file="C:/Users/zfriant/OneDrive/PCRI/participants/2019_11/part_ordre_corrige.csv", row.names = FALSE)

path_temp = "C:/Users/zfriant/OneDrive/PCRI/traitement/tableau/connexion/"
# path_s = "C:/Users/zfriant/OneDrive/PCRI/traitement/2021_04//"

contract <- read.csv2(file=paste0(path_temp, "contract.csv"), na="", fileEncoding = "UTF-8")
save(contract, file=paste0(path_temp, "contract.RData"))
