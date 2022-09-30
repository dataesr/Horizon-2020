importation_data <- function(path){
  
  DOSSIER = "proposal/"
  INPUT_FILE = c("proposal", "proposal_applicants", "eit_proposal", "eit_applicant","proposal_applicants_dept")
  for (i in INPUT_FILE){
    assign(i, read.csv(file = paste0(path, DOSSIER, i, ".csv"), na.strings = "", encoding = "UTF-8"))
  
  }
    
DOSSIER = "project/"
INPUT_FILE = c("p2p_participants","p2p_projects", "eit_kava_participants", "eit_activities", "projects", "participants", "participants_dept")
for (i in INPUT_FILE){
  assign(i, read.csv(file = paste0(path, DOSSIER, i, ".csv"), na.strings = "", encoding = "UTF-8"))
  
}
  # #PROPOSAL
  # prop <- read.csv(file = paste0(chemin_bd, "proposal/proposal.csv"), na.strings = "", encoding = "UTF-8") 
  # prop[prop == ""]<- NA_character_
  # appl <- read.csv(file = paste0(chemin_bd, "proposal/proposal_applicants.csv"), na.strings = "", encoding = "UTF-8")
  # appl[appl == ""]<- NA_character_
  # appl_dept <- read.csv(file = paste0(chemin_bd, "proposal/proposal_applicants_dept.csv"), na.strings = "", encoding = "UTF-8")
  # appl_dept[appl_dept == ""]<- NA_character_
  # 
  #project
  # proj <- read.csv(file =  paste0(chemin_bd, "project/projects.csv"), na.strings = "", encoding = "UTF-8")
  # proj[proj == ""]<- NA_character_
  # part <- read.csv(file = paste0(chemin_bd, "project/participants.csv"), na.strings = "", encoding = "UTF-8")
  # part[part == ""]<- NA_character_
  # part_dept <- read.csv(file = paste0(chemin_bd, "project/participants_dept.csv"), na.strings = "", encoding = "UTF-8")
  # part_dept[part_dept == ""]<- NA_character_
  # 
  # art_proj <- read.csv(file = paste0(chemin_bd, "project/p2p_projects.csv"), na.strings = "", encoding = "UTF-8")
  # art_proj[art_proj == ""]<- NA_character_
  # art_part <- read.csv(file = paste0(chemin_bd, "project/p2p_participants.csv"), na.strings = "", encoding = "UTF-8")
  # art_part[art_part == ""]<- NA_character_
  # eit_act <- read.csv(file = paste0(chemin_bd, "project/eit_activities.csv"), na.strings = "", encoding = "UTF-8")
  # eit_act[eit_act == ""]<- NA_character_
  # eit_part <- read.csv(file = paste0(chemin_bd, "project/eit_kava_participants.csv"), na.strings = "", encoding = "UTF-8")
  # eit_part[eit_part == ""]<- NA_character_
  # 
  # save(list = c("prop", "appl", "appl_dept", "proj", "part", "part_dept", "art_proj", "art_part", "eit_act", "eit_part"), 
  #     file = paste0(chemin_bd, "bases.RData"))
  
  save(list = c("proposal", "proposal_applicants", "eit_proposal", "eit_applicant","proposal_applicants_dept",
                "p2p_participants","p2p_projects", "eit_kava_participants", "eit_activities", "projects", "participants", "participants_dept"),
       file = paste0(path, "bases.RData"))
}




#IMPORTATION 32 BITS
access_sql_32 <- function(db_sql = NULL, table_out = NULL, db_path = NULL) {
  library(svSocket)
  
  # variables to make values uniform
  sock_port <- 8642L
  sock_con <- "sv_con"
  ODBC_con <- "a32_con"
  
  if (file.exists(db_path)) {
    
    # build ODBC string
    ODBC_str <- local({
      s <- list()
      s$path    <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db_path)))
      s$driver  <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
      s$threads <- "Threads=4"
      s$buffer  <- "MaxBufferSize=4096"
      s$timeout <- "PageTimeout=5"
      paste(s, collapse=";")
    })
    
    # start socket server to transfer data to 32 bit session
    startSocketServer(port=sock_port, server.name="access_query_32", local=TRUE)
    
    # build expression to pass to 32 bit R session
    expr <- "library(svSocket)"
    expr <- c(expr, "library(RODBC)")
    expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC_con, ODBC_str))
    expr <- c(expr, sprintf("%1$s <- sqlQuery(%3$s, \"%2$s\")", table_out, db_sql, ODBC_con))
    expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock_con, sock_port))
    expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, table_out, table_out))
    expr <- c(expr, "odbcCloseAll()")
    expr <- c(expr, sprintf("close(%s)", sock_con))
    expr <- paste(expr, collapse=";")
    
    # launch 32 bit R session and run the expression we built
    prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
    system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)
    
    # stop socket server
    stopSocketServer(port=sock_port)
    
    # display table fields
    message("Retrieved: ", table_out, " - ", paste(colnames(get(table_out)), collapse=", "))
  } else {
    warning("database not found: ", db_path)}}

# importation_nomenclatures <- function() {
#   
#   THEMA_PROP <- data.frame()
#   sql <- "SELECT * FROM H20_REF_THEMA_ALL"
#   access_sql_32(sql, "THEMA_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
#   
#   THEMA_PROJ <- data.frame()
#   sql <- "SELECT * FROM H20_REF_THEMA_ALL"
#   access_sql_32(sql, "THEMA_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))
#   
#   
#   COUNTRY_PROP <- data.frame()
#   sql <- "SELECT * FROM H20_REF_COUNTRY"
#   access_sql_32(sql, "COUNTRY_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
#   
#   COUNTRY_PROJ <- data.frame()
#   sql <- "SELECT * FROM H20_REF_COUNTRY"
#   access_sql_32(sql, "COUNTRY_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))
#   
#   TOPIC_PROP <- data.frame()
#   sql <- "SELECT * FROM H20_REF_TOPIC"
#   access_sql_32(sql, "TOPIC_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
#   TOPIC_PROJ <- data.frame()
#   sql <- "SELECT * FROM H20_REF_TOPIC"
#   access_sql_32(sql, "TOPIC_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))
#   
#   ACT_PROP <- data.frame()
#   sql <- "SELECT * FROM H20_REF_ACTION_TYPES"
#   access_sql_32(sql, "ACT_PROP", paste0(chemin_bd,"/H2020_Proposals_eCORDA.accdb"))
#   ACT_PROJ <- data.frame()
#   sql <- "SELECT * FROM H20_REF_ACTION_TYPES"
#   access_sql_32(sql, "ACT_PROJ", paste0(chemin_bd,"/H2020_Grants_eCORDA.accdb"))
#   
# }





