# Ajout du lib de Call
appels_h2020 <- fromJSON("http://ec.europa.eu/research/participants/portal/data/call/h2020/calls.json") %>% 
  .[["callData"]] %>%
  .[["Calls"]]


appels_h2020 <- tibble::data_frame(call = appels_h2020$CallIdentifier$CallId,
                                   lib_call = appels_h2020$Title,
                                   topics = appels_h2020$CallBudgetOverview)


###############################################################################