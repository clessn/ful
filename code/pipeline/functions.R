### function
get_n_comm <- function(commtype, data = DataComm){
  output <- data %>% 
    mutate(is_commtype = ifelse(comm_type == commtype, 1, 0),
           initorder = row_number()) %>%
    arrange(date_comm) %>% 
    group_by(UL_NO_CODE) %>% 
    mutate(n = cumsum(is_commtype)) %>% 
    ungroup() %>% 
    arrange(initorder) %>% 
    pull(., n)
  return(output)
}

don_first <- function(datecomms, datedons, data = DataRep){
  # S'assure que les dates sont au format Date
  datecomms <- as.Date(datecomms)
  datedons <- as.Date(datedons)
  
  # Utilise ifelse pour gérer les vecteurs
  output <- ifelse(is.na(datedons), 
                   0, 
                   ifelse(datecomms >= datedons, 1, 0))
  return(output)
}













