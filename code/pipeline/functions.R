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













