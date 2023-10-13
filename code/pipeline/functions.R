# function

get_n_comm <- function(ul_id, commtype, date, data = DataComm){
  date <- as.Date(date)
  filtered_data <- data[data$UL_NO_CODE == ul_id &
                          data$comm_type == commtype &
                          data$date_comm < date,]
  n <- nrow(filtered_data)
  return(n)
}

















