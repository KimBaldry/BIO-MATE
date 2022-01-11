### calculate TCHLA
calc_TCHLA <- function(pig_data){
  
  idx = which(is.na(pig_data$TCHLA) & !is.na(pig_data$Chla))
  pig_data$TCHLA[idx] = rowSums(pig_data[idx,c("Chla","DVChla","Chla_ide","Chla_allom","Chla_prime")],na.rm = T)
  return(pig_data)
  
}