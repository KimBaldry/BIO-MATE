### calculate TCHLA
calc_TCHLA <- function(pig_data){
  
  idx = which(is.na(pig_data$TCHLA))
  pig_data$TCHLA[idx] = rowSums(pig_data[idx,c("Chla","DVChla","Chla_ide","Chla_ allom","Chla_prime")],na.rm = T)
  return(pig_data)
  
}