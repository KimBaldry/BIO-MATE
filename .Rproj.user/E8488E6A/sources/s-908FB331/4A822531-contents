rm_duplicates <- function(data, fuzzy = T, meth.dup = "mean"){
  
  
  # All pigment names
  pig_names = colnames(data)[-c(1:which(colnames(data) == "DEPTH"),which(colnames(data) == "PIG_SOURCE"):length(colnames(data)))]
  
  # find hard duplicates and average (this will create an average of replicates, or just remove duplicates)
  # combine information from duplicates (only )
  dup_df = data[,c("EXPOCODE","DEPTH","PIG_METHOD","STNNBR","CASTNO","TIME_analyser","DATE_analyser","LAT_analyser","LON_analyser")]
  dup_df$LAT_analyser = round(as.numeric(dup_df$LAT_analyser),digits = 2)
  dup_df$LON_analyser = round(as.numeric(dup_df$LON_analyser),digits = 2)
  dup_idx = which(duplicated(dup_df))
  data_clean = data[-dup_idx,]
  data_clean$LATITUDE = as.numeric(data_clean$LATITUDE)
  data_clean$LONGITUDE = as.numeric(data_clean$LONGITUDE)
  data_clean$DEPTH = as.numeric(data_clean$DEPTH)
  
  data_clean$n_dups = 1
  # average or median duplicates
  for(dup in dup_idx)
  {
    # duplicated line
    dup_line = data[dup,]
    attach(data_clean, warn.conflicts = F)
    idx_in_clean = which(EXPOCODE == dup_line$EXPOCODE & DEPTH == dup_line$DEPTH & PIG_METHOD == dup_line$PIG_METHOD & STNNBR == dup_line$STNNBR & CASTNO == dup_line$CASTNO)
    detach(data_clean)
    if(dup != dup_idx[1]){
      if(any(dup_idx[1:(which(dup_idx == dup)-1)] %in% idx_in_clean)){next}}
    # matching lines in clean data
    data_match = data %>% dplyr::filter(EXPOCODE == dup_line$EXPOCODE,DEPTH == dup_line$DEPTH,PIG_METHOD == dup_line$PIG_METHOD,STNNBR == dup_line$STNNBR, CASTNO == dup_line$CASTNO)
    if(dup>dup_idx[1]){data_match_all = rbind(data_match_all, data_match)}else{data_match_all = data_match}
    # Average or median
    for(pig in pig_names){
      if(any(!is.na(data_match[,pig]))){
        if(meth.dup == "mean"){data_clean[idx_in_clean,pig] = mean(as.numeric(data_match[,pig]), na.rm = T)}
        if(meth.dup == "median"){data_clean[idx_in_clean,pig] = median(as.numeric(data_match[,pig]), na.rm = T)}
      }
      #print("dup averaged")
    }
    data_clean$PIG_SOURCE[idx_in_clean] = paste(data_match$PIG_SOURCE,collapse = "-")
    data_clean$n_dups[idx_in_clean] = nrow(data_match)
  }
  
  
  
  ### Fuzzy duplicates are within 1 m of another measurement
  # need to speed this up
  if(fuzzy){
    
    data_clean$FM = NA
    attach(data_clean, warn.conflicts = F)
    for(rw in 1:(nrow(data_clean)-1)){
      row_data = data_clean[rw,]
      idx_in_clean = which(EXPOCODE == row_data$EXPOCODE & DEPTH > row_data$DEPTH - 1 & DEPTH < row_data$DEPTH - 1 & PIG_METHOD == row_data$PIG_METHOD & STNNBR == row_data$STNNBR & CASTNO == row_data$CASTNO)
      fuzzy_match = data_clean[c((rw+1):nrow(data_clean)),] %>% dplyr::filter(EXPOCODE == row_data$EXPOCODE,DEPTH > row_data$DEPTH - 1 ,DEPTH < row_data$DEPTH - 1, PIG_METHOD == row_data$PIG_METHOD,STNNBR == row_data$STNNBR, CASTNO == row_data$CASTNO)
      if(rw > 1){fuzzy_match_all = rbind(fuzzy_match_all, fuzzy_match)}else{fuzzy_match_all = fuzzy_match}
      if(rw > 1){fuzzy_rows = c(fuzzy_rows,rw)}else{
        fuzzy_rows = rw}
    # Average or median
    for(pig in pig_names){
      if(any(!is.na(fuzzy_match[,pig]))){
        if(meth.dup == "mean"){data_clean[idx_in_clean,pig] = mean(fuzzy_match[,pig], na.rm = T)}
        if(meth.dup == "median"){data_clean[idx_in_clean,pig] = median(fuzzy_match[,pig], na.rm = T)}
      }}
    data_clean$FM[idx_in_clean] = rw
    data_clean$PIG_SOURCE[idx_in_clean] = paste(fuzzy_match$PIG_SOURCE,collapse = "-")
    rm(row_data)
    }
    detach(data_clean)

    # remove fuzzy duplicates
    fdx = which(!is.na(data_clean$FM) & base::duplicated(data_clean$FM))
    if(!is.empty(fdx)){data_clean = data_clean[-fdx,]}
  }
  
  n_dup = nrow(data) - nrow(data_clean)
  print(paste("found and removed",n_dup,"duplicated rows"))
  if(exists("data_match_all")){
    write.csv(data_match_all,file.path("./results","Duplicated_rows_removed.csv"),row.names = F)}
  
  return(data_clean)
}
