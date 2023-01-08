compile_prof <- function(data_path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/reformatted_data"){

ctd_files = list.files(file.path(data_path,"profiling_sensors"), pattern = ".csv", full.names = F)

CTD_IDs = unlist(strsplit(ctd_files,"_ctd1.csv"))
# create a data frame with split info
ctd_split = strsplit(CTD_IDs, split = "_")
CTD_info = data.frame("CTD_ID" = CTD_IDs, "EXPOCODE" = sapply(ctd_split, "[[", 1), "STNNBR" = sapply(ctd_split, "[[", 3), "CASTNO"= sapply(ctd_split, "[[", 4), stringsAsFactors = F)
  
  for(idx in 1:nrow(CTD_info)){
    FLUOR_exists = NA
    BBP_exists = NA
    CP_exists = NA
    XMISS_exists = NA
    
    ctd_file = file.path(file.path(data_path,"profiling_sensors"),paste(CTD_info$CTD_ID[idx],"_ctd1.csv",sep = ""))
    
    # open file and read relevent lines
    f <- file( ctd_file, open = "r" )
    time_s = NA
    time_b = NA
    time_e = NA
    while( TRUE ){
      line <- readLines( f, 1L ,skipNul = T)
      if( grepl( "DATE =", line ) ){
        CTD_info$DATE[idx] <- trimws(sub("DATE =", "", line ))
        date = CTD_info$DATE[idx]
      }
      if( grepl( "SHIP =", line ) ){
        CTD_info$SHIP[idx] <- trimws(sub("SHIP =", "", line ))
        ship = CTD_info$SHIP[idx]
      }
      if( grepl( "CTD_START_TIME =", line ) ){
        t_line = line
        time_s<- trimws(sub("UTC","",sub("CTD_START_TIME =", "", line )))
        
      }
      if( grepl( "CTD_BOTTOM_TIME =", line ) ){
        time_b <- trimws(sub("UTC","",sub( "CTD_BOTTOM_TIME =", "", line )))
        
      }
      if( grepl( "CTD_END_TIME =", line ) ){
        time_e <- trimws(sub("UTC","",sub( "CTD_END_TIME =", "", line )))
        
      }
      if( grepl( "CTD_START_LATITUDE =", line ) ){
        CTD_info$LATITUDE_s[idx] <- as.numeric(sub("CTD_START_LATITUDE =", "", line ))
      }
      if( grepl( "CTD_START_LONGITUDE =", line ) ){
        CTD_info$LONGITUDE_s[idx] <- as.numeric(sub("CTD_START_LONGITUDE =", "", line ))
      }
      if( grepl( "CTD_BOTTOM_LATITUDE =", line ) ){
        CTD_info$LATITUDE_b[idx] <- as.numeric(sub("CTD_BOTTOM_LATITUDE =", "", line ))
      }
      if( grepl( "CTD_BOTTOM_LONGITUDE =", line ) ){
        CTD_info$LONGITUDE_b[idx] <- as.numeric(sub("CTD_BOTTOM_LONGITUDE =", "", line ))
      }
      if( grepl( "CTD_END_LATITUDE =", line ) ){
        CTD_info$LATITUDE_e[idx] <- as.numeric(sub("CTD_END_LATITUDE =", "", line ))
      }
      if( grepl( "CTD_END_LONGITUDE =", line ) ){
        CTD_info$LONGITUDE_e[idx] <- as.numeric(sub("CTD_END_LONGITUDE =", "", line ))
      }
    if(grepl("CTDPRS", line)){
      CTD_info$FLUOR = grepl("CTDFLUOR",line)
      CTD_info$BBP = grepl("CTDBBP700",line)
      CTD_info$CP = grepl("CTDBEAMCP",line)
      CTD_info$XMISS = grepl("CTDXMISS",line)
      break}
    
  }
close( f )


  }
return(CTD_info)
}