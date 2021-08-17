insert_phys <- function(data, BIOMATE_path){
  
  data_clean = data %>% filter(CTD_IDs != "U", !is.na(CTD_IDs) )
  data_clean$DEPTH = as.numeric(data_clean$DEPTH)
  ctd_path =file.path(path,"profiling_sensors")
  data_clean$MLD = NA
  data_clean$CHL50 = NA
  data_clean$MLD_FLAG = NA
  data_clean$CTDSAL = NA
  data_clean$CTDTMP =
    data_clean$CTDFLUOR = NA
  # add TRANSMittance later - need path lengths for some conversions
  data_clean$CTDBBP700 = NA
  ### match profiling sensor data
  for(pf in unique(data_clean$CTD_IDs[data_clean$CTD_IDs != "U" & !is.na(data_clean$CTD_IDs)])){
    mdx = which(data_clean$CTD_IDs == pf)
    ctd_file = file.path(ctd_path,paste(pf,"_ctd1.csv",sep = ""))
    
    f <- file( ctd_file, open = "r" )
    n=0
    while( TRUE ){
      line <- readLines( f, 1L)
      n = n+1
      
      if(grepl("CTDPRS", line)){break}
    }
    close(f)
    
    prof_data = as.data.frame(fread(ctd_file,strip.white = T , stringsAsFactors = F, skip = n+1,na.strings =  "-999"))
    f_headers = as.character(fread(ctd_file,stringsAsFactors = F, skip = n-1, nrows = 1, header = F))
    colnames(prof_data) = f_headers
    ### 5 m averages for TEMP, SAL, FLUOR, TRANS
    # grab +/- 2.5 m around the Sample Depth
    # remove any outliers > 3std of other measurments
    # calculate the average
    # insert into the table
    # For underway this will be a 15 min average
    prof_data$DEPTH = swDepth(prof_data$CTDPRS, latitude = data_clean$LATITUDE[mdx[1]])
    
    
    if(any(grepl("CTDSAL",f_headers)) & any(!is.na(prof_data$CTDSAL))){
      for(md in mdx){
        data_clean$CTDSAL[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDSAL))], prof_data$CTDSAL[which(is.finite(prof_data$CTDSAL))],data_clean$DEPTH[md])
      }
    }
    if(any(grepl("CTDTMP",f_headers))& any(!is.na(prof_data$CTDTMP))){
      for(md in mdx){
        data_clean$CTDTMP[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDTMP))], prof_data$CTDTMP[which(is.finite(prof_data$CTDTMP))],data_clean$DEPTH[md])
      }
    }
    if(any(grepl("CTDFLUOR",f_headers)) & any(!is.na(prof_data$CTDFLUOR))){
      for(md in mdx){
        data_clean$CTDFLUOR[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDFLUOR))], prof_data$CTDFLUOR[which(is.finite(prof_data$CTDFLUOR))],data_clean$DEPTH[md])
        # note eco MLD doesnt work with the low quality data
        data_clean$CHL50[mdx] = CHL_50(prof_data$CTDPRS[is.finite(prof_data$CTDFLUOR)],prof_data$CTDFLUOR[is.finite(prof_data$CTDFLUOR)])
      }
    }
    if(any(grepl("CTDBBP700",f_headers))& any(!is.na(prof_data$CTDBBP700))){
      for(md in mdx){
        data_clean$CTDBBP700[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDBBP700))], prof_data$CTDBBP700[which(is.finite(prof_data$CTDBBP700))],data_clean$DEPTH[md])
      }
    }
    
    ### MLD calculation - 0.03 density threshold - this is a typical definition of MLD for the SO
    if(all(any(grepl("CTDPRS",f_headers)), any(grepl("CTDSAL",f_headers)), any(grepl("CTDTMP",f_headers)),!is.na(data_clean$LATITUDE[mdx[1]]))){
      MLD_calc = MLD(prof_data$CTDPRS,prof_data$CTDSAL, prof_data$CTDTMP,lat = data_clean$LATITUDE[mdx[1]],dens_thresh = 0.03)
      data_clean$MLD[mdx] = MLD_calc$MLD
      data_clean$MLD_FLAG[mdx] = MLD_calc$FLAG
      
      
    }
    
    
    
  }
  data_clean$DEPTH = as.character(data_clean$DEPTH)
   data2 = left_join(data, data_clean)
  
  return(data2)
}
