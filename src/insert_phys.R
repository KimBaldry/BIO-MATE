insert_phys <- function(data, BIOMATE_path){
  
  data_clean = data %>% filter(CTD_IDs != "U", !is.na(CTD_IDs) )
  data_clean$DEPTH = as.numeric(data_clean$DEPTH)
  ctd_path =file.path(BIOMATE_path,"profiling_sensors")
  data_clean$MLD = NA
  data_clean$CHL50 = NA
  data_clean$EMLD = NA
  data_clean$EMLD_QI = NA
  data_clean$MLD_FLAG = NA
  data_clean$CTDSAL = NA
  data_clean$CTDTMP =
  data_clean$CTDFLUOR = NA
  data_clean$FZ_insitu = NA
  # add TRANSMittance later - need path lengths for some conversions
  data_clean$CTDBBP700 = NA
  ### match profiling sensor data
  for(pf in unique(data_clean$CTD_IDs)){
    print(pf)
    mdx = which(data_clean$CTD_IDs == pf)
    ctd_file = file.path(ctd_path,paste(pf,"_ctd1.csv",sep = ""))
    
    f <- file( ctd_file, open = "r" )
    n=0
    while( TRUE ){
      line <- readLines( f, 1L)
      n = n+1
      if( grepl( "#BIOMATE_CITE_TAGS:", line ) ){
        cite <- trimws(sub("#BIOMATE_CITE_TAGS:", "", line ))
      }
      
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
    prof_data = prof_data %>% filter(!is.na(DEPTH))
    max_depth = max(prof_data$DEPTH, na.rm = T)
    
    if(any(grepl("CTDSAL",f_headers)) & any(!is.na(prof_data$CTDSAL)) & !sd(prof_data$CTDSAL, na.rm = T) %in% c(NA,0)){
      for(md in mdx){
        data_clean$CTDSAL[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDSAL))], prof_data$CTDSAL[which(is.finite(prof_data$CTDSAL))],data_clean$DEPTH[md])
      }
    }
    if(any(grepl("CTDTMP",f_headers))& any(!is.na(prof_data$CTDTMP)) & !sd(prof_data$CTDTMP, na.rm = T)  %in% c(NA,0)){
      for(md in mdx){
        data_clean$CTDTMP[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDTMP))], prof_data$CTDTMP[which(is.finite(prof_data$CTDTMP))],data_clean$DEPTH[md])
      }
    }
    if(any(grepl("CTDFLUOR",f_headers)) & any(!is.na(prof_data$CTDFLUOR)) & !sd(prof_data$CTDFLUOR, na.rm = T)  %in% c(NA,0)){
      # note eco MLD doesnt work with the low quality data
        fluor_mod = apply_bsm(prof_data$DEPTH[prof_data$DEPTH < 500], prof_data$CTDFLUOR[prof_data$DEPTH < 500])
        if(!all(is.na(fluor_mod))){
        fluor_smth = fluor_mod$fluor.out
        if(max_depth >= 500){
        fluor_smth[prof_data$DEPTH >= 500] = prof_data$CTDFLUOR[prof_data$DEPTH >= 500]}
        data_clean$CHL50[mdx] = CHL_50(prof_data$DEPTH[is.finite(fluor_smth)],fluor_smth[is.finite(fluor_smth)])
        E = Eco_MLD(prof_data$DEPTH[is.finite(fluor_smth)],fluor_smth[is.finite(fluor_smth)])
        data_clean$EMLD[mdx] = E$EMLD}
      
      for(md in mdx){
        data_clean$CTDFLUOR[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDFLUOR))], prof_data$CTDFLUOR[which(is.finite(prof_data$CTDFLUOR))],data_clean$DEPTH[md])
        
      }
    }
    if(any(grepl("CTDBBP700",f_headers))& any(!is.na(prof_data$CTDBBP700))  & !sd(prof_data$CTDBBP700, na.rm = T)  %in% c(NA,0)){
      for(md in mdx){
        data_clean$CTDBBP700[md] = mean_5m(prof_data$DEPTH[which(is.finite(prof_data$CTDBBP700))], prof_data$CTDBBP700[which(is.finite(prof_data$CTDBBP700))],data_clean$DEPTH[md])
      }
    }
    
    ### MLD calculation - 0.03 density threshold - this is a typical definition of MLD for the SO
    if(all(any(grepl("CTDPRS",f_headers)), any(grepl("CTDSAL",f_headers)), any(grepl("CTDTMP",f_headers)),!is.na(data_clean$LATITUDE[mdx[1]])) & !sd(prof_data$CTDSAL, na.rm = T) %in% c(NA,0) & !sd(prof_data$CTDTMP, na.rm = T) %in% c(NA,0)){
      sal_mod = fit_bp_segmented(prof_data$DEPTH[prof_data$DEPTH < 500], prof_data$CTDSAL[prof_data$DEPTH < 500])
      if(!all(is.na(sal_mod))){
        sal_smth = sal_mod$fluor.out
      if(max_depth >= 500){
        sal_smth[prof_data$DEPTH >= 500] = prof_data$CTDSAL[prof_data$DEPTH >= 500]}

      tmp_mod = fit_bp_segmented(prof_data$DEPTH[prof_data$DEPTH < 500], prof_data$CTDTMP[prof_data$DEPTH < 500])
      if(!all(is.na(tmp_mod))){
      tmp_smth = tmp_mod$fluor.out
      if(max_depth >= 500){
        tmp_smth[prof_data$DEPTH >= 500] = prof_data$CTDTMP[prof_data$DEPTH >= 500]}

      MLD_calc = MLD(prof_data$CTDPRS,sal_smth, tmp_smth,lat = data_clean$LATITUDE[mdx[1]],dens_thresh = 0.03)
      data_clean$MLD[mdx] = MLD_calc$MLD
      data_clean$MLD_FLAG[mdx] = MLD_calc$FLAG
      data_clean$FZ_insitu[mdx] = front_class_insitu( prof_data$CTDPRS, prof_data$CTDTMP, prof_data$CTDSAL,data_clean$LATITUDE[mdx[1]],data_clean$LONGITUDE[mdx[1]])
      DENS = as.numeric(rho(sal_smth,tmp_smth,P = 0))
      if(!is.na(data_clean$MLD[mdx[1]])){
      data_clean$TLD[mdx] = TLD(data_clean$MLD[mdx[1]], DENS,prof_data$DEPTH)}

      }
      }

    }
    
  }
  data_clean$DEPTH = as.character(data_clean$DEPTH)
  data_clean$PROF_cite = cite
   data2 = left_join(data, data_clean)
  
  return(data2)
}
