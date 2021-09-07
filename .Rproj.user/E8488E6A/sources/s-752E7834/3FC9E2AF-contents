data_path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/reformatted_data"

#compile_metadata <- function(data_path){}

# expocodes for PIG data
pig_path = file.path(data_path,"pigments")
pig_files = list.files(pig_path, pattern = "*.csv")
pig_expos = unique(substr(pig_files,1,12))
# expocodes for PROF data
prof_path = file.path(data_path,"profiling_sensors")
prof_files = list.files(prof_path, pattern = "*.csv")
prof_expos = unique(substr(prof_files,1,12))
# all expocodes with files
all_expos = unique(c(prof_expos,pig_expos))

### log available data
meta = data.frame("EXPOCODE" = all_expos)
meta = meta %>% mutate(PROF = EXPOCODE %in% prof_expos, PIG = EXPOCODE %in% pig_expos)
# to add - PROF_match, PROF_citation, PIG_citation, Fluor, cruise report, doi's, contacts, source, WOD cruise code

### Platform, country, NODC and WOD codes
countries = fread("./Rpackage/inst/codes/NODC_countrylist.csv")
platforms = fread("./Rpackage/inst/codes/WOD_s_3_platform.csv")
platforms$`Platform Name` = unlist(lapply(platforms$`Platform Name` , function(x){strsplit(x, split = "\\(")[[1]][1]}))
# add missing codes
platforms[nrow(platforms)+1,] = c("11259","RUB3","AKADEMIK TRYOSHNIKOV")
platforms[nrow(platforms)+1,] = c("10005","35XI","Tara")
# match codes
meta$Country = countries$Country[match(substr(meta$EXPOCODE,1,2), countries$NODCCode)]
meta$Platform = platforms$`Platform Name`[match(substr(meta$EXPOCODE,1,4), platforms$`NODC code`)]

### Start and end dates
meta = meta %>% mutate(START = as.Date(substr(meta$EXPOCODE,5,12), format = "%Y%m%d"))
meta$END = as.character(imported_meta$END[match(meta$EXPOCODE, imported_meta$EXPOCODE)])
meta = meta %>% mutate(END = as.Date(END, format = "%Y%m%d"))

### EXPOCODE Synonyms
meta$SYNONYMS = imported_meta$EXPOCODE_synonyms[match(meta$EXPOCODE, imported_meta$EXPOCODE)]

### PROF data
# is there FLUOR data?
# doi, contacts, source, citation
meta$FLUOR_exists = NA
meta$PROF_doi = NA
meta$PROF_contact = NA
meta$PROF_institution = NA
meta$PROF_source = NA
meta$PROF_cite = NA
meta$PROF_PI = NA
meta$PROF_nfiles = NA

for(ex in prof_expos){
  sub_prof_files = prof_files[which(substr(prof_files,1,12) == ex)]
  FLUOR_exists = NA
  BBP_exists = NA
  CP_exists = NA
  XMISS_exists = NA
  for(ctd in 1:length(sub_prof_files)){
  
    # grab header data
    f <- file(file.path(prof_path,sub_prof_files[ctd]), open = "r" )
    n=0
    while( TRUE ){
      n = n+1
      line <- readLines( f, 1L )
      if(ctd == 1){
        if(grepl( "#SOURCED_FROM:", line ) ){
          sc <- trimws(sub("#SOURCED_FROM:", "", line ))
        }
        if(grepl( "#DOI:", line ) ){
          doi <- trimws(sub("#DOI:", "", line ))
        }
        if(grepl( "#contact:", line ) ){
          con <- trimws(sub("#contact:", "", line ))
        }
        if(grepl( "#INSTITUTION:", line ) ){
          inst <- trimws(sub("#INSTITUTION:", "", line ))
        }
        if(grepl( "#PI:", line ) ){
          pi <- trimws(sub("#PI:", "", line ))
        }
      }
      if(grepl("CTDPRS", line)){
        FLUOR_exists[ctd] = grepl("CTDFLUOR",line)
        BBP_exists[ctd] = grepl("CTDBBP700",line)
        CP_exists[ctd] = grepl("CTDBEAMCP",line)
        XMISS_exists[ctd] = grepl("CTDXMISS",line)
        break}
      
    }
    close( f )
    if(ctd == length(sub_prof_files)){
      meta$FLUOR_exists[which(meta$EXPOCODE == ex)] = any(FLUOR_exists)
      meta$PROF_doi[which(meta$EXPOCODE == ex)] = doi
      meta$PROF_contact[which(meta$EXPOCODE == ex)] = con
      meta$PROF_PI[which(meta$EXPOCODE == ex)] = pi 
      meta$PROF_institution[which(meta$EXPOCODE == ex)] = inst
      meta$PROF_source[which(meta$EXPOCODE == ex)] = sc
      meta$PROF_nfiles[which(meta$EXPOCODE == ex)] = length(sub_prof_files)
    #  meta$PROF_cite =
      rm(sc,doi,FLUOR_exists,inst,con)
    }
  }
}

### PIG data
# doi, contacts, source, citation
meta$PIG_doi = NA
meta$PIG_contact = NA
meta$PIG_institution = NA
meta$PIG_source = NA
meta$PIG_cite = NA
meta$PIG_method = NA
meta$PIG_HPLC = NA
meta$PIG_FLUOR = NA
meta$PIG_PI = NA
meta$PIG_nfiles = NA

for(ex in pig_expos){
  sub_pig_files = pig_files[which(substr(pig_files,1,12) == ex)]
  sc = NA
  doi = NA
  con = NA
  mt = NA
  pi = NA
  inst = NA  
  for(pig in 1:length(sub_pig_files)){
    # grab header data
    f <- file(file.path(pig_path,sub_pig_files[pig]), open = "r" )
    n=0
    
    while( TRUE ){
      n = n+1
      line <- readLines( f, 1L )
      if(grepl( "#SOURCED_FROM:", line ) ){
        sc[pig] <- trimws(sub("#SOURCED_FROM:", "", line ))
      }
      if(grepl( "#DOI:", line ) ){
        doi[pig] <- trimws(sub("#DOI:", "", line ))
      }
      if(grepl( "#contact:", line ) ){
        con[pig] <- trimws(sub("#contact:", "", line ))
      }
      if( grepl( "#ANALYSIS_METHOD:", line ) ){
        mt[pig] <- trimws(sub("#ANALYSIS_METHOD:", "", line ))
      }
      if(grepl( "#PI:", line ) ){
        pi[pig] <- trimws(sub("#PI:", "", line ))
      }    
      if(grepl( "#INSTITUTION:", line ) ){
        inst[pig] <- trimws(sub("#INSTITUTION:", "", line ))
      }
      if(grepl("CTD_IDs | DATE | TIME_s | TIME_b| TIME_e | LATITUDE", line)){
        # FLUOR_exists[pig] = grepl("FCHLORA",line)
        # HPLC_exists[pig] = grepl("Chla | TCHLA",line)
        break}
    }
    close( f )

  }
    # meta$PIG_FLUOR[which(meta$EXPOCODE == ex)] = any(FLUOR_exists)
    # meta$PIG_HPLC[which(meta$EXPOCODE == ex)] = any(HPLC_exists)
    meta$PIG_doi[which(meta$EXPOCODE == ex)] = paste(doi, collapse = "; ")
    meta$PIG_contact[which(meta$EXPOCODE == ex)] = paste(con, collapse = "; ")
    meta$PIG_institution[which(meta$EXPOCODE == ex)] = paste(inst, collapse = "; ")
    meta$PIG_PI[which(meta$EXPOCODE == ex)] = paste(pi, collapse = "; ") 
    meta$PIG_source[which(meta$EXPOCODE == ex)] = paste(sc, collapse = "; ")
    meta$PIG_method[which(meta$EXPOCODE == ex)] = paste(mt, collapse = "; ")
    meta$PIG_nfiles[which(meta$EXPOCODE == ex)] = length(sc)
    #  meta$PROF_cite =
   rm(sc,doi,FLUOR_exists,mt,HPLC_exists)  
  
}

### citation data

### report data
