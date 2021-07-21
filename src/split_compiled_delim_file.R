# Title: split_compiled_delim_file.R
# Author: K. Baldry - IMAS/UTAS
# Created: 02/01/2020
#
# This script will break up a delimited text file into sub-files based on EXPOCODES
#
#
# To run this script you will need an acompanying text file that matches EXPOCODES to a 
# ECPOCODE synonym that is used as a reference within the file
# This text file must be named "[file_name]_EXPOCODE_synonym.csv" where file_name is the name of the file to be split
# it must be a text, comma delimited file with two columns. 
# The first column named "EXPOCODE" should contain the EXPOCODE as used in the data aggregation
# The second column named "synonym" muct contain the EXPOCODE synonym that appears in the file
# A new folder will be created by the script in the loction of the file (path) for each EXPOCODE 
# The split files will be saved as comma delimited text files in their respective locations
# 
#
# path - the path where the file is located
# file_name - the name of the file to be split
# delim - the text delimiter of the file to be split
# line_start - the line in which the data table starts (i.e. the line the variale headers are on)
# expo_split - set to T if working with EXPOCODE synonyms data to split further into EXPOCODES. 
# synonym_var_name - the variable, containing EXPOCODE synonyms, that the file is to be split by
# station_split - set to T if working with CTD data to split further into CTD_stations. This will output multiple files to the output folder
# station_var_name - the variable containing station IDs
# fillcell - set to T to fill cells for read.table

split_compiled_delim_file = function(path, file_name, delim, line_start = 1, expo_split = T,synonym_var_name, station_split = F, station_var_name = NA, fillcell = F){
  
  ### Read in dat and set up folder
  # create the new folder if it doesn't exist already
  fname = file_path_sans_ext(file_name)

  # read in the file to be split
  data = read.table(file = file.path(path,file_name), header = T, sep = delim, skip = line_start - 1, stringsAsFactors = F, strip.white = T,fill = fillcell)
  
  if(expo_split){
  # check that the EXPOCODE_synonym file exists. If not, throw an error message
  EXPOCODE_synonym_path = file.path(path,paste(fname,"_EXPOCODE_synonym.csv", sep = ""))
  if(!file.exists(EXPOCODE_synonym_path)){stop("Error: The EXPOCODE synonym path does not exixt or is incorrectly named")}
  
  # read in the EXPOCODE_synonym file
  EXPO_data = read.csv(file = EXPOCODE_synonym_path, header = T, stringsAsFactors = F, strip.white = T)
  
  ### split the file by EXPOCODES
  # list the voyages that appear in the file
  EXPOCODE_list = unique(data[,synonym_var_name])
  
  # list the voyages to be extracted
  EXPOCODE_list2 = unique(EXPO_data[,"synonym"])
  
  # cross check - throw error if all of the EXPOCODE synonyms do not appear in the original file
  if(!all(EXPOCODE_list2 %in% EXPOCODE_list)){stop(paste("Error: some of the EXPOCODE synonyms in",file_name,"do not match those in",paste(fname,"_EXPOCODE_synonym.csv", sep = "")))}
  
  EXPOS = unique(EXPO_data[,"EXPOCODE"])
  # now subset by the EXPOCODE_synonyms and save the subsetted file
  for(ex in EXPOS){
    ex_syn = EXPO_data[which(EXPO_data[,"EXPOCODE"] == ex),"synonym"]
    
    # create destination folder
    ex_folder = file.path(path,ex)
    if(!file.exists(ex_folder))
    {dir.create(ex_folder)}
    
    sub_data = data %>% dplyr::filter(data[,synonym_var_name] %in% ex_syn)
    
    if(station_split){
      for(sn in unique(sub_data[,station_var_name])){
        sub_sub_data = sub_data %>% dplyr::filter(sub_data[,station_var_name] == sn)
        write.csv(sub_sub_data, file = file.path(path,ex,paste(ex,"_",sn,"_from_", fname,".csv", sep = "")), row.names = F)
      }
    }else{
            write.csv(sub_data, file = file.path(path,ex,paste(ex,"_from_", fname,".csv", sep = "")), row.names = F)
          }
  
    tf = file(file.path(path,ex,paste("Data_amendments",Sys.Date(),".txt", sep = "")),open = "wt")
    writeLines(paste("The following files were created from",file_name,"using the function split_compiled_delim_file():"),tf)
    created_files = list.files(file.path(path, ex),pattern = ".csv")
    for(sp in created_files)
    {
      writeLines(paste("-",sp),tf)
    }
    close(tf)
  
  }}else{
    for(sn in unique(data[,station_var_name])){
      sn_2 =  gsub("[^[:alnum:]]","",sn)
      sub_sub_data = data %>% dplyr::filter(data[,station_var_name] == sn)
      if(!file.exists(file.path(path,"split"))){dir.create(file.path(path,"split"))}
      write.csv(sub_sub_data, file = file.path(path,"split",paste(sn_2,"_from_", fname,".csv", sep = "")), row.names = F)
      
      tf = file(file.path(path,"split",paste("Data_amendments",Sys.Date(),".txt", sep = "")),open = "wt")
      writeLines(paste("The following files were created from",file_name,"using the function split_compiled_delim_file():"),tf)
      created_files = list.files(file.path(path, "split"),pattern = ".csv")
      for(sp in created_files)
      {
        writeLines(paste("-",sp),tf)
      }
      close(tf)
      
      }
  }

  return("successful run")
  
}