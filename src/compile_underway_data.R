compile_uwy <- function(data_path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/reformatted_data"){
  
  ctd_files = list.files(file.path(data_path,"underway_sensors"), pattern = ".csv", full.names = F)
  
  EX = unlist(strsplit(ctd_files,"_UWY.csv"))
  # create a data frame with split info
  CTD_info = data.frame("EXPOCODE" = EX, stringsAsFactors = F)
  
  for(idx in 1:nrow(CTD_info)){
    
    ctd_file = file.path(file.path(data_path,"underway_sensors"),paste(CTD_info$EX[idx],"_UWY.csv",sep = ""))
    
    # open file and read relevent lines
    f <- file( ctd_file, open = "r" )
    n = 0
    while( TRUE ){
      n = n+1
      line <- readLines( f, 1L ,skipNul = T)

      if( grepl( "SHIP =", line ) ){
        ship <- trimws(sub("SHIP =", "", line ))
        
      }
          if(grepl("DATE, TIME", line)){
        break}
      
    }
    
    close( f )
  
    
    file_data = as.data.frame(fread(ctd_file,strip.white = T , stringsAsFactors = F, skip = n+1,na.strings =  "-999"))
    f_headers = as.character(fread(ctd_file,stringsAsFactors = F, skip = n-1, nrows = 1, header = F))
    colnames(file_data) = f_headers
    
    file_data$DATE = as.character(file_data$DATE)
    file_data$TIME = as.character(file_data$TIME)
    file_data$LONGITUDE = as.numeric(file_data$LONGITUDE)
    file_data$LATITUDE = as.numeric(file_data$LATITUDE)
    
    file_data = file_data[,which(!is.na(colnames(file_data)))]
    # add header data
    file_data$SHIP = ship
    file_data$EXPOCODE = CTD_info$EX[idx]
    if(!exists("data",inherits = F)){data = file_data}else{
      # append file data
      data = bind_rows(data,file_data)
    }
    rm(file_data)
    print(paste(ctd_file,"compiled"))
  }
  return(data)

    
  }
