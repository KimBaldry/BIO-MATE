## This function adds extra Aurora CTDFLUOR data for the initial data product

# add extra aurora australis data: 09AR19980228, 09AR19990713, 09AR20010101, 09AR20011029
extra_data = data.frame("EXPOCODE" =  c("09AR19980228", "09AR19990713", "09AR20010101", "09AR20011029"))
extra_data$data.path = file.path("E:/Data_downloads/AADC/CTD",extra_data$EXPOCODE)
extra_data$F_col = c(10,10,11,11)
extra_data$n_heads = c(15,15,15,15)
extra_data$na.string = c("-9","NaN","NA","NA")

path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/reformatted_data"
ctd_files = list.files(file.path(path,"profiling_sensors"),full.names = T)
# Read BIOMATE profiling sensor data
for(ex in 1:nrow(extra_data)){
 files = ctd_files[grep(extra_data$EXPOCODE[ex],ctd_files)]
 for(fl in files){
  ## get lines and data
   f <- file( fl, open = "r" )
   n=0
   t = 0
   while( TRUE ){
     line <- readLines( f, 1L)
     n = n+1
     if(grepl("#ORIGINAL_CTDFILE:", line)){orig_file = gsub(pattern = "#ORIGINAL_CTDFILE: ",replacement = "", line)}
     if(t == "stopnext"){unit_line = line
     break}
     if(grepl("CTDPRS", line)){
     head_line = line
     t = "stopnext"
     }
     
   }
   close(f)
   
   prof_data = as.data.frame(fread(fl,strip.white = T , stringsAsFactors = F, skip = n,na.strings =  "-999"))

   # Read CTD data and choose fluorescence column   
   nf = paste(extra_data$data.path[ex],orig_file, sep = "/")
   
   f= file(nf, open = "r")
   lines = readLines(f,extra_data$n_heads[ex] - 1)
   close(f)
   
   if(grepl("fluorescence",lines[extra_data$n_heads[ex] - 1])){
   new_data = as.data.frame(read_table2(nf,col_names = F, skip = extra_data$n_heads[ex], na = extra_data$na.string[ex],col_types = cols()))
   prof_data$CTDFLUOR = new_data[,extra_data$F_col[ex]]
   prof_data[is.na(prof_data)] <- -999
   
   # rewrite BIOMATE data file   
   f <- file( fl, open = "r" )
   start_lines <- readLines( f, n-2)
   close(f)
  
  fd <- file(fl, open = "w" )
  writeLines(start_lines,fd)
  writeLines(paste(head_line,",CTDFLUOR"),fd)
  writeLines(paste(unit_line,","),fd)
  for(row in 1:nrow(prof_data))
    {writeLines(toString(as.numeric(prof_data[row,])),fd)}
  close(fd)
   }
 }
}



