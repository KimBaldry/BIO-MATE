# Title: create_package_data.R
# Author: K. Baldry - IMAS/UTAS
# Created: 17/07/2021
#
# This script updates BIO-MATE.
#
# 
#
library(bibtex)
library(roxygen2)
source("./src/compile_metadata.R")
# update_package <- function(){}

### EXPOCODE tables
countries = fread("../Rpackage/inst/codes/NODC_countrylist.csv")
platforms = fread("../Rpackage/inst/codes/WOD_s_3_platform.csv")
platforms$`Platform Name` = unlist(lapply(platforms$`Platform Name` , function(x){strsplit(x, split = "\\(")[[1]][1]}))
# add missing codes
platforms[nrow(platforms)+1,] = c("11259","RUB3","AKADEMIK TRYOSHNIKOV")
platforms[nrow(platforms)+1,] = c("10005","35XI","Tara")

### Citation .bib file
# citation files are updated within bibpath
# compile new citations into one file
bibpath = "./product_data/supporting_information/citations"
path_to_bib_files = list.files(bibpath, pattern = "*.bib")
combined_bib <- ""
for (path_to_bib_file in path_to_bib_files) {
  
  fileCon <- file(file.path(bibpath,path_to_bib_file))
  content <- readLines(fileCon)
  close(fileCon)
  
  combined_bib <- paste0(combined_bib, "\n", "\n", trimws(paste0(content, collapse="\n")))
  
}
cat(combined_bib, file=file.path("../Rpackage/inst/citations","BIO-MATE_references.bib"), "\n")
# create bib object and save in R package
bib = read.bib(file.path("../Rpackage/inst/citations","BIO-MATE_references.bib"))


### Source information
source_info = read.csv("./product_data/supporting_information/BIOMATE_SOURCES.txt", stringsAsFactors = F)

### Method information
method_info = read.csv("./product_data/supporting_information/BIOMATE_Methods.txt", stringsAsFactors = F)

# compile processing metadata - only do once
# library(data.table)
# library(BIOMATE)
# path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/"
# 
# for(c in c("regression_test", "C1", "C2", "C3", "C4", "C5")){
#   meta = fread(file.path(path,"BIO-MATE","product_data","processing_metadata",c,"PIG_meta.csv"),header = T,strip.white = T,stringsAsFactors = F)
#   if(c != "regression_test"){
#     META = rbind(META,meta)}else{META = meta}
# }
# META = META[rowSums(matrix(unlist(lapply(as.matrix(META),is.empty)), ncol = ncol(META))) != ncol(META),]
# 
# # Methods look-up table
# Mdf = unique((META[,c("analysis_type","Method")]))
# Mdf = Mdf[order(Mdf$analysis_type),]
# write.csv(Mdf, file=file.path("./product_data/supporting_information","BIOMATE_METHODS.txt"),row.names = F)

BIOMATE_overview = compile_metadata()


### save data
save(BIOMATE_overview, bib,source_info,method_info,platforms, countries,file =file.path("../Rpackage/data","BIOMATE.rda"))

### Compile package
roxygen2::roxygenise("../Rpackage")
