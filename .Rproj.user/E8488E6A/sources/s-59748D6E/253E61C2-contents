library(data.table)
library(BIOMATE)
path = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/"

for(c in c("regression_test", "C1", "C2", "C3", "C4", "C5")){
  meta = fread(file.path(path,"BIO-MATE","product_data","processing_metadata",c,"PIG_meta.csv"),header = T,strip.white = T,stringsAsFactors = F)
  if(c != "regression_test"){
    META = rbind(META,meta)}else{META = meta}
  }
META = META[rowSums(matrix(unlist(lapply(as.matrix(META),is.empty)), ncol = ncol(META))) != ncol(META),]  

for(c in c("regression_test", "C1", "C2", "C3", "C4", "C5")){
  meta = fread(file.path(path,"BIO-MATE","product_data","processing_metadata",c,"PROF_meta.csv"),header = T,strip.white = T,stringsAsFactors = F)
  if(c != "regression_test"){
    PROFMETA = rbind(PROFMETA,meta[,1:54])}else{PROFMETA = meta[,1:54]}
}
PROFMETA = PROFMETA[rowSums(matrix(unlist(lapply(as.matrix(PROFMETA),is.empty)), ncol = ncol(PROFMETA))) != ncol(PROFMETA),]  
