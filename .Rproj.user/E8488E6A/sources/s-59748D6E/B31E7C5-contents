# change all _ctd to _prof in references
# direct to citation files 
library(stringr)
meta_path = "./Rpackage/data/citations"
file_names = list.files(meta_path)

for(i in file_names){
  lines <- readLines(file.path(meta_path,i))

  ref_idx = grep("@", lines)
  ctd_idx = unlist(lapply("ctd", function(x){grep(x,lines[ref_idx], ignore.case = T)}))
  if(length(ctd_idx) > 0){
  for(j in ctd_idx){
    lines[ref_idx[j]] = str_replace(lines[ref_idx[j]], "_ctd", "_prof")
  }
  }
  # Overwrite the file
  OutName = file.path(meta_path, i)
  file.remove(OutName)
  outFile <- file(OutName, "w")
  writeLines(lines, outFile)
  close(outFile)
}
