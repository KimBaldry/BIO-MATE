

calculate_expos = function(data_path){
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
  return(list("All" = all_expos, "PROF" = prof_expos, "PIG" = pig_expos, "PROF_files" = prof_files, "PIG_files" = pig_files))
}


