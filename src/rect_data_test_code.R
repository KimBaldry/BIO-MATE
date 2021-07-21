# Test for Ben
# set fl to filename/path
library(data.table)

# get number of the header line to skip
# I have some code that automatically picks out the number of the header line. For the file I sent you, its line 14
n = 14
if(length(n) == 0){ n = 1}
# check for extra lines before the data table starts. (e.g unit lines)
# There should be some numeric data in the table, but not in the header or unit lines.
b = n+1
line = fread(fl,stringsAsFactors = F, skip = n, nrows = 1, header = F, keepLeadingZeros = T)
while(!any(unlist(lapply(line,is.numeric)))){
  line = fread(fl,stringsAsFactors = F, skip = b, nrows = 1, header = F, keepLeadingZeros = T)
  b = b+1}

### Get data table
# Use fread to read the data table - this will adapt if there is a units line or not.

# catch the out-of-format lines here for debugging
#tryCatch({data = as.data.frame(fread(fl,stringsAsFactors = F, skip = n, na.strings = info$missing_value,strip.white = T , header = F))}, warning=function(w) print(fl))

# rectangular data shouldnt be read with fread - I have a indicator for file structure and isolated code below for the rectangular data
    data = read_table2(fl,col_names = F, skip = b-1, na = as.character(info$missing_value),col_types = cols())
    # get the header line
    headers = fread(fl, skip = n-1, nrows = 1, header = F, stringsAsFactors = F, keepLeadingZeros = T)
    headers = as.character(headers[1,])

    headers = headers[headers != "NA"]
    # assign colnames
    colnames(data)[1:length(headers)] = headers
    
  