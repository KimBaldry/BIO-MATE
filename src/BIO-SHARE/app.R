### BIO-SHARE
# An R shiny app to accompany the BIO-MATE product. Allows users to reformat their own data and submit to Git for addition to new versions of the data product
#
# app.R has 3 components
# 1. user interface object
# 2. server function
# 3. call to the shinyApp

library(shiny)
library(shinyWidgets)
library(dplyr)
library(rhandsontable)
# 1. user interface object
ui <- navbarPage(
 "BIO-SHARE",
  tabPanel("Upload files", 
           fluidPage(
             p("Upload published data files. BIO-SHARE can only handle processing one data stream at a time."),
             fileInput("upload_PROF", NULL, accept = c(".csv", ".tsv",".tab",".nc",".all",".sb",".txt"),multiple = T), 
             selectInput("stream", label = "Data stream to run", choices = c("PROF", "PIG")),
           )),
  tabPanel("Split files",
           fluidPage(
             p("Fill in the below information to split your file. Note to use the split function for multiple streams, enter the information for one data stream then select run. After this do the same for the next data stream."),
             actionButton("split", label = "Run"),

             
             textInput("split_delim",label = "File Delimiter"),
             p("The text delimiter of the file to be split. Put the delimeter symbol here, remember tab is /t."),
             numericInput("split_line_start", "Header line", value = 1, min = 1, max = 100),
             p("The line in which the data table starts (i.e. the line the variable headers are on)"),
             selectInput("expo_split", "Split by EXPOCODES", choices = c("T","F"), selected = "T"),
             fileInput("ex_lookup", NULL, accept = c(".csv"),multiple = F),
             p("Set to T if working with EXPOCODE synonyms data to split further into EXPOCODES. Note you need a supplementry comma delimited file with two columns. The first column named 'EXPOCODE' should contain the EXPOCODE as used in the data aggregation. The second column named 'synonym' muct contain the EXPOCODE synonym that appears in the file."),
             textInput("synonym_var_name", "Variable with EXPOCODE synonyms"),
             p("Only needed when Split by EXPOCODES = T. The variable, containing EXPOCODE synonyms, that the file is to be split by."),
            selectInput("station_split", "Split by station", choices = c("T","F"), selected = "T"),
             p("Set to T if working with PROF/CTD data to split further into profileing stations."),
            textInput("station_var_name", "Variable with station ID"),
             p("Only needed if Split by station = T. The variable containing station IDs"),
           selectInput("fillcell", "Fill cells", choices = c("T","F"), selected = "F"),
           p("logical. If TRUE then in case the rows have unequal length, blank fields are implicitly added. See 'fill' and 'Details' in 'read.table' for more information.")
           )
           
           ), 
  tabPanel("Enter metadata",
           fluidPage(
             rHandsontableOutput("hot")
           )
           ),
  tabPanel("Upload BibTEX files"
           ), 
  tabPanel("Run BIOMATE"
           ),
  tabPanel("Upload to GitHub"
           ),
  tabPanel("Download data files", 
           fluidPage(
             downloadButton("download")
           )
           )
  
  
  
  
)

# 2.server function
server <- function(input, output){
  load(url("https://github.com/KimBaldry/BIO-MATE/raw/main/data_descriptor_paper/metadata_info.RData"))
  file_paths = reactive({ # this should be where the files are stored not their names 
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    if(all(ext %in% c("csv", "tsv","tab","nc","all","sb","txt"))){input$upload$datapath
           }else{validate("Invalid file; Please upload a supported file: .all, .sb, .csv, .tab, .txt, .nc. Submit an issue to https://github.com/KimBaldry/BIO-MATE if you would like another file format supported in BIO-MATE.")}
  })
  
#  split_delim_file(dirname(file_paths), input$upload$name, input$delim, input$line_start, input$expo_split, input$synonym_var_name, input$station_split, input$station_var_name, input$fillcell)
  
  values <- reactiveValues()
  observe({
    if (!is.null(input$hot)) {
      p_meta = hot_to_r(input$hot)
    } else {
      if (is.null(values[["p_meta"]]))
        p_meta <- p_meta
      else
        p_meta <- values[["p_meta"]]
    }
    values[["p_meta"]] <- p_meta
  })
  
  
  output$hot =  renderRHandsontable({p_meta$`User entry` = "" # make this column column # 3 and somehow change width of column
    rhandsontable(p_meta) %>% hot_col(c(1:4), readOnly = TRUE)})
  # output$download <- downloadHandler(
  #   filename = function(){
  #     paste0(input$text,".zip")
  #     
  #   },
  #   content = function(file){
  #     #go to a temp dir to avoid permission issues
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     files <- NULL;
  #     
  #     #loop through the sheets
  #     for (i in 1:input$sheet){
  #       #write each sheet to a csv file, save the name
  #       fileName <- paste(input$text,"_0",i,".csv",sep = "")
  #       write.table(data()$wb[i],fileName,sep = ';', row.names = F, col.names = T)
  #       files <- c(fileName,files)
  #     }
  #     #create the zip file
  #     zip(file,files)
  #   }
  # )
}

# 3. call to shinyApp

shinyApp(ui, server)
