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

load(url("https://github.com/KimBaldry/BIO-MATE/raw/main/data_descriptor_paper/metadata_info.RData"))
load(url("https://github.com/KimBaldry/BIO-MATE/raw/main/product_data/next_version/package_data/BIOMATE.rda"))

p_meta$`User input` = ""
p_meta = p_meta[,c(1:2,5,4:3)]

# 1. user interface object
ui <- navbarPage(
 "BIO-SHARE",
 ################# Upload published data ###########################
  tabPanel("Upload published data", 
           fluidPage(
             p("Upload published data files. BIO-SHARE can only handle processing one data stream at a time."),
             fileInput("upload", NULL, accept = c(".csv", ".tsv",".tab",".nc",".all",".sb",".txt"),multiple = T), 
             #selectInput("run_expocode", label = "EXPOCODE", choices(c("new", ))) # reference to existing expocodes in package metadata
             selectInput("stream", label = "Data stream to ingest", choices = c("PROF", "PIG")),
             p("Information is needed to properly cite the data"),
             selectInput("source",label = "Source citation", choices = c("new source",source_info$source)), # translate to p_meta
             conditionalPanel(condition = "input.source == 'new source'", 
                              rHandsontableOutput("hot_source"), 
                              fileInput("upload_source", "BibTEX files for source", accept = c(".bib"),multiple = T)),            
             checkboxInput("showsource","Show existing BIO-MATE data sources"),
             conditionalPanel(condition = "input.showsource == true", dataTableOutput("source_info"))
             
             
           )),
########### Split files ########################################
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
           )), 
 ### Enter metadata #####################################################
  tabPanel("Enter metadata",
           fluidPage(#p("EXPOCODE metadata") # check dates and ship to see if exists in system
             p("File format information"),
             rHandsontableOutput("hot1"),
             p("Data aquisition information"),
             rHandsontableOutput("hot2"),
             p("Location Data information"),
             rHandsontableOutput("hot3"),
             conditionalPanel(condition = "input.stream == 'PROF'",p("Profiling sensor data information"),
             rHandsontableOutput("hot4")),
             conditionalPanel(condition = "input.stream == 'PIG'", p("Pigment and POC data information"),
             rHandsontableOutput("hot5"))
           )
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
# example here https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html
col.hi.render = "
    function(instance, td) {
Handsontable.renderers.TextRenderer.apply(this, arguments);
td.style.background = 'lightblue';
}"


server <- function(input, output){
  # check existence of EXPOCODE and stream data in the system

 ############## Upload published data #################### 
  file_paths = reactive({ # this should be where the files are stored not their names 
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    if(all(ext %in% c("csv", "tsv","tab","nc","all","sb","txt"))){input$upload$datapath
           }else{validate("Invalid file; Please upload a supported file: .all, .sb, .csv, .tab, .txt, .nc. Submit an issue to https://github.com/KimBaldry/BIO-MATE if you would like another file format supported in BIO-MATE.")}
  })
  
  output$source_info = renderDataTable(source_info) 
  
#  split_delim_file(dirname(file_paths), input$upload$name, input$delim, input$line_start, input$expo_split, input$synonym_var_name, input$station_split, input$station_var_name, input$fillcell)

  
  #### Enter Metadata ########  
  values <- reactiveValues()
  observe({
    if(any(!is.null(input$hot1), !is.null(input$hot2), !is.null(input$hot3), !is.null(input$hot4), !is.null(input$hot5))){
      if(!is.null(input$hot1)){
        temp = hot_to_r(input$hot1)
        idx = unlist(lapply(temp[,1], function(x){which(p_meta[,1] == x)}))
        p_meta[idx,] = hot_to_r(input$hot1)
      }
      if(!is.null(input$hot2)){
        temp = hot_to_r(input$hot2)
        idx = unlist(lapply(temp[,1], function(x){which(p_meta[,1] == x)}))
        p_meta[idx,] = hot_to_r(input$hot2)
      }
      if(!is.null(input$hot3)){
        temp = hot_to_r(input$hot3)
        idx = unlist(lapply(temp[,1], function(x){which(p_meta[,1] == x)}))
        p_meta[idx,] = hot_to_r(input$hot3)
      }
      if(!is.null(input$hot4)){
        temp = hot_to_r(input$hot4)
        idx = unlist(lapply(temp[,1], function(x){which(p_meta[,1] == x)}))
        p_meta[idx,] = hot_to_r(input$hot4)
      }
      if(!is.null(input$hot5)){
        temp = hot_to_r(input$hot5)
        idx = unlist(lapply(temp[,1], function(x){which(p_meta[,1] == x)}))
        p_meta[idx,] = hot_to_r(input$hot5)
      }
      } else {
      if (is.null(values[["p_meta"]]))
        p_meta <- p_meta
      else
        p_meta <- values[["p_meta"]]
    }
    values[["p_meta"]] <- p_meta
  })
  
  # possible selection options for fixed when have time later. Including check on EXPOCODE
  
  output$hot1 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[1:7,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% 
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})
  
  output$hot2 =  renderRHandsontable({tab = p_meta[8:15,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200, row_highlight = c(1,2)) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})
  
  output$hot3 =  renderRHandsontable({tab = p_meta[16:41,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% 
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})
  
  output$hot4 =  renderRHandsontable({tab = p_meta[42:61,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% 
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})
  
  output$hot5 =  renderRHandsontable({tab = p_meta[62:105,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% 
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})
  
  ####### Download Files ########
  
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
