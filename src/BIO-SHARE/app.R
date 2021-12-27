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
library(BIOMATE)

load(url("https://github.com/KimBaldry/BIO-MATE/raw/main/data_descriptor_paper/metadata_info.RData"))
load(url("https://github.com/KimBaldry/BIO-MATE/raw/main/product_data/next_version/package_data/BIOMATE.rda"))
local_out = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/Projects/BIO-MATE/BIO-MATE/product_data/next_version"

p_meta$`User input` = ""
p_meta = p_meta[,c(1:2,5,4:3)]

#To implement:
# add data citation tags
# check existing EXPOCODES
# show citations existing for EXPOCODE
# show available data for EXPOCODE
# integrate uploaded citations to bib object DONE
# integrate uploaded method citations to bib object
# when running biomate upload metadata to github including new_source and new_method info and new bib objects
# run split file code




# 1. user interface object
ui <- navbarPage(
 "BIO-SHARE",
 ################# Upload published data ###########################
  tabPanel("Upload published data", 
           fluidPage(
             p("Upload published data files. BIO-SHARE can only handle processing one data stream at a time."),
             fileInput("upload", NULL, accept = c(".csv", ".tsv",".tab",".nc",".all",".sb",".txt"),multiple = T), 
             conditionalPanel(condition = "!is.null(output.error1)",textOutput("error1")),
             #selectInput("run_expocode", label = "EXPOCODE", choices(c("new", ))) # reference to existing expocodes in package metadata
             selectInput("stream", label = "Data stream to ingest", choices = c("PROF", "PIG")),
             p("Information is needed to properly cite the data"),
             ## Source Information
             selectInput("source",label = "Source citation", choices = c("new source",source_info$source)), # translate to p_meta
             conditionalPanel(condition = "input.source == 'new source'", 
                              rHandsontableOutput("hot_source")) ,
                        
             checkboxInput("showsource","Show existing BIO-MATE data sources", value = F),
             conditionalPanel(condition = "input.showsource == true", dataTableOutput("source_info_out")),
             ## Method information
             conditionalPanel(condition = "input.stream == 'PIG'", selectInput("method",label = "Method citation", choices = c("new method",method_info$Method))), # translate to p_meta
             conditionalPanel(condition = "input.method == 'new method' ",
                              rHandsontableOutput("hot_method")),

             checkboxInput("showmethod","Show existing BIO-MATE pigment measurement methods"),
             conditionalPanel(condition = "input.showmethod == true", dataTableOutput("method_info_out")),

             fileInput("upload_bibtex", "BibTEX file/s", accept = c(".bib"),multiple = T)
             
             
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

  tabPanel("Run BIOMATE",
           fluidPage(
             actionButton("runBIOMATE", label = "Run BIO-MATE")
           )
           ),
  tabPanel("Upload to GitHub"
           ),
  tabPanel("Download data files", 
           fluidPage(
             downloadButton("download")
           )
           )
  
  
  
  
)

################ 2.server function #########################################################
# example here https://cran.r-project.org/web/packages/rhandsontable/vignettes/intro_rhandsontable.html
col.hi.render = "
    function(instance, td) {
Handsontable.renderers.TextRenderer.apply(this, arguments);
td.style.background = 'lightblue';
}"


server <- function(input, output, session){
  # check existence of EXPOCODE and stream data in the system
  
  
  values <- reactiveValues()
 ############## Upload published data #################### 

  
  observeEvent(input$upload,{
    ext_tmp <- tools::file_ext(input$upload$name)
    
    values$ext <- paste(".",ext_tmp,sep = "")
    values$file_path = unique(dirname(input$upload$datapath))
    output$error1 <- renderText({
      validate(need(all(ext_tmp %in% c("csv", "tsv","tab","nc","all","sb","txt")),"Invalid file; Please upload a supported file: .all, .sb, .csv, .tab, .txt, .nc. Submit an issue to https://github.com/KimBaldry/BIO-MATE if you would like another file format supported in BIO-MATE."))
    })
 })

  
  ## sourceinfomation
  output$source_info_out = renderDataTable(source_info)
  new_source = data.frame("source" = character(), "citations"= character(), "url"= character(), "aknowledgement" = character(), stringsAsFactors = F)
  new_source[1,] =  NA

  observe({
    if(!is.null(input$hot_source)){
      new_source = hot_to_r(input$hot_source)

    } else {
      if (is.null(values[["new_source"]]))
        new_source <- new_source
      else
        new_source <- values[["new_source"]]
    }
    values[["new_source"]] <- new_source
  })

  output$hot_source= renderRHandsontable(rhandsontable(new_source))

  output$source_info = renderDataTable(source_info)


  ## Method informaton
  new_method = data.frame("analysis_type" = character(), "Method"= character(), "citation"= character(), "long_AT" = character(), stringsAsFactors = F)
  new_method[1,] =  NA

  values <- reactiveValues()

  observe({
    if(!is.null(input$hot_method)){
      new_method = hot_to_r(input$hot_method)
    } else {
      if (is.null(values[["new_method"]]))
        new_method <- new_method
      else
        new_method <- values[["new_method"]]
    }
    values[["new_method"]] <- new_method
  })

  output$hot_method = renderRHandsontable(rhandsontable(new_method))

  output$method_info_out = renderDataTable(method_info)


  ### read BibTEX files
  observeEvent(input$upload_bibtex, {

    combined_bib <- ""
  for (path_to_bib_file in input$upload_bibtex) {

    fileCon <- file(path_to_bib_file)
    content <- readLines(fileCon)
    close(fileCon)

    combined_bib <- paste0(combined_bib, "\n", "\n", trimws(paste0(content, collapse="\n")))

  }
  bibpath = unique(dirname(input$upload_bibtex))
  # write temp file
  cat(combined_bib, file=file.path(unique(dirname(input$upload_bibtex)),"BIO-MATE_references.bib"), "\n")
  # create bib object from temp file
  values$new_bib = read.bib(file.path(bibpath,"BIO-MATE_references.bib"))
})


 ############## Split data ######################
  observeEvent(input$split,{
    req(input$upload)
    split_delim_file(dirname(values$file_path), input$upload$name, input$delim, input$line_start, input$expo_split, input$synonym_var_name, input$station_split, input$station_var_name, input$fillcell)
    values$filepaths = file.path(values$filepaths,"split")
    })


  #### Enter Metadata ########
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

    # ## metadata already entered
     if(!is.null(input$upload)){
       p_meta$`User input`[which(p_meta$`Processing metadata variable` == "file_type")] = ifelse(values$ext == "nc", "netcdf", "text delim")
       p_meta$`User input`[which(p_meta$`Processing metadata variable` == "path")] = values$file_path
       p_meta$`User input`[which(p_meta$`Processing metadata variable` == "extention")] = values$ext
     }

    if(!is.null(input$hot_source)){
      p_meta$`User input`[which(p_meta$`Processing metadata variable` == "source")] = values$new_source$source
    }
    if(!is.null(input$hot_method)){
      p_meta$`User input`[which(p_meta$`Processing metadata variable` == "Method")] = values$new_method$Method
      p_meta$`User input`[which(p_meta$`Processing metadata variable` == "analysis_type")] = values$new_method$analysis_type
    }


    values[["p_meta"]] <- p_meta
  })

  # possible selection options for fixed when have time later. Including check on EXPOCODE
  output$hot1 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[1:7,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})

  output$hot2 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[8:15,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200, row_highlight = c(1,2)) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>% hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})

  output$hot3 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[16:41,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})

  output$hot4 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[42:61,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})

  output$hot5 =  renderRHandsontable({
    p_meta = values[["p_meta"]]
    tab = p_meta[62:105,] %>% filter(`Data stream/s` == "all" | grepl(input$stream, `Data stream/s`, ignore.case = T))
    rhandsontable(tab, width = 1200) %>% hot_col(c(1:2,4:5), readOnly = TRUE) %>% hot_table(highlightRow = TRUE) %>%  hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(colWidths = c(200, 100, 200, 250, 400)) %>% hot_col("User input", renderer = col.hi.render)})

  ####### Run BIO-MATE ##########
  observeEvent(input$runBIOMATE, {
    req(input$stream, input$upload)
    process_meta = t(values$p_meta[,3])
    colnames(process_meta) = values$p_meta[,1]
    out_dir = file.path(values$file_path,"meta")
    if(!file.exists(out_dir))
    {dir.create(out_dir)}
    write.csv(process_meta, file.path(out_dir, paste(input$stream, "_meta.csv", sep = "")))
    if(input$stream == "PROF"){
      BIOMATE::PROF_to_WHPE(outdir,file.path(local_out,"reformatted_data" ))
    }
    if(input$stream == "PIG"){BIOMATE::PIG_to_WHPE(outdir,file.path(local_out,"reformatted_data" ))}

  })
  
  # WHEN WRITING new data TO GITHUB
  ## add new source and method info to data 
  
  ## add new citations to bib
  
  ## save processing metadata
  
  ## save reformatted files
  
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

