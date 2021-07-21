### BIO-SHARE
# An R shiny app to accompany the BIO-MATE product. Allows users to reformat their own data and submit to Git for addition to new versions of the data product
#
# app.R has 3 components
# 1. user interface object
# 2. server function
# 3. call to the shinyApp

library(shiny)

# 1. user interface object
ui <- fluidPage(
  titlePanel("BIO-SHARE your data to BIO-MATE!")
  
  
  
)

# 2.server function
server <- function(input, output){
  
}

# 3. call to shinyApp