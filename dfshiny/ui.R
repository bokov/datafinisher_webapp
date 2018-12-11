library(shiny);library(shinyjs);

options(shiny.maxRequestSize=50*1024^2);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs()
  
  # Application title
  ,titlePanel("Datafinisher WebApp")
  
  # Sidebar 
  ,sidebarLayout(
    
    sidebarPanel(width=3
      
      ,fileInput("infile", "Choose CSV File"
                ,multiple = FALSE,width = '200px',
                ,accept = c("text/csv","text/tsv",".csv",".tsv"
                            ,".tab",".txt"
                            ,"text/tab-separated-values,text/plain"
                            ,"text/comma-separated-values,text/plain"
                            ))
      ,actionButton('debug','Debug')
      )

    # Preview
    ,mainPanel(
      tabsetPanel(
         tabPanel('Debug'
                  ,uiOutput('test')
                  ,queryBuilderOutput('qbtest'))
        ,tabPanel('Transform Data'
                  ,uiOutput('tb_transform'))
        ,tabPanel('Input Data',dataTableOutput('tb_infile_prev'))
        )
      )
    )
#   ,tags$script("
# document.getElementById('infodivs').onclick = function(){
# Shiny.onInputChange('selectedinfodiv',document.getElementsByClassName('in')[0].getElementsByClassName('shiny-input-container')[0].id)
# }
#               ")
))
