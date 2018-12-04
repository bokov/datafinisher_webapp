library(shiny);library(shinyjs);

options(shiny.maxRequestSize=50*1024^2);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Datafinisher WebApp"),
  
  # Sidebar 
  sidebarLayout(
    
    sidebarPanel(
      
       fileInput("infile", "Choose CSV File"
                ,multiple = FALSE
                ,accept = c("text/csv","text/tsv",".csv",".tsv"
                            ,".tab",".txt"
                            ,"text/tab-separated-values,text/plain"
                            ,"text/comma-separated-values,text/plain"
                            ))
      ,actionButton('debug','Debug')
      ),

    # Preview
    mainPanel(
      tabsetPanel(
         tabPanel('Transform Data',uiOutput('test'))
        ,tabPanel('Input Data',dataTableOutput('infile_prev'))
        )
      )
    )
))
