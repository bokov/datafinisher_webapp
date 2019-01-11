library(shiny);library(shinyjs);library(queryBuilder);

options(shiny.maxRequestSize=50*1024^2);

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  includeCSS('df.css')
  ,useShinyjs()
  
  # Application title
  #,titlePanel("Datafinisher WebApp")
  
  ,fluidRow(
     column(2,h3("Datafinisher WebApp",id='apptitle'))
    ,column(3,fileInput("infile", "Choose CSV File"
            ,multiple = FALSE,width = '200px'
            ,accept = c("text/csv","text/tsv",".csv",".tsv"
                        ,".tab",".txt"
                        ,"text/tab-separated-values,text/plain"
                        ,"text/comma-separated-values,text/plain"
            )),id='infile_ui')
    ,column(1,actionButton('debug','Debug'),id='debug_button')
  )
  # ,actionButton('btDumpcols','Column Info')
  # ,div(id='debugval','Waiting for debug value...')
  ,mainPanel(width=12,id='maintabs'
    # tabsetPanel ----
      ,hidden(tabsetPanel(
        tabPanel(span('Input',br(),'Data')
                  ,dataTableOutput('tb_infile_prev'))
        ,tabPanel(span('Transform',br(),'Columns')
                  ,uiOutput('tb_transform'))
        ,tabPanel(span('Custom',br(),'Transforms')
                  # transform name
                  ,textInput('customTrName'
                             ,'Choose a name for your transformation'
                             ,'custom')
                  # description
                  ,textAreaInput('customTrDesc'
                                 ,'Please write a brief description of what you
                                   intend for your custom column transformation
                                   to be (required)')
                  # multi-select columns
                  ,uiOutput('customWhichCols')
                  # fields to return
                  ,hidden(uiOutput('customWhichFields'))
                  # aggregation
                  ,hidden(selectInput('customAggregate'
                                ,'If multiple values exist for the same visit, how do you wish to aggregate them?',selectize = F
                               ,choices=''))
                  # qb widget
                  ,hidden(a(id='customQBhead'
                     ,class='btn btn-default'
                     ,`data-toggle`='collapse'
                     ,href='#customQB'
                     ,role="button"
                     ,`aria-expanded`="false"
                     ,`aria-controls`="customQB"
                     ,'Advanced filtering options'
                       ))
                  ,div(class='collapse card card-body'
                       ,id='customQB'
                       #,'Placeholder Text'
                       ,queryBuilderOutput('qbtest')
                       )
                  # save
                  ,disabled(actionButton('customSave','Save'))
                  # cancel
                  ,actionButton('customCancel','Cancel')
                  )
        )
       ))
))
