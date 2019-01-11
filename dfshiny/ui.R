library(shiny);library(shinyjs);library(queryBuilder);

options(shiny.maxRequestSize=50*1024^2);

termsofuse <- div(h4('Disclaimer'),"
This WebApp is provided for free as-is without guarantee of suitability for any 
purpose whatsoever. By uploading a data file to this app you're agreeing to the 
following: 1) you have sole responsibility for insuring that you are permitted 
by law and your institution's policies to process your data through this app;
2) that the author or deployer of this app may track and analyze your usage 
patterns in order to improve the usability of this app; and 3) that you will 
hold the author and deployer of this app harmless in the event of any adverse 
consequences of your use of it.");

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css')
  ,useShinyjs()
  
  # Application title
  #,titlePanel("Datafinisher WebApp")
  
  ,fluidRow(
     column(1,img(src='sitelogo_color.png',width='45px'))
    ,column(2,h3("Datafinisher WebApp",id='apptitle'))
    ,column(4,fileInput("infile", "Choose a CSV file to upload"
            ,multiple = FALSE,width = '400px'
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
      ,div(id='termsofuse',termsofuse)
      ,hidden(tabsetPanel(
        tabPanel(span('Input',br(),'Data')
                  ,dataTableOutput('tb_infile_prev'))
        ,tabPanel(span('Transform',br(),'Columns')
                  ,uiOutput('tb_transform'))
        ,tabPanel(span('Customize',br(),'Transforms')
                  # transform name
                  ,textInput('customTrName'
                             ,'Choose a name for your transformation'
                             ,'custom',width='80vh')
                  # description
                  ,textAreaInput('customTrDesc'
                                 ,'Please write a brief description of what you
                                   intend for your custom column transformation
                                   to be (required)',width='80vh')
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
        ,tabPanel(span('Terms of',br(),'Use'),termsofuse)
        )
       ))
))
