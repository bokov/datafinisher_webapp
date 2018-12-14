library(shiny);library(shinyjs);library(queryBuilder);

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
                ,multiple = FALSE,width = '200px'
                ,accept = c("text/csv","text/tsv",".csv",".tsv"
                            ,".tab",".txt"
                            ,"text/tab-separated-values,text/plain"
                            ,"text/comma-separated-values,text/plain"
                            ))
      ,actionButton('debug','Debug')
      ,div(id='debugval','Waiting for debug value...')
      # ,disabled(
      #   # div(class='panel panel-primary',id='makecustom'
      #   #             ,div(id='makecustom-heading'
      #   #                  ,class='panel-heading collapsed',role='tab'
      #   #                  ,`data-toggle`='collapse',`data-target`=)
      #   tags$button(id='makecustom'
      #               ,'Define custom filter for ',br()
      #               ,span(class='activecolidtxt'
      #                     ,'(none selected)')))
      # ,hidden(div(id='customui'
      #             ,"Custom filter for column "
      #             ,span(class='activecolidtxt','(none selected)')
      #             ,queryBuilderOutput('qbtest')
      #             ))
      )

    # Preview
    ,mainPanel(
      tabsetPanel(
         tabPanel(span('Debug',br())
                  ,uiOutput('test')
                  #,queryBuilderOutput('qbtest')
                  )
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
        ,tabPanel(span('Input',br(),'Data')
                  ,dataTableOutput('tb_infile_prev'))
        )
      )
    )
#   ,tags$script("
# document.getElementById('infodivs').onclick = function(){
# Shiny.onInputChange('selectedinfodiv',document.getElementsByClassName('in')[0].getElementsByClassName('shiny-input-container')[0].id)
# }
#               ")
))
