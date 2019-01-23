library(shinyjs); library(shinyalert);

options(shiny.maxRequestSize=50*1024^2);


shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css')
  ,useShinyjs()
  ,useShinyalert()
  ,fluidRow(
     column(1,img(src='sitelogo_color.png',width='45px'))
    ,column(2,h3("Datafinisher WebApp",id='apptitle'))
    ,column(4
            ,hidden(fileInput("infile", div("Choose a CSV file to upload"
                                     )
                        ,multiple = FALSE,width = '400px'
                        ,accept = c("text/csv","text/tsv",".csv",".tsv"
                                    ,".tab",".txt",".db"
                                    ,"application/x-sqlite3"
                                    ,"text/tab-separated-values,text/plain"
                                    ,"text/comma-separated-values,text/plain")
                       ))
            ,id='infile_ui')
    ,column(1,id='helpDebug'
            ,span(id='hInfile',icon('question-circle'))
            ,if(!shinyapps) actionButton('debug','Debug') else c()
            )
  )
  # ,actionButton('btDumpcols','Column Info')
  # ,div(id='debugval','Waiting for debug value...')
  ,mainPanel(width=12,id='maintabs'
    # tabsetPanel ----
      ,div(id='termsofuse', helptext$hMainInfo,class='regulartext')
      ,hidden(tabsetPanel(
        tabPanel(span(id='tInputData','Input',br(),'Data'
                      ,span(id='hInputData',icon('question-circle')))
                 ,conditionalPanel(condition="
                  $('html').hasClass('shiny-busy') && 
                  $('.nav-tabs>.active>a>span')[0].id=='tInputData'",
                                   div("Reading your file, please wait..."
                                       ,id="loading_message_in"))
                 #,helper(strong(),type='inline',content=help_tinput,colour=hcol)
                 ,dataTableOutput('tb_infile_prev'))
        ,tabPanel(span(id='tTransform','Transform',br(),'Columns'
                       ,span(id='hTransform',icon('question-circle')))
                  #,helper(strong(),type='inline',content=help_ttransform,colour=hcol)
                  ,uiOutput('tb_transform'))
        ,tabPanel(span(id='tCustomTrans','Customize',br(),'Transforms'
                       ,span(id='hCustomTrans',icon('question-circle')))
                  # transform name
                  ,textInput('customTrName'
                             ,'Choose a name for your transformation'
                             ,'custom',width='80vw')
                  ,span(id='hCustomTrName',icon('question-circle'))
                  ,hr()
                  # description
                  ,div(id='customTrDescGrp'
                       ,textAreaInput('customTrDesc'
                                 ,'Please write a brief description of what you
                                   intend for your custom column transformation
                                   to be (required)',width='80vw')
                  )
                  ,span(id='hCustomTrDesc',icon('question-circle'))
                  ,hr()
                  # multi-select columns
                  ,uiOutput('customWhichCols')
                  ,span(id='hCustomWhichCols',icon('question-circle'))
                  ,hr()
                  # fields to return
                  ,hidden(div(id='customWhichFieldsGrp'
                              ,uiOutput('customWhichFields')
                              ,span(id='hCustomWhichFields',icon('question-circle'))
                              ,hr()
                              ))
                  # aggregation
                  ,hidden(div(id='customAggregateGrp'
                              ,selectInput('customAggregate'
                                           ,'If multiple values exist for the same visit, how do you wish to aggregate them?'
                                           ,selectize = F,choices='')
                          ,span(id='hCustomAggregate',icon('question-circle')),hr()
                          )
                  )
                  
                  # qb widget
                  ,hidden(a(id='customQBHead'
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
                       ,span(id='hCustomQBHead',icon('question-circle'))
                       #,'Placeholder Text'
                       ,queryBuilderOutput('qbtest',height = '100%')
                       )
                  # save
                  ,hr()
                  ,span(id='hCustomSave',icon('question-circle')),HTML('&nbsp;')
                  ,disabled(actionButton('customSave','Save'))
                  # cancel
                  ,actionButton('customCancel','Cancel')
                  )
        ,tabPanel(span('Output',br(),'Preview'
                       ,span(id='hOutput',icon('question-circle'))
                  ,id='tOutPrev')
                  ,conditionalPanel(condition="
                  $('html').hasClass('shiny-busy') && 
                  $('.nav-tabs>.active>a>span')[0].id=='tOutPrev'",
                                    div("Preparing your file, please wait..."
                                        ,id="loading_message_out"))
                  #,helper(strong(),type='inline',content=help_toutput,colour=hcol)
                  ,actionButton('outprev','Generate/Update Preview of Results'
                                ,icon=icon('eye'))
                  ,hidden(actionButton('outwrite','Prepare Results for Download'
                                       ,icon=icon('play')))
                  ,hidden(downloadButton('outdownload','Download Full Results'))
                  ,hr()
                  ,dataTableOutput('tb_outfile_prev'))
        ,tabPanel(span('Information',br(),HTML('&nbsp;')),id='tInfo'
                  ,h4('User Agreement')
                  ,helptext$disclaimer
                  ,helptext$hMainInfo)
        )
    # end tabsetpanel ----
       ))
))
