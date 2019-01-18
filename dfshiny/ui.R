library(shinyjs); library(shinyhelper); library(shinyalert);

options(shiny.maxRequestSize=50*1024^2);

termsofuse <- div(h4('Disclaimer'),p(class='regulartext'
,"This WebApp is provided for free as-is without guarantee of suitability for any 
purpose whatsoever. By uploading a data file to this app you're agreeing to the 
following: 1) you have sole responsibility for insuring that you are permitted 
by law and your institution's policies to process your data through this app;
2) that the author or deployer of this app may track and analyze your usage 
patterns in order to improve the usability of this app; and 3) that you will 
hold the author and deployer of this app harmless in the event of any adverse 
consequences of your use of it.")
,h4('Instructions'),p(class='regulartext',"DataFinisher is part of a pipeline for 
getting data from an i2b2 query into a standardized tabular form compatible with
Excel, SAS, R, and almost any other software. The overall pipeline looks like 
this:",br(),img(class='bigimg',src='docs/i2b2_db_df.png'),br(),"The KUMC-developed DataBuilder
app extracts the visits selected by an i2b2 query along with user-specified 
data elements as a SQLite database file that is like a miniature version of the 
i2b2 database containing only the requested patients, visits, and observations. 
The job of ",HTML('Data<i>Finisher</i>')," is to 'finish the job' by taking this 
file and turning it into a plain-text (comma-delimited, tab-delimited, etc.) 
spreadsheet where each visit is a row (sorted by patient number and date) and 
each i2b2 data element as a column. Though this is an ordinary spreadsheet, 
there is additional metadata embedded in it that DataFinisher uses to let the 
user change how the data is represented and with what granularity. So you could 
break out the same i2b2 variable into multiple columns, transform data into 
more convenient formats, and filter which observations show up in a given column.
You can even create your own custom rules for transforming data.")
,p("To use DataFinisher, you can upload either a SQLite db file created by 
DataBuilder or a spreadsheet that has been previously created by DataFinisher.
This means that you can keep coming back and amending how your data is 
represented at your convenience without having to submit a new i2b2 
data-extraction request.")
,p(strong("If you do not have a file to try this on, here is some simulated data 
in",a(href='demodata.csv',".csv format",target='_blank'),"and in"
,a(href='demodata.db',"SQLite .db format",target='_blank'))
," (adapted from ",a(href='http://i2b2.org/','https://i2b2.org/'),"). These are
demo datasets that contain no PHI, for you to download and then upload back so 
you can try out this app.")
,h4('About'),p(class='regulartext',"Written by "
,a(href='mailto:bokov@uthscsa.edu',"Alex F. Bokov, Ph.D."),"at "
,a(href='http://deb.uthscsa.edu/services_cird.html'
   ,'Clinical Informatics Research Division'),"of the"
,a(href='http://deb.uthscsa.edu/'
   ,'Department of Epidemiology and Biostatistics'),"of "
,a(href='https:/www.uthscsa.edu/','UTHealth San Antonio')
,"with mentorship from"
,a(href='http://urology.uthscsa.edu/','UTHealth Department of Urology')
," and ",a(href='https://www.partners.org','Partners Healthcare'),". This work
was made possible by support from:"
,a(href='https://iims.uthscsa.edu/'
   ,'Institute for Integration of Medicine and Science'),"; the "
,a(href='http://som.uthscsa.edu/DeansOffice/DeansOffice.asp'
,"Long School of Medicine")," KL2 Award; NIH/NCATS: UL1TR001120; and PCORI CDRN: 
1306-04631 & 1501-26643.")
,p(class='regulartext'
,"The latest version of this open source software is freely available from the "
,a(href='https://github.com/bokov/datafinisher_webapp/'
   ,'datafinisher_webapp repository on GitHub'),". If/when you deploy 
DataFinisher at your i2b2 site, I suggest running this web-app inside your own 
firewall, and you definitely should not under any circumstances use this public 
instance with data that contains HIPAA identifiers (there are no geniuine 
identifiers in the demo data provided here)")
);



shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css')
  ,useShinyjs()
  ,useShinyalert()
  ,fluidRow(
     column(1,img(src='sitelogo_color.png',width='45px'))
    ,column(2,h3("Datafinisher WebApp",id='apptitle'))
    ,column(4
            ,fileInput("infile", div("Choose a CSV file to upload"
                                     )
                        ,multiple = FALSE,width = '400px'
                        ,accept = c("text/csv","text/tsv",".csv",".tsv"
                                    ,".tab",".txt",".db"
                                    ,"application/x-sqlite3"
                                    ,"text/tab-separated-values,text/plain"
                                    ,"text/comma-separated-values,text/plain")
                       )
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
      ,div(id='termsofuse',termsofuse)
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
                  ,span(id='WIP_name',icon('question-circle'))
                  ,hr()
                  # description
                  ,div(id='customTrDescGrp'
                       ,textAreaInput('customTrDesc'
                                 ,'Please write a brief description of what you
                                   intend for your custom column transformation
                                   to be (required)',width='80vw')
                  )
                  ,span(id='WIP_desc',icon('question-circle'))
                  ,hr()
                  # multi-select columns
                  ,uiOutput('customWhichCols')
                  ,span(id='WIP_cols',icon('question-circle'))
                  ,hr()
                  # fields to return
                  ,hidden(div(id='customWhichFieldsGrp'
                              ,uiOutput('customWhichFields')
                              ,span(id='WIP_fields',icon('question-circle'))
                              ,hr()
                              ))
                  # aggregation
                  ,hidden(div(id='customAggregateGrp'
                              ,selectInput('customAggregate'
                                           ,'If multiple values exist for the same visit, how do you wish to aggregate them?'
                                           ,selectize = F,choices='')
                          ,span(id='WIP_agg',icon('question-circle')),hr()
                          )
                  )
                  
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
                       ,span(id='WIP_qb',icon('question-circle'))
                       #,'Placeholder Text'
                       ,queryBuilderOutput('qbtest',height = '100%')
                       )
                  # save
                  ,hr()
                  ,span(id='WIP_save',icon('question-circle')),HTML('&nbsp;')
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
        ,tabPanel(span('Information',br(),HTML('&nbsp;')),id='tInfo',termsofuse)
        )
    # end tabsetpanel ----
       ))
))
