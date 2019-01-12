library(shinyjs);

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
,h4('Instructions'),p(class='regulartext',"This web app is designed to 
post-process i2b2 extracts produced by "
,a(href='https://github.com/UTHSCSA-CIRD/datafinisher','DataFinisher')
,".",strong("Here is some ",a(href='demodata.csv',"simulated data"
                              ,target='_blank'))
," (adapted from ",a(href='http://i2b2.org/','https://i2b2.org/'),") "
,"for you to download and then upload back so you can try out DataFinisher")
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
firewall, and you definitely should not use under any circumstances use this 
public instance if your data contains HIPAA identifiers")
);

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css')
  ,useShinyjs()
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
    ,if(!shinyapps) {
      column(1,actionButton('debug','Debug'),id='debug_button')} else c()
  )
  # ,actionButton('btDumpcols','Column Info')
  # ,div(id='debugval','Waiting for debug value...')
  ,mainPanel(width=12,id='maintabs'
    # tabsetPanel ----
      ,div(id='termsofuse',termsofuse)
      ,hidden(tabsetPanel(
        tabPanel(span('Input',br(),'Data')
                 ,conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   div("Reading your file, please wait..."
                                       ,id="loading_message"))
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
    # end tabsetpanel ----
       ))
))
