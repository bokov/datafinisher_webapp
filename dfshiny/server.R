library(shiny); library(shinyjqui); library(bsplus);
library(reticulate); library(readr);

colInfoBox <- function(incolname,incols){
  #' incols:    reactivevalues sub-object that is a python DFCol
  #'            object
  #' incolname: char
  incolinfo <- if('name' %in% names(incols[[incolname]]$colmeta)){
    incols[[incolname]]$colmeta$name } else 'static column';
  incoldivid <- paste0('cbg_',incolname);
  if(length(incols[[incolname]]$rules)>0){
    incolvals <- names(incols[[incolname]]$rules);
    incolchoices <- lapply(incolvals,function(ii) {
      with(incols[[incolname]]$rules[[ii]]
           ,span(ii,br(),span(ruledesc,class='annotation')
                 ,class=if(suggested) 'rulenamesugg' else 'rulename'))});
    cbg <- checkboxGroupInput(incoldivid,'Available Represntations:'
                              ,choiceNames = incolchoices
                              ,choiceValues = incolvals);
    cls <- 'colinfodiv'} else {
      cbg <- span();cls='colinfodiv_static';
    }
    div(incolname,br(),span(incolinfo,class='annotation')
      ,cbg,class=cls);
}

shinyServer(function(input, output, session) {
  
  #py$sys$path <- c(py$sys$path,paste0(getwd(),'/datafinisher'));
  source_python('df_reticulate.py');
  
  rv <- reactiveValues(uitest=div(
     orderInput('source', 'Source'
                ,items = factor(sample(month.abb,15,rep=T))
                ,as_source = TRUE, connect = 'dest')
    ,orderInput('dest', 'Dest', items = NULL
               , placeholder = 'Drag items here...')
    #verbatimTextOutput('order'))
    ));
  
  rvp <- reactiveValues();
  
  # Renders a sample of the uploaded data and as a 
  # side effect creates the UI for manipulating it.
  # TODO: Find a way to trigger the processing without
  #       having to click this tab.
  output$tb_infile_prev <- renderDataTable({
    # Don't attempt to produce output until file exists
    req(input$infile);
    # Peek at the file type. If it's CSV, use read_csv()
    if(input$infile$type == 'text/csv'){
      dat <- read_csv(input$infile$datapath)
    } else {
      # If not csv, assume tab-delimited and *try* 
      # read_tsv()
      dat <- try(read_tsv(input$infile$datapath))
      # if there was an error or there is only one
      # column in the result, assume we guessed wrong
      # and fail over to using read_csv() after all
      # TODO: more general guessing of delimiters or
      #       optional user-supplied delimiters
      if(is(dat,'try-error')||ncol(dat)==1){
        dat <- read_csv(input$infile$datapath);
      };
    }
    
    # Now we have a sample data-file!
    rvp$dfmeta <- py$DFMeta(names(dat),as.character(dat[1,])
                            ,suggestions=py$autosuggestor);
    # create column controls
    infodivs <- bs_accordion('infodivs');
    for(ii in rvp$dfmeta$inhead){
      infodivs <- bs_append(infodivs,ii
                            ,colInfoBox(ii,rvp$dfmeta$incols));
    }
    rv$ui_transform <- div(infodivs,id='infodivs_parent',onclick="
function(){ Shiny.onInputChange('selectedinfodiv',document.getElementsByClassName('in')[0].getElementsByClassName('shiny-input-container')[0].id)}
                           ");
    # infodivs <- sapply(rvp$dfmeta$inhead
    #                     ,colInfoBox,rvp$dfmeta$incols
    #                     ,simplify = F);
    # return a preview of the input
    return(head(dat[-1,],200));
  });
  
  output[['tb_transform']] <- renderUI({rv$ui_transform});
  
  observeEvent(input$debug,{
    req(rvp$dfmeta);
    # rv$tv <- list()
    # rv$tv$testname <- rvp$dfmeta$inhead[42];
    # rv$tv$testcol <- rvp$dfmeta$incols[[rv$tv$testname]];
    # rv$tv$testrules <- rv$tv$testcol$rules; 
    # rv$tv$testrulesinfo <- sapply(
    #   names(rv$tv$testrules), function(xx) {
    #     with(rv$tv$testrules[[xx]]
    #          ,list(id=sprintf(gsub('^\\{0\\}','%s'
    #                                ,extractors[[1]][2])
    #                           ,rv$tv$testname)
    #                ,descr=ruledesc,suggested=suggested))
    #     },simplify=F)
    # rv$tv$testrulesdivs <- lapply(
    #   rv$tv$testrulesinfo,function(xx) {
    #     with(xx
    #          ,jqui_draggable(div(id=paste0('cb_',id),id,br()
    #                              ,span(descr,class='annotation')
    #                              ,class='btn btn-default')
    #                          ,options=list(scope='dest')))
    #     });
    # 
    # rv$tv$workingui <- div(
    #   orderInput('source', 'Source'
    #              ,items = sample(month.abb,15,rep=T)
    #              ,as_source = TRUE, connect = 'dest')
    #   ,orderInput('dest', 'Dest', items = NULL
    #               , placeholder = 'Drag items here...'));
      
    browser();
    # rv$uitest <- with(rv$tv
    #                   ,div(
    #   # orderInput('source', 'Source'
    #   #            ,items = testrulesdivs
    #   #            ,as_source = TRUE, connect = 'dest')
    #   div(testrulesdivs,id='FOO')
    #   # ,jqui_droppable(div('Drag items here...')
    #   #                 ,options=list(scope='dest'))
    #   ,jqui_sortable(div('Drag items here...',id='dest')
    #                  ,options=list(scope='dest'))
    #   # ,orderInput('dest', 'Dest', items = NULL
    #   #             , placeholder = 'Drag items here...')
    #   )
    #   );
  });
  
  output$test <- renderUI({rv$uitest});

})