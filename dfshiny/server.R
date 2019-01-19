library(bsplus); library(reticulate); library(readr); library(shinyjqui);

# reminder: the interactive debugger for reticulate is repl_python 

# load various useful stuff
source('templates.R');
source('www/docs/helptext.R')

dfLogger <- function(dfmeta,...){
  do.call(rbind,lapply(dfmeta$errlog,function(yy){
    setNames(data.frame(sapply(yy,function(xx){
      c(xx,NA)[1];
      },simplify=F)),c('errno','row','errcode','errmsg','incol','outcol'));
  }));
  };

dumpOutputCols <- function(id='dumpcols',input=input,rv=rv,...){
  js <- sprintf("
  oo = {};
  $('[id^=chosen-]').map(function(){kk=this.id.replace('chosen-','');
  vv=$(this).find('div').map(function(){return this.id}).get(); 
  if(vv.length>0) oo[kk]=vv;});
  Shiny.onInputChange('%s',oo);
        ",id);
  runjs(js); 
  cat('\n*** dumping column info ***\n');
  warn('dumpOutputCols is disabled pending rewrite to no longer use dfinfolist')
  # sapply(names(input$dumpcols),function(ii) {
  #   rv$dfinfolist[[ii]]$chosen[unlist(input$dumpcols[[ii]])]
  #   },simplify=F);
  # cat(jsonlite::toJSON(rv$dumpcols,pretty=1)
  #     ,file=paste0('dumpcols_',as.numeric(Sys.time()),'.json'));
}

# attach a proper index to a sortable input
sortableWatcher<-function(targetid,inputid
                          ,selector='auto',event='sortupdate'){
  # if targetid has no leading hash, add one
  if(!grepl('^#',targetid)) targetid <- paste0('#',targetid);
  # if inputid has a leading hash remove it
  if(missing(inputid)) inputid <- targetid;
  inputid <- gsub('^#','',inputid);
  if(missing(selector)) selector <- paste0(targetid,'>div');
  js <- sprintf("
  $('%1$s').sortable().on('%2$s',function(event,ui){
    Shiny.onInputChange('uigroup','%1$s');
    Shiny.onInputChange('%3$s', $('%4$s').map(function(){
                return this.id}).get())})"
                ,targetid,event,inputid,selector);
  #print(js);
  runjs(js);
}

addChosen <- function(incolid,availableid,userArgs=list(),input,...){
  obj <- py$dfmeta[incolid];
  # add object. If it already exists in the python backend, it 
  # will overwrite/update the existing values and return NULL 
  # If it doesn't already exist, it will return a list with all
  # the values needed to create the HTML below
  
  # First, obtain the user input if it exists
  # TODO: disable the accompanying Add/Update button if selectize is empty
  if(missing(userArgs)){
    userArgs <- list(CC=input[[obj$rules[[availableid]]$selid]])};
  
  objinfo <- obj$prepChosen(obj$rules[[availableid]],userArgs=userArgs);
  if(!is.null(objinfo)){
    insertUI(paste0('#',objinfo$divIDchosen),where='beforeEnd',immediate=T
             ,ui=withHtmlTemplate(objinfo,templates$divchosen
                                  # create a button specifically for removing
                                  # the HTML being created here
                                  ,delbutton=actionButton(objinfo$delbid
                                                          ,'Remove'
                                                          ,class='btn-danger'))
    );
    runjs(sprintf("$('#%s').trigger('sortupdate')",objinfo$divIDchosen));
    onclick(objinfo$delbid
            ,removeChosen(objinfo$parent_name,objinfo$longname,rv));
  }
}

removeChosen <- function(incolid,finalid,...){
  finalid <- gsub('^#','',finalid);
  objinfo <- py$dfmeta[incolid]$chosen[[finalid]];
  py$dfmeta[incolid]$unprepChosen(finalid);
  removeUI(paste0('#',objinfo$longname),immediate = T);
  runjs(sprintf("$('#%s').trigger('sortupdate')",objinfo$divIDchosen));
}

withHtmlTemplate <- function(env,template,...){
  env <- c(env,list(...),text_=template);
  do.call(htmlTemplate,env);
}

# create the starting UI elements for an incol 
# (calls buildRule for individual available rules)
buildDFCols <- function(incolid,rulenames=T){
  # apparently py magically just shows up in scope without being passed
  obj <- py$dfmeta[incolid]$getDict();
  if(missing(rulenames)) myrules <- obj$rules else {
    myrules <- try(obj$rules[rulenames])
    if(class(myrules)[0] == 'try-error'){
      print('Uh oh, list subsetting problem');
      browser();}}
  out0 <- lapply(myrules,buildRule,unique_codes=obj$unique_codes);
  out1 <- withHtmlTemplate(obj,templates$multidivavailable,innerDivs=out0);
  out2 <- if(obj$as_is_col) span() else{
    withHtmlTemplate(obj,templates$incolui,divavailable=out1)};
  out3 <- withHtmlTemplate(obj,templates$divfull,incolui=out2);
  if(!obj$as_is_col){
    jqui_sortable(ui=paste0('#',obj$divIDchosen)
                  ,options=list(axis='y',items='div'))};
  out3;
}

# Build an 'available' div for a single rule
buildRule <- function(rule,unique_codes,rulename,incolid
                      ,selclass='transform-argsel'
                      ,sellab='For the following codes:'
                      ,template=templates$divavailable){
  if(missing(rule)) rule <- py$dfmeta[incolid]$rules[rulename];
  if(missing(unique_codes)) {
    unique_codes <- py$dfmeta[rule$parent_name]$unique_codes;}
  rsel <- if(rule$split_by_code && length(unique_codes)>1){
    div(class=selclass,selectizeInput(rule$selid,multiple=T,label=sellab
                                      ,choices=unique_codes))} else span();
  withHtmlTemplate(rule,template,xxsel=rsel)
}

#' validNames: make sure a name is unique and has legal characters
validNames <- function(newname #,existingnames=c()
                       ,id,session=getDefaultReactiveDomain()){
  outname <- py$dfmeta$makeNameUnq(newname,'rulename',maxlen=int(12))
  if(!is.null(id)) updateTextInput(session,id,value=outname);
  return(outname);
}    

# shinyServer ----
shinyServer(function(input, output, session) {
  # server init ----
  # load the stuff we need from datafinisher
  observe_helpers(help_dir = 'www/docs');
  cat('\n*** starting server init ***\n');
  source_python('df_reticulate.py');
  runjs("$('#customQB').on('hidden.bs.collapse',function(){
        Shiny.onInputChange('qbtest_out',null);
        Shiny.onInputChange('qbtest_validate',true);
      })");
  
  rv <- reactiveValues();
    # uitest=div(
    #  orderInput('source', 'Source'
    #             ,items = factor(sample(month.abb,15,rep=T))
    #             ,as_source = TRUE, connect = 'dest')
    # ,orderInput('dest', 'Dest', items = NULL
    #            , placeholder = 'Drag items here...')
    # );
  
  # obtain either a pre-existing file or uploaded by user ----
  observeEvent(input$infile,{
    cat('\n*** checking infile ***\n');
    req(input$infile$datapath);
    rv$infile <- input$infile$datapath;
    rv$infilename <- input$infile$name;});
  
  observeEvent(session$clientData$url_search,{
    message('\n*** checking url_search ***\n');
    if(!is.null(dfile<-parseQueryString(session$clientData$url_search)$dfile)){
      dfile <- file.path(trusted_indir,basename(dfile));
      if(file.exists(dfile)){
        rv$infile <- dfile;
        rv$infilename <- basename(dfile);}
    }
    message('\n*** done checking url_search ***\n');
    });
  
  message('\n*** line 177 ***\n');
  
  # create dfmeta ----
  observeEvent(rv$infile,{
    cat('\n*** creating dfmeta ***\n');
    # put up modal alert
    shinyalert(title='Please wait.',messages$mLoading
               ,closeOnEsc = F,showConfirmButton = F);
    py_run_string(sprintf("dfmeta=DFMeta(fref='%s',suggestions=autosuggestor)"
                          ,rv$infile));
    # Indicator for the rest of the webapp that the core object is ready
    rv$have_dfmeta <- Sys.time();
    message('\n*** dfmeta created ***\n');
    closeAlert();
  });
  
  
  # read input data ----
  # Renders a sample of the uploaded data  
  output$tb_infile_prev <- renderDataTable({
    req(rv$have_dfmeta);
    # TODO: temporary, will create simpler py-side method
    message('\n*** dat loaded ***\n');
    hide('termsofuse');
    show(selector = '#maintabs>.tabbable');
    # remove modal alert
    return(read_delim(paste0(py$dfmeta$sampleInput(nrows = 300)
                             ,collapse='\n'),py$dfmeta$data$dialect$delimiter));
  },options=list(scrollY='50vh',scroller=T,scrollX=T,processing=T
                 ,searching=F
                 ,columns=I(paste0('[',paste0(
                   ifelse(py$dfmeta$inhead %in% py$dfmeta$getDynIDs()
                          ,'{className:"dfDyn"}','null'
                          ),collapse=','),']'))
                 ));

  outputOptions(output,'tb_infile_prev',suspendWhenHidden=F);
  
  # populate the 'Transform Data' tab ----
  output$tb_transform <- renderUI({
    req(rv$have_dfmeta)
    message('\n*** running tb_transform ***\n');
    # create startingdivs ----
    # Just the column divs, as built by buildDFCols
    # This is the slowest step
    rv$dfstartingdivs <- sapply(py$dfmeta$inhead,buildDFCols,simplify=F);
    message('\n*** dfstartingdivs created ***\n');

    # This is the part that detects clicks on the Add/Update buttons
    # Have to wrap the expr argument in substitute because otherwise
    # it doesn't correctly read the ii value. Wierd.
    for(xx in py$dfmeta$getColIDs(asdicts=T
                                  ,ids='incolid'
                                  ,childids=c('rulename','addbid')
                                  ,childtype='rules')){
      eval(substitute(
        onclick(addbid,addChosen(incolid,rulename,rv=rv,input=input),add=T)
        ,env=xx));
    }
    message('\n*** addbid onclicks created ***\n');
    
    # create infodivs (collapsible column controls) ----
    statdivs <- py$dfmeta$getStatIDs();
    infodivs <- bs_accordion('infodivs');
    infodivs <- bs_set_opts(infodivs,use_heading_link=T);
    for(ii in names(rv$dfstartingdivs)){
      isolate({
        if(ii %in% statdivs){
          infodivs <- htmltools::tagAppendChild(
            infodivs
            ,div(class='panel panel-default pn-colstatic dfcol-static'
                 ,div(class='panel-heading',ii)
                 ,div(class='panel-body',rv$dfstartingdivs[[ii]])));
        } else {
          infodivs<-bs_append(infodivs,ii,rv$dfstartingdivs[[ii]])
        }
      });
    }
    runjs("$('.collapse').on('shown.bs.collapse', function (e) {
        Shiny.onInputChange('activecolid',($('.in>.panel-body>div').attr('id')));
    })")
    message('\n*** infodivs created ***\n');
    
    # update outputs ----
    # Populate the ui_transform, needed by the 'Transform Data' panel
    ui_transform <- div(infodivs,id='infodivs_parent');
    message('\n*** ui_transform created ***\n');
    
    runjs("Shiny.onInputChange('choosewait',+ new Date())");
    
    show(selector = '#maintabs>.tabbable');
    ui_transform;
  });
  
  # create help content ----
  lapply(names(helptext),function(ii) {
    message('\n*** loading help buttons ***\n');
    onclick(ii,shinyalert(text = helptext[[ii]],confirmButtonCol = hcol
                          ,className = 'dfHelp'))});

    # set output options so stuff starts rendering before tab active
  outputOptions(output,'tb_transform',suspendWhenHidden=F);

  # Wait for the divIDchosen to load and then make them sortable
  observeEvent(input$choosewait,{
    req(rv$have_dfmeta);
    print('Checking choosewait');
    if(input$choosewait!=0){
      for(ii in unlist(py$dfmeta$getColIDs(ids='divIDchosen'))){
        eval(substitute(sortableWatcher(ii)))};
      print('Running sortableWatcher');
      runjs("
if( $('[id^=c-].ui-sortable').length == 0 ) {
  xx = + new Date() } else {
    /* This is where we track the currenly opened column panel */
    $('.collapse').on('shown.bs.collapse', function (e) {
      activecolid = $('.in>.panel-body>div').attr('id');
      if(activecolid == undefined){activecolid = '(none selected)'};
      $('.activecolidtxt').text(activecolid);
      Shiny.onInputChange('activecolid',activecolid);
    }).on('hide.bs.collapse',function(e){
      activecolid = '(none selected)';
      $('.activecolidtxt').text(activecolid);
      Shiny.onInputChange('activecolid',activecolid);
    });
    xx = 0}; Shiny.onInputChange('choosewait',xx);");};
  });
  
  message('\n*** line 303 ***\n');
  
  # custom rule names ----
  # make sure custom rules have names that are safe, legal, 
  # and unique
  observeEvent(c(input$customTrName,rv$infile,rv$have_dfmeta),{
    req(rv$have_dfmeta);
    message('\n*** checking customTrName ***\n');
    validNames(input$customTrName,id='customTrName');
    });
  
  # offer a choice of columns from which to select ones that will have
  # access to this transform. Only fields available in all these columns
  # will be options in the transform
  output$customWhichCols <- renderUI({
    req(rv$dfstartingdivs);
    message('\n*** updating customWhichCols ***\n');
    if(input$choosewait==0){
      selectizeInput('customSelCols'
                     ,label='Select the main column or columns in your data for
                             which this transformation should be available:'
                     ,choices=sort(unlist(py$dfmeta$getColIDs(ids='incolid')))
                     ,multiple=T)} else {
                       span()}});
  
  # when choice of columns changes, update the permitted list of variables
  observeEvent(c(input$customSelCols,input$customTrDesc),{
    req(rv$have_dfmeta);
    message('\n*** updating custom widgets ***\n');
    applicable <- T; out <- filterlist;
    for(ii in input$customSelCols){
      iimeta <- py$dfmeta[ii]$colmeta
      applicable <- applicable & sapply(filterlist,function(xx) {
        eval(xx$criteria,envir=iimeta)});
      if(!is.null(iimeta$ccd_list)){
        newcodes <- strsplit(iimeta$ccd_list,",")[[1]];
        out$concept_cd$values <- union(out$concept_cd$values,newcodes);
        } else cat('\n',ii,' has no ccd_list\n');
      };
    # They have NULL values not because they lack codes but 
    # them into text instead of selectives
    if(is.null(out$concept_cd$values)){
      out$concept_cd$values <- NULL;
      out$concept_cd$input <- 'text';
    }
    rv$currentFilterlist <- out[applicable];
    validchoices <- sapply(rv$currentFilterlist,`[[`,'name');
    print('Updated rv$currentFilterList');
    ready <- !is.null(input$customTrDesc) && input$customTrDesc != '' &&
      !is.null(input$customSelCols) && input$customSelCols != '';
    if(ready & length(validchoices)==0) {
      showNotification('There are no fields that are shared by all the main
                        columns you selected. Please change your selection.'
                       ,type='error',duration=30);
      ready<-F;
    }
    if(ready) {
      rv$qbtest<- queryBuilder(filters=unname(rv$currentFilterlist)
                               ,allow_empty=T,height = '100%');
      rv$customWhichFieldsReady <- selectizeInput('customSelFields'
                                                  ,label='Select the field or
                                                        fields you wish this
                                                        transformation to 
                                                        return:'
                                                  ,multiple=T
                                                  ,choices=validchoices);
      output$customWhichFields <- renderUI(rv$customWhichFieldsReady);
      show('customWhichFieldsGrp');
      show('customQBhead');
    } else {
      hide('customWhichFieldsGrp');
      runjs("$('#customQB').collapse('hide')");
      hide('customQBhead');
      hide('customAggregateGrp');
    }
  });

  # re/create the querybuilder UI
  output$qbtest <- renderQueryBuilder({
    message('\n*** rendering qbtest ***\n');
    print('rendering qbtest');
    runjs("Shiny.onInputChange('qbinit',+ new Date())");
    rv$qbtest});
  
  # fix the labels on queryBuilder
  observeEvent(input$qbinit,{
    cat('\n*** Attempting to update QB filter labels ***\n');
    for(xx in rv$currentFilterlist) {
      runjs(sprintf("
      $('#qbtest').data('queryBuilder').filters.filter(function(el){
              return el.id=='%s';})[0].label = '%s';"
              ,xx$name,xx$label))
    }
    runjs("$('#qbtest').queryBuilder('reset')");
    });
  
  # update aggregator selection widget
  observeEvent(c(input$customSelFields,input$customTrDesc
                 ,input$qbtest_validate)
               ,{
                 message('\n*** checking if custom widgets ready ***\n');
                 ready <- !is.null(input$customSelFields) &&
                   input$customSelFields != '' &&
                   !is.null(input$customTrDesc) &&
                   input$customTrDesc != '' &&
                   !is.null(input$customSelCols) &&
                   input$customSelCols != '';
                 if(ready){
                   # determine if numeric aggregation possible
                   numagg <- length(input$customSelFields)==1 &&
                     input$customSelFields %in% grep('_num$'
                                                     ,names(filterlist),val=T);
                   # make list of choices
                   choices<- list(
                     `Last non-missing value`='last'
                     ,`First non-missing value`='first'
                     ,`Minimal value (by lexical order if text)`='min'
                     ,`Maximal value (by lexical order if text)`='max'
                     ,`Any non missing values? (T/F)`='any'
                     ,`Concatenate together all unique non-missing values`='concatunique'
                     );
                   if(numagg) choices <- c(choices,list(Average='mean'
                                                        ,Median='median'));
                   # update customAggregate choices
                   updateSelectInput(session,'customAggregate'
                                     ,label='If there are more than one result
                                             for the same visit, how do you wish
                                             to aggregate them?'
                                     ,choices=choices);
                   show('customAggregateGrp');
                   if(is.null(input$qbtest_validate)||input$qbtest_validate) {
                     enable('customSave')
                     } else disable('customSave');
                   } else {
                     hide('customAggregateGrp');
                     disable('customSave');
                   }
                   });
  # If cancel pressed on custom rule tab, reset to empty values
  observeEvent(input$customCancel,{
    updateSelectizeInput(session,'customSelFields',selected=character(0));
    updateSelectizeInput(session,'customSelCols',selected=character(0));
    updateTextAreaInput(session,'customTrDesc',value='');
    validNames('custom',id='customTrName');
  });
  
  # When the save button is pressed on Custom Transforms tab
  observeEvent(input$customSave,{
    # customSave ----
    message('\n*** customSave pressed ***\n');
    rulename <- validNames(input$customTrName,id='customTrName');
    selector <- if(is.null(input$qbtest_out)||length(input$qbtest_out$rules)==0){
      "ALL";
    } else input$qbtest_out;
    newrule <- list(
      rulename=rulename
      ,ruledesc=input$customTrDesc
      ,split_by_code=F
      ,selector=selector
      ,fieldlist=input$customSelFields
      ,aggregator=input$customAggregate
      ,custom=T
    );
    # Add the newly created rules to dfmeta and turn the output (captured 
    # inline as newinfo) into HTML (the newdivs final output in this chain)
    newdivs <- lapply(newinfo<-py$dfmeta$userDesignedRule(
      newrule,rulename,as.list(input$customSelCols))
      ,buildRule,unique_codes=c());
    rv$newdivs <- newdivs; rv$newinfo <- newinfo;
  });
  
  message('\n*** line 476 ***\n');
  
  # Populate the new divs, new info created in Custom Transforms
  observeEvent(c(rv$newdivs,rv$newinfo),{
    req(rv$have_dfmeta);
    message('\n*** making custom rules available ***\n');
    for(ii in names(rv$newinfo)){
      # insert the new div
      insertUI(paste0('#',py$dfmeta[ii]$divIDavailable,'>div')
               ,'beforeEnd',rv$newdivs[[ii]],immediate = T);
      # add onclick event to allow actually adding/removing this rule
      eval(substitute(onclick(addbid
                              ,addChosen(parent_name,rulename,rv=rv,input=input)
                              ,add=T),env=rv$newinfo[[ii]]));
    };
    # empty out description field and selectizeinput fields
    runjs("$('#customCancel').click()");
  });
  
  # output preview! ----
  observeEvent(input$outprev,{
    req(rv$have_dfmeta);
    message('\n*** preparing output preview ***\n');
    hide('outdownload');hide('outwrite');
    # get the output column order
    chsnames <- sapply(py$dfmeta$getColIDs(ids=c('divIDchosen','incolid'))
                      ,function(xx) setNames(list(input[[xx[1]]]),xx[2]));
    # TODO: fix this so that Python catches null entries, including where all
    # are null, so that the web front end doesn't need to
    chsnames <- chsnames[!sapply(chsnames,is.null)];
    #.dbg <- try(
    py$dfmeta$finalizeChosen(chsnames);
    #);
    #if(class(.dbg)[1]=='try-error') browser();
    tempname <- py$dfmeta$processRows(tempfile(),nrows=300);
    #py$dfmeta$fhandle$seek(0); py$dfmeta$nrows = 3;
    output$tb_outfile_prev <- renderDataTable(
      rv$testout <- read_delim(tempname
                               # TODO: wtf is the second row broken?
                               ,delim=py$dfmeta$data$dialect$delimiter)[-2,]
      ,options=list(scrollY='50vh',scroller=T,scrollX=T,processing=T
                    ,searching=F
                    ,columns=I(paste0('[',paste0(
                      ifelse(py$dfmeta$getHeaders() %in% py$dfmeta$getDynIDs()
                             ,'{className:"dfDyn"}','null'
                      ),collapse=','),']'))
                    ,initComplete=I("
      function(settings, json) {
        Shiny.onInputChange('tb_outfile_prev_state','loaded');
      }")));
    show('outwrite');
    });
  
  # If it's necessary to do something after the dataTable loads, uncomment this
  # observeEvent and put that behavior into its payload
  # observeEvent(input$tb_outfile_prev_state,{
  #   browser();
  # });
  
  observeEvent(input$outwrite,{
    foutname<-py$dfmeta$processRows(outfile = tempfile()
                                    ,returnwhat = 'filename');
    fnicename <- paste0('DF_',gsub('\\.db$','.csv',basename(rv$infilename)));
    output$outdownload <- downloadHandler(filename=fnicename
                                          ,content=function(con) {
                                            file.copy(foutname,con)});
    show('outdownload');
  });

  # Save the user-controlled parts of the current UI state
  # to internal variable (currently for testing, in the future might be the
  # basis for exporting settings for later reuse)
  observeEvent(input$btDumpcols,{
    rv$dumpcols <- dumpOutputCols(input=input,rv=rv);
    cat('\n***\n',names(rv$dumpcols),'\n***\n');
  });
  
  message('\n*** line 553 ***\n');
# Testing ----
  observeEvent(input$debug,{
    browser();
   });
})

