# uncomment below line if reticulate gets stubborn about python version
reticulate::use_python('/usr/bin/python',required = TRUE);
library(bsplus); library(reticulate); library(readr); library(shinyjqui);
library(DT);

# reminder: the interactive debugger for reticulate is repl_python 
# 
# load various useful stuff
source('templates.R');
source('functions.R');


# shinyServer ----
shinyServer(function(input, output, session) {
  # server init ----
  debugtrigger <- reactiveFileReader(5000,session=NULL,filePath = '.debugnow'
                                     ,readFunc = file.exists);
  shinyalert('User Agreement',text=helptext$disclaimer
             # user agreement ----
             ,html=T,confirmButtonText = 'I agree',confirmButtonCol = hcol
             ,className = 'dfDisclaimer',closeOnEsc = F
             ,animation = 'slide-from-top'
             ,callbackR = function() {
               rv[['disclaimerAgreed']] <- T;
               show('infile')});
  # load the stuff we need from datafinisher
  #observe_helpers(help_dir = 'www/docs');
  source_python('df_reticulate.py');
  runjs("$('#customQB').on('hidden.bs.collapse',function(){
        Shiny.onInputChange('qbtest_out',null);
        Shiny.onInputChange('qbtest_validate',true);
      })");
  rv <- reactiveValues(disclaimerAgreed=F);
    # uitest=div(
    #  orderInput('source', 'Source'
    #             ,items = factor(sample(month.abb,15,rep=T))
    #             ,as_source = TRUE, connect = 'dest')
    # ,orderInput('dest', 'Dest', items = NULL
    #            , placeholder = 'Drag items here...')
    # );
  
  
  # obtain either a pre-existing file or uploaded by user ----
  observeEvent(c(input$infile,rv$disclaimerAgreed),{
    req(input$infile$datapath,rv$disclaimerAgreed);
    rv$infile <- input$infile$datapath;
    rv$infilename <- input$infile$name;});
  
  observeEvent(session$clientData$url_search,{
    if(!is.null(dfile<-parseQueryString(session$clientData$url_search)$dfile)){
      dfile <- file.path(trusted_files,basename(dfile));
      if(file.exists(dfile)){
        rv$infile <- dfile;
        rv$infilename <- basename(dfile);} else {
          message('*** Did not find ',dfile,' ***');
          nofilemsg <- as.character(tagList('
          If you double-checked the link and it is correct then it could mean 
          your results are still being processed.',br()
          ,a(href=paste0(session$clientData$url_pathname
                         ,session$clientData$url_search)
             ,target='_TOP',class='btn btn-info'
             ,'See if the file is ready yet.')
          ,a(href=session$clientData$url_pathname
             ,target='_TOP',class='btn btn-info'
             ,'Go to main DataFinisher page')
          ));
          shinyalert('File not (yet) available.'
                     ,text=nofilemsg,closeOnEsc = FALSE
                     ,closeOnClickOutside = FALSE,html=T,showConfirmButton=F
                     ,animation = 'slide-from-top');
          }}});
  
  # create dfmeta ----
  observeEvent(c(rv$infile,rv$disclaimerAgreed),{
    req(rv$infile,rv$disclaimerAgreed);
    showModal(modalDialog(messages$mLoading,title='Please wait.',footer=NULL));
    py_run_string(sprintf("dfmeta=DFMeta(fref='%s',suggestions=autosuggestor)"
                          ,rv$infile));
    # Indicator for the rest of the webapp that the core object is ready
    rv$have_dfmeta <- Sys.time();
    message('\n*** dfmeta created ***\n');
    removeModal();
  });
  
  
  # read input data ----
  observeEvent(rv$have_dfmeta,{
    req(rv$have_dfmeta);
    hide('termsofuse');
    removeModal();
    show(selector = '#maintabs>.tabbable');
    dat <- try(read_delim(paste0(py$dfmeta$sampleInput(nrows = 300)
                                 ,collapse='\n')
                          ,py$dfmeta$data$dialect$delimiter));
    # + DataTable preview of input ----
    output$tb_infile_prev <- DT::renderDataTable(
      DT::datatable(dat,extensions=c('Scroller')
                    ,autoHideNavigation=T,rownames=F,fillContainer=T
                    ,options=list(processing=T,searching=F,scroller=T
                                  ,dom='Bfrtip' #,pageLength=12
                                  ,scrollX=T,scrollY='50vh'
                                  ,columns=JS(
                                    paste0('['
                                           ,paste0(ifelse(py$dfmeta$inhead %in%
                                                            py$dfmeta$getDynIDs()
                                                          ,'{className:"dfDyn"}'
                                                          ,'null')
                                                   ,collapse=','),']'))
                                  )
      ),server=F);
    outputOptions(output,'tb_infile_prev',suspendWhenHidden=F);
  });

  # populate the 'Transform Data' tab ----
  output$tb_transform <- renderUI({
    req(rv$have_dfmeta)
    # create startingdivs ----
    # Just the column divs, as built by buildDFCols
    # This is the slowest step
    rv$dfstartingdivs <- sapply(py$dfmeta$inhead,buildDFCols,helptext=helptext
                                ,simplify=F);
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
          infodivs<-bs_append(infodivs,py$dfmeta[ii]$getColIDs('incoldesc')[[1]]
                              ,rv$dfstartingdivs[[ii]])
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
    onclick(ii,shinyalert(text = helptext[[ii]],confirmButtonCol = hcol,html = T
                          ,className = 'dfHelp'))});

    # set output options so stuff starts rendering before tab active
  outputOptions(output,'tb_transform',suspendWhenHidden=F);

  # Wait for the divIDchosen to load and then make them sortable
  observeEvent(input$choosewait,{
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
    xx = 0};
    // Below prevents clicking on a selectize box from triggering the help popup
    $('.selectize-control').click(function(ee){ee.stopPropagation();});
    Shiny.onInputChange('choosewait',xx);");};
  });
  
  
  
  # custom rule names ----
  # make sure custom rules have names that are safe, legal, 
  # and unique
  observeEvent(c(input$customTrName,rv$infile,rv$have_dfmeta),{
    req(rv$have_dfmeta);
    validNames(input$customTrName,id='customTrName');
    });
  
  # offer a choice of columns from which to select ones that will have
  # access to this transform. Only fields available in all these columns
  # will be options in the transform
  output$customWhichCols <- renderUI({
    req(rv$dfstartingdivs);
    if(input$choosewait==0){
      selectizeInput('customSelCols'
                     ,label='Select the main column or columns in your data for
                             which this transformation should be available:'
                     ,choices=sort(unlist(py$dfmeta$getColIDs(ids='incolid')))
                     ,multiple=T)} else {
                       span()}});
  
  # when choice of columns changes, update the permitted list of variables
  observeEvent(c(input$customSelCols,input$customTrDesc),{
    req(rv$have_dfmeta)
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
    } else {
      hide('customWhichFieldsGrp');
      runjs("$('#customQB').collapse('hide')");
      hide('customQBhead');
      hide('customAggregateGrp');
    }
  });

  # re/create the querybuilder UI
  output$qbtest <- renderQueryBuilder({
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
                   show('customQBHead');
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
  
  
  
  # Populate the new divs, new info created in Custom Transforms
  observeEvent(c(rv$newdivs,rv$newinfo),{
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
    hide('outdownload');hide('outwrite');hide('dictdownload');
    # get the output column order
    chsnames <- sapply(py$dfmeta$getColIDs(ids=c('divIDchosen','incolid'))
                      ,function(xx) setNames(list(input[[xx[1]]]),xx[2]));
    # TODO: fix this so that Python catches null entries, including where all
    # are null, so that the web front end doesn't need to
    chsnames <- chsnames[!sapply(chsnames,is.null)];
    .dbg <- try(
    py$dfmeta$finalizeChosen(chsnames)
    );
    if(class(.dbg)[1]=='try-error'){
      shinyalert('An error has occurred. If possible, please send a copy of the 
                 file you were working with and the steps that were taken prior
                 to this error and email them to bokov@uthscsa.edu with the 
                 subject "DataFinisher Bug". Thanks, and sorry for the
                 inconvenience.');
      browser();
    }
    tempname <- py$dfmeta$processRows(tempfile(),nrows=300);
    # get the metadata for all columns that have dynamic data
    colmetas <- py$dfmeta$getColIDs(ids=c('incolid','colmeta')
                                    ,childtype='chosen',asdicts=T);
    colmetas <- setNames(sapply(colmetas,`[[`,'colmeta',simplify=F)
                         ,sapply(colmetas,`[[`,'incolid'));
    # the suffixes that got added to columns (might not work with custom)
    rulesuffixes <- paste0('_',sapply(py$dfmeta$rules,`[[`,'rulesuffix'),'$'
                           ,collapse='|');
    # headers
    outheaders <- py$dfmeta$getHeaders();
    # fields to collect for data dictionary
    dictfields <- sapply(colmetas,names,simplify=F) %>% unlist %>% 
      setdiff(.,c('__builtins__','ccd_list','done','xx'));
    # metadata dictionary
    dict <- sapply(outheaders,function(ii){
      if(ii %in% names(colmetas)) return(colmetas[[ii]][dictfields]) 
      # if this is a derived or dynamic column
      else if((jj<-gsub(rulesuffixes,'',ii)) %in% names(colmetas)){
        iiout <- colmetas[[jj]][dictfields]}
      # if this is a static column
      else iiout <- c(list(static=py$dfmeta[[ii]]$colmeta),
                      setNames(as.list(rep_along(dictfields,NA)),dictfields));
      iiout <- lapply(iiout,function(kk) c(unlist(kk),NA)[1]) %>% 
        data.frame(stringsAsFactors = FALSE);
      return(iiout);
      }, simplify=F) %>% rbindlist(fill=TRUE) %>%
      # prepend a column with current column names and current column names 
      # without unique prefixes (for joins to later versions of the same data)
      cbind(colname=outheaders,durablename=mapply(gsub,(.)$colcd,'',outheaders)
            ,.);
    # Note: need to create a download button in the UI and a download handler
    write_csv(dict,dictfile<-tempfile(gsub('\\.(db|csv)',''
                                           ,paste0(basename(rv$infilename)
                                                   ,'_dict_'))
                                      ,fileext = '.csv'),na='');
    message('Created data dictionary');
    
    #py$dfmeta$fhandle$seek(0); py$dfmeta$nrows = 3;
    output$tb_outfile_prev <- DT::renderDataTable(
    DT::datatable(read_delim(tempname
                             ,delim=py$dfmeta$data$dialect$delimiter)[-2,]
                  ,extensions=c('Scroller')
                  ,autoHideNavigation=T,rownames=F,fillContainer=T
                  ,options=list(processing=T,searching=F,scroller=T
                                ,dom='Bfrtip' #,pageLength=12
                                ,scrollX=T,scrollY='50vh'
                                ,columns=JS(paste0('[',paste0(
                                  ifelse(py$dfmeta$getHeaders() %in% 
                                           py$dfmeta$getDynIDs()
                                         ,'{className:"dfDyn"}','null')
                                  ,collapse=','),']'))
                          ## This event is for signalling that table done
                          ## loading, see below
                          #       ,initComplete=JS("function(settings, json) {
                          # Shiny.onInputChange('tb_outfile_prev_state','loaded');
                          #           }")
                                )), server=F);
    output$dictdownload <- downloadHandler(filename=basename(dictfile)
                                          ,content=function(con) {
                                            file.copy(dictfile,con)});
    show('outwrite');show('dictdownload');
    });
  
  # If it's necessary to do something after the dataTable loads, uncomment this
  # observeEvent and put that behavior into its payload
  # observeEvent(input$tb_outfile_prev_state,{
  #   browser();
  # });
  
  # outputWrite ----
  observeEvent(input$outwrite,{
    fnicename <- paste0('DF_',gsub('\\.db$','.csv',basename(rv$infilename)));
    foutname<-py$dfmeta$processRows(outfile = file.path(tempdir(),fnicename)
                                    ,returnwhat = 'filename');
    if(file.size(foutname)>zip_cutoff){
      message('\n*** large output file, zipping ***\n');
      foutname_final <- paste0(foutname,'.zip');
      zip(foutname_final,foutname,flags='-j9');
      suffix_final <- '.zip';
    } else {foutname_final <- foutname; suffix_final <- '';}
    fnicename <- paste0(fnicename,suffix_final);
    message(sprintf('\n*** %s ready for download as %s ***\n'
                    ,foutname_final,fnicename));
    output$outdownload <- downloadHandler(filename=fnicename
                                          ,content=function(con) {
                                            file.copy(foutname_final,con)});
    # get the headers
    
    # if as_is_col and colmeta != '', treat as hardcoded value
    # if as_is_col and colmeta == '' and the non-suffix version exists
    #    replace ii with the non-suffix version
    # getDict, munge NULLs to NAs
    show('outdownload');
  });

  # Save the user-controlled parts of the current UI state
  # to internal variable (currently for testing, in the future might be the
  # basis for exporting settings for later reuse)
  observeEvent(input$btDumpcols,{
    rv$dumpcols <- dumpOutputCols(input=input,rv=rv);
    cat('\n***\n',names(rv$dumpcols),'\n***\n');
  });
  
# Testing ----
  # trigger debug with a file ('.debugnow') if the UI is hung
  observeEvent(debugtrigger(),{if(debugtrigger()) browser();})
  # trigger debug from UI
  observeEvent(input$debug,{
    browser();
   });
})

