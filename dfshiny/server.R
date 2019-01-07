library(shiny); library(shinyjqui); library(bsplus); library(queryBuilder);
library(reticulate); library(readr);

# load various useful stuff
source('templates.R');

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
    Shiny.onInputChange('uigroup','chosenid');
    Shiny.onInputChange('%3$s', $('%4$s').map(function(){
                return this.id}).get())})"
                ,targetid,event,inputid,selector);
  #print(js);
  runjs(js);
}
# validNames ----
#' validNames: make sure a name is unique and has legal characters
#'
#' @param newname         string
#' @param existingnames   character vector
#' @param id              optional string, ID of a textInput
#' @param session         current session (optional)
#'
#' @return uniquified name, with side effect of updating a textInput
#'         if ID given
validNames <- function(newname,existingnames=c(),id=NULL
                       ,session=getDefaultReactiveDomain()){
  name0 <- gsub('[_.]+','_',make.names(newname));
  name1 <- tail(gsub('[_.]+','_',make.names(c(existingnames,name0)
                                            ,unique=T)),1);
  if(!is.null(id)) updateTextInput(session,id,value=name1);
  return(name1);
}    



# End validNames ----

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
    runjs(sprintf("$('%s').trigger('sortupdate')",objinfo$divIDchosen));
    onclick(objinfo$delbid
            ,removeChosen(objinfo$parent_name,objinfo$longname,rv));
  }
}

removeChosen <- function(incolid,finalid,...){
  finalid <- gsub('^#','',finalid);
  remtarget <- paste0('#',py$dfmeta[incolid]$chosen[[finalid]]$shortname)
  py$dfmeta[incolid]$unprepChosen(finalid);
  removeUI(remtarget,immediate = T);
}

withHtmlTemplate <- function(env,template,...){
  env <- c(env,list(...),text_=template);
  do.call(htmlTemplate,env);
}

# create the starting UI elements for the available columns
buildDFCols <- function(incolid,rulenames=T){
  # apparently py magically just shows up in scope without being passed
  obj <- py$dfmeta[incolid]$getDict();
  if(missing(rulenames)) myrules <- obj$rules else {
    myrules <- try(obj$rules[rulenames])
    if(class(myrules)[0] == 'try-error'){
      print('Uh oh, list subsetting problem');
      browser();}}
  out0 <- lapply(myrules,function(xx){
    xxsel <- if (xx$split_by_code && length(obj$unique_codes)>1){
      div(class='transform-argsel'
          ,selectizeInput(xx$selid,multiple=T,label='For the following codes:'
                          ,choices=obj$unique_codes))} else span();
    withHtmlTemplate(xx,templates$divavailable,xxsel=xxsel);
    });
  out1 <- withHtmlTemplate(obj,templates$multidivavailable,innerDivs=out0);
  out2 <- if(obj$as_is_col) span() else{
    withHtmlTemplate(obj,templates$incolui,divavailable=out1)};
  out3 <- withHtmlTemplate(obj,templates$divfull,incolui=out2);
  if(!obj$as_is_col){
    jqui_sortable(ui=paste0('#',obj$divIDchosen),options=list(axis='y',items='div'))};
  out3;
}

# flattenRules ----
#' flattenRules: take a rules list and make each
#' rule a flat named list so the overall nesting 
#' max depth is 2
#'
#' @param incolid: char, name of parent dfcol
#' @param rules: named list
#'
#' @return list
#'
#' @examples
flattenRules <- function(incolid,rules){
  out <- list();
  for(ii in rules){
    iiout <- ii[c('suggested','criteria','split_by_code','ruledesc','filter')];
    for(jj in ii$extractors){
      # make name
      jjid <- sprintf(gsub('\\{0\\}','%s',jj[2]),incolid);
      # flatten and merge
      jjout <- as.list(setNames(jj[1:3],c('extr','colidtmpl','args')));
      jjout$addbid <- paste('addb',jjout$extr,jjid,sep='-');
      jjout$parent_name <- incolid;
      jjout$own_name<- jjid;
      # assign
      out[[jjid]]<-c(jjout,iiout);
    }
  }
  out;
}

# End flattenRules ----

shinyServer(function(input, output, session) {
  # server init ----
  # load the stuff we need from datafinisher
  source_python('df_reticulate.py');
  runjs("$('#customQB').on('hidden.bs.collapse',function(){
        Shiny.onInputChange('qbtest_out',null);
        Shiny.onInputChange('qbtest_validate',true);
      })");
  
  rv <- reactiveValues(uitest=div(
     orderInput('source', 'Source'
                ,items = factor(sample(month.abb,15,rep=T))
                ,as_source = TRUE, connect = 'dest')
    ,orderInput('dest', 'Dest', items = NULL
               , placeholder = 'Drag items here...')
    ));
  
  
  # Renders a sample of the uploaded data and as a 
  # side effect creates the UI for manipulating it.
  # TODO: Find a way to trigger the processing without
  #       having to click this tab.
  output$tb_infile_prev <- renderDataTable({
    
    # read input data ----
    # Don't attempt to produce output until file exists
    req(input$infile);
    # Peek at the file type. If it's CSV, use read_csv()
    if(input$infile$type == 'text/csv'){
      dat <- read_csv(input$infile$datapath,n_max=1000)
    } else {
      # If not csv, assume tab-delimited and *try* 
      # read_tsv()
      dat <- try(read_tsv(input$infile$datapath,n_max=1000))
      # if there was an error or there is only one
      # column in the result, assume we guessed wrong
      # and fail over to using read_csv() after all
      # TODO: more general guessing of delimiters or
      #       optional user-supplied delimiters
      if(is(dat,'try-error')||ncol(dat)==1){
        dat <- read_csv(input$infile$datapath,n_max=1000);
      };
    }
    
    # create dfmeta ----
    py$inhead <- r_to_py(names(dat),convert = T);
    py$inmeta <- r_to_py(as.character(dat[1,]),convert = T);
    py_run_string('dfmeta=DFMeta(inhead,inmeta,suggestions=autosuggestor)');
    rv$have_dfmeta <- T;
    message('\n*** dfmeta created ***\n');
    
    # create startingdivs ----
    # Just the column divs, as built by buildDFCols
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
    rv$ui_transform <- div(infodivs,id='infodivs_parent');
    message('\n*** ui_transform created ***\n');
    
    # populate the 'Transform Data' tab
    output[['tb_transform']] <- renderUI(isolate(rv$ui_transform));
    
    runjs("Shiny.onInputChange('choosewait',+ new Date())");
    
    # set output options so stuff starts rendering before tab active
    outputOptions(output,'tb_infile_prev',suspendWhenHidden=F);
    outputOptions(output,'tb_transform',suspendWhenHidden=F);
    
    # return a sample of the input for the 'Input Data' tab
    return(head(dat[-1,],100));
  });
  
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
    xx = 0}; Shiny.onInputChange('choosewait',xx);");};
  });
  
  # make sure custom rules have names that are safe, legal, 
  # and unique
  observeEvent(c(input$customTrName,input$customSave,input$infile),{
    req(rv$have_dfmeta);
    validNames(input$customTrName,names(py$dfmeta$rules),id='customTrName');
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
                     ,choices=unlist(py$dfmeta$getColIDs(ids='incolid'))
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
    # because they have too many. Catch those cases and turn
    # them into text instead of selectives
    if(is.null(out$concept_cd$values)){
      out$concept_cd$values <- NULL;
      out$concept_cd$input <- 'text';
    }
    validchoices<-names(rv$currentFilterlist <- out[applicable]);
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
                               ,allow_empty=T);
      rv$customWhichFieldsReady <- selectizeInput('customSelFields'
                                                  ,label='Select the field or
                                                        fields you wish this
                                                        transformation to 
                                                        return:'
                                                  ,multiple=T
                                                  ,choices=validchoices);
      show('customWhichFields');
      show('customQBhead');
    } else {
      hide('customWhichFields');
      runjs("$('#customQB').collapse('hide')");
      hide('customQBhead');
      hide('customAggregate');
    }
  });
  
  output$customWhichFields <- renderUI(rv$customWhichFieldsReady);
  
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
                   show('customAggregate');
                   if(is.null(input$qbtest_validate)||input$qbtest_validate) {
                     enable('customSave')
                     } else disable('customSave');
                   } else {
                     hide('customAggregate');
                     disable('customSave');
                   }
                   });
  
  observeEvent(input$customCancel,{
    # reset to empty values
    updateSelectizeInput(session,'customSelCols',selected=character(0));
    updateTextAreaInput(session,'customTrDesc',value='');
    updateTextInput(session,'customTrName',value = 'custom');
  });
  
  # When the save button is pressed on Custom Transforms tab
  observeEvent(input$customSave,{
    rulesuffix<-validNames(input$customTrName,names(py$dfmeta$rules)
                       ,id='customTrName',session);
    # the save button should be disabled if the input is 
    # invalid, so if input$qbtest_out is null, that's because
    # the user has chosen not to filter
    selector <- if(is.null(input$qbtest_out)||length(input$qbtest_out$rules)==0){
      "ALL";
    } else input$qbtest_out;
    newrule <- list(
        ruledesc=input$customTrDesc
       ,criteria=sprintf("colid in ['%s']"
                         ,paste0(input$customSelCols,collapse="','"))
       ,split_by_code=F
       ,selector=selector
       ,fieldlist=input$customSelFields
       ,aggregator=input$customAggregate
       ,rulesuffix=rulesuffix
      );
    # empty out description field
    updateTextAreaInput(session,'customTrDesc',value='');
    # TODO: why is everything after the description field automatically 
    # cleared when "Save" is clicked? Useful, but troubling.
    
    # TODO: modify buildDFCols() so that it can iterate over individual columns
    # and rules, returning HTML
    isolate({
      # for each eligible main column...
      for(ii in forincols) {
        # prepare it
        #iiavailable <- flattenRules(ii,list(transform))[[1]];
        # insert it into the available list for that column
        # rv$dfinfolist is being phased out, commenting out below
        #rv$dfinfolist[[ii]]$rules[[iiavailable$longname]]<-iiavailable;
        # add to the available UI divs
        insertUI(paste0("#avail-",ii,">div"),'beforeEnd'
                 , withHtmlTemplate(iiavailable,templates$divavailable
                                    ,xxsel=span())
                 ,immediate=T);
        # instrument the Add/Update
        eval(substitute(onclick(iivals[3]
                                ,addChosen(iivals[1]
                                           ,iivals[2],rv=rv,input=input),add=T)
                        ,env=list(iivals=iivals)));
        };
      });
  });
  
  # Save the user-controlled parts of the current UI state
  # to internal variable (currently for testing, in the future might be the
  # basis for exporting settings for later reuse)
  observeEvent(input$btDumpcols,{
    rv$dumpcols <- dumpOutputCols(input=input,rv=rv);
    cat('\n***\n',names(rv$dumpcols),'\n***\n');
  });

# Testing ----
  observeEvent(input$debug,{
    browser();
   });
  
  output$test <- renderUI({print('rendering test');rv$uitest});

})

