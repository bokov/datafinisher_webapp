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
  runjs(js); runjs(js);
  cat('\n*** dumping column info ***\n');
  sapply(names(input$dumpcols),function(ii) {
    rv$dfinfolist[[ii]]$chosen[unlist(input$dumpcols[[ii]])]
    },simplify=F);
  cat(jsonlite::toJSON(rv$dumpcols,pretty=1)
      ,file=paste0('dumpcols_',as.numeric(Sys.time()),'.json'));
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

addChosen2 <- function(chosenid,incolid,rv){
  obj <- py$dfmeta[incolid]$getDict();
  browser();
  # needed info:
    # * selid-modified outcol name
    # * delbid
    # * which main column?
    # ? name to be added to R chosen list
    # targetid (#c-)
    # OK longname
    # OK ruledesc
    # 
  # check if inserted
}

# TODO: this is the next thing to fix !
addChosen <- function(incolid,availableid,rv,input,finalid=availableid){
  # create the data structure for the new output column
  incoldata <- rv$dfinfolist[[incolid]];
  payload <- incoldata$rules[[availableid]]
  # derive needed IDs
  targetid <- paste0('#',incoldata$divIDchosen) #paste0('#c-',incolid);
  selid <- payload$selid #paste0('sl-',finalid);
  finalid <- payload$longname #gsub('\\{0\\}',incolid,payload$colidtmpl);
  # if this is code-specific, append the selected codes
  if(payload$split_by_code){
    finalid <- paste0(finalid,'_',gsub('[^A-Za-z0-9_]','_'
                                       ,paste(gsub('^.*:','',input[[selid]])
                                              ,collapse='_')))};
  payload$own_name <- finalid;
  payload$delbid <- paste0('db-',finalid);
  # TODO: replace this with HTML template
  if(!payload$longname %in% names(rv$dfinfolist[[incolid]]$chosen)){
    insertUI(targetid,where='beforeEnd',immediate=T
             ,ui=withHtmlTemplate(payload,templates$divchosen
                                  ,delbutton=actionButton(payload$delbid
                                                          ,'Remove'
                                                          ,class='btn-danger'))
             );
    runjs(sprintf("$('%s').trigger('sortupdate')",finalid));
    onclick(payload$delbid,removeChosen(incolid,payload$longname,rv));
  }
  # add it to the chosen columns
  rv$dfinfolist[[incolid]]$chosen[[payload$longname]] <- payload;
  
}

removeChosen <- function(incolid,finalid,rv){
  finalid <- gsub('^#','',finalid);
  rv$dfinfolist[[incolid]]$chosen[[finalid]]<-NULL;
  # turning finalid back into an #id selector
  finalid <- paste0('#',finalid);
  removeUI(finalid,immediate = T);
  #runjs(sprintf("$('%s').sortable('refresh')",finalid));
  runjs(sprintf("$('%s').trigger('sortupdate')",finalid));
}


withHtmlTemplate <- function(env,template,...){
  env <- c(env,list(...),text_=template);
  do.call(htmlTemplate,env);
}

# create the starting UI elements for the available columns
buildDFCols <- function(incolid){
  # apparently py magically just shows up in scope without being passed
  obj <- py$dfmeta[incolid]$getDict();
  out0 <- lapply(obj$rules,function(xx){
    xxsel <- if (xx$split_by_code && length(obj$unique_codes)>1){
      div(class='transform-argsel'
          ,selectizeInput(xx$selid,label='For the following codes:'
                          ,choices=obj$unique_codes))} else span();
    withHtmlTemplate(xx,templates$divavailable,xxsel=xxsel);
    });
  out1 <- withHtmlTemplate(obj,templates$multidivavailable,innerDivs=out0);
  out2 <- if(obj$as_is_col) p() else{
    withHtmlTemplate(obj,templates$incolui,divavailable=out1)};
  out3 <- withHtmlTemplate(obj,templates$divfull,incolui=out2);
  if(!obj$as_is_col){
    jqui_sortable(ui=paste0('#',obj$divIDchosen),options=list(axis='y',items='div'))};
  out3;
}
#' 
cleanDFCols <- function(incolid,obj){
  obj <- obj[[incolid]]; out <- list(); #out2 <- list();
  # for(ii in c('as_is_col','colmeta','dfcol','unique_codes','incolid')){
  #   out[[ii]] <- obj[[ii]];
  # }
  attrs <- names(obj);
  attrs <- attrs[sapply(attrs,function(ii) mode(obj[[ii]]))!='function'];
  for(ii in attrs) out[[ii]] <- obj[[ii]];
  # R-specific starts
  out$innerDivs <- lapply(out$rules,function(xx){
    xxsel<-if(xx$split_by_code && !is.null(out$unique_codes) && 
              out$unique_codes!=''){
      #selid <- paste0('sel-',xx$own_name);
      div(selectizeInput(xx$selid,label='For the following codes:'
                         ,choices=out$unique_codes)
          ,class='transform-argsel')} else span();
    withHtmlTemplate(xx,templates$divavailable,xxsel=xxsel);
  });
  out$divavailable <- withHtmlTemplate(
    out,templates$multidivavailable
  );
  out$chosen <- list();
  #out$incolid <- obj$name;
  out$incolui <- if(out$as_is_col) p() else {
    withHtmlTemplate(out,templates$incolui)};
  out$divfull <- withHtmlTemplate(out,templates$divfull);
  if(!out$as_is_col){
    jqui_sortable(ui=paste0('#',out$divIDchosen)
                  ,options=list(axis='y',items='div'))};
  out;
}

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

shinyServer(function(input, output, session) {
  
  # load the stuff we need from datafinisher
  source_python('df_reticulate.py');
  dfns <- py_run_string('from df_fn import shortenwords,dropletters'
                        ,local=T);
  
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

  rvp <- reactiveValues();
  
  #onclick('makecustom',show('customui'));
  
  # Renders a sample of the uploaded data and as a 
  # side effect creates the UI for manipulating it.
  # TODO: Find a way to trigger the processing without
  #       having to click this tab.
  output$tb_infile_prev <- renderDataTable({
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
    
    # Now we have a sample data-file!
    rvp$dfmeta <- py$DFMeta(names(dat),as.character(dat[1,])
                            ,suggestions=py$autosuggestor);
    # testing out processing within main
    py$inhead <- r_to_py(names(dat),convert = T);
    py$inmeta <- r_to_py(as.character(dat[1,]),convert = T);
    
    # The following works perfectly!
    py_run_string('dfmeta=DFMeta(inhead,inmeta,suggestions=autosuggestor)');
    # all.equal(rvp$dfmeta,py$dfmeta)
    # all.equal(py$dfmeta['v122_Acqrd_absnc']
    # ,py$dfmeta2$incols[['v122_Acqrd_absnc']])
    # 
    # These work too:
    # with(py$dfmeta['v122_Acqrd_absnc']$getDict(),div(id=incolid))
    # So, cleanDFCols should now only need to generate HTML based on variables
    # sent to it (and it should be renamed accordingly)
    
    message('\n*** dfmeta created ***\n');
    
    # data for populating UI and recording choices
    # REMOVE SOON
    rv$dfinfolist <- sapply(rvp$dfmeta$inhead
                            ,function(ii) {
                              cleanDFCols(ii,rvp$dfmeta$incols)
                              },simplify=F);
    
    # Just the column divs
    rv$dfstartingdivs <- sapply(py$dfmeta$inhead,buildDFCols,simplify=F);
    message('\n*** dfinfolist created ***\n');
    
    # all possible Add/Update names and chosen divs
    divIDs <- c()
    isolate({
      for(xx in rv$dfinfolist) if(length(xx$rules)>0) for(yy in xx$rules) {
        divIDs<-rbind(divIDs,with(yy
                                  ,data.frame(incolid=parent_name,longname,shortname
                                              ,addbid,selid
                                              ,divIDavailable=xx$divIDavailable
                                              ,divIDchosen=xx$divIDchosen
                                              ,stringsAsFactors=F)))};
    });
    
    message('\n*** divIDs created ***\n');
    
    # This is the part that detects clicks on the Add/Update buttons
    # Have to wrap the expr argument in substitute because otherwise
    # it doesn't correctly read the ii value. Wierd.
    
    # TODO: prepend the selected code if the rule takes an argument
    # isolate({for(ii in unique(divIDs$addbid)){
    #   ids <- subset(divIDs,addbid==ii);
    #   eval(substitute(onclick(xx
    #                           ,addChosen(yy$incolid[1]
    #                                      ,yy$divIDavailable[1]
    #                                      ,rv,input)
    #                           ,add=T),env=list(xx=ii,yy=ids)))};
    # });
    for(xx in py$dfmeta$getColIDs(asdicts=T
                                  ,ids='incolid'
                                  ,childids=c('rulename','addbid')
                                  ,childtype='rules')){
      eval(substitute(
        onclick(addbid,addChosen(incolid,rulename,rv,input),add=T)
        ,env=xx));
    }
    message('\n*** addbid onclicks created ***\n');
    
    # Save the divIDs for later access
    rv$divIDs <- divIDs;

    # create column controls
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
    # for(ii in rvp$dfmeta$inhead){
    #   isolate({
    #     if(rv$dfinfolist[[ii]]$as_is_col){
    #       infodivs <- htmltools::tagAppendChild(
    #         infodivs
    #         ,div(class='panel panel-default pn-colstatic dfcol-static'
    #              ,div(class='panel-heading',ii)
    #              ,div(class='panel-body',rv$dfinfolist[[ii]]$divfull)));
    #     } else {
    #       infodivs<-bs_append(infodivs,ii,rv$dfinfolist[[ii]]$divfull);
    #     }
    #     });
    # }
    runjs("$('.collapse').on('shown.bs.collapse', function (e) {
        Shiny.onInputChange('activecolid',($('.in>.panel-body>div').attr('id')));
    })")
    message('\n*** infodivs created ***\n');
    
    
    # Populate the ui_transform, needed by the 'Transform Data' panel
    rv$ui_transform <- div(infodivs,id='infodivs_parent');
    message('\n*** ui_transform created ***\n');
    
    # populate the 'Transform Data' tab
    output[['tb_transform']] <- renderUI(isolate(rv$ui_transform));
    
    # TODO: update to py functions
    # make the chosen divs sortable and register inputs
    #for(ii in unique(divIDs$chosenid)){
    # for(ii in unique(divIDs$divIDchosen)){
    #   eval(substitute(sortableWatcher(ii)))};
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
      #for(ii in unique(rv$divIDs$chosenid)){
      #for(ii in unique(rv$divIDs$divIDchosen)){
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
  
  # observeEvent(input$activecolid,{
  #   if(input$activecolid=='(none selected)'){
  #     disable('makecustom');hide('customui');} else {
  #       # TODO elsewhere:
  #       #   delimiter for multiselect
  #       enable('makecustom')}});
  
  # make sure custom rules have names that are safe, legal, 
  # and unique
  observeEvent(c(input$customTrName,input$customSave),{
    validNames(input$customTrName,names(rvp$dfmeta$rules)
               ,id='customTrName');
    });
  
  # offer a choice of columns from which to chose the ones that will have
  # access to this transform. Only fields available in all these columns
  # will be options in the transform
  output$customWhichCols <- renderUI({
    #req(rv$divIDs);
    req(rv$dfstartingdivs);
    if(input$choosewait==0){
      selectizeInput('customSelCols'
                     ,label='Select the main column or columns in your data for
                             which this transformation should be available:'
                     #,choices=unique(rv$divIDs$incolid)
                     ,choices=unlist(py$dfmeta$getColIDs(ids='incolid'))
                     ,multiple=T)} else {
                       span()}});
  
  # when choice of columns changes, update the permitted list of variables
  observeEvent(c(input$customSelCols,input$customTrDesc),{
    applicable <- T; out <- filterlist;
    for(ii in input$customSelCols){
      applicable <- applicable & sapply(filterlist,function(xx) {
        eval(xx$criteria,envir=rv$dfinfolist[[ii]]$colmeta)});
      if(!is.null(rv$dfinfolist[[ii]]$colmeta$ccd_list)){
        newcodes <- strsplit(rv$dfinfolist[[ii]]$colmeta$ccd_list,",")[[1]];
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

  observeEvent(input$customSave,{
    forincols <- input$customSelCols;
    trname<-validNames(input$customTrName,names(rvp$dfmeta$rules)
                       ,id='customTrName',session);
    nametemplate<-paste0('{0}_',trname);
    # the save button should be disabled if the input is 
    # invalid, so if input$qbtest_out is null, that's because
    # the user has chosen not to filter
    filter <- if(is.null(input$qbtest_out)||length(input$qbtest_out$rules)==0){
      "TRUE";
    } else {
      queryBuilder:::recurseFilter(input$qbtest_out);
    }
    transform <- list(
       extr=trname
      ,colidtmpl=nametemplate
      ,ruledesc=input$customTrDesc
      ,fields=input$customSelFields
      ,aggregate=input$customAggregate
      ,filter=filter
      # compatibility
      ,args=NA
      ,criteria=sprintf("colid %%in%% %s",paste(forincols,collapse=','))
      ,split_by_code=F
      ,suggested=F
      ,extractors=list(list(trname,nametemplate,list()))
      );
    # empty out description field
    updateTextAreaInput(session,'customTrDesc',value='');
    # TODO: why is everything after the description field automatically 
    # cleared when "Save" is clicked? Useful, but troubling.
    browser();
    isolate({
      # add to the master rules list
      rvp$dfmeta$rules[[trname]]<-transform;
      # for each eligible main column...
      for(ii in forincols) {
        # prepare it
        iiavailable <- flattenRules(ii,list(transform))[[1]];
        # insert it into the available list for that column
        rv$dfinfolist[[ii]]$rules[[iiavailable$longname]]<-iiavailable;
        # add to the available UI divs
        insertUI(paste0("#avail-",ii,">div"),'beforeEnd'
                 , withHtmlTemplate(iiavailable,templates$divavailable
                                    ,xxsel=span())
                 ,immediate=T);
        # update the divIDs data.frame
        rv$divIDs<-rbind(rv$divIDs
                         ,iivals<-with(iiavailable
                               ,c(parent_name,own_name,addbid
                                  ,paste0('chosen-',parent_name))));
        # instrument the Add/Update
        eval(substitute(onclick(iivals[3]
                                ,addChosen(iivals[1]
                                           ,iivals[2],rv,input),add=T)
                        ,env=list(iivals=iivals)));
        };
      });
  });
  
  observeEvent(input$btDumpcols,{
    rv$dumpcols <- dumpOutputCols(input=input,rv=rv);
    cat('\n***\n',names(rv$dumpcols),'\n***\n');
  });

  observeEvent(input$debug,{
    req(rv$dfinfolist);
    t_incolid <- names(rv$dfinfolist)[42];
    t_dat <- rv$dfinfolist[[t_incolid]];
    
    browser();
   });
  
  output$test <- renderUI({print('rendering test');rv$uitest});

})

