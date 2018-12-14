library(shiny); library(shinyjqui); library(bsplus); library(queryBuilder);
library(reticulate); library(readr);

# load various useful stuff
source('templates.R');

# attach a proper index to a sortable input
sortableWatcher<-function(targetid,inputid
                          ,selector='auto',event='sortupdate'){
  # if targetid has no leading hash, add one
  if(!grepl('^#',targetid)) targetid <- paste0('#',targetid);
  # if inputid has a leading hash remove it
  #inputid <- gsub('^#','',inputid);
  if(missing(inputid)) inputid <- targetid;
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

# TODO: For some reason the %in% input[[targetid]] stuff is flaky
# still allows duplicate values. Might want to just look at 
# names(rv$dfinfolist[[incol]]$chosen) and act solely on that.

addChosen <- function(incolid,availableid,rv,input,finalid=availableid){
  delbid <- paste0('delb-',finalid);
  targetid <- paste0('#chosen-',incolid);
  # create the data structure for the new output column
  incoldata <- rv$dfinfolist[[incolid]];
  payload <- with(incoldata,c(available[[availableid]][c('extr','args','ruledesc','split_by_code','parent_name')]
                              ,list(own_name=finalid,delbid=delbid)));
  if(!finalid %in% names(rv$dfinfolist[[incolid]]$chosen)){
    insertUI(targetid,where='beforeEnd',immediate=T
             ,ui=div(id=finalid,finalid
                     ,actionButton(delbid,'Remove'
                                   ,class='btn-danger'),br()
                     ,span(payload$ruledesc,class='annotation')))
    }
  # add it to the chosen columns
  rv$dfinfolist[[incolid]]$chosen[[finalid]] <- payload;
  runjs(sprintf("$('%s').trigger('sortupdate')",finalid));
  #browser();
  # if(!finalid %in% input[[targetid]]){
  #   insertUI(targetid,where='beforeEnd',immediate=T
  #            ,ui=div(id=finalid,finalid
  #                    ,actionButton(delbid,'Remove'
  #                                  ,class='btn-danger'),br()
  #                    ,span(payload$ruledesc,class='annotation')))
  #   };
  onclick(delbid,removeChosen(incolid,finalid,rv));
  #runjs(sprintf("$('%s').sortable('refresh')",finalid));
  #runjs(sprintf("$('%s').trigger('sortupdate')",finalid));
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

#' getAvailable: provide the actual available rules
#' specification for a DFCol object
#'
#' @param incolid: char, name of parent dfcol
#' @param obj: DFMeta.dfcols attribute
#'
#' @return list
#'
#' @examples
cleanDFCols <- function(incolid,obj){
  obj <- obj[[incolid]]; out <- list();
  for(ii in c('as_is_col','colmeta','dfcol','unique_codes')){
    out[[ii]] <- obj[[ii]];
  }
  # TODO: catch static incols and set their coldesc to something else
  out$incoldesc <- if(out$as_is_col) '(static column)' else {
    out$colmeta$name};
  out$available <- flattenRules(incolid,obj$rules);
  out$divIDavailable <- paste0('avail-',incolid);
  out$divavailable <- withHtmlTemplate(
    out,templates$multidivavailable
    ,innerDivs=lapply(out$available,withHtmlTemplate
                      ,templates$divavailable));
  # out$divavailable <- div(id=out$divIDavailable
  #                         ,lapply(out$available
  #                                 ,function(xx) {
  #                                   with(xx
  #                                        ,div(own_name
  #                                             ,actionButton(addbid
  #                                                           ,'Add/Update'
  #                                                           ,class='btn btn-success')
  #                                             ,br()
  #                                             ,span(ruledesc
  #                                                   ,class='annotation')
  #                                             ))}
  #                                 ));
  out$divIDchosen <- paste0('chosen-',incolid);
  out$chosen <- list();
  out$incolid <- obj$name;
  out$incolui <- if(out$as_is_col) p() else {
    withHtmlTemplate(out,templates$incolui)};
  # out$incolui <- if(out$as_is_col) p() else {
  #   with(out
  #        ,tagList(div(id=divIDchosen,tags$b('Chosen:'))
  #                 ,div(id=divIDavilable,tags$b('Available:'),divavailable)))};
  out$divfull <- withHtmlTemplate(out,templates$divfull);
  # out$divfull <- with(out,div(id=incolid,incolid,br()
  #                             ,span(incoldesc,class='annotation')
  #                             ,incolui));
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
    iiout <- ii[c('suggested','criteria','split_by_code','ruledesc')];
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
    #,queryBuilderOutput('qbtest')
    #verbatimTextOutput('order'))
    ));
  
  filterlist <- list(
     concept_cd=list(name = 'cc'
                    , type = 'string'
                    , input = 'selectize'
                    , label = 'Concept Code (concept_cd)'
                    , values=c()
                    , criteria = quote(isTRUE(ccd > 1)))
    ,modifier_cd=list(name = 'mc', type = 'string', input = 'text'
                      ,label='Modifier Code (modifier_cd)'
                      ,criteria=quote(isTRUE(mod > 0)))
    ,instance_num=list(name='ix',type='integer'
                       ,label='Instance Number (instance_num)'
                       ,criteria=quote(isTRUE(mxinsts>1)))
    ,valtype_cd=list(name = 'vt', type = 'string', input = 'text'
                     ,label='Value Type (valtype_cd)'
                     ,criteria=quote(isTRUE(valtype_cd>0)))
    ,tval_char=list(name = 'tc', type = 'string', input = 'text'
                    ,label='Text Val (tval_char)'
                    ,criteria=quote(isTRUE(tval_char>0)))
    ,nval_num=list(name='nv',type='double'
                   ,label='Numeric Value (nval_num)'
                   ,criteria=quote(isTRUE(nval_num>0)))
    ,valueflag_cd=list(name = 'vf', type = 'string', input = 'text'
                       ,label='Outside Reference Range (valueflag_cd)'
                       ,criteria=quote(isTRUE(valueflag_cd>0)))
    ,quantity_num=list(name='qt',type='double'
                       ,label='Quantity (quantity_num)'
                       ,criteria=quote(isTRUE(quantity_num>0)))
    ,units_cd=list(name = 'un', type = 'string', input = 'text'
                   ,label='Unit of Measure (units_cd)'
                   ,criteria=quote(isTRUE(units_cd>0)))
    ,location_cd=list(name = 'lc', type = 'string', input = 'text'
                      ,label='Location Code (location_cd)'
                      ,criteria=quote(isTRUE(location_cd>0)))
    ,confidence_num=list(name='cf',type='double'
                         ,label='Confidence Level (confidence_num)'
                         ,criteria=quote(isTRUE(confidence_num>0))));
  
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
    message('\n*** dfmeta created ***\n');
    
    # data for populating UI and recording choices
    rv$dfinfolist <- sapply(rvp$dfmeta$inhead
                            ,function(ii) {
                              cleanDFCols(ii,rvp$dfmeta$incols)
                              },simplify=F);
    message('\n*** dfinfolist created ***\n');
    
    # all possible Add/Update names and chosen divs
    # rv$divIDs <- do.call(
    #   rbind,sapply(rv$dfinfolist,function(xx){
    #     if(!xx$as_is_col) data.frame(chosen=xx$divIDchosen
    #                                 ,addbid=sapply(xx$available
    #                                                ,function(yy)yy$addbid)
    #                                 ,stringsAsFactors = F)},simplify=F));
    # baseline button values ...no longer needed?
    isolate({divIDs <- setNames(data.frame(t(do.call(
      cbind,sapply(rv$dfinfolist,function(xx) {
        if (length(xx$available)>0){
                              sapply(xx$available,function(yy) {
                                with(yy
                                     ,cbind(parent_name,own_name
                                            ,addbid))})
                              }}))),stringsAsFactors = F)
      ,c('incolid','availableid','addbid'));
    divIDs$chosenid <- paste0('chosen-',divIDs$incolid);
    });
    
    message('\n*** divIDs created ***\n');
    
    # This is the part that detects clicks on the Add/Update buttons
    # Have to wrap the expr argument in substitute because otherwise
    # it doesn't correctly read the ii value. Wierd.
    isolate({for(ii in divIDs$addbid){
      ids <- subset(divIDs,addbid==ii);
      eval(substitute(onclick(xx
                              ,addChosen(yy$incolid[1]
                                         ,yy$availableid[1]
                                         ,rv,input)
                              ,add=T),env=list(xx=ii,yy=ids)))};
    });
    
    # Save the divIDs for later access
    rv$divIDs <- divIDs;

        # create column controls
    infodivs <- bs_accordion('infodivs');
    infodivs <- bs_set_opts(infodivs,use_heading_link=T);
    for(ii in rvp$dfmeta$inhead){
      isolate({
        if(rv$dfinfolist[[ii]]$as_is_col){
          infodivs <- htmltools::tagAppendChild(
            infodivs
            ,div(class='panel panel-default pn-colstatic'
                 ,div(class='panel-heading',ii)
                 ,div(class='panel-body',rv$dfinfolist[[ii]]$divfull)));
        } else {
          infodivs<-bs_append(infodivs,ii,rv$dfinfolist[[ii]]$divfull);
        }
        });
    }
    # Not quite working, but seems on the right track: making the static columns
    # back to non-accordion
    #
    # for(ii in setdiff(names(rv$dfinfolist),rv$divIDs$incolid)) {
    # runjs(sprintf("$('#%s').parent().removeClass('panel-collapse collapse in').parent().show()",ii))
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
    # make the chosen divs sortable and register inputs
    for(ii in unique(divIDs$chosenid)){
      eval(substitute(sortableWatcher(ii)))};
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
      for(ii in unique(rv$divIDs$chosenid)){
        eval(substitute(sortableWatcher(ii)))};
      print('Running sortableWatcher');
      runjs("
if( $('[id^=chosen-].ui-sortable').length == 0 ) {
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
  
  observeEvent(input$activecolid,{
    if(input$activecolid=='(none selected)'){
      disable('makecustom');hide('customui');} else {
        # get the non-empty fields from colmeta
        # use them to generate updated querybuilder
        # use them to generate updated multiselect
        # 
        # TODO elsewhere:
        #   delimiter for multiselect
        #   aggregator menu 
        #   save button 
        enable('makecustom')}});
  
  # make sure custom rules have names that are safe, legal, 
  # and unique
  observeEvent(input$customTrName,{
    cleanedName <- gsub('[_.]+','_',make.names(input$customTrName));
    updateTextInput(session,'customTrName'
                    ,value=tail(gsub('[_.]+','_'
                                     ,make.names(c(names(rvp$dfmeta$rules)
                                                   ,cleanedName)
                                                 ,unique = T)),1));
    });
  
  # offer a choice of columns from which to chose the ones that will have
  # access to this transform. Only fields available in all these columns
  # will be options in the transform
  output$customWhichCols <- renderUI(
    if(input$choosewait==0){
      selectizeInput('customSelCols'
                     ,label='Select the main column or columns in your data for
                             which this transformation should be available:'
                     ,choices=unique(rv$divIDs$incolid),multiple=T)} else {
                       span()});
  
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
    })
  
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
  # present user with choice of fields
  #output$customWhichFields <- renderUI({
  #  req(rv$currentFilterlist);
 #  observeEvent(rv$currentFilterlist,{
 #    validchoices <- names(rv$currentFilterList);
 #    #if(length(rv$currentFilterList)==0) span() else {
 #    cat('\nUpdating customWhichFields with',validchoices,'\n');
 #    out <- selectizeInput('customSelFields'
 #                  ,label='Select the field or fields you wish this
 #                  transformation to return:',multiple=T
 #                  ,choices=validchoices);
 #      
 #  #});
 #    output$customWhichFields <- renderUI(out);
 # #     };
 #    });
  observeEvent(input$customCancel,{
    # reset to empty values
    updateSelectizeInput(session,'customSelCols',selected=character(0));
    updateTextAreaInput(session,'customTrDesc',value='');
    updateTextInput(session,'customTrName',value = 'custom');
  })

  observeEvent(input$customSave,{
    forincols <- input$customSelCols;
    nametemplate<-paste0('{0}_',trname <- input$customTrName);
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
    isolate({
      # add to the master rules list
      rvp$dfmeta$rules[[trname]]<-transform;
      # for each eligible main column...
      for (ii in forincols) {
        # prepar it
        iiavailable <- flattenRules(ii,list(transform))[[1]];
        # insert it into the available list for that column
        rv$dfinfolist[[ii]]$available[[iiavailable$own_name]]<-iiavailable;
        # add to the available UI divs
        insertUI(paste0("#avail-",ii,">div"),'beforeEnd'
                 , withHtmlTemplate(iiavailable,templates$divavailable)
                 ,immediate=T);
        # update the divIDs data.frame
        rv$divIDs<-rbind(rv$divIDs
                         ,iivals<-with(iiavailable
                               ,c(parent_name,own_name,addbid
                                  ,paste0('chosen-',parent_name))));
        # instrument the Add/Update
        onclick(addbid
                ,addChosen(iivals$incolid[1]
                           ,iivals$availableid[1]
                           ,rv,input),add=T);
        }
        
      });
  });

  observeEvent(input$debug,{
    req(rv$dfinfolist);
    t_incolid <- names(rv$dfinfolist)[42];
    t_dat <- rv$dfinfolist[[t_incolid]];
    
    browser();
  });
  
  output$test <- renderUI({print('rendering test');rv$uitest});

})
