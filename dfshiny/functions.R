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

addChosen <- function(incolid,availableid,userArgs=list(CC=NULL),input,...){
  obj <- py$dfmeta[incolid];
  # add object. If it already exists in the python backend, it 
  # will overwrite/update the existing values and return NULL 
  # If it doesn't already exist, it will return a list with all
  # the values needed to create the HTML below
  
  # First, obtain the user input if it exists
  # TODO: disable the accompanying Add/Update button if selectize is empty
  # if(missing(userArgs)){
  #   userArgs <- list(CC=input[[obj$rules[[availableid]]$selid]])};
  # 
  if(length(obj$rules[[availableid]]$args)>0 && missing(userArgs)){
    userArgs <- input[[obj$rules[[availableid]]$selid]];
    if(is.null(userArgs)){warning('
                                  User input required for this rule but not provided. Ignoring.'); 
      return();
    } else userArgs <- list(CC=userArgs);
  }
  
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
buildDFCols <- function(incolid,rulenames=T,helptext){
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
    # the separate available and chosen help IDs for this column
    hAv <- paste0('hAv',obj$incolid); hCh <- paste0('hCh',obj$incolid);
    withHtmlTemplate(obj,templates$incolui,divavailable=out1
                     ,helpAv=span(id=hAv,icon('question-circle'))
                     ,helpCh=span(id=hCh,icon('question-circle'))
    )};
  #if(!obj$as_is_col) browser();
  out3 <- withHtmlTemplate(obj,templates$divfull,incolui=out2);
  if(!obj$as_is_col){
    onclick(hAv,shinyalert(text = helptext$hDivAvailable
                           ,confirmButtonCol = hcol,html = T
                           ,className = 'dfHelp'));
    onclick(hCh,shinyalert(text = helptext$hDivChosen
                           ,confirmButtonCol = hcol,html = T
                           ,className = 'dfHelp'));
    jqui_sortable(ui=paste0('#',obj$divIDchosen)
                  ,options=list(axis='y',items='div'));
  };
  out3;
}

# Build an 'available' div for a single rule
buildRule <- function(rule,unique_codes,rulename,incolid
                      ,selclass='transform-argsel'
                      ,sellab='For the following codes:'
                      ,selhelpid=''
                      ,selhelp=helptext$hSelCodes
                      ,template=templates$divavailable){
  if(missing(rule)) rule <- py$dfmeta[incolid]$rules[rulename];
  if(missing(unique_codes)) {
    unique_codes <- py$dfmeta[rule$parent_name]$unique_codes;}
  rsel <- if(rule$split_by_code && length(unique_codes)>1){
    if(missing(selhelpid)) selhelpid <- paste0('h',rule$selid);
    sellab <- tagList(span(id=selhelpid,icon('question-circle')),sellab);
    onclick(selhelpid,shinyalert(text = selhelp,confirmButtonCol = hcol
                                 ,html = T,className = 'dfHelp'));
    div(class=selclass,selectizeInput(rule$selid,multiple=T,label=sellab
                                      ,choices=unique_codes))} else span();
  out<-withHtmlTemplate(rule,template,xxsel=rsel);
}

#' validNames: make sure a name is unique and has legal characters
validNames <- function(newname #,existingnames=c()
                       ,id,session=getDefaultReactiveDomain()){
  outname <- py$dfmeta$makeNameUnq(newname,'rulename'
                                   ,maxlen=as.integer(12))
  if(!is.null(id)) updateTextInput(session,id,value=outname);
  return(outname);
}    
