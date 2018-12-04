library(shiny); library(reticulate); library(readr);

shinyServer(function(input, output, session) {
  
  #py$sys$path <- c(py$sys$path,paste0(getwd(),'/datafinisher'));
  source_python('df_reticulate.py');
  
  rv <- reactiveValues(uitest=textInput('foo','Bar'));
  
  rvp <- reactiveValues();
  
  output$infile_prev <- renderDataTable({
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
    return(head(dat[-1,],200));
  });
  
  observeEvent(input$debug,{
    browser();
  })
  
  output$test <- renderUI({rv$uitest});

})
