shinyapps <- file.exists('queryBuilder_0.0.2.9000.tar.gz');

if(shinyapps){
  if (!file.exists("R-lib")) dir.create("R-lib");
  .libPaths( c(normalizePath("R-lib/"), .libPaths()) );
  install.packages('queryBuilder_0.0.2.9000.tar.gz',repos = NULL);
  if (!do.call(require, list("queryBuilder"))) {
    install.packages("queryBuilder_0.0.2.9000.tar.gz", repos = NULL
                     , type = "source");}
}

# Libraries needed by both ui.R and server.R ----
library(shiny);library(dplyr);
do.call(library, list("queryBuilder"));
c()
