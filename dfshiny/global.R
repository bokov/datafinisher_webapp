shinyapps <- file.exists('queryBuilder_0.0.2.9000.tar.gz');

# Let me just say that Winston Chang is a genius
# https://gist.github.com/wch/c3653fb39a00c63b33cf
# The if statement below is based on wch's work above
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

# Variables needed by both ui.R and server.R ----
hcol <- '#008c99';
# Do not leave as-is in a production environment! Either change it to a path
# where you intend input files to automatically be deposited or to something
# that will never exist on your system.
trusted_indir <- 'www'; 

c()
