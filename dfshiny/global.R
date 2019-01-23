shinyapps <- file.exists('shinyapps');
#if(file.exists('projlib.R')) source('projlib.R');
source('projlib.R');

# Let me just say that Winston Chang is a genius
# https://gist.github.com/wch/c3653fb39a00c63b33cf
# The if statement below is based on wch's work above
# if(shinyapps){
#   if (!file.exists("R-lib")) dir.create("R-lib");
#   .libPaths( c(normalizePath("R-lib/"), .libPaths()) );
#   install.packages('queryBuilder_0.0.2.9000.tar.gz',repos = NULL);
#   if (!do.call(require, list("queryBuilder"))) {
#     install.packages("queryBuilder_0.0.2.9000.tar.gz", repos = NULL
#                      , type = "source");}
# }

# Libraries needed by both ui.R and server.R ----
#library(shiny);
library(dplyr);
do.call(library, list("queryBuilder"));

# Variables needed by both ui.R and server.R ----
hcol <- '#008c99';
# Set the below to a path that Shiny has access to but the web user does NOT
# i.e. do not set it to 'www' or anything inside it
trusted_files <- 'trusted_files'; 
# above what file-size should DataFinisher zip a file prior to download?
zip_cutoff <- 20*1024^2; #20mb
# help text
source('www/docs/helptext.R');
c()
