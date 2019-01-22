
.projlib <- 'R-lib';
.projrep <- 'https://cloud.r-project.org';
.projpkg <- c(
  'dplyr','bsplus','reticulate','readr','shinyjqui','shinyjs','shinyalert'
  ,'devtools'
)

if (!file.exists(.projlib)) dir.create(.projlib);
if(any(grepl(.projlib,.libPaths()))){
  .libPaths( c(normalizePath(.projlib), .libPaths()) );
}
for(ii in .projpkg){if(!require(ii)){
  install.packages(ii,lib='R-lib',repos = .projrep);
  library(ii);
  }};

print(ls(.projpkg));
c()
