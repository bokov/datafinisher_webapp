
.projlib <- 'R-lib';
.projrep <- 'https://cloud.r-project.org';
.projpkg <- c(
  'dplyr','bsplus','reticulate','readr','shinyjqui','shinyjs','shinyalert'
  ,'devtools'
);
.projlog <- c();

if (!file.exists(.projlib)) dir.create(.projlib);
if(any(grepl(.projlib,.libPaths()))){
  .libPaths( c(normalizePath(.projlib), .libPaths()) );
}
for(ii in .projpkg){if(!require(ii)){
  .projlog <- c(.projlog,sprintf('missing: %s',ii));
  iiout<-try(install.packages(ii,lib='R-lib',repos = .projrep));
  if(is(iiout,'try-error')){
    .projlog<-c(.projlog,sprintf('error: %s',attr(iiout,'condition')$message));
  }
  if(require(ii)) .projlog <- c(.projlog,sprintf('installed: %s',ii)) else {
    .projlog <- c(.projlog,sprintf('failed to install: %s',ii));
  }
  }};
if(require(devtools)){
  qbout <- try(devtools::install_github('harveyl888/queryBuilder'));
  if(is(qbout,'try-error')){
    .projlog <- c(.projlog,'installed: queryBuilder')} else {
      .projlog <- c(.projlog,sprintf('error: %s'
                                     ,attr(qbout,'condition')$message))
    }
  } else .projlog <- c(.projlog,'not devtools so no queryBuilder');


print(.projpkg);
c()
