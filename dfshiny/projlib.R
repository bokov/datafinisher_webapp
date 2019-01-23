# local package directory whose existence triggers this script
.projlib <- normalizePath('R-lib');

# default CRAN repo
.projrep <- 'https://cloud.r-project.org';

# packages to load BEFORE adding .projlib to the library path
# (uncomment the line after next and put in package names)
.projpre <- c();
.projpre <- c('dplyr');


# packages to load AFTER all the installs (in the order given)
# (uncomment the line after next and put in package names)
.projpst <- c();
.projpst <- c('queryBuilder');

# packages to install locally
# (uncomment the line after next and put in package names)
.projins <- c();
#.projins <- c('htmlwidgets');

# packages to install from .tar.gz (named list)
# (uncomment the line after next and put in package names)
.projtgz <- c();
#.projtgz <- c(queryBuilder='queryBuilder_0.0.2.9000.tar.gz');

# where to log results
.projlog <- c();

# where to save log 
.projlsv <- 'www/projlog.txt';

if(file.exists(.projlib)){
  if(!.projlib %in% .libPaths()){
    message(sprintf('Adding %s to .libPaths()',.projlib));
    .libPaths( c(normalizePath(.projlib), .libPaths()) )} else {
      message(sprintf('%s already in .libPaths()',.projlib));
    };
  
  if(file.exists('projlibrun')){
    message(sprintf('.projlibrun file exists so bypassing installations'));
  } else {
    # install from repos in .projlib
    for(ii in .projins){
      message(sprintf('Attempting to install %s',ii));
      try(install.packages(ii,lib=.projlib,repos = .projrep));
      .projlog <- c(.projlog
                    ,sprintf('%s: %s',ii
                             ,if(do.call(require
                                         ,list(package=ii,character.only=T))){
        'SUCCESS'} else 'FAILURE'));
    }
    
    # install from .tar.gz in .projlib
    for(ii in names(.projtgz)){
      if(!file.exists(.projtgz[[ii]])){
        .projlog <- c(.projlog,sprintf('File not found: %s',.projtgz[[ii]]));
      } else {
        message(sprintf('Attempting to install %s from file',ii));
        try(install.packages(.projtgz[[ii]],repos = NULL,lib=.projlib));
        if(do.call(require,list(package=ii,character.only=T))){
          .projlog <- c(.projlog,sprintf('%s: SUCCESS',ii));
          } else {
            try(install.packages(.projtgz[[ii]],repos = NULL,lib=.projlib
                             ,type='source'));
            .projlog <- c(.projlog
                          ,sprintf('%s: %s',ii
                                   ,if(do.call(require(list(package=ii
                                                            ,character.only=T)))){
                                     'SUCCESS'} else 'FAILURE'));
          }
      }
    }
  }
  
  file.create('projlibrun');
  if(!is.null(.projlsv) && ! .projlsv %in% c(NA,'')){
    write(.projlog,file = .projlsv);
  }
  
  for(ii in .projpre) {
    message(sprintf('Pre-loading %s',ii));
    do.call(library,list(package=ii,character.only = T,lib=.libPaths()[-1]));}

  for(ii in .projpst){
    message(sprintf('Post-loading %s',ii));
    do.call(library,list(package=ii,character.only = T))}
}


c()
