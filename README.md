# datafinisher_webapp
Web front-end for modifying DataFinisher files

## Quick Start
First, clone this repo along with its dependency. In bash or some similar 
command line:
```
git clone git@github.com:bokov/datafinisher_webapp
cd datafinisher_webapp
git checkout refactor_00
git submodule update --init --recursive
R
```

Then, in an R session...
```
install.packages(c('shinyjqui','bsplus','reticulate','readr','shiny','dplyr'
                    ,'queryBuilder','shinyjs','shinyalert'))
# assuming you are in the top-level project directory...
shiny::runApp('dfshiny')
```

The hard part is not DataFinisher, really. The hard part is deploying
[DataBuilder](https://informatics.gpcnetwork.org/trac/Project/wiki/BuilderSaga)

## Here is what passes for an API at this stage...

These are all the calls that Shiny currently makes to DFMeta attributes:
```
py$dfmeta$data$dialect$delimiter;
py$dfmeta$inhead;
```
These are all the calls that Shiny currently makes to to four distinct DFMeta
methods, `getStatIDs`, `makeNameUnq`, `useDesignedRule`, and `getColIDs`.
```
py$dfmeta$getStatIDs();
py$dfmeta$makeNameUnq(newname,'rulename',maxlen=int(12))
py$dfmeta$userDesignedRule(newrule,rulename,as.list(input$customSelCols));

py$dfmeta$getColIDs(ids='divIDchosen');
py$dfmeta$getColIDs(ids='incolid');
py$dfmeta$getColIDs(asdicts=T,ids='incolid'
                    ,childids=c('rulename','addbid')
                    ,childtype='rules');
                    
```
No calls are being made to any Python object that isn't part of a DFMeta object
