# datafinisher_webapp
Web front-end for modifying DataFinisher files

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
