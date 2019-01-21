templates <- list(

 divavailable = '<div class="available-transform panel panel-body panel-default">
{{longname}}
<button class="btn action-button btn-success" id={{addbid}} type="button">Add/Update</button>{{xxsel}}<br/>
  <span class="annotation">{{ruledesc}}</span>
    </div>'
 
,multidivavailable = '<div id={{divIDavailable}}>{{innerDivs}}</div>'

,incolui='<div id={{divIDchosen}}><hr/>{{helpCh}}<strong>Chosen...</strong></div><hr/>
<div id={{divIDavailable}}>{{helpAv}}<strong>Available...</strong>
{{divavailable}}</div>
'

,divfull ='<div id={{incolid}}><strong>{{incolid}}:</strong><!--br/-->
  <span class="annotation">{{incoldesc}}</span>
  {{incolui}}
</div>
'

,divchosen ='<div id={{longname}} class="panel panel-body panel-default">
{{longname}}{{delbutton}}<br/><span class="annotation">{{ruledesc}}</span>
</div>
'
);

filterlist <- list(
  concept_cd=list(name = 'cc'
                  , type = 'string'
                  , input = 'selectize'
                  , label = 'Concept Code (concept_cd)'
                  , values=c()
                  , criteria = quote(isTRUE(ccd > 1)))
  ,modifier_cd=list(name = 'mc', type = 'string', input = 'text'
                    ,label='Modifier Code (modifier_cd)'
                    ,criteria=quote(isTRUE(mod > 0)))
  ,instance_num=list(name='ix',type='integer'
                     ,label='Instance Number (instance_num)'
                     ,criteria=quote(isTRUE(mxinsts>1)))
  ,valtype_cd=list(name = 'vt', type = 'string', input = 'text'
                   ,label='Value Type (valtype_cd)'
                   ,criteria=quote(isTRUE(valtype_cd>0)))
  ,tval_char=list(name = 'tc', type = 'string', input = 'text'
                  ,label='Text Val (tval_char)'
                  ,criteria=quote(isTRUE(tval_char>0)))
  ,nval_num=list(name='nv',type='double'
                 ,label='Numeric Value (nval_num)'
                 ,criteria=quote(isTRUE(nval_num>0)))
  ,valueflag_cd=list(name = 'vf', type = 'string', input = 'selectize'
                     ,label='Outside Reference Range (valueflag_cd)'
                     ,values=c('H','A','@','L')
                     ,criteria=quote(isTRUE(valueflag_cd>0)))
  ,quantity_num=list(name='qt',type='double'
                     ,label='Quantity (quantity_num)'
                     ,criteria=quote(isTRUE(quantity_num>0)))
  ,units_cd=list(name = 'un', type = 'string', input = 'text'
                 ,label='Unit of Measure (units_cd)'
                 ,criteria=quote(isTRUE(units_cd>0)))
  ,location_cd=list(name = 'lc', type = 'string', input = 'text'
                    ,label='Location Code (location_cd)'
                    ,criteria=quote(isTRUE(location_cd>0)))
  ,confidence_num=list(name='cf',type='double'
                       ,label='Confidence Level (confidence_num)'
                       ,criteria=quote(isTRUE(confidence_num>0))));

c()
