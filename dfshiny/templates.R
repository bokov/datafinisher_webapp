templates <- list(

 divavailable = '<div class="available-transform panel panel-body panel-default">
{{own_name}}
<button class="btn action-button btn-success" id={{addbid}} type="button">Add/Update</button>{{xxsel}}<br/>
  <span class="annotation">{{ruledesc}}</span>
    </div>'
 
,multidivavailable = '<div id={{divIDavailable}}>{{ innerDivs }}</div>'

,incolui='<div id={{divIDchosen}}><b>Chosen:</b></div>
<div id={{divIDavailable}}><b>Available:</b>{{divavailable}}</div>
'

,divfull ='<div id={{incolid}}>{{ incolid }}<br/>
  <span class="annotation">{{incoldesc}}</span>
  {{incolui}}
</div>
'
);

c()
