import csv,json,re #,ast (for literal_eval, not yet used)
from os import path
from df_fn import DFMeta,DFCol,autosuggestor
from rules import rules2
# import pandas as pd (for DataFrame, not yet used)

# to allow ridiculously large cells
csv.field_size_limit(100000000)


def rdfmeta(myheader,rawmeta=None,rules=rules2,suggestions=autosuggestor,**kwargs):
	      out = DFMeta(myheader,rawmeta,rules,suggestions=suggestions,**kwargs)
	      return out.getHeaders(bycol=True)