#import argparse,sys,os,csv,json,re #,ast (for literal_eval, not yet used)
import sys,os,csv,json
#from os import path

sys.path+=[os.getcwd()+'/datafinisher']

from df_fn import DFMeta,autosuggestor #,DFCol
if os.path.isfile('autosuggestor.json'):
      print('autosuggestor.json found in app directory')
      autosuggestor=json.loads(''.join([xx for xx in open('autosuggestor.json').read().split('\n') if not xx.startswith('#')]))
elif os.path.isfile('datafinisher/autosuggestor.json'):
      print('autosuggestor.json found in datafinisher subdirectory')
      autosuggestor=json.loads(''.join([xx for xx in open('datafinisher/autosuggestor.json').read().split('\n') if not xx.startswith('#')]))
else: print('No custom autosuggester files found')
#from df_fn import shortenwords,dropletters,makeTailUnq,ob2tag
from rules import rules
# import pandas as pd (for DataFrame, not yet used)

# to allow ridiculously large cells
csv.field_size_limit(100000000)
