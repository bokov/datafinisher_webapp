#import argparse,sys,os,csv,json,re #,ast (for literal_eval, not yet used)
import sys,os,csv
#from os import path

sys.path+=[os.getcwd()+'/datafinisher']

from df_fn import DFMeta,autosuggestor #,DFCol
#from df_fn import shortenwords,dropletters,makeTailUnq,ob2tag
from rules import rules2
# import pandas as pd (for DataFrame, not yet used)

# to allow ridiculously large cells
csv.field_size_limit(100000000)
