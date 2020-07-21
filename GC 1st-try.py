import statsmodels.api as sm
from statsmodels.tsa.stattools import grangercausalitytests
import pandas as pd
import numpy as np

dataA = pd.read_csv('/Users/xunuo/Desktop/A/output_pops.csv')
dataA.columns = ["S1","S2"]
dataA

#Null Hypothesis: 2nd species has no correlation to 1st species 
grangercausalitytests(dataA[['S1', 'S2']], maxlag=100)
#According to GC test, all P-value < 0.05, so we reject the Null hypothesis. 


