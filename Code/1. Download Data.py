#/Users/Andrew/anaconda3/envs/myenv/bin/python python3
import wrds
import os
import pandas as pd

# open link to wrds account - need to add your user name
db = wrds.Connection(wrds_username='')

# pull in sp500 returns
sp500 = db.raw_sql("SELECT caldt, sprtrn FROM crsp.dsp500")
sp500 = sp500.dropna()

# pull in fama french data
ff = db.raw_sql("SELECT * FROM ff.factors_daily")

# pull in the crsp variables we need for data after 2009
crsp = db.raw_sql("SELECT date, cusip, permno, permco, issuno, hsiccd, prc, vol, ret, shrout \
                  FROM crsp.dsf WHERE date >= '2009-01-01'")

# pull in the crsp variables we need for data after over volatile period - 1999-2009
crsp_fc = db.raw_sql("SELECT date, cusip, permno, permco, issuno, hsiccd, prc, vol, ret, shrout \
                  FROM crsp.dsf WHERE date >= '1999-01-01' AND date < '2009-01-01'")

# change file directory 
os.chdir('/Users/Andrew/Box Sync/Projects/Event Studies/Data')

# save the data as csvs to reload with R so we don't have to redo every time
sp500.to_csv('sp500.csv', index=False)
ff.to_csv('ff.csv', index=False)
crsp.to_csv('crsp.csv', index=False)
crsp_fc.to_csv('crsp_fc.csv', index=False)
