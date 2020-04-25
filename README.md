# BaGel
Codes to replicate analysis in Baker &amp; Gelbach (2020)

1. Download Data.py - downloads all of the data for this study from the wrds server. You need to put in your account details. 

2. Clean Data.R - is the primary data cleaning script for the crsp data

3. Main_No_FF.R - this file runs the simulations for the codes from 2009-2019, without Fama-French/Carhart factors.

4. Main_FF.R - this file runs the simulations for 2009-2019 with FFC factors. 

5. No_FF_FC.R - this file runs the simulations for 1999-2019 without FFC factors.

6. FF_FC.R - this file runs the simulatiotns for 1999-2019 with FFC factors.

7. Cross-Sectional Sims.R - This file runs the cross-sectional event study simulations. 

8. Make Figures and Tables.R - This file creates all of the figures and tables in the paper with the results of the prior code.
