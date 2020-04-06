# load packages
library(here)
library(lubridate)
library(glmnet)
library(parallel)
library(tidyverse)
library(quantreg)
library(grf)
library(caret)
library(hqreg)

# deal with package conflict
select <- dplyr::select

# set directory to data
setwd(here::here("Data"))
# read in files
files = paste(c("crsp", "crsp_fc", "sp500", "ff"), ".csv", sep = "")

for (k in 1:length(files)) {
  name = substr(files[k], 1, nchar(files[k]) - 4)
  assign(name, read_csv(paste(files[k])))
}

# make function to make data for both recent period and financial crisis period
make_data <- function(crsp) {
  
  # calculate log returns for the series and drop missing values and prices below $5
  crsp <- crsp %>% 
    # drop missing and make sure that price is > 5, make log return series
    filter(!is.na(ret) & (prc > 5 | prc < 0)) %>% mutate(log_ret = log(ret + 1))
  
  # make a log sp 500 series
  sp500 <- sp500 %>% mutate(log_sp500 = log(sprtrn + 1))
  
  # make log series for the fama french indices
  makelog <- function(x, na.rm = FALSE) (log(x + 1))
  ff <- ff %>% mutate_at(c("smb", "hml", "umd"), makelog)
  
  # bring in fama french data to sp500 500 data
  sp500 <- left_join(sp500, ff %>% select(date, smb, hml, umd), by = c("caldt" = "date"))
  
  # drop sic codes 6726, 6798, and 9999 which are investment trusts and miscellaneous
  # and also stocks that have three digit sic codes for some reason
  crsp <- crsp %>% 
    filter(!(hsiccd %in% c(6726, 6798, 9999, NA)) & nchar(hsiccd) > 3)
  
  # merge sp500 into our crsp data
  crsp <- left_join(crsp, sp500 %>% select(caldt, log_sp500, smb, hml, umd), by = c("date" = "caldt")) 
  
  # create three-digit sic codes based on leading three digits
  crsp <- crsp %>%
    mutate(hsiccd_3 = substr(hsiccd, 1, 3))
  
  # identify which trading date is 250 days prior for each date
  lag250 <- crsp %>% select(date) %>% unique() %>% mutate(lag250 = lag(date, 250))
  
  # merge back into crsp dataset
  crsp <- left_join(crsp, lag250)
  
  # create flag for if they have a full 250 trading series
  crsp <- crsp %>% 
    group_by(permno) %>% 
    mutate(full = ifelse(lag(date, 250) == lag250, 1, 0)) %>% 
    replace_na(list(full = 0)) %>% 
    ungroup()
  
  # get the number of peer firms and the peer equally weighted return index
  # for the 4 and 3 digit sic with full return series
  peer_ind4 <- crsp %>%
    # group by date, industry, and whether the stock has a full return series
    group_by(hsiccd, date, full) %>% 
    # get count, return, and log return
    summarize(npeers4 = n(),
              ewret4 = mean(ret, na.rm = TRUE),
              log_ewret4 = log(ewret4 + 1)) %>% 
    # keep only the full return series
    filter(full == 1) %>% 
    select(-full)
  
  # do the same thing for the three-sic industries
  peer_ind3 <- crsp %>% 
    group_by(hsiccd_3, date, full) %>% 
    summarize(npeers3 = n(),
              ewret3 = mean(ret, na.rm = TRUE),
              log_ewret3 = log(ewret3 + 1)) %>% 
    filter(full == 1) %>% 
    select(-full)
  
  # merge the three and four sic industry variables into crsp
  crsp <- left_join(crsp, peer_ind4) %>% left_join(., peer_ind3)
  
  # for any firms with less than 8 peers at SIC 4 digit, use SIC 3 digit
  crsp <- crsp %>%
    mutate(flag = ifelse(npeers4 < 8, 1, 0),
           ewret = ifelse(npeers4 < 8, ewret3, ewret4),
           log_ewret = ifelse(npeers4 < 8, log_ewret3, log_ewret4),
           npeers = ifelse(npeers4 < 8, npeers3, npeers4))
  
  # get a counter variable by stock id to make sure we have at least 
  # 250 trading dates prior to estimate the model over
  crsp <- crsp %>% 
    group_by(permno) %>% 
    mutate(ticker = 1:n()) %>% 
    ungroup()
  
  # get a smaller sample to randomly sample from 
  crsp2 <- crsp %>% 
    # keep just the variables we need
    select(date, permno, ticker, npeers3, hsiccd, hsiccd_3, flag, npeers, full) %>% 
    # keep only dates with >= 250 dates prior and at least 5 peers from sic3
    filter(ticker >= 251 & npeers3 >= 5 & full == 1) %>% 
    ungroup()
  
  # set seed number (from a 20 in my pocket)
  seedno <- 86635328
  set.seed(seedno)
  
  # randomly sample 10,000 stock days with replacement
  random_sample <- sample_n(crsp2, 10000, replace = TRUE)
  
  # drop extraneous variables to run faster
  crsp <- crsp %>% 
    select(date, permno, ticker, log_ret, log_sp500, log_ewret, hsiccd, hsiccd_3, full, prc)
  
  if (year(min(crsp$date)) == 2009) {
    # save
    saveRDS(random_sample, "random_sample.rds")
    saveRDS(crsp, "final_crsp.rds")
    saveRDS(sp500, "sp500.rds")
  } else if (year(min(crsp$date)) == 1999) {
    # save
    saveRDS(random_sample, "random_sample_fc.rds")
    saveRDS(crsp, "final_crsp_fc.rds")
    saveRDS(sp500, "sp500.rds")
  }
}

# run over our two data sets
make_data(crsp)
make_data(crsp_fc)

