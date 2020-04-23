# load packages
library(tidyverse)
library(lubridate)
library(parallel)
library(kableExtra)
library(gdata)
library(MCPanel)
library(glmnet)
library(scales)
library(ggthemes)
library(pbapply)

# read in files
crsp <- read_csv(here::here("Data", "crsp.csv"))
sp500 <- read_csv(here::here("Data", "sp500.csv"))
ff <- read_csv(here::here("Data", "ff.csv"))

#########################################################################################################
### MAKE SIM DATA  ######################################################################################
#########################################################################################################

# Set up simulation. 1000 portfolios of 50 securities.
# Constraints = no missing returns from -19 to +10. 
# Need at least 50 observations from -249 to -11. 

# First, get the 1,000 dates that will determine the portfolio
# get list of dates with at leat 249 pre-obs
dates <- crsp %>% 
  # get unique dates
  select(date) %>% 
  unique() %>% 
  # remove the bottom 249 because you need at least that many pre-obs
  slice(-c(1:249, (nrow(.) - 11):nrow(.))) %>% 
  pull(date)

# sample randomly the dates
# set a seed
set.seed(30291640)
rand_dates <- sample(dates, 1000)

# get the unique trading dates
trade_dates <- unique(crsp$date)

# function to determine relative dates
rel_date <- function(date0, i) {
  t0 <- which(trade_dates == date0)
  trade_dates[t0 + i]
}

# function to make returns in log format
makelog <- function(x) {
  log(x + 1)
}

# keep just the variables we need
crsp <- crsp %>%
  # drop missing returns
  filter(!is.na(ret)) %>%
  select(date, permno, prc, ret, hsiccd) %>%
  left_join(., sp500 %>%
              rename(date = caldt, mkt = sprtrn) %>%
              select(date, mkt)) %>%
  left_join(., ff %>% select(date, smb, hml, umd)) %>%
  # make returns log
  mutate_at(c("ret", "mkt", "smb", "hml", "umd"), makelog) %>%
  # get 3 digit sic
  mutate(hsiccd_3 = substr(hsiccd, 1, 3),
         hsiccd_2 = substr(hsiccd, 1, 2))

# make function to make our portfolios
get_portfolio <- function(i) {
  set.seed(30291640)

  # determine days +10, -10, -11, -19, and -249 relative to randomly selected dates
  d_0 <- rand_dates[i]
  d_plus_10 <- rel_date(d_0, 10)
  d_minus_10 <- rel_date(d_0, -10)
  d_minus_11 <- rel_date(d_0, -11)
  d_minus_19 <- rel_date(d_0, -19)
  d_minus_249 <- rel_date(d_0, -249)

  # get set of potential firms to sample from
  # first make smaller dataset to run faster
  small_dt <- crsp %>% filter(date %>% between(d_minus_249, d_plus_10))

  # get potential firms to randomly select from
  potential_firms <- small_dt %>%
    # filter on sic code
    filter(!(hsiccd %in% c(6726, 6798, 9999, NA)) & nchar(hsiccd) > 3) %>%
    # enforce restrictions described above
    group_by(permno) %>%
    mutate(flag = ifelse(
      # full set of dates in -19:10
      lead(date, 10) == d_plus_10 & lag(date, 19) == d_minus_19 &
        # at least 50 obs in -249:-11
        length(date[date %>% between(d_minus_249, d_minus_11)]) >= 50, 1, 0)) %>%
    # keep just the event date and firms that satisfy the restriction
    ungroup() %>%
    filter(date == d_0 & flag == 1) %>%
    # pull the firm identifiers
    pull(permno) %>%
    unique()

  # filter potential firms by whether they have at least 5 sic3 peers, and flag for whether
  # we should use sic 4 or sic 3
  k <- 0
  firms <- tibble(permno = numeric(), flag = integer())

  # loop through till we hit 50 that we can use
  while (k < 50) {

    # sample a firm
    p <- sample(potential_firms, 1, replace = FALSE)
    # reset potential firms
    potential_firms <- potential_firms[potential_firms != p]

    # get dates
    non_na_dates <- small_dt %>% filter(permno == p) %>% pull(date)
    length_dates <- length(non_na_dates)

    # get sic industry codes
    sic <- small_dt %>% filter(permno == p) %>% slice(1) %>% pull(hsiccd)
    sic3 <- small_dt %>% filter(permno == p) %>% slice(1) %>% pull(hsiccd_3)

    # number sic3
    num_3 <- small_dt %>%
      filter(permno != p & date %in% non_na_dates & hsiccd_3 == sic3) %>%
      group_by(permno) %>%
      count %>%
      filter(n == length_dates) %>%
      pull(permno)

    # if less than 5 in sic3, skip
    if (length(num_3) < 5) {
      next
    }

    k <- k + 1

    # number sic4
    num_4 <- small_dt %>%
      filter(permno != p & date %in% non_na_dates & hsiccd == sic) %>%
      group_by(permno) %>%
      count %>%
      filter(n == length_dates) %>%
      pull(permno)

    firms <- bind_rows(firms, tibble(permno = p, flag = ifelse(length(num_4) < 8, 1, 0),
                                     peers = ifelse(length(num_4) < 8, list(num_3), list(num_4))))
  }

  # make data with the 50 firms -249:10, and bring in mkt return and ff variables
  small_dt <- small_dt %>%
    filter(permno %in% firms$permno) %>%
    left_join(firms)

  # making expanded grid to do cross correlations later
  expanded_data <- expand.grid(permno = unique(small_dt$permno), date = unique(small_dt$date)) %>%
    arrange(permno) %>%
    # bring in our data
    left_join(small_dt) %>%
    # make a variable equal to the portfolio number and date 0
    mutate(portfolio = i,
           date_0 = d_0) %>%
    # keep variables we need and rearrange
    as_tibble()

  return(expanded_data)
}

# grab the potfolio specific data
# parallelize and run
cl <- makeCluster(10)

## export data and functions to the clusters
clusterExport(cl, c("crsp", "sp500", "ff", "trade_dates", "rand_dates",
                    "get_portfolio", "rel_date", "makelog"))

# export needed programs
clusterEvalQ(cl, c(library(tidyverse), library(lubridate), library(here)))

## run the command on the clusters
sims <-  do.call(rbind, pblapply(cl = cl, X = 1:1000, FUN = get_portfolio))
stopCluster(cl)

# save intermediate file
saveRDS(sims, file = here::here("Output", "portfolios.rds"))
sims <- read_rds(here::here("Output", "portfolios.rds"))

# function to estimate MM MODEL by group
MM <- function(dt, d1, d2) {
  mod <- lm(ret ~ mkt + smb + hml + umd,
            data = dt %>% dplyr::filter(date %>% between(d1, d2)))
  return(predict(mod, dt))
}

# function to estimate MMPI model
MMPI <- function(dt, d1, d2) {
  mod <- lm(ret ~ mkt + smb + hml + umd + peer_index,
            data = dt %>% dplyr::filter(date %>% between(d1, d2)))
  return(predict(mod, dt))
}

# Imbens Doudchenko Method
ID <- function(dt, d1) {

  # which dates are we estimating
  missing <- which(is.na(dt$ret) | dt$date > d1)

  # reformate returns
  returns <- dt %>% select(-date) %>% select(ret, everything())

  # make mask matrix
  mask <- matrix(1,  ncol = ncol(returns), nrow = nrow(returns))
  mask[missing, 1] <- 0

  # set seed - for some reason the results for mod 20 aren't staying constant
  set.seed(53875576)

  # run the prediction
  en <- t(en_mp_rows(t(returns), t(mask), num_folds = 10))

  # output results
  return(en[, 1])
}

# ENR-U
ENR_U <- function(dt, d1, d2){

  # number of days in estimation period
  dateobs <- which(dt$date >= d1 & dt$date <= d2 & !is.na(dt$ret))
  nn <- length(dateobs)

  # make a constant folds matrix for the cross-validation (10 samples)
  set.seed(53875576)
  folds <- sapply(1:10, function (i) {sample(rep(1:10, 24)[1:nn], replace = FALSE)})

  # make matrices to feed in to glmnet
  Y_est <- dt$ret[dateobs] %>% as.matrix()
  X_est <- dt[dateobs, -c(1:2)] %>% as.matrix()
  X <- dt[, -c(1:2)] %>% as.matrix()

  # function to get the minimizing values by alpha
  bya <- function(a) {
    
    # subfunction to calculate by k-fold
    byfold <- function(j) {
      
      # estimate cross validated glmnet on fold with alpha = a and get the cv error by lambda
      tibble(a = a,
             fold = j,
             lambda = cv.glmnet(X_est, Y_est, foldid = folds[, j], alpha = a)$lambda,
             cv = cv.glmnet(X_est, Y_est, foldid = folds[, j], alpha = a)$cvm)
    }
    # estimate for folds 1:10
    data_a <- map_dfr(1:10, byfold)
  }
  # estimate over a in 0 to 1, by 0.1
  full_mat <- map_dfr(seq(0, 1, by = 0.1), bya)
  
  # average over the set of k-folds and get the values of alpha, lambda that minimze mean MSE
  table <- full_mat %>% 
    group_by(a, lambda) %>% 
    summarize(mean = mean(cv)) %>% 
    ungroup() %>% 
    arrange(mean) %>% 
    slice(1)
  
  return(as.numeric(predict(glmnet(X_est, Y_est, alpha = table$a), 
                            X, s = table$lambda, exact = T)))
}

# lasso model
LASSO <- function(dt, d1, d2) {

  # number of days in estimation period
  dateobs <- which(dt$date >= d1 & dt$date <= d2 & !is.na(dt$ret))
  nn <- length(dateobs)

  # make a constant folds matrix for the cross-validation (10 samples)
  set.seed(53875576)
  folds <- sapply(1:10, function (i) {sample(rep(1:10, 24)[1:nn], replace = FALSE)})

  # make matrices to feed in to glmnet
  Y_est <- dt$ret[dateobs] %>% as.matrix()
  X_est <- dt[dateobs, -c(1:2)] %>% as.matrix()
  X <- dt[, -c(1:2)] %>% as.matrix()

  # function to calculate by k-fold
  byfold <- function(j) {
    
    # estimate cross validated glmnet on fold with alpha = 1 and get the cv error by lambda
    tibble(fold = j,
           lambda = cv.glmnet(X_est, Y_est, foldid = folds[, j], alpha = 1)$lambda,
           cv = cv.glmnet(X_est, Y_est, foldid = folds[, j], alpha = 1)$cvm)
  }
  
  # estimate over a in 0 to 1, by 0.1
  full_mat <- map_dfr(1:10, byfold)
  
  # average over the set of k-folds and get the values of alpha, lambda that minimze mean MSE
  table <- full_mat %>% 
    group_by(lambda) %>% 
    summarize(mean = mean(cv)) %>% 
    ungroup() %>% 
    arrange(mean) %>% 
    slice(1)
  
  return(as.numeric(predict(glmnet(X_est, Y_est, alpha = 1), 
                            X, s = table$lambda, exact = T)))
}

# function to calculate log avereage
log_avg <- function(x) log(mean(exp(x) - 1) + 1)

# function to estimate our five models
estimate_models <- function(dt) {

  # get min non-missing obs
  min <- which(!is.na(dt$ret))[1]
  # identify the permno
  p <- dt$permno[1]
  # identify the peers
  peers <- dt$peers[[min]]
  # dates
  dates <- dt$date
  # estimation window
  min_estimation_date <- dt$date[min]
  max_estimation_date <- rel_date(dt$date_0[1], -11)

  # get data in format we need it
  returns <- crsp %>%
    # keep just our days and permnos
    filter(permno %in% c(p, peers) & date %in% dates) %>%
    # kepe just the variables we need
    select(date, permno, ret, mkt, smb, hml, umd) %>%
    # identify the target firm and set firmid to 1 so we can put it first
    mutate(permno = replace(permno, permno == p, 1)) %>%
    # make peer index
    group_by(date) %>%
    mutate(peer_index = log_avg(ret[which(permno != 1)])) %>%
    ungroup() %>%
    # reclassify the data as wide
    pivot_wider(names_from = permno, values_from = ret, names_prefix = "p_") %>%
    # reset the name for the target firms stock to ret
    rename(ret = p_1) %>%
    # make peer index equally weighted return
    select(date, ret, everything()) %>%
    arrange(date)

  # estimate models
  tibble(MM = MM(returns, min_estimation_date, max_estimation_date),
         MMPI = MMPI(returns, min_estimation_date, max_estimation_date),
         ID = ID(returns, max_estimation_date),
         ENR_U = ENR_U(returns, min_estimation_date, max_estimation_date),
         LASSO = LASSO(returns, min_estimation_date, max_estimation_date))
}

# Estimate over our 1000 portfolios of 50 stocks
# function by simulation
run_sim <- function(i) {
  sims %>% filter(portfolio == i) %>%
    group_by(permno) %>%
    do(estimate_models(.)) %>%
    mutate(simulation = i)
}

# parallelize and run
cl <- makeCluster(10)

## export data and functions to the clusters
clusterExport(cl, c("crsp", "sims", "MM", "MMPI", "ID", "ENR_U", "LASSO",
                    "estimate_models", "run_sim", "rel_date",
                    "trade_dates", "log_avg"))

# export needed programs
clusterEvalQ(cl, c(library(tidyverse), library(lubridate), library(here),
                   library(MCPanel), library(glmnet)))
  
## run the command on the clusters
out <- do.call(rbind, pblapply(cl = cl, X = 1:1000, FUN = run_sim))
stopCluster(cl)
saveRDS(out, file = here::here("Output", "out.rds"))

# bind the results to our simulation data and save
sims <- bind_cols(sims, out)

# save the simulated data
saveRDS(sims, file = here::here("Output", "cs_sims.rds"))
sims <- read_rds(here::here("Output", "cs_sims.rds"))
