#load packages
library(here)
library(lubridate)
library(glmnet)
library(parallel)
library(tidyverse)
library(quantreg)
library(caret)
library(pbapply)
library(MCPanel)

# deal with package conflict
select <- dplyr::select

# load our three data sets
random_sample <- read_rds(here::here("Data", "random_sample_fc.rds"))
crsp <- read_rds(here::here("Data", "final_crsp_fc.rds"))
sp500 <- read_rds(here::here("Data", "sp500.rds"))

# Models 1 - 2 (ols)
models_1_2 <- function(returns) {
  # make peer return
  returns_2 <- as.data.frame(
    cbind(returns,
          # peer return is the log of the average returns
          peer = apply(returns[, str_detect(colnames(returns), "comp_")], 1,  
                       function(x) log(mean(exp(x) - 1) + 1))))
  
  # run 1 and 2 factor ols
  reg1 <- lm(dep ~ mkt, 
             data = as.data.frame(returns_2[1:250, ]))
  reg2 <- lm(dep ~ mkt + peer, 
             data = as.data.frame(returns_2[1:250, ]))
  
  # save the residuals
  out <- bind_rows(
    tibble(
      model = rep(1, 251), 
      day = 1:251,
      actual = returns_2[1:251, 1], 
      pred = predict(reg1, returns_2), 
      resid = actual - pred
    ),
    tibble(
      model = rep(2, 251), 
      day = 1:251,
      actual = returns_2[1:251, 1], 
      pred = predict(reg2, returns_2), 
      resid = actual - pred
    )  
  )
  
  out
}

# function to calculate models 3-5 with different X covariate matrices and penalties
mods_3_5 <- function(Y, X, folds, penalty, mod){
  
  # function to calculate by alpha the minimizing lambda and MSE
  min_lam <- function(a) {
    
    # store lambda, mse values from our 10 k-folds
    lambda <- do.call(rbind, 
                      lapply(1:10, function(j) {
                        # run a cross-validated penalized regression with a on fold j
                        cv <- cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, j], alpha = a, penalty.factor = penalty)
                        # save in a dataframe and merge together
                        data.frame(cv$lambda, cv$cvm, j)
                      }
                      )
    )
    
    # Find the lambda and fold that minimizes MSE for each level of alpha
    min_lambda <- lambda %>% 
      # bring in the minmium standard error values
      left_join(., lambda %>% group_by(cv.lambda) %>% summarize(mean = mean(cv.cvm))) %>% 
      # by fold find the values of lambda that minimizes mean squared error
      group_by(j) %>% 
      arrange(cv.cvm) %>% 
      slice(1) %>% 
      ungroup() %>% 
      arrange(mean) %>% 
      slice(1) %>% 
      select(cv.lambda, j)
    
    # pull a, lambda*, the mse, and the fold
    c(a, min_lambda$cv.lambda, 
      min(cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, min_lambda$j],
                    alpha = a, penalty.factor = penalty)$cvm), min_lambda$j)
  }
  
  # get table by alpha of the minimizing values
  table <- do.call(rbind, lapply(seq(0.1, 1, 0.1), min_lam))
  
  # pull the out of sample squared prediction error from the best model
  # set colnames
  colnames(table) <- c("alpha", "lambda", "mse", "j")
  table <- as.data.frame(table) %>% 
    # get the global mean squared error
    arrange(mse) %>% 
    slice(1)
  
  out <- tibble(
    model = rep(mod, 251), 
    day = 1:251,
    actual = Y[1:251], 
    pred = predict(cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, table$j], alpha = table$alpha,
                             penalty.factor = penalty), X[1:251, ], s = table$lambda, exact = T), 
    resid = actual - pred
  )
  out
}

# model 6 - 1 peer index through regularization
mod_6 <- function(Y, X, folds, returns, mod) {
  
  # store lambda, mse values from our 10 k-folds
  lambda <- 
    do.call(rbind, 
            lapply(1:10, function(j) {
              cv <- cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, j], alpha = 1)
              data.frame(cv$lambda, cv$cvm, j)
            }
            )
    )
  
  # Find the lambda and fold that minimizes MSE
  min_lambda <- lambda %>% 
    left_join(., lambda %>% group_by(cv.lambda) %>% summarize(mean = mean(cv.cvm))) %>% 
    group_by(j) %>% 
    arrange(cv.cvm) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(mean) %>% 
    slice(1) %>% 
    select(cv.lambda, j)
  
  # get the beta coefficients from the minimizing regression
  betas <- predict(cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, min_lambda$j], alpha = 1),
                   type = "coefficient", s = "lambda.min", exact = T)
  
  # get nonzero columns
  cols <- which(betas[-1] != 0) + 1
  
  # create matrix with non-zero weighted firms if any
  if (length(cols) > 0) {
    XX <- cbind(1, 
                returns[1:251, "mkt"], 
                peer = apply(as.matrix(returns[1:251, cols], nrow = 251), 1,  
                             function(x) log(mean(exp(x) - 1) + 1))) %>% as.matrix()
  } else {
    XX <- cbind(1, 
                returns[1:251, "mkt"]) %>% as.matrix()
  }
  
  # get the beta coefficients
  betas = matrix(solve(t(XX[1:250, ])%*%XX[1:250, ])%*%t(XX[1:250, ])%*%Y[1:250])
  
  # get the predicted returns
  preds <- betas[1] + XX[, -1] %*% matrix(betas[2:length(betas)]) 
  
  # output matrix
  out <- tibble(
    model = rep(mod, 251), 
    day = 1:251,
    actual = Y[1:251], 
    pred = preds, 
    resid = actual - pred
  )
  out
}

# estimate models 3 - 6 on our return dataset
models_3_6 <- function(returns) {
  
  # outcome matrix
  Y <- returns[, 1] %>% as.matrix()
  
  # covariate matrix with peer firms and market and ff factors
  X <- returns[, -1] %>% as.matrix()
  
  # covariate matrix with peer firms but no market return or ff factors
  X2 <- returns[, str_detect(colnames(returns), "comp")] %>% as.matrix()
  
  # covariate matrix with a market, and peer return
  MKT_PEER <- cbind(returns[, "mkt"], 
                    apply(returns[, str_detect(colnames(returns), "comp")], 1,  function(x) log(mean(exp(x) - 1) + 1))) %>% 
    as.matrix()
  
  # run model 6 - regularization with CV on mkt, ff factors, and peer index
  mod3 <- mods_3_5(Y, MKT_PEER, folds, c(rep(1, ncol(MKT_PEER))), mod = 3)
  
  # run model 7 - regularization with CV on mkt, ff factor, and all firms
  mod4 <- mods_3_5(Y, X, folds, c(rep(1, ncol(X))), mod = 4)
  
  # run model 8 - regulation with CV on mkt and all firms, forcing inclusion of market
  mod5 <- mods_3_5(Y, X, folds, c(rep(1, ncol(X) - 1), 0), mod = 5)
  
  # run model 9 - create one index with non-zero weighted firms
  mod6 <- mod_6(Y, X2, folds, returns, mod = 6)
  
  # run all models
  bind_rows(mod3, mod4, mod5, mod6)
}

# time series cross validation modles (7-8)
models_7_8 <- function(returns) {
  
  # get modified returns data set with just mkt + peer return
  returns_2 <- as.data.frame(cbind(returns[, c("dep", "mkt")], 
                                   peer = apply(returns[, str_detect(colnames(returns), "comp")],
                                                1,  function(x) log(mean(exp(x) - 1) + 1))))
  
  # set up the training control method. Set a minimum estimation window of 50 days
  myTimeControl <- trainControl(method = "timeslice",
                                initialWindow = 50,
                                horizon = 1,
                                fixedWindow = FALSE)
  
  # manually create a hyperparameter grid. Test alpha [0.1, 1] and get the lambda estimation sequence
  # from glmnet (has a formula to calculate an effective range)
  grid <- do.call(rbind, lapply(seq(0.1, 1, 0.1),
                                function(a) {
                                  expand.grid(
                                    alpha = a,
                                    # get the lambda values that glmnet determines are necessary
                                    lambda = glmnet(as.matrix(returns_2[, -1]), 
                                                    as.matrix(returns_2[, 1]), alpha = a)$lambda)
                                }))
  
  # Train the model using our dataset                    
  glmnet.mod7 <- train(dep ~ mkt + peer,
                       data = returns_2[1:250, ],
                       method = "glmnet",
                       family = "gaussian",
                       trControl = myTimeControl,
                       tuneGrid = grid)
  
  # calculate the hyperparameter grid for all alphas for full model
  grid <- do.call(rbind, lapply(seq(0.1, 1, 0.1),
                                function(a) {
                                  expand.grid(
                                    alpha = a,
                                    lambda = glmnet(as.matrix(returns[, -1]), 
                                                    as.matrix(returns[, 1]), alpha = a)$lambda)
                                }))
  
  glmnet.mod8 <- train(dep ~ .,
                       data = returns[1:250, ],
                       method = "glmnet",
                       family = "gaussian",
                       trControl = myTimeControl,
                       tuneGrid = grid)
  #save the residuals
  out <- bind_rows(
    tibble(
      model = rep(7, 251), 
      day = 1:251,
      actual = returns_2[1:251, 1], 
      pred = predict(glmnet.mod7, returns_2[1:251, -1]), 
      resid = actual - pred),
    tibble(
      model = rep(8, 251), 
      day = 1:251,
      actual = returns[1:251, 1], 
      pred = predict(glmnet.mod8, returns[1:251, -1]), 
      resid = actual - pred)  
  )
  out
}

# model 9 Lasso
mod_9 <- function(returns) {
  
  # make design matrices
  X <- returns[, -1] %>% as.matrix()
  Y <- returns[, 1] %>% as.matrix()
  
  # store lambda, mse values from our 10 k-folds
  lambda <- do.call(rbind,
                    lapply(1:10, function(j) {
                      # run a cross-validated penalized regression with a on fold j
                      cv <- cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, j], alpha = 1)
                      # save in a dataframe and merge together
                      data.frame(cv$lambda, cv$cvm, j)
                    }
                    )
  )
  
  # pick fold based on the minimum MSE 
  jmin <- lambda %>% arrange(cv.cvm) %>% slice(1) %>% pull(j)
  preds <- predict(cv.glmnet(X[1:250, ], Y[1:250], foldid = folds[, jmin], alpha = 1),
                   X[1:251, ], s = "lambda.min", exact = T)
  
  # estimate model with the fold and minimum lambda
  out <- tibble(
    model = rep(9, 251), 
    day = 1:251,
    actual = Y[1:251], 
    pred = preds %>% as.numeric, 
    resid = actual - pred
  )
  out
}  

# model to do synth ala Imbens/Doudchenko with their coding (model 10)
mod_10 <- function(returns) {
  
  # make mask matrix (all 1s but 0 in one obs we don't have)
  mask <- matrix(1,  ncol = ncol(returns), nrow = nrow(returns))
  mask[251, 1] <- 0
  
  # run the prediction
  adh <- t(adh_mp_rows(t(returns), t(mask)))
  
  # output results
  out <- tibble(
    model = rep(10, 251), 
    day = 1:251,
    actual = returns[, 1], 
    pred = adh[, 1],
    resid = actual - pred
  )
  out
}

# elastic net from ID (MCPanel package) Model 11
mod_11 <- function(returns) {
  
  # make mask matrix 
  mask <- matrix(1,  ncol = ncol(returns), nrow = nrow(returns))
  mask[251, 1] <- 0
  
  # set seed
  set.seed(53875576)
  
  # run the prediction
  en <- t(en_mp_rows(t(returns), t(mask), num_folds = 10))
  
  # output results
  out <- tibble(
    model = rep(11, 251), 
    day = 1:251,
    actual = returns[, 1], 
    pred = en[, 1],
    resid = actual - pred
  )
  out
}

# create code to run simulation 
simulation <- function(i) {
  
  # set parameters: stock id, and dates from random sample
  firmid <- as.numeric(random_sample[i, 2])
  flag <- as.numeric(random_sample[i, 7])
  reldate <- as.numeric(random_sample[i, 3])
  d1 <- crsp[which(crsp$permno == firmid & 
                     crsp$ticker == reldate - 250), "date"] %>% pull()
  d2 <- random_sample[i, 1] %>% pull()
  
  # get a dataset with the individual firms in the peer group
  # use three digit sic if less than 8 peers, otherwise four digit sic
  # flag = 1 if has less than 8 peers
  if (flag == 1) {
    
    # get firms in three digit sic within date range with full return series
    firms <- crsp %>% 
      filter(hsiccd_3 == random_sample$hsiccd_3[i] & date == d2 & full == 1) %>% 
      pull(permno)
    
    # pull all returns for peer firms identified above between set dates
    returns <- crsp %>% filter(permno %in% firms & date >= d1 & date <= d2) %>% 
      select(date, permno, log_ret) %>%
      # identify the target firm and set firmid to 1 so we can put it first
      mutate(permno = replace(permno, permno == firmid, 1)) %>%
      arrange(permno, date) %>% 
      # reclassify the data as wide
      spread(permno, log_ret) %>% select(-date) %>% 
      # bring in market return variable
      cbind(sp500 %>% filter(caldt >= d1 & caldt <= d2) %>% pull(log_sp500)) %>% 
      # give sensible variable names
      setNames(c("dep", paste("comp", 1:(length(firms) - 1), sep = "_"), "mkt"))
    
  } else {
    
    # do same thing but with SIC 4 industry firms if more than 8 peer firms
    firms <- crsp %>% 
      filter(hsiccd == random_sample$hsiccd[i] & date == d2 & full == 1) %>% 
      pull(permno)
    
    returns <- crsp %>% filter(permno %in% firms & date >= d1 & date <= d2) %>% 
      select(date, permno, log_ret) %>%
      mutate(permno = replace(permno, permno == firmid, 1)) %>%
      arrange(permno, date) %>% 
      spread(permno, log_ret) %>% select(-date) %>% 
      cbind(sp500 %>% filter(caldt >= d1 & caldt <= d2) %>% pull(log_sp500)) %>% 
      setNames(c("dep", paste("comp", 1:(length(firms) - 1), sep = "_"), "mkt"))
  }
  
  # return squared error terms from the models
  bind_rows(
    models_1_2(returns),
    models_3_6(returns),
    models_7_8(returns),
    mod_9(returns),
    mod_10(returns),
    mod_11(returns)
  ) %>% 
    mutate(simulation = i,
           permno = firmid)
}

# set seed
set.seed(53875576)

# make a constant folds matrix for the cross-validation (10 samples)
# which will be used for all the following models
folds = sapply(1:10, function(i) {sample(rep(1:10, 25), replace = FALSE)})

# function to catch errors and continue with NAs
robustsim <- function (i) {
  return(tryCatch(simulation(i), error = function(e) 
    tibble(model = 0, day = 0, actual = 0, pred = 0, resid = 0, simulation = i, permno = 0)))
}

# parallelize and run over all of our observations
# set cores
cl <- makeCluster(10)

## export data and functions to the clusters
clusterExport(cl, c("crsp", "random_sample", "simulation", "folds", "robustsim",
                    "sp500", "models_1_2", "models_3_6", "models_7_8", 
                    "mod_9", "mod_10", "mod_11", "mods_3_5", "mod_6"))

# export needed programs
clusterEvalQ(cl, c(library(tidyverse), library(lubridate), library(here), library(quantreg), library(grf),
                   library(caret), library(glmnet), library(MCPanel)))

## run the command on the clusters
sims <- do.call(rbind, pblapply(cl = cl, X = 1:10000, FUN = robustsim))
stopCluster(cl)

# write the results to file
write_csv(sims, path = here::here("Output", "simulations_final_no_ff_fc.csv"))
saveRDS(sims, file = here::here("Output", "sims_no_ff_fc.rds"))
