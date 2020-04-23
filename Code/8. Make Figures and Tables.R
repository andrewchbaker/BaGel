# load programs
library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(scales)
library(broom)
library(ggthemes)
library(cowplot)
library(patchwork)
library(latex2exp)
library(parallel)

# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank()))

# set output path for overleaf
outpath <- ""

###################################################################################################
# TASK: Make TSCV plot                                        								                    #
###################################################################################################
# make the grid
data <- expand.grid(x = 1:26, y = 1:20) %>% 
  mutate(color = case_when(
    x + y == 27 ~ "red",
    x + y > 27 ~ "gray",
    x + y < 27 ~ "blue"
  ))

#plot
p1 <- data %>% 
  ggplot(aes(x = x, y = y, group = y, color = color)) + 
  geom_point(size = 2) + 
  geom_line(color = "black", size = 1/10) +
  scale_color_manual(values = c("blue", "gray", "red", "white")) + 
  labs(x = expression("Time" %->% ""),
       y = expression("" %<-% "Time")) +
  theme(axis.text = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())

# drop data
rm(data)

# save  
ggsave(filename =  paste(outpath, "TSCV.png", sep = "/"), p1, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Ratio Average Squared Residuals              								                              #
#####################################################################################################

# read in main results without FF factors
sims <- readr::read_rds(here::here("Output", "sims_no_ff.rds"))

# read in results with ff_factors
sims_ff <- readr::read_rds(here::here("Output", "sims_ff.rds"))

# specify the column names as the models
# the results without fama/french/carhart factors
no_ff <- c("MM", "MMPI", "ENR", "ENR-U", "ENR-FMI", "ENR-LEW", 
           "ENR-TSCV", "ENR-TSCV-U", "LASSO", "SYNTH", "ID")

# with fama/french/carhart factors
ff <- paste(no_ff, "FFC", sep = "-")

# bring in the names to the files
sims <- sims %>% left_join(., tibble(model = 1:11, name = no_ff))
sims_ff <- sims_ff %>% left_join(., tibble(model = 1:11, name = ff)) %>% arrange(model)

# combine the two sets of results
sims <- bind_rows(sims, sims_ff) 
rm(sims_ff)

# get the MSE from the MM model in the estimation period
scale_mse <- sims %>% 
  # just estimation window for the MM
  filter(name == "MM" & day < 251) %>% 
  # get mean squared error by simulation date
  group_by(simulation) %>% 
  summarize(scale_mse = mean(resid^2))

# merge the scaling denominator into the full dataset
sims <- left_join(sims, scale_mse)

# bring in our new standardized variables
plotdata <- sims %>% 
  # keep just the event date
  filter(day == 251) %>% 
  # generate squared prediction error and our new outcome variable
  mutate(squared_error = resid^2,
         val = resid^2 / scale_mse)

# plot the average ratio by model
p2 <- sims %>% 
  # keep only the event date
  filter(day == 251) %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    # Get the root of the model in a variable name
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # get the average and error bars by model
  group_by(name_root, type) %>% 
  summarize(mean_resid2 = mean(resid^2)) %>% 
  ungroup() %>% 
  mutate(mean_ratio = mean_resid2 / mean_resid2[which(name_root == "MM" & type == "No FFC")]) %>% 
  # specify the variable as an ordered factor to order vertically  
  group_by(name_root) %>% 
  mutate(root_ratio = -mean_ratio[which(type == "FFC")]) %>% 
  ungroup() %>% 
  mutate(name_root = fct_reorder(name_root, root_ratio)) %>% 
  # plot
  ggplot() +
  geom_point(aes(x = name_root,
                 y = mean_ratio, group = type, color = type, shape = type),
             position = position_dodge(width = 1), size = 2) + 
  labs(x = "Model", y = "Average Squared Residual / Average Squared Residual (MM)") + 
  theme(axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

#saveRDS(p8, file = here::here("Figs_Tables", "fig6.rds"))
ggsave(filename =  paste(outpath, "ratio_avg_squared_error.png", sep = "/"), p2, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Ratio of Squared Residual to MM RMSE                                             								                      #
#####################################################################################################
# get the ordering for the y axis of model ordering from ratio plot 
plot_levels <- sims %>% 
  # keep only the event date
  filter(day == 251) %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    # Get the root of the model in a variable name
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # get the average and error bars by model
  group_by(name_root, type) %>% 
  summarize(mean_resid2 = mean(resid^2)) %>% 
  ungroup() %>% 
  mutate(mean_ratio = mean_resid2 / mean_resid2[which(name_root == "MM" & type == "No FFC")]) %>% 
  # create a variable to order the axis for the plot by mean error in the base models
  group_by(name_root) %>%
  mutate(ordering = mean_ratio[which(type == "FFC")]) %>% 
  ungroup() %>% 
  dplyr::select(name_root, ordering) %>% 
  unique() %>% 
  dplyr::arrange(ordering)

# plot the two estimates side by side
p3 <- plotdata %>% 
  # get the averages by model
  group_by(name) %>% 
  # get the values we're going to plot
  summarize(
    ymin = mean(val) - 1.96*(sd(val)/sqrt(10000)),
    ymax = mean(val) + 1.96*(sd(val)/sqrt(10000)),
    mean = mean(val)) %>%
  ungroup() %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # bring in other ordering
  left_join(plot_levels) %>% 
  ggplot() +
  geom_pointrange(aes(x = reorder(name_root, -ordering),
                      y = mean, ymin = ymin, ymax = ymax, group = type, color = type, shape = type),
                  position = position_dodge(width = 1)) + 
  labs(x = "Model", y = "Squared Error / MM MSE") + 
  theme(axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

# save
ggsave(filename =  paste(outpath, "squared_normalized_error.png", sep = "/"), p3, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Figure 2  Power + Parametric + FFC                   								                      #
#####################################################################################################

# get the rmse and the critical values for model/simulation combinations
crit_values <- sims %>% 
  # look at just estimation period returns
  filter(day <= 250) %>% 
  # get the squared prediction errors
  mutate(val = resid^2) %>% 
  # by model/simulation calculate the critical statistics
  group_by(simulation, name) %>% 
  summarize(rmse = sqrt(mean(val)),
            lower = sort(resid)[25],
            upper = sort(resid)[226])

# the rejection frequencies we want to test
pseudo_ar <- c(0, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1)

# reduce the data to just the event dates (day 251)
sims_event_date <- sims %>% 
  filter(day == 251) %>% 
  # merge in the critical values
  left_join(., crit_values)

# make function to get rejection frequencies
rej_freq <- function(model_name, type) {
  # make vector to store results
  out <- rep(NA, length(pseudo_ar) + 1)
  # put the model name in the first place
  out[1] <- model_name
  # for each of the pseudo ARs add the value and test
  for (i in 1:length(pseudo_ar)) {
    # amount of AR to add
    k = pseudo_ar[i]
    # put in whether the pseudo excess return would be rejected by the test
    out[i + 1] <- sims_event_date %>% 
      # filter the specific model 
      filter(name == model_name) %>%
      # do either parametric or sq test
      mutate(sig =
               if (type == "parametric") {
                 ifelse((resid - k) / rmse < qt(0.10, df = 250), 1, 0) 
               }
             else if (type == "sq") {
               ifelse((resid - k) < lower, 1, 0)
             }) %>% 
      summarize(sig = mean(sig)) %>% 
      pull(sig)
  }
  out
}

#  estimate the parametric models and put into a table
parametric_table <- t(sapply(c(no_ff, ff), rej_freq, type = "parametric"))

# plot
p4 <- parametric_table %>% 
  as_tibble %>%
  setNames(c("Model", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "10%")) %>%  
  pivot_longer(-Model, names_to = "excess_return", values_to = "value") %>% 
  mutate(
    type = ifelse(substr(as.character(Model), nchar(as.character(Model)) - 3,nchar(as.character(Model))) == "-FFC", 
                  "FFC", 
                  "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    name_root = ifelse(type == "FFC", 
                       substr(as.character(Model), 1, nchar(as.character(Model)) - 4),
                       as.character(Model))) %>% 
  #merge in ordering
  left_join(., plot_levels) %>% 
  mutate(excess_return = factor(excess_return, levels = c("0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "10%"))) %>% 
  filter(Model %in% ff & excess_return %in% c("0%", "1%", "2%", "3%", "5%", "10%")) %>% 
  ggplot(aes(x = reorder(name_root, -ordering), y = as.numeric(value), group = excess_return, shape = excess_return, color = excess_return)) + 
  geom_point(size = 2) + 
  labs(shape = expression(delta),
       color = expression(delta)) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Model", y = "Rejection Frequency") + 
  scale_y_continuous(breaks = c(0, .1, .25, .5, .75, 1), labels = percent) +
  geom_hline(yintercept = 0.10, linetype = "dashed") +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

# save
ggsave(filename =  paste(outpath, "power_parametric.png", sep = "/"), p4, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Figure 3  Power + SQ + FFC                   								                              #
#####################################################################################################

# run the sq test rejections and put in table
sq_table <- t(sapply(c(no_ff, ff), rej_freq, type = "sq"))

# plot
p5 <- sq_table %>% 
  as_tibble %>%
  setNames(c("Model", "0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "10%")) %>%  
  pivot_longer(-Model, names_to = "excess_return", values_to = "value") %>% 
  mutate(
    type = ifelse(substr(as.character(Model), nchar(as.character(Model)) - 3,nchar(as.character(Model))) == "-FFC", 
                  "FFC", 
                  "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    name_root = ifelse(type == "FFC", 
                       substr(as.character(Model), 1, nchar(as.character(Model)) - 4),
                       as.character(Model))) %>% 
  #merge in ordering
  left_join(., plot_levels) %>% 
  mutate(excess_return = factor(excess_return, levels = c("0%", "0.5%", "1%", "2%", "3%", "4%", "5%", "10%"))) %>% 
  filter(Model %in% ff & excess_return %in% c("0%", "1%", "2%", "3%", "5%", "10%")) %>% 
  ggplot(aes(x = reorder(name_root, -ordering), y = as.numeric(value), group = excess_return, shape = excess_return, color = excess_return)) + 
  geom_point(size = 2) + 
  labs(shape = expression(delta),
       color = expression(delta)) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Model", y = "Rejection Frequency") + 
  scale_y_continuous(breaks = c(0, .1, .25, .5, .75, 1), labels = percent) +
  geom_hline(yintercept = 0.10, linetype = "dashed") +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

# save
ggsave(filename =  paste(outpath, "power_sq.png", sep = "/"), p5, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Average Residual                            								                              #
#####################################################################################################
# plot mean and confidence interval of the residual value by model
p6 <- sims %>% 
  # keep only the event date
  filter(day == 251) %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    # Get the root of the model in a variable name
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # specify the variable as an ordered factor to order vertically  
  mutate(name_root = factor(name_root, levels = plot_levels %>% arrange(-ordering) %>% pull(name_root))) %>% 
  # get the average and error bars by model
  group_by(name_root, type) %>% 
  summarize(mean = mean(resid),
            ymin = mean - 1.96*(sd(resid)/sqrt(10000)),
            ymax = mean + 1.96*(sd(resid)/sqrt(10000))) %>% 
  # plot
  ggplot() +
  geom_pointrange(aes(x = name_root,
                      y = mean, ymin = ymin, ymax = ymax, group = type, color = type, shape = type),
                  position = position_dodge(width = 1)) + 
  geom_hline(yintercept = 0) +
  labs(x = "Model", y = "Average Residual") + 
  theme(axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  coord_flip() + 
  scale_color_brewer(palette = 'Set1') + 
  scale_y_continuous(limits = c(-0.0115, 0.0115)) 

#sasve
ggsave(filename =  paste(outpath, "average_residual.png", sep = "/"), p6, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Ratio Average Squared Residuals FC Period    								                              #
#####################################################################################################
# read in main results without FF factors
sims <- readr::read_rds(here::here("Output", "sims_no_ff_fc.rds"))

# read in results with ff_factors
sims_ff <- readr::read_rds(here::here("Output", "sims_ff_fc.rds"))

# specify the column names as the models
# the results without fama/french/carhart factors
no_ff <- c("MM", "MMPI", "ENR", "ENR-U", "ENR-FMI", "ENR-LEW", 
           "ENR-TSCV", "ENR-TSCV-U", "LASSO", "SYNTH", "ID")

# with fama/french/carhart factors
ff <- paste(no_ff, "FFC", sep = "-")

# bring in the names to the files
sims <- sims %>% left_join(., tibble(model = 1:11, name = no_ff))
sims_ff <- sims_ff %>% left_join(., tibble(model = 1:11, name = ff)) %>% arrange(model)

# combine the two sets of results
sims <- bind_rows(sims, sims_ff) 
rm(sims_ff)

# get the MSE from the MM model in the estimation period
scale_mse <- sims %>% 
  # just estimation window for the MM
  filter(name == "MM" & day < 251) %>% 
  # get mean squared error by simulation date
  group_by(simulation) %>% 
  summarize(scale_mse = mean(resid^2))

# merge the scaling denominator into the full dataset
sims <- left_join(sims, scale_mse)

# bring in our new standardized variables
plotdata <- sims %>% 
  # keep just the event date
  filter(day == 251) %>% 
  # generate squared prediction error and our new outcome variable
  mutate(squared_error = resid^2,
         val = resid^2 / scale_mse)

# plot the average ratio by model
p7 <- sims %>% 
  # keep only the event date
  filter(day == 251) %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    # Get the root of the model in a variable name
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # get the average and error bars by model
  group_by(name_root, type) %>% 
  summarize(mean_resid2 = mean(resid^2)) %>% 
  ungroup() %>% 
  mutate(mean_ratio = mean_resid2 / mean_resid2[which(name_root == "MM" & type == "No FFC")]) %>% 
  # specify the variable as an ordered factor to order vertically  
  group_by(name_root) %>% 
  mutate(root_ratio = -mean_ratio[which(type == "FFC")]) %>% 
  ungroup() %>% 
  mutate(name_root = fct_reorder(name_root, root_ratio)) %>% 
  # plot
  ggplot() +
  geom_point(aes(x = name_root,
                 y = mean_ratio, group = type, color = type, shape = type),
             position = position_dodge(width = 1), size = 2) + 
  labs(x = "Model", y = "Average Squared Residual / Average Squared Residual (MM)") + 
  theme(axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

ggsave(filename =  paste(outpath, "ratio_avg_squared_error_fc.png", sep = "/"), p7, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Ratio of Squared Residual to MM RMSE FC
#####################################################################################################
# get the ordering for the y axis of model ordering from ratio plot 
plot_levels <- sims %>% 
  # keep only the event date
  filter(day == 251) %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    # Get the root of the model in a variable name
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # get the average and error bars by model
  group_by(name_root, type) %>% 
  summarize(mean_resid2 = mean(resid^2)) %>% 
  ungroup() %>% 
  mutate(mean_ratio = mean_resid2 / mean_resid2[which(name_root == "MM" & type == "No FFC")]) %>% 
  # create a variable to order the axis for the plot by mean error in the base models
  group_by(name_root) %>%
  mutate(ordering = mean_ratio[which(type == "FFC")]) %>% 
  ungroup() %>% 
  dplyr::select(name_root, ordering) %>% 
  unique() %>% 
  dplyr::arrange(ordering)

# plot the two estimates side by side
p8 <- plotdata %>% 
  # get the averages by model
  group_by(name) %>% 
  # get the values we're going to plot
  summarize(
    ymin = mean(val) - 1.96*(sd(val)/sqrt(10000)),
    ymax = mean(val) + 1.96*(sd(val)/sqrt(10000)),
    mean = mean(val)) %>%
  ungroup() %>% 
  # create variable with the type of model (FFC or No FFC) and the name root (e.g. MM, MMPI, etc.)
  mutate(
    type = ifelse(
      substr(as.character(name), nchar(as.character(name)) - 3,nchar(as.character(name))) == "-FFC", 
      "FFC", 
      "No FFC"),
    type = factor(type, levels = c("No FFC", "FFC")),
    name_root = ifelse(
      type == "FFC", 
      substr(as.character(name), 1, nchar(as.character(name)) - 4),
      as.character(name))) %>% 
  # bring in other ordering
  left_join(plot_levels) %>% 
  ggplot() +
  geom_pointrange(aes(x = reorder(name_root, -ordering),
                      y = mean, ymin = ymin, ymax = ymax, group = type, color = type, shape = type),
                  position = position_dodge(width = 1)) + 
  labs(x = "Model", y = "Squared Error / MM MSE") + 
  theme(axis.text.x = element_text(hjust = 1),
        axis.title = element_text(size = 12),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Set1') + 
  coord_flip()

# save
ggsave(filename =  paste(outpath, "squared_normalized_error_fc.png", sep = "/"), p8, dpi = 500,
       width = 7, height = 5, units = "in")

#####################################################################################################
# TASK: 	Sankey Plots     								                              
#####################################################################################################
#model names
no_ff <- c("MM", "MMPI", "ENR", "ENR-U", "ENR-FMI", "ENR-LEW", 
           "ENR-TSCV", "ENR-TSCV-U", "LASSO", "SYNTH", "ID")

# download files and merge
sims_no_ff <- readRDS(here::here("Output", "sims_no_ff.rds")) %>% 
  left_join(tibble(model = 1:11, name = no_ff))

sims_ff <- readRDS(here::here("Output", "sims_ff.rds")) %>% 
  left_join(tibble(model = 1:11, name = no_ff))

sims_no_ff_fc <- readRDS(here::here("Output", "sims_no_ff_fc.rds")) %>% 
  left_join(tibble(model = 1:11, name = no_ff))

sims_ff_fc <- readRDS(here::here("Output", "sims_ff_fc.rds")) %>% 
  left_join(tibble(model = 1:11, name = no_ff))

## Now make function to get Figure 2
# make function to get rankings - first for figure 1 methodology
ranking_fig1 <- function(data1, data2) {
  
  # make merge dataset
  merge <- tibble(model = 1:11,
                  name = no_ff)
  
  # merge in the names
  data1 <- data1 %>% left_join(merge)
  data2 <- data2 %>% left_join(merge)
  
  # make functions to get ranking for each data set
  get_rank <- function(data, timep) {
    
    # bring in our new standardized variables and get rankings
    data %>% 
      # keep just the event date
      filter(day == 251) %>% 
      # generate squared prediction error and our new outcome variable
      mutate(val_new = resid^2) %>% 
      # get the averages by model
      group_by(name, model) %>% 
      summarize(mean = mean(val_new)) %>% 
      ungroup() %>% 
      mutate(scale = mean / mean[which(model == 1)]) %>% 
      # create a rank variables
      mutate(rank = rank(scale),
             df = timep) %>% 
      arrange(rank)
  }  
  bind_rows(get_rank(data1, 0), get_rank(data2, 1))
}

# run over FF and No FF to get our datasets
fig1_no_ffc <- ranking_fig1(sims_no_ff, sims_no_ff_fc)
fig1_ffc <- ranking_fig1(sims_ff, sims_ff_fc)

# make function to get rankings - first for figure 1 methodology
ranking_fig2 <- function(data1, data2) {
  
  # make merge dataset
  merge <- tibble(model = 1:11,
                  name = no_ff)
  
  # merge in the names
  data1 <- data1 %>% left_join(merge)
  data2 <- data2 %>% left_join(merge)
  
  # make functions to get ranking for each data set
  get_rank <- function(data, timep) {
    
    # get the MSE from the MM model in the estimation period
    scale_mse <- data %>% 
      # just estimation window for the MM
      filter(model == 1 & day < 251) %>% 
      # get mean squared error by simulation date
      group_by(simulation) %>% 
      summarize(scale_mse = mean(resid^2))
    
    # bring in our new standardized variables and get rankings
    data %>% 
      # keep just the event date
      filter(day == 251) %>% 
      # merge in scaled values
      left_join(scale_mse) %>% 
      # generate squared prediction error and our new outcome variable
      mutate(val_new = resid^2 / scale_mse) %>% 
      # get the averages by model
      group_by(name) %>% 
      summarize(mean = mean(val_new)) %>% 
      # create a rank variables
      mutate(rank = rank(mean),
             df = timep) %>% 
      arrange(rank)
  }  
  bind_rows(get_rank(data1, 0), get_rank(data2, 1))
}

# run over FF and No FF to get our datasets
fig2_no_ffc <- ranking_fig2(sims_no_ff, sims_no_ff_fc)
fig2_ffc <- ranking_fig2(sims_ff, sims_ff_fc)

# plot function
makeplot <- function(data, figure) {
  
  data %>% 
    mutate(hjust = ifelse(df == 0, 1, 0)) %>% 
    ggplot(aes(x = as.factor(df), y = -rank, group = name)) + 
    geom_line(aes(color = name), show.legend = FALSE) +
    geom_text(aes(x = df + 1, label = name, hjust = hjust)) + 
    ggtitle(TeX(figure)) +
    scale_x_discrete(labels = c("0" = "1999-2009", "1" = "2009-2019")) + 
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text =  element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank())
}

# make our four plots and then plot them
plot1 <- makeplot(fig1_no_ffc, "\\widehat{R}_{oos}^k & No FFC")
plot2 <- makeplot(fig1_ffc, "\\widehat{R}_{oos}^k & FFC")
plot3 <- makeplot(fig2_no_ffc, "\\widehat{R}_{het}^k & No FFC")
plot4 <- makeplot(fig2_ffc, "\\widehat{R}_{het}^k & FFC")

p9 <- plot1 + plot2 + plot3 + plot4 + plot_layout(nrow = 2)

# save
ggsave(filename =  paste(outpath, "sankey_plots.png", sep = "/"), p9, dpi = 500,
       width = 8, height = 6, units = "in")

#####################################################################################################
# TASK: 	Time Period Diff Summary     								                              
#####################################################################################################
# vector for models
top_models <- c("ID", "ENR-U", "ENR-FMI", "ENR-TSCV-U", "LASSO")

# combine the four datasets together
plot2data <- bind_rows(fig2_ffc, fig2_no_ffc, 
                       fig1_ffc %>% select(colnames(fig2_ffc)), 
                       fig1_no_ffc %>% select(colnames(fig2_no_ffc))) %>% 
  # change rank
  mutate(rank = 12 - rank) %>%
  group_by(name) %>% 
  mutate(totalmean = mean(rank)) %>% 
  ungroup() %>% 
  # group into colors
  mutate(color = case_when(
    name %in% top_models ~ "g2",
    !(name %in% top_models) ~ "g1"
  ))

# plot
p10 <- plot2data %>% 
  ggplot(aes(x = fct_reorder(name, -totalmean), y = totalmean)) + 
  geom_point(aes(y = rank), color = "gray", position = "jitter", size = 4) + 
  geom_point(aes(x = name, y = totalmean, fill = color, group = color),
             shape = 23, size = 7) +
  labs(x = "Model", y = "Rank") + 
  scale_fill_brewer(palette = 'Set1') + 
  geom_vline(xintercept = 5.5, linetype = "dashed") + 
  geom_vline(xintercept = 18.5, linetype = "dashed") + 
  scale_y_continuous(breaks = c(11, 8, 5, 2, 0), 
                     labels = 12 - c(11, 8, 5, 2, 0)) + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = 'none') + 
  guides(x = guide_axis(n.dodge = 2))

# save
ggsave(filename =  paste(outpath, "model_ranks.png", sep = "/"), p10, dpi = 500,
       width = 8, height = 6, units = "in")

#####################################################################################################
# TASK: 	Cross-Sectional Results    								                              
####################################################################################################
# read in files
crsp <- read_csv(here::here("Data", "crsp.csv"))
sp500 <- read_csv(here::here("Data", "sp500.csv"))
ff <- read_csv(here::here("Data", "ff.csv"))

# read in cross-sectional simulation results
sims <- read_rds(here::here("Output", "cs_sims.rds"))

#########################################################################################################
### MAKE POWER PLOTS ####################################################################################
#########################################################################################################
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

# first make a dataset with all the variables we need so that we can change the imputed abnormal return
make_plot_data <- function(i) {
  
  # subset data
  expanded_data <- sims %>% 
    filter(portfolio == i) %>% 
    # make excess returns
    mutate(MM_RESID = ret - MM,
           MMPI_RESID = ret - MMPI,
           ENR_U_RESID = ret - ENR_U,
           ID_RESID = ret - ID,
           LASSO_RESID = ret - LASSO) %>% 
    # keep variables we need and reshape long
    select(date, permno, ret, portfolio, mkt, smb, hml, umd, peer_index, date_0, 
           MM_RESID, MMPI_RESID, ENR_U_RESID, ID_RESID, LASSO_RESID) %>% 
    # pivot longer so we can group this faster
    pivot_longer(MM_RESID:LASSO_RESID, names_to = "model", values_to = "ab_ret")
  
  # determine days +10, -10, -11, -19, and -249
  d_0 <-  expanded_data %>% pull(date_0) %>% unique()
  d_plus_10 <- rel_date(d_0, 10)
  d_minus_10 <- rel_date(d_0, -10)
  d_minus_11 <- rel_date(d_0, -11)
  d_minus_19 <- rel_date(d_0, -19)
  d_minus_249 <- rel_date(d_0, -249)
  
  # get date range by model - OLS and FF are the -249 to -11, and MARKET is -10:10 (fn table 3)
  # also relevant covariates
  date_range <- unique(expanded_data$date)[which(unique(expanded_data$date) %>% 
                                                   between(d_minus_249, d_minus_11))]
  
  # make function to get data by model 
  get_mod_data <- function(mod) {
    
    # get covariates by model
    if (mod == "MM_RESID") {
      covariates = c("mkt", "smb", "hml", "umd")
    } else {
      covariates = c("mkt", "smb", "hml", "umd", "peer_index")
    }
    
    # get the average residaul cross-correlation across securities
    avg_r_cc = expanded_data %>%
      filter(date %in% date_range) %>% 
      select(permno, date, model, ab_ret) %>%
      filter(model == mod) %>% 
      pivot_wider(id_cols = date, names_from = permno, values_from = ab_ret) %>% 
      select(-date) %>% 
      cor(use = "pairwise.complete.obs") %>% 
      gdata::lowerTriangle() %>% 
      mean(na.rm = TRUE)
    
    # get the d_t adjustment statistics by permno
    # get the big X covariate denominator
    XX <- expanded_data %>% 
      # filter
      filter(date %in% date_range) %>% 
      # grab covariates
      select(date, permno, covariates) %>% 
      drop_na() %>% 
      unique() %>% 
      mutate(constant = 1) %>% 
      select(constant, everything())
    
    # get little x covariate for numerator (on event date)
    x_t <- expanded_data %>% 
      filter(date == d_0) %>%
      select(date, permno, covariates) %>% 
      drop_na() %>% 
      unique() %>% 
      mutate(constant = 1) %>% 
      select(constant, everything())
    
    # get d_t by permno
    get_dt <- function(p) {
      X <- XX %>% filter(permno == p) %>% select(constant, covariates) %>% as.matrix()
      x <- x_t %>% filter(permno == p) %>% select(constant, covariates) %>% as.matrix()
      
      tibble(permno = p, 
             d_t = x%*%solve(t(X)%*%X)%*%t(x))
    }
    
    d_t <- map_dfr(unique(expanded_data$permno), get_dt)
    
    # output variables we need
    tibble(model = rep(mod, 50), 
           permno = d_t$permno,
           avg_r_cc = rep(avg_r_cc, 50),
           d_t = d_t$d_t)
  }
  
  # run over our five models
  mod_data <- map_dfr(unique(expanded_data$model), get_mod_data)
  
  # bring the avg correlation data back into main dataset
  expanded_data <- left_join(expanded_data, mod_data, by = c("permno", "model"))
  
  # get the regression residual SDs by firm and model
  si <- expanded_data %>% 
    group_by(permno, model) %>% 
    filter(date %in% date_range) %>% 
    summarize(si = sd(ab_ret, na.rm = TRUE))
  
  # merge back in
  expanded_data <- left_join(expanded_data, si, by = c("permno", "model"))
  
  # get scaled abnormal returns = AR_it / (s_i * sqrt(1 + d_t)) and keep just event dates
  expanded_data %>% 
    mutate(A_it = ab_ret / (si * sqrt(1 + d_t))) %>% 
    # keep just date 0
    filter(date == d_0)
}

# parallelize and run
cl <- makeCluster(4)

## export data and functions to the clusters
clusterExport(cl, c("sims", "trade_dates", "rand_dates", "rel_date", "make_plot_data"))

# export needed programs
clusterEvalQ(cl, c(library(tidyverse), library(lubridate), library(here)))

## run the command on the clusters
plot_data <- do.call(rbind, parLapply(cl, 1:1000, make_plot_data))
stopCluster(cl)

# make data with rejection frequencies as a function of added abnormal return
rej_data_c <- function(c) {
  
  plot_data %>% 
    # add in c
    mutate(ab_ret = ab_ret + c,
           A_it = ab_ret / (si * sqrt(1 + d_t)),
           c = c) %>% 
    # group by portfolio and model and calculate rejections
    group_by(portfolio, model, c) %>% 
    # calculate test statistics
    summarize(AAR = mean(ab_ret),
              UNADJ = (AAR * sqrt(50))/sqrt((1/50)*sum(si^2*(1 + d_t))),
              BMP = (mean(A_it)*sqrt(50))/sqrt((1/49) * sum((A_it - mean(A_it))^2)),
              ADJ_BMP = BMP * sqrt((1 - avg_r_cc[1])/(1 + 49*avg_r_cc[1])),
              # rejections
              UNADJ = ifelse(abs(UNADJ) >= 1.96, 1, 0),
              BMP = ifelse(abs(BMP) >= 1.96, 1, 0),
              ADJ_BMP = ifelse(abs(ADJ_BMP) >= 1.96, 1, 0)) %>% 
    pivot_longer(UNADJ:ADJ_BMP, names_to = "test", values_to = "rejection") %>% 
    group_by(model, test, c) %>% 
    summarize(rejection = mean(rejection))
}

# run from -.03 to 0.03
pp <- map_dfr(seq(-.02, .02, length.out = 100), rej_data_c)

# plot rejections holding test constant
rejection_plot <- pp %>% 
  ungroup() %>% 
  mutate(model = str_remove(model, "_RESID"),
         test = factor(test, levels = c("UNADJ", "BMP", "ADJ_BMP")),
         model = factor(model, levels = c("MM", "MMPI", "ENR_U", "LASSO", "ID"))) %>% 
  ggplot(aes(x = c, y = rejection, group = model, linetype = model, shape = model, color = model)) + 
  geom_point(size = 2) + geom_line(size = 2) + 
  scale_y_continuous(labels = percent) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = "Imputed Level of Abnormal Return", y = "Rejection Frequency") + 
  facet_wrap(~test, ncol = 3) + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20))

# plot rejections holding specification constant
rejection_plot2 <- pp %>% 
  ungroup() %>% 
  mutate(model = str_remove(model, "_RESID"),
         test = factor(test, levels = c("UNADJ", "BMP", "ADJ_BMP")),
         model = factor(model, levels = c("MM", "MMPI", "ENR_U", "LASSO", "ID"))) %>% 
  ggplot(aes(x = c, y = rejection, group = test, linetype = test, shape = test, color = test)) + 
  geom_point(size = 2) + geom_line(size = 2) + 
  scale_y_continuous(labels = percent) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = "Imputed Level of Abnormal Return", y = "Rejection Frequency") + 
  facet_wrap(~model, nrow = 3) + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 18))

#save
ggsave(filename =  paste(outpath, "rejection_plot_bytest.png", sep = "/"), rejection_plot, dpi = 500,
       width = 12, height = 6, units = "in")

ggsave(filename =  paste(outpath, "rejection_plot_bymod.png", sep = "/"), rejection_plot2, dpi = 500,
       width = 8, height = 6, units = "in")

#########################################################################################################
### MAKE POWER PLOTS [-1%, 1%] ##########################################################################
#########################################################################################################

# plot rejections holding test constant
rejection_plot <- pp %>% 
  ungroup() %>% 
  filter(c %>% between(-0.01, 0.01)) %>% 
  mutate(model = str_remove(model, "_RESID"),
         test = factor(test, levels = c("UNADJ", "BMP", "ADJ_BMP")),
         model = factor(model, levels = c("MM", "MMPI", "ENR_U", "LASSO", "ID"))) %>% 
  ggplot(aes(x = c, y = rejection, group = model, linetype = model, shape = model, color = model)) + 
  geom_point(size = 2) + geom_line(size = 2) + 
  scale_y_continuous(labels = percent) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = "Imputed Level of Abnormal Return", y = "Rejection Frequency") + 
  facet_wrap(~test, ncol = 3) + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 20))

# plot rejections holding specification constant
rejection_plot2 <- pp %>% 
  ungroup() %>% 
  filter(c %>% between(-0.01, 0.01)) %>% 
  mutate(model = str_remove(model, "_RESID"),
         test = factor(test, levels = c("UNADJ", "BMP", "ADJ_BMP")),
         model = factor(model, levels = c("MM", "MMPI", "ENR_U", "LASSO", "ID"))) %>% 
  ggplot(aes(x = c, y = rejection, group = test, linetype = test, shape = test, color = test)) + 
  geom_point(size = 2) + geom_line(size = 2) + 
  scale_y_continuous(labels = percent) + 
  scale_color_brewer(palette = 'Set1') + 
  labs(x = "Imputed Level of Abnormal Return", y = "Rejection Frequency") + 
  facet_wrap(~model, nrow = 3) + 
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 18))

#save
ggsave(filename =  paste(outpath, "rejection_plot_bytest_1p.png", sep = "/"), rejection_plot, dpi = 500,
       width = 12, height = 6, units = "in")

ggsave(filename =  paste(outpath, "rejection_plot_bymod_1p.png", sep = "/"), rejection_plot2, dpi = 500,
       width = 8, height = 6, units = "in")

