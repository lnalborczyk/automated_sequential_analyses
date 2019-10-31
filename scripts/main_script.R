#######################################################################
# R code accompanying Beffara, Bret & Nalborczyk (2019)
# OSF projet: https://osf.io/mwtvk/
# ---------------------------------------------------------
# Sequential analyses procedure, main script
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last update: April 26, 2019
################################################################

#########################################################
# Installing / loading relevant packages
###################################################

# ggplot2 themes
if (!require("hrbrthemes") ) install.packages("hrbrthemes"); library("hrbrthemes")

# Data formatting, manipulation, and ploting
if (!require("tidyverse") ) install.packages("tidyverse"); library("tidyverse")

# Running tasks on parallel cores
if (!require("parallel") ) install.packages("parallel"); library("parallel")

# Sending emails from R (via gmail)
if (!require("gmailr") ) install.packages("gmailr"); library("gmailr")

# Bayesian multilevel regression models
if (!require("brms") ) install.packages("brms"); library("brms")

# OSF interface
if (!require("osfr") ) {
    
    install.packages("remotes")
    remotes::install_github("centerforopenscience/osfr")
    library("osfr")

}

###########################################################################
# set working directory to the script's parent folder
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ! temporary solution, only works with RStudio... !
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ---------------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path) )
setwd("/Users/Ladislas/Desktop/Blind_BF")

###################################################################
# Scheduling task (retrieving data and running analyses)
#############################################################

####################################################
# For windows from R (not tested)
# Using the taskscheduleR package
# https://github.com/bnosac/taskscheduleR
###############################################

# if (!require("taskscheduleR") ) install.packages("taskscheduleR")
# library(taskscheduleR)

# get a data.frame of all tasks
# tasks <- taskscheduler_ls(id = "sequential_analysis")

# if it does not exist yet, create automation to run script every hour

# if (is.null(tasks) ) {
# taskscheduler_create(
#     taskname = "sequential_analysis", rscript = "main_script.R",
#     schedule = "HOURLY"
#     )
# }

# delete the task
# taskscheduler_delete(taskname = "sequential_analysis")

# NB: tasks can also be created/deleted from RStudio using the
# taskscheduleR add-in (see the documentation on Github)

##################################################
# For Unix/Linux systems from R
# Using the cronR package
# https://github.com/bnosac/cronR
#########################################

if (!require("cronR") ) remotes::install_github("bnosac/cronR")
library(cronR)

# check whether the task already exists
tasks <- cron_ls(id = "sequential_testing")

# if it does not exist yet, create automation to run script every hour
if (is.null(tasks) ) {
    
    script <- cron_rscript("main_script.R", log_append = FALSE)
    cron_add(script, frequency = "hourly", id = "sequential_testing")
    
}

# delete the task
# cron_add(script, frequency = "minutely", id = "sequential_testing")
# cron_clear(ask = FALSE)

# NB: tasks can also be created/deleted from RStudio using the
# cronR add-in (see the documentation on Github)

##################################################################
# Retrieving the data files
######################################################

# count the number of datafiles that have already been downloaded
n_data <- list.files(path = ".", pattern = "csv") %>% length %>% as.numeric

# retrieve OSF data
osf_data <-
    # retrieve the OSF project
    osf_retrieve_node("mwtvk") %>%
    # list components
    osf_ls_nodes() %>%
    # retrieve the component containing the data
    filter(name == "data_emo_stroop_xp") %>%
    # list .csv files in this repository
    osf_ls_files(pattern = "csv", n_max = Inf) %>%
    # remove .csv files ending with "_1"
    filter(!str_detect(name, "_1") )

# IF new data is present on OSF, download it, else, quit the script
if (nrow(osf_data) > n_data) {
    
    osf_data %>%
        # mutate(path = paste(here("data"), "/", .$name, sep = "") ) %>%
        # for each line (each csv file)
        {split(., 1:nrow(.) )} %>%
        # download it
        lapply(osf_download, overwrite = TRUE)
        # download it and put it in the "data" folder
        # lapply(osf_download, path = paste(here("data"), .$name, sep = "/ "), overwrite = TRUE)
        # lapply(osf_download, path = glue::glue(here("data"), .$name, sep = ""), overwrite = TRUE)

    } else {
    
    quit(save = "no")
    
    }
    
###############################################################
# Importing and formatting the data
######################################################

# listing all cvs files
data_files <- 
    list.files(path = ".", pattern = "csv") %>%
    # converting to dataframe
    data.frame(file = ., stringsAsFactors = FALSE) %>%
    # removing "deleted" files
    filter(!stringr::str_detect(file, "DELETED") ) %>%
    # splitting file name by date of acquisition
    separate(col = file, into = c("day", "hour"), remove = FALSE, sep = "\\ ") %>%
    mutate(day = sub(".*stroop_", "", day), hour = sub(".csv*", "", hour) ) %>%
    # sorting files by date of acquisition
    arrange(day, hour)

for (i in 1:nrow(data_files) ) {
    
    if (i == 1) data <- NULL
    
    # import data in a dataframe
    d <- read.csv(data_files$file[i]) # %>% data.frame
    
    if (nrow(d) == 0) { # if file is empty
        
        next # go to next iteration
        
    } else {
        
        temp_data <- 
            d %>%
            # removing the last (empty) column
            select(-ncol(.) ) %>%
            # keeping only the relevant columns
            select(
                stimulus = stim, actor, emotion = emo, word = print_word,
                congruency = congr, corr, resp.keys, resp.corr, resp.rt,
                participant, date
            ) %>%
            # convert response time to ms
            mutate(resp.rt = resp.rt * 1000) %>%
            # remove NAs
            na.omit
        
    }
    
    if (is.null(data) ) data <- temp_data else data <- rbind(data, temp_data)
    
}

# remove all files except data
rm(list = setdiff(ls(), "data") )

##########################################################
# Sequential analyses
# ---------------------------------------
# 1) Defining the model of interest
# 2) Fitting it sequentially
################################################

# defining/fitting the model (multilevel exgaussian model)

model <- brm(
    resp.rt ~ 1 + congruency + (1 + congruency | participant),
    # exgaussian model
    family = exgaussian(),
    # defining weakly informative priors (for the scale of the data)
    prior = c(
        # prior(normal(500, 100), class = Intercept),
        prior(normal(0, 10), class = b)
        # prior(exponential(0.01), class = sd),
        # prior(exponential(0.01), class = sigma)
        ),
    # specifying the dataset
    data = data,
    # number of chains
    chains = 2,
    # number of parallel cores
    cores = parallel::detectCores(),
    # number of iterations and warmup
    warmup = 2000, iter = 5000,
    control = list(adapt_delta = 0.95),
    # sampling from prior (needed to compute the BF)
    sample_prior = TRUE
    )

# retrieving the sequential_analysis() function (should be in the same repository)

source("sequential_analyses.R")

# running and storing the results of the sequential analysis (stop or continue)
# see sequential_analyses.R for documentation on the function arguments

results <- sequential_analysis(
    model, cleaning = TRUE, type = "SBF",
    nmin = 20, step = 1, hypothesis = "congruency = 0",
    threshold = 20, rope = c(-0.1, 0.1), precision = 0.16, blind = FALSE
    )

# saving the SBF evolution
# save(results, file = "sbf_evolution.rds")

# plotting the evolution

# results %>%
#     ggplot(aes(x = participant, y = 1 / BF) ) +
#     geom_hline(yintercept = 20, linetype = 2) +
#     geom_segment(
#         aes(x = 20, xend = 23, y = 1 / 0.03853322, yend = 1 / 0.03853322),
#         colour = "orangered", linetype = 3
#         ) +
#     geom_segment(
#         aes(x = 23, xend = 23, y = 10, yend = 1 / 0.03853322),
#         colour = "orangered", linetype = 3
#         ) +
#     geom_line() +
#     geom_smooth(colour = "black", method = "lm") +
#     geom_point(pch = 21, fill = "white", size = 2) +
#     hrbrthemes::theme_ipsum_rc() +
#     labs(
#         title = "Evolution of the sequential Bayes factor procedure",
#         subtitle = expression(
#             paste(
#                 "Evolution of the  ", BF[10], " computed from ",
#                 n[min], " = 20 participants to ", n[max],
#                 " = 45 participants", sep = ""
#                 )
#             ),
#         x = "Sample size",
#         y = expression(paste("Bayes factor (", BF[10], ")", sep = "") )
#         )

############################################################
# Sending the results by e-mail (using gmailr)
######################################################

email <- # writes email
    mime(
        To = "brice.beffara@gmail.com",
        To = "ameliebret@gmail.com",
        To = "ladislas.nalborczyk@gmail.com",
        From = "ladislas.nalborczyk@gmail.com",
        Subject = "Sequential analysis",
        body = results # results of the sequential analysis (stop or continue)
        )

# sends email
send_message(email)

# quits rstudio
quit(save = "no")
