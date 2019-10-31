###################################################################################
# --------------- Shiny App - Automated and reproducible procedure for SBF ------ #
###################################################################################

library(shinythemes)
library(shinyhelper)
library(hrbrthemes)
library(tidyverse)
library(stringr)
library(shiny)

source("bias_factor.R")

################################################################################
############################# USER INTERFACE ###################################
################################################################################

ui <- shinyUI(
    navbarPage(
        title = "A fully automated, transparent and reproducible protocol for sequential analyses",
        # choose a theme
        # themeSelector(),
        # set a theme
        theme = shinytheme("sandstone"),
        
        ##################################################################################
        # --------------------- UI: Instructions Panel --------------------------------- #
        ##################################################################################
        
        tabPanel(
            title = "Instructions",
            # includes html file containing the instructions
            # includeMarkdown("instructions.md"),
            includeHTML("instructions.html"),
            # includes a footer
            hr(),
            HTML(
                paste(
                    "Written by <a href='https://www.barelysignificant.com'>
                    Ladislas Nalborczyk</a>. Last update: April 26th, 2019"
                    )
                )
            ),
        
        ###################################################################################
        # ---------------------- UI: Script generation ---------------------------------- #
        ###################################################################################
        
        tabPanel(
            title = "Scripts generation",
            fluidPage(
                
                #############################################
                # Sequential Design UI: Step 1 (Automation) #
                #############################################
                
                fluidRow(p("Important note: this application is meant to facilitate the creation of R scripts to automate sequential testing procedures. More specifically, it automatically writes around 90% of the code the user would have to write to use such a procedure. However, it is almost certain that the produced R code will not work immediately. It will require some minor tweakings from the user, such as checking the local path, making sure that the scripts and the data are in the same repository, adapting the data import step to specific properties of the data under consideration, and so on. For more information, please have a look at our tutorial paper.") ),
                
                fluidRow(h3("Step 1: Create an automation") ),
                
                fluidRow(
                    sidebarPanel(
                        width = 6,
                        # style = "background-color:#eeeeee;",
                        selectInput(
                            inputId = "OS",
                            label = "What operating system are you working on?",
                            choices = c("Unix/Linux", "Windows"),
                            selected = "Unix/Linux",
                            width = "100%"
                            )  %>%
                            helper(
                                size = "s",
                                colour = "black",
                                type = "inline",
                                title = "What about macOS?",
                                content = c(
                                    "The mac operating system is also a Unix system."
                                )
                            ),
                        selectInput(
                            inputId = "task",
                            label = "How often do you want the script to be executed?",
                            choices = c("minutely", "hourly", "daily"),
                            selected = "hourly",
                            width = "100%"
                            )
                        )
                    ),
                
                ##########################################################
                # Sequential Design UI: Step 2 (Importing data from OSF) #
                ##########################################################
                
                fluidRow(h3("Step 2: Importing data from OSF") ),
                
                fluidRow(
                    sidebarPanel(
                        width = 6,
                        # style = "background-color:#eeeeee;",
                        textInput(
                            inputId = "OSFid",
                            label = "ID of the OSF project/component (last part of the URL) where the data is stored",
                            placeholder = "mwtk",
                            width = "100%"
                            ),
                        textInput(
                            inputId = "localdata",
                            label = "Path of the local folder (i.e., the result of getwd()) where the data will be stored",
                            placeholder = "/Users/John/Desktop/seqtest/data",
                            width = "100%"
                            )
                        )
                    ),
                
                #####################################################
                # Sequential Design UI: Step 3 (Defining the model) #
                #####################################################
                
                fluidRow(h3("Step 3: Defining the (brms) model") ),
                
                fluidRow(
                    sidebarPanel(
                        width = 6,
                        # style = "background-color:#eeeeee;",
                        # fileInput(
                        #     inputId = "userdata",
                        #     label = "Drop your data here (in csv). Should be in long format.",
                        #     width = "100%"
                        #     ),
                        # # display first six rows of the data
                        # mainPanel(
                        #     tableOutput("contents")
                        #     ),
                        textInput(
                            inputId = "formula",
                            label = "Model formula",
                            placeholder = "response ~ 1 + condition + (1 + condition | participant)",
                            width = "100%"
                            ) %>%
                            helper(
                                size = "s",
                                colour = "black",
                                type = "inline",
                                title = "What is a model formula?",
                                content = c(
                                    "The brms package uses a syntax similar to models fitted with lm or lme4 (get help by typing ?formula in the console)."
                                )
                            ),
                        selectInput(
                            inputId = "family",
                            label = "What family for the response?",
                            choices = list(
                                `Gaussian` = "gaussian()", `Log-Normal` = "lognormal()",
                                `Ex-Gaussian` = "exgaussian()", `Binomial` = "binomial()"
                                ),
                            selected = "gaussian",
                            width = "100%"
                            ),
                        textInput(
                            inputId = "hypothesis",
                            label = "Hypothesis",
                            placeholder = "condition = 0",
                            width = "100%"
                            ) %>%
                            helper(
                                size = "s",
                                colour = "black",
                                type = "inline",
                                content = c(
                                    "Refers to the hypothesis to be tested. Syntax should be similar to the syntax used in the brms::hypothesis() method (see ?brms::hypothesis for help)."
                                )
                            )
                        )
                    ),
                
                ######################################################
                # Sequential Design UI: Step 4 (Sequential analysis) #
                ######################################################
                
                fluidRow(h3("Step 4: Defining the sequential analysis procedure") ),
                
                fluidRow(
                    sidebarPanel(
                        width = 6,
                        # style = "background-color:#eeeeee;",
                        selectInput(
                            inputId = "procedure",
                            label = "What sequential analysis procedure do you want to conduct?",
                            choices = list(
                                `Sequential Bayes Factor` = "SBF", `HDI+ROPE` = "HDI+ROPE",
                                `Precision-based` = "precision"
                                ),
                            selected = "SBF",
                            width = "100%"
                            ),
                        fluidRow(
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                textInput(
                                    inputId = "nmin",
                                    label = "Minimum sample size",
                                    placeholder = "20",
                                    value = 20,
                                    width = "100%"
                                    )
                                ),
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                textInput(
                                    inputId = "nmax",
                                    label = "Maximum sample size",
                                    placeholder = "200",
                                    value = 100,
                                    width = "100%"
                                    )
                                ),
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                textInput(
                                    inputId = "step",
                                    label = "Step",
                                    placeholder = "1",
                                    value = 1,
                                    width = "100%"
                                    )  %>%
                                    helper(
                                        size = "s",
                                        colour = "black",
                                        type = "inline",
                                        content = c(
                                            "When step = 1, the analysis is performed every participant."
                                        )
                                    )
                                ),
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                conditionalPanel(
                                    condition = "input.procedure == 'SBF'",
                                    textInput(
                                        inputId = "threshold",
                                        label = "Threshold",
                                        placeholder = "10",
                                        value = 10,
                                        width = "100%"
                                        )
                                    )
                                ),
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                conditionalPanel(
                                    condition = "input.procedure == 'HDI+ROPE'",
                                    textInput(
                                        inputId = "rope",
                                        label = "ROPE",
                                        placeholder = "c(-0.5, 0.5)",
                                        value = "c(-0.5, 0.5)",
                                        width = "100%"
                                        )
                                    )
                                ),
                            column(
                                width = 6,
                                # style = "background-color:#eeeeee;",
                                conditionalPanel(
                                    condition = "input.procedure == 'precision'",
                                    textInput(
                                        inputId = "precision",
                                        label = "Precision",
                                        placeholder = "0.5",
                                        value = 0.5,
                                        width = "100%"
                                        )
                                    )
                                )
                            )
                        )
                    ),
                
                ######################################################
                # Sequential Design UI: Step 5 (email configuration) #
                ######################################################
                
                fluidRow(h3("Step 5: E-mail configuration") ),
                
                fluidRow(
                    sidebarPanel(
                        width = 6,
                        #style = "background-color:#eeeeee;",
                        textInput(
                            inputId = "email",
                            label = "Please type your e-mail address (should be at gmail) to receive the results of the sequential testing procedure",
                            placeholder = "william.james@gmail.com",
                            width = "100%"
                            ),
                        downloadButton(
                            outputId = "download",
                            label = HTML("Download script for sequential analyses")
                            )
                        )
                    )
                
                ), # end fluidPage
            
            # footer
            hr(),
            HTML(
                paste(
                    "Written by <a href='https://www.barelysignificant.com'>
                    Ladislas Nalborczyk</a>. Last update: April 26th, 2019"
                    )
                )
            
            ), # end panel "scripts generation"
        
        ######################################################################################
        # -------------------------- UI: Bias factor --------------------------------------- #
        ######################################################################################
        
        tabPanel(
            title = "Bias factor",
            fluidPage(
                
                fluidRow(h3("Simulating SBF trajectories and potential biases") ),
                fluidRow(h4("The code underlying this model can be found at https://osf.io/7bd8u/.") ),
                
                fluidRow(
                    sidebarLayout(
                        sidebarPanel(
                            id = "sidebar",
                            width = 4,
                            sliderInput(
                                inputId = "nsims",
                                label = "Select a number of simulations",
                                value = 50,
                                min = 1,
                                max = 100,
                                step = 1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "nsamples",
                                label = "Select a number of observations",
                                value = 300,
                                min = 100,
                                max = 500,
                                step = 1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "drift",
                                label = "Select a drift value (population value of the effect)",
                                value = 0,
                                min = -1,
                                max = 1,
                                step = 0.1,
                                width = "500px"
                                ),
                            sliderInput(
                                inputId = "prior",
                                label = "Select a value for the prior beliefs/expectations (bias)",
                                value = 0,
                                min = -1,
                                max = 1,
                                step = 0.1,
                                width = "500px"
                                ),
                            actionButton(
                                inputId = "refresh",
                                label = "Generate new samples",
                                width = "100%"
                                )
                            ),
                        mainPanel(
                            id = "MainPanel",
                            width = 8,
                            plotOutput("BFdist.plot", width = "100%", height = "600px")
                            )
                        )
                    )
                ),
            
            # footer
            hr(),
            HTML(
                paste(
                    "Written by <a href='https://www.barelysignificant.com'>
                    Ladislas Nalborczyk</a>. Last update: April 26th, 2019"
                    )
                )
            
            ) # end panel bias factor
        
        )
    ) # end UI

################################################################################
################################# SERVER #######################################
################################################################################

server <- function (input, output) {
    
    ##################################################################################
    # ---------------------- Server: scripts generation ---------------------------- #
    ##################################################################################
    
    # Uses 'helpfiles' directory by default
    observe_helpers(withMathJax = TRUE)
    
    # Create ready-to-download scripts

    output$download <- downloadHandler(

        filename <- "sequential_analysis.R",
        
        content <- function (file) {
            
            # rscript <- readLines("/Users/Ladislas/Desktop/Blind_BF/shiny_app/template.R")
            
            ####################
            # compose R script #
            ####################

            rscript <- str_glue(

                '#########################################################
# R code accompanying Beffara, Bret & Nalborczyk (2019) #
# OSF projet: https://osf.io/mwtvk/                     #
# ----------------------------------------------------- #
# Sequential analyses procedure                         #
# Written by Ladislas Nalborczyk                        #
# E-mail: ladislas.nalborczyk@gmail.com                 #
# Last update: April 26, 2019                           #
#########################################################

##########################################
# Installing / loading relevant packages #
##########################################

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
if (!require("osfr") ) {{

    install.packages("remotes")
    remotes::install_github("centerforopenscience/osfr")
    library("osfr")

}}

########################
# retrieving user data #
########################

# retrieve exploitation system
OS <- \"{input$OS}\"

# set working directory
setwd(\"{input$localdata}\")

##########################################################
# Scheduling task (retrieving data and running analyses) #
##########################################################

if (OS == "Windows") {{
    
    #############################################
    # For windows from R (not tested)           #
    # Using the taskscheduleR package           #
    # https://github.com/bnosac/taskscheduleR   #
    #############################################
                
    if (!require("taskscheduleR") ) install.packages("taskscheduleR")
    library(taskscheduleR)
    
    # get a data.frame of all tasks
    tasks <- taskscheduler_ls(id = "sequential_analysis")
    
    # if it does not exist yet, create automation to run script every hour
    
    if (is.null(tasks) ) {{

        taskscheduler_create(
            taskname = "sequential_analysis", rscript = "sequential_analysis.R",
            schedule = toupper(\"{input$task}\")
            )

    }}
    
    # delete the task
    # taskscheduler_delete(taskname = "sequential_analysis")
    
    # NB: tasks can also be created/deleted from RStudio using the
    # taskscheduleR add-in (see the documentation on Github)

    }} else if (OS == "Unix/Linux") {{

    #####################################
    # For Unix/Linux systems from R     #
    # Using the cronR package           #
    # https://github.com/bnosac/cronR   #
    #####################################
    
    if (!require("cronR") ) remotes::install_github("bnosac/cronR")
    library(cronR)
    
    # check whether the task already exists
    tasks <- cron_ls(id = "sequential_testing")
    
    # if it does not exist yet, create automation to run script every hour
    if (is.null(tasks) ) {{
    
        script <- cron_rscript("main_script.R", log_append = FALSE)
        cron_add(script, frequency = \"{input$task}\", id = "sequential_testing")
    
    }}
    
    # delete the task
    # cron_clear(ask = FALSE)
    
    # NB: tasks can also be created/deleted from RStudio using the
    # cronR add-in (see the documentation on Github)

}}

#############################
# Retrieving the data files #
#############################

# count the number of datafiles that have already been downloaded
n_data <- list.files(path = ".", pattern = "csv") %>% length %>% as.numeric

# retrieve OSF data
osf_data <-
    # retrieve the OSF project
    osf_retrieve_node(\"{input$OSFid}\") %>%
    # list data files in this repository
    osf_ls_files(pattern = "csv", n_max = Inf)

# IF new data is present on OSF, download it, else, quit the script
if (nrow(osf_data) > n_data) {{

    osf_data %>%
        # for each line (each csv file)
        {{split(., 1:nrow(.) )}} %>%
        # download it
        lapply(osf_download, overwrite = TRUE)
    
    }} else {{
    
    quit(save = "no")

}}

#####################################
# Importing and formatting the data #
#####################################

data_files <- list.files(path = ".", pattern = "csv")

for (i in 1:length(data_files) ) {{

    if (i == 1) data <- NULL
    
    # import data in a dataframe
    d <- read.csv(data_files[i]) %>% data.frame
    
    if (nrow(d) == 0) {{ # if file is empty
    
        next # go to next iteration
    
    }} else {{
    
    temp_data <-
        d %>%
        # remove NAs
        na.omit
    
    }}
    
    if (is.null(data) ) data <- temp_data else data <- rbind(data, temp_data)

}}

###########################################
# Sequential analyses                     #
# --------------------------------------- #
# 1) Defining the model of interest       #
# 2) Fitting it sequentially              #
###########################################

# defining/fitting the model

model <- brm(
    {input$formula},
    # exgaussian model
    family = {input$family},
    # defining weakly informative priors (for the scale of the data)
    prior = prior(normal(0, 10), class = b),
    # specifying the dataset
    data = data,
    # number of chains
    chains = 2,
    # number of parallel cores
    cores = parallel::detectCores(),
    # number of iterations and warmup
    warmup = 2000, iter = 5000,
    # sampling from prior (needed to compute the BF)
    sample_prior = TRUE
    )

#########################################
# Description of the function arguments #
#########################################

# Model should be a brmsfit model (i.e., a model fitted with the brms package)

# If the "cleaning" argument is set to "TRUE", proceed to sequential outliers
# rejections and errors removal.

# Type refers to the kind of sequential analysis procedure.
# It should be either a Sequential Bayes Factor procedure (SBF),
# an HDI + ROPE sequential procedure (HDI_ROPE) or a sequential analysis based on precision (precision).

# nmin defines the minimum sample size (when to start the sequential analysis)
# step refers to the step of the iteration (e.g., if step = 1,
# the analysis is done for every new observation)

# hypothesis refers to the specific parameter of interest in the above defined model,
# syntax should be similar to the syntax used in the brms::hypothesis()
# method (see ?brms::hypothesis for help)

# rope defines the region of practical equivalence (ROPE) on the scale of the Cohen d,
# only for the HDI_ROPE and for the (default) precision procedure
# NB: the Cohen d it obtained by the dividing the mean difference by the square root
# of the sum of all sources of variation

# If the "blind" argument is true, the function only returns a "continue" or
# "stop" message, depending on the a priori defined threshold / precision
# NB: if blind, the function considers that it is ran during data collection,
# thus, it considers the whole dataset. Instead, when blind is FALSE, the function
# computes the outcome of interest from nmin to nmax (whole dataset).

#-------------------------------------------------------------------------

sequential_analysis <- function (
    model, cleaning = TRUE,
    type = c("SBF", "HDI_ROPE", "precision"),
    nmin = 20, step = 1, hypothesis,
    threshold = 10, rope = NULL, precision = NULL, blind = TRUE
    ) {{
    
    # checking the class of the model (should be a brms model)
    if (class(model) != "brmsfit") stop ("Error: model should be of class brmsfit")
    
    # checking whether the hypothesis has been specified
    if (is.null(hypothesis) ) stop ("Error: an hypothesis should be specified, see ?brms::hypothesis")
    
    # identifying the desired sequential analysis procedure
    if (length(type) > 1) {{
        
        type <- type[1]
        
        warning ("Sequential procedure should be either SBF, HDI_ROPE, or precision. SBF is set by default.")
        
    }}
    
    # retrieve data from the brms model
    df <- model$data
    
    # retrieve nmax (maximum sample size) from dataset
    nmax <- n_distinct(df$participant)
    
    # defines the range of sequential analysis (from nmin to nmax)
    ns <- seq(from = nmin, to = nmax, by = step)
    
    ###########################################################
    # Sequential procedure
    ##########################################
    
    for (i in 1:length(ns) ) {{
        
        ##################################################
        # Preliminary data processing
        ##########################################
        
        # if blind, extract the reduced dataset
        
        if (blind == TRUE) {{
            
            df_seq <- df %>%
                filter(as.integer(as.factor(participant) ) %in% 1:ns[i])
            
        }} else {{ # else, considers the whole dataset
            
            df_seq <- df
            
        }}
        
        # if we want iterative cleaning
        
        if (cleaning == TRUE) {{
            
            # removing missing data
            df_seq <- df_seq %>% na.omit
            
        }}
        
        ###############################
        # Sequential analyses by type #
        ###############################
        
        # refitting the model with incremental dataset
        seq_model <- update(
            model, newdata = df_seq,
            chains = 2, cores = parallel::detectCores()
            )
        
        ###########################
        # Sequential Bayes Factor #
        ###########################
        
        if (type == "SBF") {{
            
            # compute the BF01 via Savage-Dickey method (see ?brms::hypothesis)
            temp_res <- hypothesis(
                seq_model, hypothesis = hypothesis
            )$hypothesis$Evid.Ratio
            
            if (is.na(temp_res) == TRUE) stop ("Error: Resulting BF is equal to NA. Please define more sensible priors on the parameter of interest.")
            
            if (blind == TRUE) {{
                
                # if blind, return the instruction (stop or continue)
                
                if (temp_res <= 1 / threshold | temp_res >= threshold) {{
                    
                    return ("Stop data collection \\U0001f389 \\U0001f389")
                    
                }} else {{
                    
                    return ("Continue data collection \\U0001f47b \\U0001f47b")

                }}
                
            }} else {{
                
                # if not blind, saves the current BF
                temp_res <- data.frame(
                    participant = ns[i],
                    BF = temp_res,
                    threshold = threshold
                    )
                
            }}
            
            #################################
            # HDI+ROPE sequential procedure #
            #################################
            
        }} else if (type == "HDI_ROPE") {{
            
            if (is.null(rope) ) stop ("Error: When running a sequential an HDI + ROPE sequential procedure, the user should define... a ROPE.")
            
            # (default) ROPE for the Cohen d
            if (is.null(rope) ) rope <- c(-.1, .1)
            
            # retrieve posterior samples
            post <- posterior_samples(seq_model)
            
            # retrieve the parameter of interest
            term <- paste0("b_", gsub( " .*$", "", hypothesis) )
            
            # posterior ditribution of the mean difference
            mean_diff <- post[, term]
            
            # posterior distribution of the denominator (standardiser)
            std <- post %>%
                # select all variance components
                select(starts_with("sd"), starts_with("sigma") ) %>%
                # taking the square of them
                mutate_all(.funs = funs(.^2) ) %>%
                # square root of the sum of all variance components
                mutate(std = rowSums(.[1:ncol(.)]) %>% sqrt) %>%
                # extracting the result
                pull(std)
            
            # posterior ditribution of the Cohen d effect size
            post_d <- mean_diff / std
            
            # computes 95% credible interval
            hdi <- posterior_summary(post_d)[3:4]
            
            # define lower bound of the credible interval
            lower <- min(hdi)
            
            # define upper bound of the credible interval
            upper <- max(hdi)
            
            if (blind == TRUE) {{
                
                # check whether the HDI is overlapping with the ROPE
                temp_res <- case_when(
                    lower < min(rope) & upper < min(rope) ~ 1,
                    lower > min(rope) & upper < max(rope) ~ 1,
                    lower > min(rope) & upper > max(rope) ~ 1,
                    TRUE ~ 0
                    )
                
                if (temp_res == 1) {{
                    
                    return ("Stop data collection \\U0001f389 \\U0001f389")
                    
                }} else {{
                    
                    return ("Continue data collection \\U0001f47b \\U0001f47b")
                    
                }}
                
            }} else {{
                
                # if not blind, saves the current HDI
                temp_res <- data.frame(
                    participant = ns[i],
                    lower = lower,
                    upper = upper
                    )
                
            }}
            
            #######################################
            # Precision-based sequential analysis #
            #######################################
            
        }} else if (type == "precision") {{
            
            if (is.null(precision) | is.null(rope) ) stop ("Error: When running a precision-based sequential procedure, the user should define a precision level and/or a ROPE.")
            
            # (default) ROPE for the Cohen d
            if (is.null(rope) ) rope <- c(-.1, .1)
            
            # (default) precision for the Cohen d as 0.8 * width(rope)
            if (is.null(precision) ) precision <- 0.8 * (max(rope) - min(rope) )
            
            # retrieve posterior samples
            post <- posterior_samples(seq_model)
            
            # retrieve the parameter of interest
            term <- paste0("b_", gsub( " .*$", "", hypothesis) )
            
            # posterior ditribution of the mean difference
            mean_diff <- post[, term]
            
            # posterior distribution of the denominator (standardiser)
            std <- post %>%
                # select all variance components
                select(starts_with("sd"), starts_with("sigma") ) %>%
                # taking the square of them
                mutate_all(.funs = funs(.^2) ) %>%
                # square root of the sum of all variance components
                mutate(std = rowSums(.[1:ncol(.)]) %>% sqrt) %>%
                # extracting the result
                pull(std)
            
            # posterior ditribution of the Cohen d effect size
            post_d <- mean_diff / std
            
            # computes 95% credible interval
            hdi <- posterior_summary(post_d)[3:4]
            
            # compute precision (range of the credible interval)
            temp_res <- max(hdi) - min(hdi)
            
            if (blind == TRUE) {{
                
                if (temp_res <= precision) {{
                    
                    return ("Stop data collection \\U0001f389 \\U0001f389")
                    
                }} else {{
                    
                    return ("Continue data collection \\U0001f47b \\U0001f47b")
                    
                }}
                
            }} else {{
                
                # if not blind, saves the current precision
                temp_res <- data.frame(
                    participant = ns[i],
                    obtained_precision = temp_res,
                    target_precision = precision
                )
                
            }}
            
        }} # end of precision
        
        # rbind results
        if (!exists("res") ) res <- temp_res else res <- rbind(res, temp_res)
        
    }} # end of the sequential analysis
    
    # if not blind, outputs the results of the sequential analysis
    return (res)
    
}}

# running and storing the results of the sequential analysis (stop or continue)

results <- sequential_analysis(
    model, cleaning = TRUE, type = \"{input$procedure}\",
    nmin = {input$nmin}, step = {input$step}, hypothesis = \"{input$hypothesis}\",
    threshold = {input$threshold}, rope = {input$rope}, precision = {input$precision},
    blind = TRUE
    )

################################################
# Sending the results by e-mail (using gmailr) #
################################################

message <- # writes email
    mime(
        To = \"{input$email}\",
        From = \"{input$email}\",
        Subject = "Sequential analysis",
        body = results # results of the sequential analysis (stop or continue)
        )

# sends email
send_message(message)

# quits rstudio
quit(save = "no")
'
                )
            
            ##################
            # write R script #
            ##################
            
            write.table(
                rscript,
                # rscript %>% glue::glue_collapse %>% str_glue(),
                # glue::glue_collapse(rscript, sep = "\n") %>% as.character,
                quote = FALSE,
                file, sep = "", col.names = FALSE, row.names = FALSE
                )
            
        }
        
    )
    
    #####################################################################
    # ---------------------- Server: Bias factor ---------------------- #
    #####################################################################
    
    output$BFdist.plot <-
        renderPlot({
            input$refresh
                biasfactor(
                    nsims = input$nsims, nsamples = input$nsamples, origin = 0,
                    drift = input$drift, bfsd = 1, criterion = 10,
                    originsd = 0, driftsd = 0, prior = input$prior,
                    priorsd = 0
                    ) %>%
                    ggplot(aes(x = obs, y = bf, group = sim) ) +
                    geom_hline(yintercept = 0, linetype = 2) +
                    geom_line(alpha = 0.25) +
                    theme_ipsum_rc(base_size = 14) +
                    labs(
                        title = "Simulating the evolution of SBF trajectories",
                        subtitle = paste(
                            "For an effect of", input$drift,
                            "and a bias of", input$prior
                            ),
                        x = "Number of observations", y = "log(BF)"
                        )
            })
    
}

# run the application 
shinyApp(ui = ui, server = server)
