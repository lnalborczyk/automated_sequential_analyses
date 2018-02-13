##############################################################################
# R code accompanying Bret, Beffara & Nalborczyk (2018)
# OSF projet: https://osf.io/mwtvk/
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last update: February 13, 2018
#############################################################

# Reanalysig data from a replication of Schnall, Benton, & Harvey (2008, PS)
# OSF link: https://osf.io/apidb/

# Importing SAV file
# d <- foreign::read.spss(file = "rep2.sav", to.data.frame = TRUE)

# Exporting as a CSV file
# write.csv(d, "rep2.csv", row.names = FALSE)

####################################################################
# Importing data
########################################

library(tidyverse)

data <-
    # importing the csv file
    read.csv(file = "rep2.csv") %>%
    # keeping only the relevant columns
    select(Participant., Condition, Madeup_dummy, Exp_Error, mean_vignettes) %>%
    rename(Participant = Participant.)

#######################################################################################
# Codes used by the original authors: https://osf.io/5qhnw/
# Madeup_dummy: “All of my answers are made up.”(1 = yes, 0 = no)
# Exp_Error: Participants for whom errors occurred (filtered out of all analyses)
####################################################################################

# At each step, we should exclude participants who made up answers,
# or wrong trials due to experimenter errors
# i.e., filter(Madeup_dummy != 1, Exp_Error != 1)

data <- 
    data %>%
    mutate(Condition = factor(Condition) ) %>%
    # ordering dataset by participant
    arrange(Participant)

####################################################################################
# Sequential Bayes Factor
####################################################

library(BayesFactor)

# get BayesFactorExtras from github
# devtools::install_github("richarddmorey/BayesFactorExtras", subdir = "BayesFactorExtras")

################################################################################
# Defining the model comparison (the t-test) of interest
########################################################################

tBF <- ttestBF(formula = mean_vignettes ~ Condition, data = data)

################################################################################
# Writing a basic SBF function
########################################################################

# If the "cleaning" argument is set to "full", proceed to sequential outliers
# rejections and errors removal
# If the "cleaning" argument is set to "errors", proceed to errors removal only

# Here (as an example), we remove participants that are outliers on at least one of the
# measures returned by influence.measures()

# If the "blind" argument is true, the function only returns a "continue" or "stop" message,
# depending on the a priori defined threshold

seqBF <- function(BFobject, cleaning = NULL, nmin = 20, step = 1, threshold = 10, blind = TRUE) {
    
    dat <- BFobject@data
    
    nmax <- nrow(dat) %>% as.numeric
    ns <- seq(nmin, nmax, by = step)
    
    #########################################################################
    # Sequential Bayes Factor
    ##########################################
    
    for (i in 1:length(ns) ) {
        
        #########################################################################
        # Preliminary data processing...
        ##########################################
        
        # extracting reduced dataset
        dat2 <- dat[1:ns[i], ]

        # storing the reduced dataset
        BFobject@data <- dat2
        
        # if we want to go for iterative cleaning
        if (cleaning == "full") {
            
            # removing wrong trials and errors
            dat2 <- dat2 %>% filter(Madeup_dummy != 1, Exp_Error != 1)
            
            # fitting an equivalent lm model
            mod <-
                lm(
                    as.formula(BFobject@numerator$`Alt., r=0.707`@identifier$formula),
                    data = dat2
                )
            
            # identifying influential observations
            inf <- influence.measures(mod)
            inf_obs <- apply(inf$is.inf, 1, function(x) !any(x) )
            
            # removing influential observations and storing clean dataset
            BFobject@data <- dat2[inf_obs, ]
            
            } else if (cleaning == "errors") {
            
            # removing wrong trials and errors
            BFobject@data <- dat2 %>% filter(Madeup_dummy != 1, Exp_Error != 1)
            
        }
        
        #########################################################################
        # Bayes Factor computation
        ##########################################
        
        res1 <- recompute(BFobject, progress = FALSE)
        bf <- res1 %>% as.vector %>% as.numeric
        
        if (!exists("res") ) res <- bf else res <- rbind(res, bf)
        
        if (blind == TRUE) {
            
            if (tail(res, 1) < 1 / threshold | tail(res, 1) > threshold) {
                
                res <- "stop the recruitment"
                
            } else {
                
                res <- "continue the recruitment"
            }
        }
        
    }
    
    return(res)
    
}

################################################
# Automated SBF example
##################################

# with iterative cleaning (removing errors + outliers)
seq <- seqBF(tBF, cleaning = "full", nmin = 20, threshold = 6, blind = FALSE)

# with iterative cleaning (only removing errors)
seq2 <- seqBF(tBF, cleaning = "errors", nmin = 20, threshold = 6, blind = FALSE)

# without iterative cleaning
seq3 <- seqBF(tBF, cleaning = FALSE, nmin = 20, threshold = 6, blind = FALSE)

# plotting it
ts.plot(seq, xlab = "sample size", ylab = expression(BF[10]), col = "darkred", lwd = 1.5)
abline(h = 1 / 6, lty = 3)
lines(seq2, col = "darkblue", lwd = 1.5)
lines(seq3, col = "darkgreen", lwd = 1.5)

# adding a legend
legend(
    x = 100, y = 0.9,
    legend = c("SBF", "SBF_errors", "SBF_full"),
    col = c("darkgreen", "darkblue", "darkred"),
    lty = 1, lwd = 1.5)

################################################
# Blind SBF example
##################################

seqBF(tBF, cleaning = FALSE, nmin = 20, threshold = 6, blind = TRUE)
