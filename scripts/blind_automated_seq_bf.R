###################################################################################
# Reanalysig data from a replication of Schnall, Benton, & Harvey (2008, PS)
# OSF link: https://osf.io/apidb/
###########################################################

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
# Codes: https://osf.io/5qhnw/
# Madeup_dummy: “All of my answers are made up.”(1 = yes, 0 = no)
# Exp_Error: Participants for whom errors occurred (filtered out of all analyses)
####################################################################################

# At each step, we should exclude participants who made up answers,
# or wrong trials due to experimenter errors

data <- 
    data %>%
    # filter(Madeup_dummy != 1, Exp_Error != 1) %>%
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

# If the "cleaning" argument is true, proceed to outliers rejections and errors removal
# Here (as an example), we remove participants that are outliers on at least one of the
# measures returned by influence.measures()

# If the "blind" argument is true, the function only returns a "continue or stop message"
# depending on the a priori defined thresold

seqBF <- function(BFobject, cleaning = TRUE, nmin = 20, step = 1, thresold = 10, blind = TRUE) {
    
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
        if (cleaning) {
            
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
        
        }
        
        #########################################################################
        # Bayes Factor computation
        ##########################################
        
        res1 <- recompute(BFobject, progress = FALSE)
        bf <- res1 %>% as.vector %>% as.numeric
        
        if (!exists("res") ) res <- bf else res <- rbind(res, bf)
        
        if (blind == TRUE) {
            
            if (tail(res, 1) < 1 / thres | tail(res, 1) > thres) {
                
                res <- "stop the recruitment"
                
            } else {
                
                res <- "continue the recruitment"
            }
        }
        
    }
    
    return(res)
    
}

################################################
# Blind SBF example
##################################

# with iterative cleaning
seq <- seqBF(tBF, cleaning = TRUE, nmin = 20, thres = 6, blind = FALSE)
ts.plot(seq, xlab = "sample size", ylab = expression(BF[10]), col = "orangered")
abline(h = 1 / 6, lty = 3)

# without iterative cleaning
seq2 <- seqBF(tBF, cleaning = FALSE, nmin = 20, thres = 6, blind = FALSE)
lines(seq2, col = "steelblue")
