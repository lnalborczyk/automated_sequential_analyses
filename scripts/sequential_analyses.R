#######################################################################
# R code accompanying Beffara, Bret & Nalborczyk (2019)
# OSF projet: https://osf.io/mwtvk/
# ---------------------------------------------------------
# Sequential analysis function
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last update: March 5, 2019
#############################################################

# ----------------------------------------#
# Description of the function's arguments #
# ----------------------------------------#

# Model should be a brmsfit model (i.e., a model fitted with the brms package)

# If the "cleaning" argument is set to "TRUE", proceed to sequential outliers
# rejections and errors removal. Here (as an example), we remove RTs below 100ms

# Type refers to the kind of sequential analysis procedure.
# It should be either a Sequential Bayes Factor procedure (SBF),
# an HDI + ROPE sequential procedure (HDI_ROPE) or a sequential analysis based on precision (precision).

# nmin defines the minimum sample size (when to start the sequential analysis)
# step refers to the step of the iteration (e.g., if step = 1,
# the analysis is done for every new observation)

# hypothesis refers to the specific parameter of interest in the above defined model,
# syntax should be similar to the syntax used in the brms::hypothesis()
# method (see ?brms::hypothesis for help)

# rope defines the region of practical equivalence (ROPE) on the scale of the Cohen's d,
# only for the HDI_ROPE and for the (default) precision procedure
# NB: the Cohen's d it obtained by the dividing the mean difference by the square root
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
    ) {
    
    # checking the class of the model (should be a brms model)
    if (class(model) != "brmsfit") stop ("Error: model should be of class 'brmsfit'")
    
    # checking whether the hypothesis has been specified
    if (is.null(hypothesis) ) stop ("Error: an hypothesis should be specified, see ?brms::hypothesis")
    
    # identifying the desired sequential analysis procedure
    if (length(type) > 1) {
        
        type <- type[1]
        
        warning ("Sequential procedure should be either SBF, HDI_ROPE, or precision. SBF is set by default.")
        
    }
    
    # retrieve data from the brms model
    df <- model$data
    
    # retrieve nmax (maximum sample size) from dataset
    nmax <- n_distinct(df$participant)
    
    # defines the range of sequential analysis (from nmin to nmax)
    ns <- seq(from = nmin, to = nmax, by = step)
    
    ###########################################################
    # Sequential procedure
    ##########################################
    
    for (i in 1:length(ns) ) {
        
        ##################################################
        # Preliminary data processing
        ##########################################
        
        # if blind, extract the reduced dataset
        
        if (blind == TRUE) {
            
            df_seq <- df %>%
                filter(as.integer(as.factor(participant) ) %in% 1:ns[i])
            
        } else { # else, considers the whole dataset
            
            df_seq <- df
            
        }
        
        # if we want iterative cleaning
        
        if (cleaning == TRUE) {
            
            # removing missing data
            df_seq <- df_seq %>% na.omit
            
            # removing aberrantly short RTs (<100 ms)
            df_seq <- df_seq %>%
                filter(all.vars(formula(model)$formula)[1] >= 100)
            
        }
        
        ###################################################
        # Sequential analyses by type
        ##########################################
        
        # refitting the model with incremental dataset
        seq_model <- update(
            model, newdata = df_seq,
            chains = 2, cores = parallel::detectCores()
            )
        
        #################################################
        # Sequential Bayes Factor
        ##########################################
        
        if (type == "SBF") {
            
            # compute the BF01 via Savage-Dickey method (see ?brms::hypothesis)
            temp_res <- hypothesis(
                seq_model, hypothesis = hypothesis
                )$hypothesis$Evid.Ratio
            
            if (is.na(temp_res) == TRUE) stop ("Error: Resulting BF is equal to NA. Please define more sensible priors on the parameter of interest.")
            
            if (blind == TRUE) {
                
                # if blind, return the instruction (stop or continue)
                
                if (temp_res <= 1 / threshold | temp_res >= threshold) {
                    
                    return ("Stop data collection \U0001f389 \U0001f389")
                    
                } else {
                    
                    return ("Continue data collection \U0001f47b \U0001f47b")
                }
                
            } else {
                
                # if not blind, saves the current BF
                temp_res <- data.frame(
                    participant = ns[i],
                    BF = temp_res,
                    threshold = threshold
                    )
                
            }
            
            ##############################################
            # HDI+ROPE sequential procedure
            ########################################
            
        } else if (type == "HDI_ROPE") {
            
            if (is.null(rope) ) stop ("Error: When running a sequential an HDI + ROPE sequential procedure, the user should define... a ROPE.")
            
            # (default) ROPE for the Cohen's d
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
            
            # posterior ditribution of the Cohen's d effect size
            post_d <- mean_diff / std
            
            # computes 95% credible interval
            hdi <- posterior_summary(post_d)[3:4]
            
            # define lower bound of the credible interval
            lower <- min(hdi)
            
            # define upper bound of the credible interval
            upper <- max(hdi)
            
            if (blind == TRUE) {
                
                # check whether the HDI is overlapping with the ROPE
                temp_res <- case_when(
                    lower < min(rope) & upper < min(rope) ~ 1,
                    lower > min(rope) & upper < max(rope) ~ 1,
                    lower > min(rope) & upper > max(rope) ~ 1,
                    TRUE ~ 0
                )
                
                if (temp_res == 1) {
                    
                    return ("Stop data collection \U0001f389 \U0001f389")
                    
                } else {
                    
                    return ("Continue data collection \U0001f47b \U0001f47b")
                    
                }
                
            } else {
                
                # if not blind, saves the current HDI
                temp_res <- data.frame(
                    participant = ns[i],
                    lower = lower,
                    upper = upper
                    )
                
            }
            
            ###################################################
            # Precision-based sequential analysis
            #############################################
            
        } else if (type == "precision") {
            
            if (is.null(precision) | is.null(rope) ) stop ("Error: When running a precision-based sequential procedure, the user should define a precision level and/or a ROPE.")
            
            # (default) ROPE for the Cohen's d
            if (is.null(rope) ) rope <- c(-.1, .1)
            
            # (default) precision for the Cohen's d as 0.8 * width(rope)
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
            
            # posterior ditribution of the Cohen's d effect size
            post_d <- mean_diff / std
            
            # computes 95% credible interval
            hdi <- posterior_summary(post_d)[3:4]
            
            # compute precision (range of the credible interval)
            temp_res <- max(hdi) - min(hdi)
            
            if (blind == TRUE) {
                
                if (temp_res <= precision) {
                    
                    return ("Stop data collection \U0001f389 \U0001f389")
                    
                } else {
                    
                    return ("Continue data collection \U0001f47b \U0001f47b")
                    
                }
                
            } else {
                
                # if not blind, saves the current precision
                temp_res <- data.frame(
                    participant = ns[i],
                    obtained_precision = temp_res,
                    target_precision = precision
                    )
                
                }
                
            } # end of precision
        
        # rbind results
        if (!exists("res") ) res <- temp_res else res <- rbind(res, temp_res)
        
        } # end of the sequential analysis
    
    # if not blind, outputs the results of the sequential analysis
    return (res)
    
}
