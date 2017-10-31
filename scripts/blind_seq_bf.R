# get BayesFactorExtras from github
# devtools::install_github("richarddmorey/BayesFactorExtras", subdir = "BayesFactorExtras")

# if the "blind" argument is true, the function only returns a "continue or stop message"
# thres is the threshold at which to stop

seqBF <- function(BFobject, min.n = 10, step = 1, var.id = NULL, verbose = TRUE, thres = 10, blind = TRUE, ...) {
    
    dat <- BFobject@data # stores the full data set
    BFtype <- class(BFobject@numerator[[1]])
    random <- FALSE	# flag whether data has random factors or not.
    
    # Determine the number of observations if var.id is given by user
    if (!is.null(var.id) ) {
        count = dat %>% group_by_(var.id) %>% summarise(n = n() )
        max.n = nrow(count)
    }
    
    # var.id is NULL? Try to guess the grouping variable
    if (is.null(var.id) ) {
        if (verbose==TRUE) print("Trying to guess the grouping variable ...")
        switch(BFtype,
            
            # one sample t-test: each row is one observation
            BFoneSample = {
                max.n = nrow(dat)
                if (verbose==TRUE) print("No grouping factor detected.")
            },
            BFindepSample = {
                max.n = nrow(dat)
                if (verbose==TRUE) print("No grouping factor detected.")
            },
            BFlinearModel = {
                # differentiate between anovaBF and lmBF: get RHS of denominator formula; ==1: lmBF; !=1: anovaBF
                if (length(all.vars(as.formula(BFobject@denominator@identifier$formula))) == 1) {
                    # no random factor / no repeated measurement
                    max.n = nrow(dat)
                    if (verbose==TRUE) print("No grouping factor detected.")
                } else {
                    # repeated measurement
                    # TODO: This works if only one random factor is given (e.g., subject ID in a repeated measurement).
                    # Generalize to multiple random factors
                    var.id <- all.vars(as.formula(BFobject@denominator@identifier$formula))[2]
                    count = dat %>% group_by_(var.id) %>% summarise(n=n())
                    max.n = nrow(count)
                    random = TRUE
                    if (verbose==TRUE) print(paste0("Grouping factor detected: ", var.id) )
                }
            }
        )
    }
    
    # at which n's should the BF be calculated?
    ns <- seq(min.n, max.n, by=step)
    if (tail(ns, 1) != max.n) ns <- c(ns, max.n)
    
    if (verbose==TRUE) pb <- txtProgressBar(min = 0, max = length(ns), style=3)
    resSeq <- data.frame()
    # ---------------------------------------------------------------------
    # Do the sequential calculation of BFs. Each design needs it's own sampling scheme
    for (i in 1:length(ns)) {
        
        if (random == FALSE) {
            BFobject@data <- dat[1:ns[i], , drop=FALSE]
        }
        
        if (random == TRUE) {
            var.id.pos <- NULL	# hack to please CRAN
            dat$var.id.pos <- as.numeric(dat[, var.id])
            BFobject@data <- dat %>% filter(var.id.pos <= ns[i]) %>% select(-var.id.pos) %>% as.data.frame()
        }
        
        # Get rid of unused factor levels
        for (v in 1:ncol(BFobject@data)) {
            if (is.factor(BFobject@data[, v])) {
                BFobject@data[, v] <- factor(BFobject@data[, v])
            }
        }
        # Get rid of old analysis information used for increasing precision with same data
        for(v in length(BFobject)){
            BFobject@numerator[[v]]@analysis = list()
        }
        BFobject@denominator@analysis = list()
        
        # recompute Bayes factor on reduced data set
        res1 <- recompute(BFobject, progress=FALSE)
        resSeq <- rbind(resSeq, data.frame(
            n  = ns[i], 
            Alt = 1:nrow(res1@bayesFactor),
            bf = res1@bayesFactor$bf,			
            error = res1@bayesFactor$error
        ))
        if (verbose==TRUE) setTxtProgressBar(pb, i)
    }
    if (verbose==TRUE) close(pb)
    
    # Define a new final BF-object. If no user args are provided, this is identical to the original BFobject
    BFobject.new <- res1
    
    # define a new S4 class. You have to set the 'where' argument to please R CMD check: https://stat.ethz.ch/pipermail/r-devel/2007-April/045475.html
    setClass("BFBayesFactorSeq", slots = c(bayesFactorSeq="data.frame"), contains = class(BFobject), where=topenv(parent.frame()))
    
    RES <- new("BFBayesFactorSeq")
    # copy all existing slots into new object.
    for (sl in names(getSlots(class(BFobject)))) {
        slot(RES, sl) <- slot(BFobject.new, sl)
    }
    
    # add new sequential slot to results object
    RES@bayesFactorSeq <- resSeq
    
    ########################################################################
    ##### short append to the original function
    ###########################################################
    
    if (blind == TRUE) {
        
        if(tail(RES@bayesFactorSeq$bf, 1) < 1 / thres | tail(RES@bayesFactorSeq$bf, 1) > thres) {
            
            RES <- "stop the recruitment"
            
            } else {
                
                RES <- "continue the recruitment"
            }
    }
    
    #################################################################
    #################################################################
    
    return(RES)
}

print.BFBayesFactorSeq <- function(x, ...) {
    print("Sequential BayesFactor object. The final Bayes factor is:\n=========================")
	print(x)
}

################################################
# Blind SBF example
##################################
library(BayesFactor)
library(tidyverse)

df <- 
    cbind(x = rnorm(100, 100, 10), y = rnorm(100, 115, 10) ) %>%
    data.frame %>%
    gather %>%
    mutate(key = ifelse(key == "x", -0.5, 0.5) )

tBF <- ttestBF(formula = value ~ key, data = df[sample(nrow(df) ), ])
seqBF(tBF, min.n = 10, verbose = FALSE, thres = 10, blind = TRUE)
