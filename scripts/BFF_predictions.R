if(!require(BayesFactor)){install.packages("BayesFactor")}
if(!require(tidyverse)){install.packages("tidyverse")}
library(BayesFactor)
library(tidyverse)

rm(list = ls() ) # cleaning working environment

seqBF <- function(cohensd = 0.5, prior = 0.5, nSims = 100, boundary = Inf, nmin = 10, nmax = 100){
    
    options(scipen = 999) # disable scientific notation for numbers
    
    if (nmin < 10) nmin <- 10 # force nmin to be at leat equal to 10
    
    ns <- seq(nmin, nmax, by = 1)
    
    res <- matrix(NA, nrow = length(ns) * nSims, ncol = 6,
        dimnames = list(NULL, c("id", "cohensd", "boundary", "n", "logBF", "prior") ) )
    
    res.counter <- 1
    
    for (i in 1:nSims) {
        
        x <- rnorm(n = nmax, mean = 0, sd = 1)
        y <- rnorm(n = nmax, mean = 0 + cohensd, sd = 1)
        
        maxsamp <- cbind(x, y)
        
        # res0 keeps the accumulating sample variables from this specific run
        res0 <- matrix(NA, nrow = length(ns), ncol = ncol(res), dimnames = dimnames(res) )
        
        for (n in ns){
            
            # subestting data
            samp <- maxsamp[1:n,]

            # computing the BF
            logBF <- BayesFactor::ttestBF(samp[,1], samp[,2]) %>% as.vector %>% as.numeric
            
            # including a penalisation to illustrates the expected consequences of the SEAB
            # NB: this simple penalisation does not aim to be realistic,
            # it is for illustrative purposes only...
            
            if ( n > nmin && !is.na(prior ) ) { # starting from nmin
                
                # extracting previous BF
                logBF_0 <- res0[n-nmin, 5]
                
                # simply computing the mean of the last and current BFs
                # and "strengthening" it by the "prior" expectancies of the experimenter
                logBF = mean(logBF, logBF_0) * (2 * prior)

            }
            
            res0[which(ns == n), ] <- c(
                id		= i,
                true.ES	= cohensd,
                boundary = boundary,
                n		= n,
                logBF	= logBF,
                prior = prior)
            
            if (abs(logBF) >= boundary | abs(logBF) <= 1 / boundary) {break;}
            
        }
        
        res[res.counter:(res.counter + nrow(res0) - 1), ] <- res0
        res.counter <- res.counter + nrow(res0)
        
    }
    
    class(res) <- c("resBF", "matrix")
    
    # removing rows with only NAs
    res <- res[!!rowSums(!is.na(res) ), ]
    
    return(res)
    
}

#############################################################################
# plot method
########################################################

plot.resBF <- function(x, ...) {
    
    xlim = c(min(x$n), max(x$n) )
    ylim <- c(min(x$logBF) / 1.2, max(x$logBF) * 1.5 )
    
    plot(NA, xlab = expression(sample~ ~size), ylab = expression(Bayes~ ~Factor~ ~(BF[10]) ),
        bty = "l", log = "y", 
        xlim = xlim, ylim = ylim, panel.first = grid(0, NULL, lty = 3) )
    
    for (i in 1:length(unique(x$id) ) ) {
        
        lines(x$logBF[x$id==i] ~ x$n[x$id==i], lwd = 0.6, col = "snow4" )
        
    }
    
    abline(h = 1, lty = 3)

}

#################################################################################
# analysing the results
######################################################

# nSims is the number of simulated experiments
# prior represents the prior beliefs of the experimenter (between 0 (for H0) and 1 (for H1) )
# a prior of 0.5 represents neutral a priori expectancies while this penalisation can be removed
# by specifying prior = NA
# you can modify how the BF is affected by prior expectancies in the "if loop" lines 45-54

analyse <- function(cohensd = 0.5, prior = 0.5, nSims = 100, boundary = Inf, nmin = 10, nmax = 200, plot = TRUE){
    
    results <- seqBF(cohensd = cohensd, prior = prior, nSims = nSims, 
        boundary = boundary, nmin = nmin, nmax = nmax) %>% data.frame
    
    class(results) <-  c("resBF", "data.frame")
    
    # ceiling the BFs over the boundary (for aesthetics purposes)
    results$logBF[results$logBF >= boundary] <- boundary
    results$logBF[results$logBF <= 1 / boundary] <- 1 / boundary
    
    # number of boundary hits
    boundary_hit <-
        results$id[results$logBF>=unique(results$boundary)|
                results$logBF<=1/unique(results$boundary) ] %>%
        unique %>% length
    
    # number of simulations
    all_traj <- length(unique(results$id) )
    # percentage of simulations hitting the boundary
    percent <- (boundary_hit / all_traj) * 100
    # average sample number (asn)
    asn <- mean(results$n[results$logBF >= boundary|
            results$logBF <= 1 / boundary])
    # upper boundary
    hit1 <- unique(results$n[results$logBF >= unique(results$boundary)])
    # lower boundary
    hit2 <- unique(results$n[results$logBF <= 1 / unique(results$boundary)])
    
    if(plot==TRUE){
        
        plot(results)
        abline(h = unique(results$boundary), lty = 2)
        abline(h = 1 / unique(results$boundary), lty = 2)
        #text(max(results$n)/2, max(results$logBF)*1.5,
        #paste0("boundary hit: ",percent, "%,", " asn: ", round(asn, 0)  ) )
        points(hit1, rep(unique(results$boundary), length(hit1) ), pch = 20, cex = 1)
        points(hit2, rep(1 / unique(results$boundary), length(hit2) ), pch = 20, cex = 1)
        
    }
    
    ana <- data.frame(cbind(
        unique(results$cohensd), unique(results$boundary),
        unique(results$prior), percent, asn) )
    
    colnames(ana) <- c("cohensd", "boundary", "prior", "percentage", "asn")
    
    return(ana)
    
}

analyse(cohensd = 0, prior = 0.5, nSims = 20, boundary = 6, nmin = 20, nmax = 100, plot = TRUE)
