if(!require(BayesFactor)){install.packages("BayesFactor")}
if(!require(magrittr)){install.packages("tidyverse")}
library(BayesFactor)
library(tidyverse)

rm(list = ls() ) # cleaning working environment

seqBF <- function(cohensd = 0.5, prior = 0.5, nSims = 10, boundary = Inf, nmin = 10, nmax = 100){
    
    options(scipen = 999) # disable scientific notation for numbers
    
    if(nmin<10) nmin <- 10 # force nmin to be at leat equal to 10
    if(prior==0) prior <- 0.01 # avoid to multiply the BF by zero...
    
    prior <- 2 * prior # in order to scale priors on 1
    
    ns <- seq(nmin, nmax, by = 1)
    
    res <- matrix(NA, nrow = length(ns) * nSims, ncol = 6,
        dimnames = list(NULL, c("id", "cohensd", "boundary", "n", "logBF", "prior") ) )
    
    res.counter <- 1
    
    for(i in 1:nSims){
        
        x <- rnorm(n = nmax, mean = 0, sd = 1)
        y <- rnorm(n = nmax, mean = 0 + cohensd, sd = 1)
        
        maxsamp <- cbind(x, y)
        
        # res0 keeps the accumulating sample variables from this specific run
        res0 <- matrix(NA, nrow = length(ns), ncol = ncol(res), dimnames = dimnames(res) )
        
        for (n in ns) {
            
            samp <- maxsamp[1:n,]
            
            t <- t.test(samp[,1], samp[,2])$statistic # performing the t-test
            
            logBF <- BayesFactor::ttest.tstat(t, nrow(samp), nrow(samp),
                nullInterval = NULL, simple = TRUE)
            
            if(n > nmin){ # include a penalization of b_n by the value of b_n-1
                
                logBF_0 <- res0[n-nmin, 5] # define BF of n-1
                logBF = mean(logBF, logBF_0) * prior # simply taking the mean of BF n-1 and current BF
                
            }
            
            res0[which(ns == n), ] <- c(
                id		= i,
                true.ES	= cohensd,
                boundary = boundary,
                n		= n,
                logBF	= logBF,
                prior = prior / 2)
            
            if (abs(logBF) >= boundary) {break;}
            
        }
        
        res[res.counter:(res.counter + nrow(res0) - 1), ] <- res0
        res.counter <- res.counter + nrow(res0)
        
    }
    
    class(res) <- c("resBF", "matrix")
    res <- res[complete.cases(res),]
    
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
    
    for(i in 1:length(unique(x$id) ) ){
        
        lines(x$logBF[x$id==i] ~ x$n[x$id==i], lwd = 0.6, col = "snow4" )
        
    }
    
    abline(h = 1, lty = 3)

}

#################################################################################
# analyze results
######################################################

# nSims is the number of experiments
# prior represents the prior beliefs of the experimenter (between 0 (for H0) and 1 (for H1))
# you can modify how the BF is affected by prior beliefs in the "if loop" lines 44-49

analyse <- function(cohensd = 0.6, prior = 0.5, nSims = 100, boundary = 20, nmin = 10, nmax = 200, plot = TRUE){
    
    results <- seqBF(cohensd = cohensd, prior = prior, nSims = nSims, 
        boundary = boundary, nmin = nmin, nmax = nmax) %>% data.frame
    
    class(results) <-  c("resBF", "data.frame")
    
    results$logBF[results$logBF>boundary] <- boundary
    
    boundary_hit <- length(unique(results$id[results$logBF>=unique(results$boundary)]) )
    all_traj <- length(unique(results$id) ) # number of simulations
    percent <- (boundary_hit / all_traj) * 100 # percentage of sim hitting the boundary
    asn <- mean(results$n[results$logBF>=boundary]) # average sample number (asn)
    hit <- unique(results$n[results$logBF>=unique(results$boundary)])
    
    if(plot==TRUE){
        
        plot(results)
        abline(h = unique(results$boundary), lty = 2)
        #text(max(results$n)/2, max(results$logBF)*1.5, paste0("boundary hit: ", percent, "%") )
        #text(max(results$n)/2, max(results$logBF)*1.5, paste0("boundary hit: ", percent, "%,", " asn: ", round(asn, 0)  ) )
        points(hit, rep(unique(results$boundary), length(hit)), pch = 20, cex = 1)
        
    }
    
    ana <- data.frame(cbind(unique(results$cohensd), unique(results$boundary), unique(results$prior), percent, asn))
    colnames(ana) <- c("cohensd", "boundary", "prior", "percentage", "asn")
    
    return(ana)
    
}

analyse(cohensd = 0, prior = 0.01, nSims = 10, boundary = 20, nmin = 20, nmax = 100, plot = TRUE)
analyse(cohensd = 0, prior = 0.99, nSims = 10, boundary = 20, nmin = 20, nmax = 100, plot = TRUE)
