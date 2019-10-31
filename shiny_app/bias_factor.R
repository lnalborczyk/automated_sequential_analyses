#######################################################################
# R code accompanying Beffara, Bret & Nalborczyk (2019)
# OSF projet: https://osf.io/mwtvk/
# ---------------------------------------------------------
# Modelling SBF trajectories using a simple random-walk model
# Adapted from https://github.com/psy-farrell/computational-modelling
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last update: March 4, 2019
#############################################################

if (!require("hrbrthemes") ) install.packages("hrbrthemes"); library("hrbrthemes");
if (!require("tidyverse") ) install.packages("tidyverse"); library("tidyverse");
if (!require("patchwork") ) install.packages("patchwork"); library("patchwork");

##########################################################################
# Parameters
# --------------
# nsims: number of random walks (number of simulations/experiments)
# nsamples: number of times evidence is being sampled (number of observations)
# origin: starting value (should be 0 for a log(BF))
# drift: direction of the effect (0 = no effect)
# bfsd: noise in the evidence (sd of the distribution from which we sample evidence)
# criterion: distance from origin to boundary (BF threshold)
# origin sd: uncertainty (random noise) in the origin value
# drift sd: uncertainty (random noise) in the drift value
# prior: "prior belief" about drift (see above), on the same scale (prior = 0 -> no bias).
#################################################################

biasfactor <- function (
    nsims = 1e2, nsamples = 1e2, origin = 0, drift = 0, bfsd = 1,
    criterion = 10, originsd = 0, driftsd = 0, prior = 0, priorsd = 0
    ) {
    
    # initialising the results dataframe
    
    latencies <- rep(0, nsims)
    responses <- rep(0, nsims)
    evidence <- matrix(0, nsims, nsamples + 1)
    
    for (i in 1:nsims) { # for each simulated experiment
        
        # defines origin
        evidence[i, 1] <- rnorm(1, origin, originsd)
        
        # defines drift
        drift <- rnorm(1, drift, driftsd)
        
        # defines prior
        prior <- rnorm(1, prior, priorsd)
        
        # for each observation, compute BF
        for (j in 2:nsamples + 1) {
            
            # extracts last BF
            last <- evidence[i, j - 1]
            
            # defines current drift as drift + prior
            dr <- rnorm(1, drift + prior, 1)
            
            # computes cumulative evidence (BF)
            evidence[i, j] <- sum(last, rnorm(1, dr, bfsd) )
            
        }
        
        # when does it hit the boundary ?
        p <- which(abs(evidence[i, ]) >= criterion)[1]
        
        # lower or upper boundary ?
        responses[i] <- sign(evidence[i, p])
        
        # record latencies (i.e., when the boundary is hit)
        latencies[i] <- p
        
    }
    
    # reshaping results into a data.frame
    results <- t(evidence) %>%
        data.frame %>%
        gather(sim, bf) %>%
        group_by(sim) %>%
        mutate(obs = 1:length(bf) ) %>%
        ungroup %>%
        mutate(
            boundary = rep(responses, each = nsamples + 1),
            latency = rep(latencies, each = nsamples + 1)
        )
    
    # returning the results
    return(results)
    
}
