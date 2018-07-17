# This script contains helper functions for the Coefficient of Variation simulations in Section TODO

library("rstan")
suppressMessages(library(tidyverse))

coef_var_posterior_samples <- function(stan_mod, true_data_params, prior_params, n, mcmc_params, fname){
    # samples a data set, runs MCMC, then either saves or returns the result
    # will save if fname is not NA
    
    # sample data
    x <- rnorm(n,
               mean = true_data_params$mu0,
               sd = true_data_params$sigma0)
    
    sampling(stan_mod,
             data = list(x=x, n=n), 
             iter = mcmc_params$iter,
             chains = mcmc_params$chains,
             warmup = mcmc_params$warmup,
             sample_file=fname)
    
}


get_coef_var_samples<- function(fit){
    # returns MCMC coefficient of variation samples from the STAN fit object

    list_of_draws <- rstan::extract(fit)

    mu_post_samples <- list_of_draws$mu
    sigma_post_samples <- list_of_draws$sigma

    coef_var_samples <- sigma_post_samples/mu_post_samples
    
    coef_var_samples
}

empirical_cdf <- function(x, samples){
    # returns the empirical CDF evaluated at x given samples
    mean(samples <= x)
}



coef_var_interval_post_probs <- function(cv_samples, epsilon_grid, coef_var0){
    R <- length(cv_samples)
    num_eps <- length(epsilon_grid)
    
    post_probs <- matrix(0, nrow=R, ncol=num_eps)
    
    for(r in 1:R){
        for(i in 1:num_eps){
            
            post_probs[r, i] <- empirical_cdf(coef_var0 + epsilon_grid[i], cv_samples[[r]]) -
                                empirical_cdf(coef_var0 - epsilon_grid[i], cv_samples[[r]])
        }
    }
    
    post_probs
}

get_sim_name <- function(true_data_params){
    str_c('sigma=',true_data_params$sigma0, '_','mu=', true_data_params$mu0)
}



