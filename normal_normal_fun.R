# This script contains a number of helper functions for the normal-normal simulations from Section TODO
library(parallel)


normal_normal_interval_post_probs  <- function(true_data_params, n, prior_params, epsilon_grid, R){
    # samples R times from the posterior probability of [theata_0 - epsilon, theata_0 + epsilon] 
    # for a grid of epsilon values. Each sample is from the distribution of
    # P_{theta | x}([theata_0 - epsilon, theata_0 + epsilon])
    
    # model parameters
    theta_0 <- true_data_params$theta_0
    sigma_0_sq <- true_data_params$sigma_0_sq
    
    # prior params
    tau_sq <- prior_params$tau_sq
    mu <- prior_params$mu
    
    # posterior pre compute
    tau_n_sq <- 1/((1 / tau_sq) + (n / sigma_0_sq))
    
    # post_probs <- matrix(0, nrow=R, ncol=length(epsilon_grid))
    cores <- detectCores()
    pp <- mclapply(1:R, function(r) r_fun(n, theta_0, sigma_0_sq, mu, tau_sq, tau_n_sq),
                   mc.cores = cores)
    
    matrix(unlist(pp), byrow=TRUE, nrow=length(pp))
}

eps_fun <- function(epsilon, theta_0, mu_n, tau_n_sq){
    
    # posterior prob of [theta_0 - epsilon, theta_0 + epsilon]
    pnorm((theta_0 - mu_n + epsilon)/sqrt(tau_n_sq)) - 
        pnorm((theta_0 - mu_n  - epsilon)/sqrt(tau_n_sq))
}


r_fun <- function(n, theta_0, sigma_0_sq, mu, tau_sq, tau_n_sq){
    
    # sample observed data
    X <- rnorm(n=n, mean=theta_0, sd=sqrt(sigma_0_sq))
    
    # posterior parameters
    mu_n <- ((mu / tau_sq) + (n * mean(X) / sigma_0_sq)) * tau_n_sq
    
    sapply(epsilon_grid, function(epsilon) eps_fun(epsilon, theta_0, mu_n, tau_n_sq))
    
}


get_normal_normal_results_dir <- function(results_dir){
    str_c(results_dir, 'normal_normal/')
}

get_normal_normal_results_fname <- function(results_dir){
    # results_dir <- get_results_dir()
    normal_results_dir <- get_normal_normal_results_dir(results_dir)
    dir.create(normal_results_dir, showWarnings = F, recursive = T)
    exper_name <- 'normal_normal_results'
    fname <- str_c(normal_results_dir, exper_name)
    
    fname
}
