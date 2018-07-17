# This script contains functions to numerically compute the value of epsilon given simulated data

suppressMessages(library(tidyverse))

compute_epsilon <- function(post_probs, epsilon_grid, alpha, p){
    # given the posterior interval probabilities of [theta0 - epsilon, theta0 + epsilon]
    # for a grid of intervals, compute the value of epsilon corresponding
    # to P( P( [theta0 - epsilon, theta0 + epsilon] ) <= alpha) <= p
    
    probs <- colMeans(ifelse(post_probs <= alpha, 1, 0))
    df <- data.frame(prob=probs, epsilon=epsilon_grid)
    eps <- df[,'epsilon'][df[,'prob'] >= p] %>% max
    
    eps
}


compute_EAP <- function(post_probs, epsilon_grid, alpha_vals, p_vals){
    # computes epsilon for a grid of alphas and p values

    # stores epsilon - alpha - p triples
    EAP <- matrix(0, nrow = length(alpha_vals)*length(p_vals), ncol=3)
    index <- 0
    for(j in 1:length(alpha_vals)){
        for(k in 1:length(p_vals)){
            index <- index + 1
            
            alpha <- alpha_vals[j]
            p <- p_vals[k]
            
            # print(paste('alpha =', alpha, 'p =', p))
            
            eps <- compute_epsilon(post_probs, epsilon_grid, alpha, p)
            EAP[index, ] <- c(alpha, p, eps)
        }
    }
    
    EAP <- data.frame(EAP)
    colnames(EAP) <- c('alpha', 'p', 'epsilon')
    
    EAP
}
