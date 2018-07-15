# This script computes the data used to create the figures in Section TODO of the paper
# For each sample size we repeate the following R times:
    # sample a data set, compute the value of epsilon for given values of (alpha, p)
# The results from this simulation are saved to disk and the figures are created with run_normal_normal_make_plots.R

source('false_conf_fun.R')
source('normal_normal_fun.R')


results_dir <- 'results/'
fname <- get_normal_normal_results_fname(results_dir)


# sampling parameters  ----------------------------------------------------


true_data_params <- list(theta_0 = .1,
                         sigma_0_sq = 1)

n_vals <- c(1, 5, 10, 20, 50, 100, 200, 500, 1000)   # sample sizes to look at

# number of datasets to sample.
R <- 1000 # this script will run much faster if R <- 10


# prior prameters ---------------------------------------------------------

prior_params <- list(mu = 0, tau_sq = 100)

# False conf parameters --------------------------------------------------------
false_conf_params <- list()
epsilon_grid <- seq(from=0, to=2, by=.001)
alpha_vals <- seq(from=0, to=0.95, by=.01)
p_vals <- seq(from=0, to=1, by=.01)


epsilon_grid <- c(seq(from=0, to=.05, by=.001),
                  seq(from=.055, to=.1, by=.005),
                  seq(from=.15, to=5, by=.05))



# run simulation ----------------------------------------------------------

time_started <- Sys.time()

epsilons <- c()
PP_list <- list()
EAPs <- list()
for(i in 1:length(n_vals)){
    
    n <- n_vals[[i]]
    print(paste('n=', n, Sys.time()))
    
    post_probs <- normal_normal_interval_post_probs(true_data_params, n, prior_params, epsilon_grid, R)
    PP_list[[i]] <- post_probs
    
    EAPs[[i]] <- compute_EAP(post_probs, epsilon_grid, alpha_vals, p_vals)
}

time_finished <- Sys.time()

time_finished - time_started

save(EAPs, PP_list,
     true_data_params, prior_params, n_vals,
     R, epsilon_grid, alpha_vals, p_vals,
     time_started, time_finished,
     file=fname)

