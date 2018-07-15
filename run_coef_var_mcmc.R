# Computes MCMC samples for coefficient of variation then saves them to disk

source('coef_var_fun.R')


# sampling distribution parameters ----------------------------------------

mu0 <- 1
sigma0 <- 10

true_data_params <- list(sigma0=sigma0,
                         mu0=mu0)



n_vals <- c(5, 10, 20, 50, 100, 200, 500, 1000)   # sample sizes to look at

coef_var0 <- true_data_params$sigma0/true_data_params$mu0 # true coef var param

# number of datasets to sample
R <- 1000

# Prior parameters --------------------------------------------------------


prior_params <- list(name='flat')


# Options for MCMC --------------------------------------------------------


mcmc_params <- list(iter = 10000,
                    chains = 4,
                    warmup = 1000)

# set number of cores
options(mc.cores = parallel::detectCores() - 1)

# compile stan model
stan_mod <- stan_model("normal.stan")


# directory/file struction set up -----------------------------------------

# where we save the MCMC results
results_dir <- 'results/'
sim_name <- get_sim_name(true_data_params)
results_dir <- str_c(results_dir, 'coef_var/posterior_samples/', sim_name, '/')
dir.create(results_dir, showWarnings = T, recursive = T)


# sample posterior distributions ------------------------------------------

time_started <- Sys.time()

for(i in 1:length(n_vals)){
    n <- n_vals[i]

    for(r in 1:R){
        print(str_c('n = ', n, ', r = ', r))
        fname <- str_c(results_dir, 'n=', n, '_r=', r)
        coef_var_posterior_samples(stan_mod,
                                   true_data_params,
                                   prior_params,
                                   n,
                                   mcmc_params,
                                   fname)
    }
}

# save metadata -----------------------------------------------------------

metadata <- list()
metadata <- list(true_data_params=true_data_params,
                 n_vals=n_vals,
                 R=R,
                 prior_params=prior_params,
                 mcmc_params=mcmc_params)

metadata[['time_finished']] <- Sys.time()
saveRDS(metadata, str_c(results_dir, 'metadata'))
