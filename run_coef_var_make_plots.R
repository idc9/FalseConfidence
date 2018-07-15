# This script makes the plots for Section TODO of the paper.
# It assumes you have already computed the MCMC samples (using run_coef_var_mcmc.R)

source('false_conf_fun.R')
source('coef_var_fun.R')
library(latex2exp)

# set sampling distribution parametrs -------------------------------------


mu0 <- 1
sigma0 <- 10

n_vals <- c(5, 10, 20, 50, 100, 200, 500, 1000)   # sample sizes to look at


# set true data parameters
true_data_params <- list(sigma0=sigma0,
                         mu0=mu0)


coef_var0  <- true_data_params$sigma0/true_data_params$mu0

# False conf parameters --------------------------------------------------------
epsilon_grid <- c(seq(from=0, to=.2, by=.005),
                  seq(from=.25, to=10, by=.05))

alpha <- 0.5
p <- 0.9


# Where to find the posterior samples -------------------------------------------------


# posterior samples directory
data_dir <- str_c(system('sh get_data_dir.sh', intern = T), 'coef_var/')
sim_name <-  get_sim_name(true_data_params)
post_samples_dir <- str_c(data_dir, 'posterior_samples/', sim_name)


# where to save results ---------------------------------------------------

results_dir <- str_c(data_dir, 'results/')
dir.create(results_dir, showWarnings = F, recursive = T)
save_stub <- str_c(results_dir, sim_name, '_alpha=', alpha, '_p=', p)

# load MCMC and compute results -------------------------------------------

epsilons <- rep(NA, length(n_vals))
for(i in 1:length(n_vals)){
    n <- n_vals[i]
    
    # list of MCMC files corresponding to this value of n
    files_n <- list.files(post_samples_dir,
                          pattern = str_c('n=', n, '_'))
    
    if(length(files_n) == 0){
        print(str_c("WARNING: no posterior samples found for n = ", n))
    }
    
    # simulation repritiions
    repititions <- files_n %>% 
                    str_split( '_') %>%
                    sapply( function(x) str_split(x[2], '=', simplify=T)[2]) %>% 
                    unique %>% 
                    as.integer
    
    print(Sys.time())
    print(str_c('n = ', n, ', R = ', length(repititions)))
    
    # load coef var MCMC samples
    cv_samples <- list()
    rep_ind <- 1
    for(r in repititions){
        # the saved MCMC chains for this repitition
        chain_files <- dir(post_samples_dir,
                           pattern = str_c('n=', n, '_r=', r, '_[0-9].csv'), full.names = TRUE)
        
        cv_samples[[rep_ind]]  <- chain_files %>% 
                                read_stan_csv %>% 
                                get_coef_var_samples
        
        if(rep_ind %% 100 == 1){
            print(rep_ind)
        }
        rep_ind  <- rep_ind + 1
    }
    
    # compute posterior interval probabilities
    post_probs <- coef_var_interval_post_probs(cv_samples, epsilon_grid, coef_var0)
    
    # save post probs
    saveRDS(post_probs, str_c(save_stub, '_n=', n, '_post_probs'))
    
    epsilons[i] <- compute_epsilon(post_probs, epsilon_grid, alpha=alpha, p=p)
}

eps_vs_n_df <- data.frame(n=n_vals, epsilon=epsilons)


# make plots --------------------------------------------------------------

eps_vs_n_df <- read_csv(str_c(save_stub, '_n_vs_epsilon.csv'))


epsilon_limits <- c(0, 1.1*max(eps_vs_n_df$epsilon))


n_vs_epsilon_plt <- eps_vs_n_df %>% 
    ggplot(aes(x=n, y=epsilon)) +
    geom_point() + geom_line() +
    ylab(TeX('$\\epsilon$')) + 
    scale_y_continuous(expand=c(0, 0),
                       limits=epsilon_limits) +
    scale_x_continuous(expand=c(.01, 0),
                       limits=c(0, 1050)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(),
          axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=20),
          axis.text.y = element_text(size=15),
          axis.text.x = element_text(size=10)) 

# save and plot results ---------------------------------------------------

write_csv(eps_vs_n_df, str_c(save_stub, '_n_vs_epsilon.csv'))

eps_vs_n_df <- read_csv(str_c(save_stub, '_n_vs_epsilon.csv'))
ggsave(str_c(save_stub, '_n_vs_epsilon.pdf'), n_vs_epsilon_plt, width=3, height=3)

