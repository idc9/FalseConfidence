# This script creates the figures in Section TODO of the paper
# run_normal_normal_sim.R needs to be run before running this script.

library(tidyverse)
library(latex2exp)
source('normal_normal_fun.R')
source('viz_fun.R')


alpha <- .5
p <- .95


# load saved simulation data from run_normal_normal_sim.R ----------------------------------------------

results_dir <- 'results/'
fname <- get_normal_normal_results_fname(results_dir)
load(fname)


# make n vs. epsilon plot -----------------------------------------------------------


epsilons <- rep(0, length(n_vals))
for(i in 1:length(n_vals)){
    
    EAP <- EAPs[[i]]
    
    eap_tripple <- EAP %>%
                    approx_filter('alpha', alpha) %>% 
                    approx_filter('p', p)
    

    epsilons[i] <- eap_tripple$epsilon
}


# n vs epsilon plot -------------------------------------------------------



eps_vs_n_df <- data.frame(n=n_vals, epsilon=epsilons)

epsilon_limits <- c(0, 1.1*max(eps_vs_n_df$epsilon))

n_vs_epsilon_plt <-  eps_vs_n_df %>% 
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


# Contour plots -----------------------------------------------------------
countour_plots <- list()
for(i in 1:length(n_vals)){
    countour_plots[[i]] <- make_epsilon_countour_plot(EAPs[[i]], alpha_fixed=alpha, p_fixed=p,
                                                      countour_lines = FALSE,
                                                      n=n_vals[i])
}



# facted contour plot -----------------------------------------------------
facted_countour_plot <- make_epsilon_countour_plot_mutiple_n(EAPs,
                                                             alpha_fixed=alpha,
                                                             p_fixed=p,
                                                             n_vals,
                                                             inds2plot=c(1, 3, 6))


# save plots --------------------------------------------------------------
nn_results_dir <- get_normal_normal_results_dir(results_dir)

# ggsave(str_c(nn_results_dir, 'normal_normal_n_vs_epsilon.pdf'), n_vs_epsilon_plt, width=4, height=4)
ggsave(str_c(nn_results_dir, 'Figure8.pdf'), n_vs_epsilon_plt, width=4, height=4)

# for(i in 1:length(n_vals)){
#     ggsave(str_c(nn_results_dir, 'normal_normal_n', n_vals[i], '.pdf'),
#            countour_plots[[i]],
#            width=4, height=4)
#     
# }


ggsave(str_c(nn_results_dir, 'normal_normal_countour.pdf'), facted_countour_plot, width=9, height=3)
ggsave(str_c(nn_results_dir, 'Figure7.pdf'), facted_countour_plot, width=9, height=3)


