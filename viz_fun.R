# This script contains helper functions which create the visualizations in Sections TODO

# library(tidyverse)
library(directlabels)
library(latex2exp)

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})


make_epsilon_countour_plot <- function(EAP, alpha_fixed, p_fixed, countour_lines=FALSE, n){
    
    EAP <- EAP %>% filter(p > 0)
    alpha_vals <- EAP$alpha
    p_vals <- EAP$p
    
    ap_ind <- elementwise.all.equal(p_vals, p_fixed) & elementwise.all.equal(alpha_vals, alpha_fixed)
    
    
    if(sum(ap_ind) != 1){
        p_upper_bound <- min(p_vals[p_vals >= p_fixed])
        p_lower_bound <- max(p_vals[p_vals <= p_fixed])
        
        alpha_upper_bound <- alpha_fixed
        alpha_lower_bound <- alpha_fixed
        
        epsilon <- EAP %>%
            filter(alpha %in% c(alpha_lower_bound, alpha_upper_bound),
                   p %in% c(p_lower_bound, p_upper_bound)) %>% 
            summarise(epsilon=mean(epsilon)) %>%  .$epsilon
        
    } else{
        epsilon <- EAP[ap_ind, 'epsilon'][[1]]
    }
    
    # https://stackoverflow.com/questions/38154679/r-adding-legend-and-directlabels-to-ggplot2-contour-plot?rq=1
    
    alpha_ticks <- seq(0, to=1, by=.1)
    alpha_tick_labels <- alpha_ticks
    # alpha_ticks <- c(alpha_ticks, alpha_fixed)
    # alpha_tick_labels <- c(alpha_tick_labels, alpha_fixed)
    
    
    p_ticks <- seq(0, to=1, by=.1)
    p_tick_labels <- p_ticks
    # p_ticks <- c(p_ticks, p_fixed)
    # p_tick_labels <- c(p_tick_labels, p_fixed)
    
    
    # title <- TeX('$P_{X_1^n | \\theta_0} \\left( P_{\\theta | X_1^n}(\\[\\theta_0 - \\epsilon, \\theta_0 + \\epsilon\\]) = 1 - \\alpha \\right) = p$')
    title <- str_c('n = ', n)
    
    eps_label <- sprintf("~ epsilon == %1.3f", epsilon)
    
    plt <- ggplot(EAP, aes(x=alpha, y=p, z=epsilon)) + 
        geom_raster(aes(fill=epsilon), show.legend = TRUE) +
        scale_fill_gradient(limits=range(EAP$epsilon), high = 'red', low = 'white') + 
        # geom_contour(aes(colour = ..level..)) +
        scale_colour_gradient(guide = 'none') +
        geom_point(aes(x=alpha_fixed, y=p_fixed), shape=4, size=3) + 
        # geom_hline(yintercept = p_fixed, color='black') +
        # geom_vline(xintercept = alpha_fixed, color='black') +
        annotate("text",  x=alpha_fixed, y = p_fixed, label = eps_label, vjust=.2, hjust=-.1, color='black', parse=T, size=5) +
        scale_x_continuous(expand=c(0, 0),
                           breaks = alpha_ticks,
                           labels = alpha_tick_labels) +
        scale_y_continuous(expand=c(0, 0),
                           breaks = p_ticks,
                           labels = p_tick_labels)  +
        labs(title=title, x=TeX('$\\alpha$'), y='p', fill=TeX('$\\epsilon$')) +
        theme(panel.background = element_blank(),
              axis.title.x = element_text(size=20),
              axis.title.y = element_text(size=20),
              axis.text.y = element_text(size=10),
              axis.text.x = element_text(size=20),
              legend.title=element_text(size=20),
              plot.title = element_text(hjust = 0.5))
    
    if(countour_lines){
        plt <- plt + 
            geom_contour(aes(colour = ..level..))
        
        plt <- direct.label(plt,
                            list("far.from.others.borders",
                                 "calc.boxes",
                                 "enlarge.box",
                                 box.color = NA,
                                 fill = "transparent",
                                 "draw.rects"))
    }
    
    plt

}


make_epsilon_countour_plot_mutiple_n <- function(EAPs, alpha_fixed, p_fixed, n_vals, inds2plot){
    n_EAP <- data.frame()
    for(i in inds2plot){
        EAP <- EAPs[[i]]
        EAP <- EAP %>% filter(p > 0)
        alpha_vals <- EAP$alpha
        p_vals <- EAP$p
        
        ap_ind <- elementwise.all.equal(p_vals, p_fixed) & elementwise.all.equal(alpha_vals, alpha_fixed)
        
        if(sum(ap_ind) != 1){
            p_upper_bound <- min(p_vals[p_vals >= p_fixed])
            p_lower_bound <- max(p_vals[p_vals <= p_fixed])
            
            alpha_upper_bound <- alpha_fixed
            alpha_lower_bound <- alpha_fixed
            
            epsilon <- EAP %>%
                filter(alpha %in% c(alpha_lower_bound, alpha_upper_bound),
                       p %in% c(p_lower_bound, p_upper_bound)) %>% 
                summarise(epsilon=mean(epsilon)) %>%  .$epsilon
            
        } else{
            epsilon <- EAP[ap_ind, 'epsilon'][[1]]
        }
        
        EAP[, 'n'] <- str_c('n = ', n_vals[i])# as.character(n_vals[i])
        EAP[, 'special_epsilon'] <- epsilon
        n_EAP <- rbind(n_EAP, EAP)
    }
    

    alpha_ticks <- seq(0, to=1, by=.1)
    p_ticks <- seq(0, to=1, by=.1)
    
    dat_text <- n_EAP %>%
        group_by(n) %>% 
        summarise(special_epsilon = unique(special_epsilon)) %>% 
        mutate(label=sprintf("~ epsilon == %1.3f", special_epsilon))
    
    
    plt <- ggplot(n_EAP, aes(x=alpha, y=p, z=epsilon, fill=epsilon)) + 
        geom_raster(show.legend = TRUE) + 
        facet_grid(. ~ n) + 
        scale_fill_gradient(limits=range(n_EAP$epsilon), high = 'red', low = 'white') + 
        scale_colour_gradient(guide = 'none') +
        geom_point(aes(x=alpha_fixed, y=p_fixed), shape=4, size=3) +
        scale_x_continuous(expand=c(0, 0),
                           breaks = alpha_ticks) +
        scale_y_continuous(expand=c(0, 0),
                           breaks = p_ticks)  +
        labs(x=TeX('$\\alpha$'), y='p', fill=TeX('$\\epsilon$')) +
        theme(panel.background = element_blank(),
              axis.title.x = element_text(size=20),
              axis.title.y = element_text(size=20),
              axis.text.y = element_text(size=10),
              axis.text.x = element_text(size=10),
              legend.title=element_text(size=20),
              plot.title = element_text(hjust = 0.5)) + 
        geom_text(data = dat_text, parse = T, size=5,
                  mapping = aes(x = alpha_fixed, y = p_fixed, label = label),
                  hjust = -.1, vjust   = .2)
    
    plt
    
}


approx_filter <- function(df, colname, value){
    # equivalent to: filter(df, colname == value)
    # but robust to numerical precision errors
    df[approx_equal_vec_value(df[, colname], value), ]
}

approx_equal_vec_value <- function(vec, value){
    sapply(vec, function(x) isTRUE(all.equal(x, value)))
}

