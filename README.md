The code in this directory can be run to reproduce the results of (Carmichael and Williams, 2018). Before running the below R scripts make sure the libraries listed in requirements.txt are installed (e.g. run `Rscript install_requirements` from the command line). The scripts below assume the code/ is the current working directory.

# Section 3 (Uniform)

To create Figure 2 run
```
Rscript 
```

# Section 4 (Uniform with marginalization)

Figure 3 run

```
Rscript 
```

# Section 5 (Normal with marginalization)

To create Figures 4, 5, and 6 run

```
Rscript 
```

# Appendix A (Normal-Normal)

To create Figures 7,8 run the following two R scripts in order

```
Rscript run_normal_normal_sim.R
Rscript run_normal_normal_make_plots.R
```

The plots will then be located in results/normal_normal/. Note that run_normal_normal_sim.R may take a few hours to run.

# Appendix B (Coefficient of Variation)

To create Figure 9 run the following two R scripts in order

```
Rscript run_coef_var_mcmc.R
Rscript run_coef_var_make_plots.R
```
The plots will then be located in results/coef_var/. Note that run_coef_var_mcmc.R may take a few hours to run.
