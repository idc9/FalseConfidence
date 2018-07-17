# An exposition of the false confidence theorem

The code in this directory can be run to reproduce the results of (Carmichael and Williams, 2018). Before running the below R scripts make sure the libraries listed in requirements.txt are installed (e.g. run `Rscript install_requirements` from the command line). The scripts below assume the code/ is the current working directory.

## Section 2 (Main ideas)

To create Figure 1 run

```
Rscript sampDistPost.r
```
Produces the file `Figure1.pdf'.

## Section 3 (Uniform)

To create Figure 2 run
```
Rscript Unif_one_samp.r
```
Produces the file `Figure2.pdf'.

## Section 4 (Uniform with marginalization)

Figure 3 run

```
Rscript Unif_two_samp.r
```
This may take a few minutes to run.  Produces the file `Figure3.pdf'.

## Section 5 (Normal with marginalization)

To create Figures 4, 5, and 6 run

```
mkdir Fieller_plots_alpha_small/

Rscript Fieller_run.r 1 small 10000 .01
Rscript Fieller_run.r 2 small 10000 .01
Rscript Fieller_run.r 3 small 10000 .01
Rscript Fieller_run.r 4 small 10000 .01
Rscript Fieller_run.r 5 small 10000 .01
Rscript Fieller_run.r 6 small 10000 .01
Rscript Fieller_run.r 7 small 10000 .01
Rscript Fieller_run.r 8 small 10000 .01
Rscript Fieller_run.r 9 small 10000 .01

mkdir Fieller_plots_alpha_large/

Rscript Fieller_run.r 1 large 10000 .01
Rscript Fieller_run.r 2 large 10000 .01
Rscript Fieller_run.r 3 large 10000 .01
Rscript Fieller_run.r 4 large 10000 .01
Rscript Fieller_run.r 5 large 10000 .01
Rscript Fieller_run.r 6 large 10000 .01
Rscript Fieller_run.r 7 large 10000 .01
Rscript Fieller_run.r 8 large 10000 .01
Rscript Fieller_run.r 9 large 10000 .01

Rscript Fieller_out.r small; Rscript Fieller_out.r large
```
The series of implementations of the file `Fieller_run.r' can be run in parallel, and separating them by index number facilitates easily submitting the jobs to a computing cluster.  To produce the figures shown in the manuscript, the parameter `10000' is used which corresponds to the number of simulated data sets used for the computations, and the parameter `.01' is used which corresponds to the discretization.  This may take about a day to run, in parallel.  For quick implementation (with coarser plots), change `10000 .01' to `30 .1', for instance.  Produces the files `Figure4.pdf', `Figure5.pdf', and `Figure6.pdf'.

## Appendix A (Normal-Normal)

To create Figures 7,8 run the following two R scripts in order

```
Rscript run_normal_normal_sim.R
Rscript run_normal_normal_make_plots.R
```

The plots will then be located in results/normal_normal/. Note that run_normal_normal_sim.R may take a few hours to run.

## Appendix B (Coefficient of Variation)

To create Figure 9 run the following two R scripts in order

```
Rscript run_coef_var_mcmc.R
Rscript run_coef_var_make_plots.R
```
The plots will then be located in results/coef_var/. Note that run_coef_var_mcmc.R may take a few hours to run.
