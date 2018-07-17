# This script file produces the sampling distribution of the posterior distribution figure in the paper.
library(latex2exp)

set.seed(1000)

# True mean.
theta = 0
# True standard deviation.
sigma_sq = 10^2
# Sample size.
n = 20

num_posterior_samples = 7

# Sample posterior distributions.
post_density = NULL
for(k in 1:num_posterior_samples){
	
	# Generate Gaussian data, but note that the posterior only depends on the sample mean.
	x_bar = mean( rnorm( n=n, mean=theta, sd=sqrt(sigma_sq)) )

	# Prior mean.
	mu = 0
	# Prior variance.
	tau_sq = 10^2

	# Posterior variance.
	tau_sq_n = (1/tau_sq + n/sigma_sq)^(-1)
	# Posterior mean.
	mu_n = (mu/tau_sq + n*x_bar/sigma_sq)*tau_sq_n

	# Compute and plot the posterior density.
	grid = seq(-10,10,by=.01)
	post_density = cbind( post_density, dnorm( grid, mean=mu_n, sd=sqrt(tau_sq_n)))
}


# Make the plot.
pdf('Figure1.pdf')
plot( grid, post_density[,1], ylab=NA, xlab=TeX('$\\theta$ | $x_{1}^{n}$'), type='l', lwd=2, cex.lab=2,
	  col='white', yaxt='n', xaxt='n',
      panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),1], 0), border=NA,
	  					   col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))

for(k in 2:num_posterior_samples){
	lines( grid, post_density[,k], lwd=2, col='white',
		   panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),k], 0), border=NA,
		   						col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))
}
for(k in 1:num_posterior_samples)  lines( grid, post_density[,k], lwd=1)
polygon( x=c( theta-.8, theta-.8, theta+.8,theta+.8), y=c(0,1,1,0), col='green', border=NA, density=40,angle=-45)
dev.off()						  
							  
							  
							  
							  
							  
							  
							  
							  
							  
							  