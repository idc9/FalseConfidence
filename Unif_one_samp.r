library(latex2exp)

set.seed(1000)

# Sampling probability of the event P(A^c) < alpha.
pFun = function( epsilon, alpha, n, theta_0){
	
	s11 = (theta_0/(theta_0-epsilon))^n - (theta_0/(theta_0+epsilon))^n
	s1 = min( 1/alpha, s11^(-1)) * ((theta_0-epsilon)/theta_0)^n 
	s2 = ((theta_0+epsilon)/theta_0)^n
	s3 = (epsilon <= theta_0*((1-alpha)^(-1/n) - 1)) * (1 - ((theta_0-epsilon)/theta_0)^n) 

	return( alpha*s1 + (1 - (1 - alpha)*s2)*s3 )
}
pFun = Vectorize( pFun, vectorize.args='epsilon')


# Set the global parameters.
theta_0 = 1 
alpha = .5
dx = .001




pdf('Figure2.pdf', width=11, height=11, paper = "USr")
par(mfrow=c(1,3), pty = "s")
#---------------------------------------------------------------------------------------------------------------
# Produce the sample probability versus epsilon plots for various values of n.
#---------------------------------------------------------------------------------------------------------------
epsilon = seq( dx, 1, by=dx) 
n_vals = c(1,5)

plot( NULL, NULL, ylab='sampling probability', ylim=0:1,
      xlab=TeX('radius of $\\epsilon$-ball'), cex.lab=2, cex.main=2, cex.axis=2, xlim=range(epsilon))

# Loop over all values of n.
for(t in 1:length(n_vals)){
	n = n_vals[t]
	
	# Compute the sampling probability that the posterior P(A^c) < alpha with an emirical mean.	
	lines( epsilon, pFun( epsilon, alpha=alpha, n=n, theta_0=theta_0), lty=t, lwd=2)
}
legend( 'topright', bty='n', legend=c('n = 1','n = 5'), lwd=2, lty=1:2, cex=2)
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------------------------
# Produce the density plots for various values n.
#---------------------------------------------------------------------------------------------------------------
grid = seq( dx, 3, by=dx) 

num_posterior_samples = 7

for(t in 1:length(n_vals)){
	n = n_vals[t]
	
	# Sample posterior distributions.
	post_density = NULL
	for(k in 1:num_posterior_samples){
		
		# Sample data.
		x_max = theta_0*rbeta( n=1, shape1=n_vals[t], shape2=1)

		# Compute the posterior density.
		post_density = cbind( post_density, n * x_max^(n) * grid^(-n-1) * (grid >= x_max))
	}

	plot( grid, post_density[,1], ylab=NA, xlab=TeX('$\\theta$ | $x_{1}^{n}$'), type='l', lwd=2, 
		  cex.lab=2, cex.main=2, cex.axis=2, col='white', ylim=c(0,6),
		  main=TeX(paste0('$n = $',toString(n_vals[t]))),
		  panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),1], 0), border=NA,
		  					   col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))

	for(k in 2:num_posterior_samples){
		lines( grid, post_density[,k], lwd=2, col='white',
			   panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),k], 0), border=NA,
			   						col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))
	}
	for(k in 1:num_posterior_samples)  lines( grid, post_density[,k], lwd=1)
	polygon( x=c( theta_0-.3, theta_0-.3, theta_0+.3, theta_0+.3), y=c(0,7,7,0), col='green', border=NA, 
	         density=20, angle=-45)
	
}

#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
dev.off()
