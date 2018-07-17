library(latex2exp)

set.seed(1000)

# Marginal posterior density for psi = theta_x * theta_y.
F_psi = function( psi, n, x_max, y_max){	
	if(psi > x_max*y_max) 1 - (1 + n*log(psi/(x_max*y_max))) * (x_max*y_max/psi)^n else 0
}
F_psi = Vectorize( F_psi, vectorize.args='psi')


# Set the global parameters.
theta_x = 10
theta_y = 1
psi_0 = theta_x * theta_y
alpha = .5
dx = .001




pdf('Figure3.pdf', width=11, height=11, paper = "USr")
par(mfrow=c(1,3), pty = "s")
#---------------------------------------------------------------------------------------------------------------
# Produce the sample probability versus epsilon plots for various values of n.
#---------------------------------------------------------------------------------------------------------------
num_sim = 10000
epsilon = seq( .1, 10, by=.1) 
n_vals = c(1,5)
samp_prob = matrix( 0, length(n_vals), length(epsilon))

plot( NULL, NULL, ylab='sampling probability', ylim=0:1,
      xlab=TeX('radius of $\\epsilon$-ball'), cex.lab=2, cex.main=2, cex.axis=2, xlim=range(epsilon))

# Loop over all values of n.
for(t in 1:length(n_vals)){
	n = n_vals[t]
	
	# Loop over all values of epsilon
	for(k in 1:length(epsilon)){
		count = 0
		# Estimate the sampling probability that the posterior P(A^c) < alpha with an emirical mean.
		for(r in 1:num_sim){
	
			# Sample data. Recall max order statistic of uniform sample is beta.
			x_max = theta_x*rbeta( n=1, shape1=n, shape2=1)
			y_max = theta_y*rbeta( n=1, shape1=n, shape2=1)
	
			# A^c = ( lower, upper)
			upper = psi_0 + epsilon[k]
			lower = psi_0 - epsilon[k]
	
			# Compute posterior probability of A^c.
			if(lower > x_max*y_max){
				prob = F_psi( psi=upper, n=n, x_max, y_max) - F_psi( psi=lower, n=n, x_max, y_max)
			} else{
				prob = F_psi( psi=upper, n=n, x_max, y_max)
			}
			if(prob <= alpha) count = count +1
		}
		samp_prob[t,k] = count/num_sim	
	}
	lines( epsilon, samp_prob[t,], lty=t, lwd=2)
}
legend( 'bottomright', bty='n', legend=c('n = 1','n = 5'), lwd=2, lty=1:2, cex=2)
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------------------------
# Produce the density plots for various values n.
#---------------------------------------------------------------------------------------------------------------
grid = seq( 0, 40, by=dx) 


num_posterior_samples = 7

for(t in 1:length(n_vals)){
	n = n_vals[t]
	
	# Sample posterior distributions.
	post_density = NULL
	for(k in 1:num_posterior_samples){
		
		# Sample data.
		x_max = theta_x*rbeta( n=1, shape1=n_vals[t], shape2=1)
		y_max = theta_y*rbeta( n=1, shape1=n_vals[t], shape2=1)

		# Compute the posterior density.
		post_density = cbind( post_density, diff(F_psi( psi=grid, n=n, x_max, y_max)))
	}

	plot( grid[-1], post_density[,1], ylab=NA, xlab=TeX('$\\psi$ | $x_{1}^{n}$, $y_{1}^{n}$'), type='l', lwd=2, 
		  cex.lab=2, cex.main=2, cex.axis=2, col='white', ylim=c(0,.0005),
		  main=TeX(paste0('$n = $',toString(n_vals[t]))),
		  panel.first=polygon( x=grid[-1], y=c(0, post_density[c(-1,-length(grid[-1])),1], 0), border=NA,
		  					   col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))

	for(k in 2:num_posterior_samples){
		lines( grid[-1], post_density[,k], lwd=2, col='white',
			   panel.first=polygon( x=grid[-1], y=c(0, post_density[c(-1,-length(grid[-1])),k], 0), border=NA,
			   						col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))
	}
	for(k in 1:num_posterior_samples)  lines( grid[-1], post_density[,k], lwd=1)
	polygon( x=c( psi_0-6, psi_0-6, psi_0+6, psi_0+6), y=c(0,1,1,0), col='green', border=NA, density=20,
			 angle=-45)
}
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------
dev.off()



	