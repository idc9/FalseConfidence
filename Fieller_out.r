library(latex2exp)

set.seed(1000)

args <- commandArgs(TRUE)
size <- args[1] # The value of alpha to used: 'large' for .5; 'small' for .05
dir <- paste0('Fieller_plots_alpha_',size,'/')


# Marginal posterior density for psi = theta_x / theta_y.
pi_psi = function( psi, n, x_bar, y_bar, sigma_sq){
	
	coef = ( 2 *pi *sigma_sq *(1 + psi^2) /n )^(-.5) 
	# Estimate the expectation in the density with an emprical mean.
	exp_val = mean(abs(rnorm( n=1000, mean=(psi*x_bar + y_bar)/(1 + psi^2), sd=sqrt(sigma_sq/(n*(1 + psi^2))))))
	
	return( coef *exp_val *exp( (.5*n/sigma_sq) *( (psi*x_bar +y_bar)^2 /(1 + psi^2) -x_bar^2 -y_bar^2 ) ) )
}
pi_psi = Vectorize( pi_psi, vectorize.args='psi')

# Set the global parameters.
theta_x = .1
theta_y = .01
psi_0 = theta_x/theta_y
dx = .01





#---------------------------------------------------------------------------------------------------------------
# Produce the sample probability versus epsilon plots for various values of sigma and n.
#---------------------------------------------------------------------------------------------------------------
num_sim = 1000
epsilon = seq( .1, 10, by=.1) 
n_vals = c(1,30,100)
sigma_vals = c(.5,1,2)
samp_prob_agg = list()
for(k in 1:length(sigma_vals))  samp_prob_agg[[k]] = matrix( 0, length(n_vals), length(epsilon))

index = list( c(1,1), c(1,2), c(1,3), c(2,1), c(2,2), c(2,3), c(3,1), c(3,2), c(3,3))
for(k in 1:9){
	s = index[[k]][1]
	t = index[[k]][2]
	load(paste0(dir,'samp_prob',toString(k),'.rda'))
	samp_prob_agg[[s]][t,] = samp_prob[[s]][t,]
}


if(size=='large'){
	pdf('Figure4.pdf', width=11, height=11, paper = "USr")
} else{
	pdf('Figure5.pdf', width=11, height=11, paper = "USr")
}
par(mfrow=c(1,3), pty = "s")

# Loop over all values of sigma.
for(s in 1:length(sigma_vals)){
	sigma = sigma_vals[s]
	
	plot( NULL, NULL, main=TeX(paste0('$\\sigma = $', toString(sigma))), ylab='sampling probability', ylim=0:1,
	      xlab=TeX('radius of $\\epsilon$-ball'), cex.lab=2, cex.main=2, cex.axis=2, xlim=range(epsilon))
	
	# Loop over all values of n.
	for(t in 1:length(n_vals)){
		n = n_vals[t]
		
		lines( epsilon, samp_prob_agg[[s]][t,], lty=t, lwd=2)
	}
	legend( 'bottomleft', bty='n', legend=c('n = 1','n = 30','n = 100'), lwd=2, lty=1:3, cex=2)
	
	#print(s)
}
dev.off()
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------------------------
# Produce the density plots for various values n.
#---------------------------------------------------------------------------------------------------------------
grid = seq( -15, 15, by=dx) 
num_posterior_samples = 7
sigma = 1

pdf('Figure6.pdf', width=11, height=11, paper = "USr")
par(mfrow=c(1,3), pty = "s")

for(t in 1:length(n_vals)){
	n = n_vals[t]
	
	# Sample posterior distributions.
	post_density = NULL
	for(k in 1:num_posterior_samples){
		
		# Sample data.
		x_bar = mean(rnorm( n=n, mean=theta_x, sd=sigma))
		y_bar = mean(rnorm( n=n, mean=theta_y, sd=sigma))

		# Compute the posterior density.
		post_density = cbind( post_density, pi_psi( psi=grid, n=n, x_bar=x_bar, y_bar=y_bar, sigma_sq=sigma^2))
	}

	plot( grid, post_density[,1], ylab=NA, xlab=TeX('$\\psi$ | $x_{1}^{n}$, $y_{1}^{n}$'), type='l', lwd=2, 
		  cex.lab=2, cex.main=2, cex.axis=2, col='white', ylim=c(0,.7), xlim=c(-5,15), 
		  main=TeX(paste0('$n = $',toString(n_vals[t]))),
		  panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),1], 0), border=NA,
		  					   col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))

	for(k in 2:num_posterior_samples){
		lines( grid, post_density[,k], lwd=2, col='white',
			   panel.first=polygon( x=grid, y=c(0, post_density[c(-1,-length(grid)),k], 0), border=NA,
			   						col=rgb(147,112,219,255/6,max=255), density=80,angle=-45))
	}
	for(k in 1:num_posterior_samples)  lines( grid, post_density[,k], lwd=1)
	polygon( x=c( psi_0-4, psi_0-4, psi_0+4,psi_0+4), y=c(0,1,1,0), col='green', border=NA,density=20,angle=-45)
}
dev.off()
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

