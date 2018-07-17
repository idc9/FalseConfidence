
set.seed(1000)

args <- commandArgs(TRUE) 
# args[1] is the index: 1, 2, 3, 4, 5, 6, 7, 8, 9
# args[2] is the value of alpha to use: 'large' for .5; 'small' for .05
# args[3] is the number of samples of data to estimate the posterior P(A^c) < alpha with an emirical mean
# args[4] is the discretization

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

size = args[2]
dir = paste0('Fieller_plots_alpha_',size,'/')
alpha = ifelse( size=='large', .5, .05)
dx = as.numeric(args[4]) #.01





#---------------------------------------------------------------------------------------------------------------
# Produce the sample probability versus epsilon plots for various values of sigma and n.
#---------------------------------------------------------------------------------------------------------------
num_sim = as.integer(args[3]) #10000
epsilon = seq( .1, 10, by=.1) 
n_vals = c(1,30,100)
sigma_vals = c(.5,1,2)
samp_prob = list()
for(k in 1:length(sigma_vals))  samp_prob[[k]] = matrix( 0, length(n_vals), length(epsilon))

index = list( c(1,1), c(1,2), c(1,3), c(2,1), c(2,2), c(2,3), c(3,1), c(3,2), c(3,3))

s = index[[as.integer(args[1])]][1]
t = index[[as.integer(args[1])]][2]

sigma = sigma_vals[s]
n = n_vals[t]
		
# Loop over all values of epsilon
for(k in 1:length(epsilon)){
	count = 0
	# Estimate the sampling probability that the posterior P(A^c) < alpha with an emirical mean.
	for(r in 1:num_sim){
		
		# Sample data.
		x_bar = mean(rnorm( n=n, mean=theta_x, sd=sigma))
		y_bar = mean(rnorm( n=n, mean=theta_y, sd=sigma))
		
		# A^c = ( lower, upper)
		upper = psi_0 + epsilon[k]
		lower = psi_0 - epsilon[k]
		
		# Compute posterior probability of A^c.
		prob = sum( pi_psi( psi=seq( lower, upper, by=dx), n=n, x_bar=x_bar, y_bar=y_bar, 
		            sigma_sq=sigma^2) *dx)
		if(prob <= alpha) count = count +1
	}
	samp_prob[[s]][t,k] = count/num_sim
	print(k)
}
#---------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------

save( samp_prob, file=paste0(dir,'samp_prob',args[1],'.rda'))


