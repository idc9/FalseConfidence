// saved as normal.stan
data {
  int<lower=0> n; // number of samples
  real x[n]; // observations
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(x | mu, sigma);
}
