code_1_covariate_London <- nimbleCode({
  
  beta01 ~ dnorm(0,0.001)
  beta02 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  delta1 ~ dgamma(10,10)
  
  for(i in 1:N1) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta01*x[i,1] + log(x[i,5]) + delta1*beta1*x[i,3]
  } 
  
  for(i in (N1+1):N) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta02*x[i,2] + log(x[i,6]) + (1/delta1)*beta1*x[i,4] 
  } 
  
  
})
