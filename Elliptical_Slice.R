# L is the likelihood function
#f is the current state
#var is the variance of the Gaussian prior
#the mean of the prior is assumed to be zero
#Elliptical returns the new state f'
#p is the dimension of the variables we are sampling

Elliptical = function (f, L, var, p) 
{
  nu <- rnorm(p, 0, var) #define the ellipse
  u <- runif(1) #log-likelihood threshold
  
  #thresh is the log threshold
  thresh <- L(f) + log(u) 
  
  theta <- runif(1, 0, 2 * pi)
  theta_min <- theta - 2 * pi
  theta_max <- theta
  
  f1 <- f * cos(theta) + nu * sin(theta)
  
  if ( log(L(f1) ) > thresh) 
  {  
    return (f1)
  }
  
  while (log(L(f1)) <= thresh)
  {
    if (theta < 0) 
    {
      theta_min <- theta
    } 
    else 
    {
      theta_max <- theta
    }
    
    theta <- runif(1, theta_min, theta_max)
    
    f1 <- f * cos(theta) + nu * sin(theta)
  }
  
  return(f1)
}


