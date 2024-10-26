SliceSampler = function(init, f, w, n) {
  
  in.slice = function(x, y) { y <= f(x) }
  
  x <- numeric(n) #to store samples
  
  x[1] = init #initial value of x
  
  for (i in 2:n) 
    {
    y = runif(1, 0, f(x[i-1])) #taking y uniformly from 0 to f(x)
    
    #determining an arbitrary interval
    l = x[i-1] - w * runif(1)
    u = l + w
    #checking if lower bound is in slice
    
    l.in = in.slice(l, y)
    if (l.in) {
      
      while (l.in) {
        l = l - w
        if (l < 0) { 
          l = 0
          break
          
        }
        l.in = in.slice(l, y)
        
      }
      
    }
    
    #checking if upper bound is in the slice
    u.in = in.slice(u, y)
    #stepping out to find an accurate interval
    if (u.in) {
      while (u.in) {
        u = u + w
        if (u > 1) { 
          u = 1
          break
        }
         u.in = in.slice(u, y)
        
      }
      
    }
    x.in = FALSE
    while (!x.in) {  
      
      #sample the new x from the interval
      temp = runif(1, l, u)
      
      #check if x is in the slice
      x.in = in.slice(temp, y)
      if(x.in){
        break
      }
      
    }
    x[i] = temp
    
  }
  
  return(x)
    
}

mh <- function(target, n, init, prop_sd){
  
  x <- numeric(n) #vector to store the states of the chain
  x[1] <-init  #initial state of the chain
  for(i in 2:n){
    
    #sampling from the proposal distribution according to random walk MH
    prop <- rnorm(1,x[i-1],prop_sd) 
    
    #the conditional proposal distributions cancel out
    hastings <- target(prop)/target(x[i-1])
    
    r = min(1,hastings) 
    
    #rejection step
    if (log(r) >= 0  || runif(1) < exp(log(r)))
    {
      x[i] = prop
    }
    else x[i] = x[i-1]
    
  }
  
  return(x)
}




tar <- function(y){   
  return (dgamma(y,shape = 2,scale = 1))
}

n <- 1e5

res <- SliceSampler(0, tar, 10, n)
plot(density(res))
x1<- seq(0, 10, length = 1e5)
y1<-dgamma(x1,2,1)
lines(x1,y1,type = "l", col='red')
acf(res)
plot.ts(res)


chain <- mh(tar, n, 0, 10)
plot(density(chain))
x1<- seq(0,10, length = 1e5)
y1<-dgamma(x1,2,1)
plot(density(chain))
par(new=TRUE)
plot(x1,y1,type = "l", col='green')
acf(chain)
plot.ts(chain)
