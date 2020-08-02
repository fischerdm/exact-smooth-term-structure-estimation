######################################################
############## Linear Interpolatiion #################
######################################################

LinearInterpolation <- function(f, x){
  
  # INPUT:
  # - f: function values given at arguments x
  # - x: arguments corresponding to the function values given.
  
  # OUTPUT:
  # - returns a vector of interpolated values at x=x_0, x_0+1, ..., x_n
  
  n <- length(x)
  f_inter <- numeric(x[n]-x[1]+1)
  
  l <- 1
  for (i in 1:(n-1)){
    f_0 <- f[i]
    for (j in x[i]:(x[i+1]-1)){
      f_inter[l+1] <- f_0 + (f[i+1] - f[i])/(x[i+1] - x[i])*(j+1 - x[i])
      l <- l+1
    }
  }
  f_inter[length(f_inter)] <- f[length(x)]
  f_inter[1] <- f[1]
  f_inter
}

# Example 
# ---------------------------------------------  
# x <- c(c(1,5),c(7,10))
# f <- c(2*c(1,5),3*c(6,10))
# res <-LinearInterpolation(f, x)
# plot(y=res,x=1:length(res))







