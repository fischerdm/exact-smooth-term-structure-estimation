######################################################
##################### Bootstrap ######################
######################################################

BootstrapSpotRates <- function(x){
  # INPUT:
  # - x: vector containing swap rates for maturities
  #      1, 2, ... years. The one year swap rate in % is used
  #      as start value.
  
  # OUTPUT:
  # - returns a vector of bootstrapped annualized 
  #   spot rates
  
  spotRates <- numeric(length(x))
  spotRates[1] <- x[1]
  
  for (i in 2:length(x)) {
    spotRates[i] <- ((1+x[i])/(1-x[i]*sum(1/(1+spotRates[1:(i-1)])^(1:(i-1)))))^(1/i)-1
  }
  # Spot rates
  spotRates
}




# Example 
# --------------------------------------------- 
# x <- c(0.03060000, 0.03500000, 0.04080000, 0.04240000, 0.04400000, 0.04525000, 0.04650000, 
#        0.04733333, 0.04816667, 0.04900000, 0.05105000, 0.05310000, 0.05513333, 0.05716667, 
#        0.05920000, 0.06124000, 0.06328000, 0.06532000, 0.06736000, 0.06940000)
# spot <- BootstrapSpotRates(x)
# plot(y=spot, x=1:length(spot))
