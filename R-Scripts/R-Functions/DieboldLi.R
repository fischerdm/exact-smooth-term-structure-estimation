######################################################
############## Diebold and Li Model ##################
######################################################


# Helper Functions 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slopeHelper <- function(maturity, lambda){
  slopefac <- (1 - exp(-maturity*lambda))/(maturity*lambda)
  return(slopefac)
}

curvatureHelper <- function(maturity, lambda){
  curvaturefac <- (1 - exp(-maturity*lambda))/(maturity*lambda) - exp(-maturity*lambda)
  return(curvaturefac)
}


# DieboldLi_param: Function that returns the factor of the model of Diebold and 
# Li (2006) 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DieboldLiParameters <- function(x, maturities,lambda){
  
  # INPUT:
  # - x: Continuously compounded annualized spot rates for different maturities
  # - maturities: vector of maturities (in months)
  # -lambda: Model parameter that defines the slope and curvatue (see Diebold 
  # and Li (2006))
  
  # OUTPUT:
  # returns the factors of the Diebold and Li model, i.e. level, slope and curvature
  # as an array
  
  df <- data.frame(y=x, x1=slopeHelper(maturities, lambda), 
                   x2=curvatureHelper(maturities, lambda))
  colnames(df) <- c("y", "x1", "x2")
  #print(df)
  
  m <- lm(y~.,df)
  
  return(m$coef)
  
}

# # Test
# x <- c(0.01655083, 0.02279743, 0.02637771, 0.02846665, 0.02973802, 0.03057286,
#        0.0311826, 0.03168147, 0.03255343, 0.03337628, 0.03493158, 0.03628497,
#        0.03740061, 0.03830204, 0.0390292, 0.03961991, 0.04010492, 0.04084692,
#        0.04159859, 0.04235355)
# 
# maturities <- maturities_GLOBAL
# lambda <- 0.0609
# 
# DieboldLiParameters(x, maturities, lambda)
