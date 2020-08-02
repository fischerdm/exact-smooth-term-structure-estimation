######################################################
########### Exact smooth term structure ##############
######################################################


# helper functions
# ------------------
discountCurve <- function(t, cashflow_dates_vec, z_vec){
  
  # The method is discribed in the paper of Filipovic and Willems (2018)  
  # Remark:  The formula for psi has changed compared to the working paper in 2016
  
  # The calculated discount curve with this approach exactly replicates the observed prices given the cashflows, and 
  # is the smoothest curve among all real functions with absolutely first derivate (see Filipovic and Willems (2018), p. 6)
  
  # INPUT:
  # t: point in time at which the curve is evaluated (in months from now (t0=0)) (scalar).
  # Denoted as x in the definition of psi!
  # cashflow_dates_vec: vector of libor and swap cashflow dates (Denoted as x_j in the definition of psi. 
  # z_vec: vector calculated as in Filipovic and Willems (2018) (See p. 6).
  
  # OUTPUT:
  # d: discount curves with values at times t, i.e. y=f(t) (scalar). f is constructed as described in Filipovic and Willems (2018)
  
  # phi (vector) constructed according to Filipovic and Willems (2018)
  phi_vec <- numeric(length(cashflow_dates_vec))
  for (i in 1:length(cashflow_dates_vec)){
    
    # Phi according to Filipovic and Willems (2018)
    phi_vec[i] <- 1-1/6*(min(t, cashflow_dates_vec[i]))^3 + 1/2*t*cashflow_dates_vec[i]*(2+min(t, cashflow_dates_vec[i])) 
  }
  # discount factor at time tau
  d <- sum(z_vec*phi_vec)
  d 
}


forwardCurve <- function(t, cashflow_dates_vec, z_vec){
  # similar to 'discountCurve', but this time f*(x) on p.6 of the paper is calculated
  
  # phi (vector) constructed according to Filipovic and Willems (2018)
  phi_vec <- numeric(length(cashflow_dates_vec))
  phi_deriv_vec <- numeric(length(cashflow_dates_vec))
  for (i in 1:length(cashflow_dates_vec)){
    
    # Phi and the derivative of phi according to Filipovic and Willems (2018)
    phi_vec[i] <- 1-1/6*(min(t, cashflow_dates_vec[i]))^3 + 1/2*t*cashflow_dates_vec[i]*(2+min(t, cashflow_dates_vec[i]))
    phi_deriv_vec[i] <- cashflow_dates_vec[i] - 1/2*min(t, cashflow_dates_vec[i])^2 + cashflow_dates_vec[i]*min(t, cashflow_dates_vec[i])
  }
  # discount factor at time tau
  f <- -sum(z_vec*phi_deriv_vec)/sum(z_vec*phi_vec)
  f  
}  

############################################################################


# Exact smooth termstructure estimation (Filipovic and Willems, 2018)
termStructureFW <- function(cashflow_dates_vec, cashflow_mat, present_values_vec, t_vec){
  # term structure cestimation according to Filipovic and Willems, 2018. 
  
  # INPUT:
  # cashflow_dates_vec: vector of time points from now at which cashflows occur. The curve evaluated (in months), e.g.
  # c(0, 1/30, 3, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240, 252, 264, 276, 288, 312, 324, 336, 348, 360)
  # The entry at t=0 is 1.
  # cashflow_mat: a matrix of cash flows at the cashflow dates of financial instruments at the points in time defined by 
  # 'cashflow_dates_vec' for instruments as e.g. Tomorrow-Next, 3m Libor, 1y swap etc.
  # present_values_vec: vector of present values of the instruments. 
  # t_vec: points in time from now on at which the discount curve should be evaluated [in months], e.g. (1:360*30)/30
  
  # OUTPUT:
  # y: Discount factors at times t_vec
  

  ###### # Construction of matrix B, p.7 ######################
  
  # According to Filipovic and Willems (2018). A_ij = ph_x_i(x_j)
  A1 <- matrix(rep(cashflow_dates_vec,length(cashflow_dates_vec)), byrow=T, nrow=length(cashflow_dates_vec))
  A2 <- t(A1)

  # Matrix with values x_i ^ x_j
  A_min <- A1*(A1-A2<=0) + A2*(A1-A2>0)

  A <- 1-1/6*A_min^3 +1/2*A1*A2*(2+A_min)
  
  ###############################################################
  ###### # Construction of z, p. 7 ##############################
  
  z_vec <- t(cashflow_mat)%*%solve((cashflow_mat%*%A%*%t(cashflow_mat)))%*%present_values_vec # 
  
  # Points in the time at which the discount cuvrve is evaluated
  tMat <- matrix(t_vec, nrow=1, ncol=length(t_vec))
  
  # Discount curve
  discount_curve_vec <- as.numeric(apply(tMat, 2, function(t){discountCurve(t,cashflow_dates_vec,z_vec)}))
  discount_curve_vec
}

termStructureForwardFW <- function(cashflow_dates_vec, cashflow_mat, present_values_vec, t_vec){
  # similar to 'discountCurve', but this time f*(x) on p.6 of the paper is calculated
  
  ###### # Construction of matrix B, p.7 ######################
  
  # According to Filipovic and Willems (2018). A_ij = ph_x_i(x_j)
  A1 <- matrix(rep(cashflow_dates_vec,length(cashflow_dates_vec)), byrow=T, nrow=length(cashflow_dates_vec))
  A2 <- t(A1)
  
  # Matrix with values x_i ^ x_j
  A_min <- A1*(A1-A2<=0) + A2*(A1-A2>0)
  
  A <- 1-1/6*A_min^3 +1/2*A1*A2*(2+A_min)
  
  ###############################################################
  ###### # Construction of z, p. 7 ##############################
  
  z_vec <- t(cashflow_mat)%*%solve((cashflow_mat%*%A%*%t(cashflow_mat)))%*%present_values_vec # 
  
  # Points in the time at which the discount cuvrve is evaluated
  tMat <- matrix(t_vec, nrow=1, ncol=length(t_vec))
  
  # Discount curve
  forward_curve_vec <- as.numeric(apply(tMat, 2, function(t){forwardCurve(t,cashflow_dates_vec,z_vec)}))
  forward_curve_vec
}

