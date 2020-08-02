#______________Create_Spot_Rates_From_Libor_and_Swap_Data.R______________
#________________________________________________________________________
#
# This script creates the term struture of 
# interes rates for maturities starting from
# 1M to 240M.
# The following methods are implemented:
# 1) bootstrap (see e.g. Annaert et al. 2013)
# 2) the exact method of Filipovic and  
#    Willems (2018).
# 
# ________________________________________________________________________

rm(list = ls())

path <- getwd()

source(paste0(path,"/R-Scripts/R-Functions/LinearInterpolation.R"))
source(paste0(path,"/R-Scripts/R-Functions/Bootstrap.R"))
source(paste0(path,"/R-Scripts/R-Functions/CashFlowMatrix.R"))
source(paste0(path,"/R-Scripts/R-Functions/ExactSmoothTermStructure.R"))

# Data
# -------------------------------------------
maturities_names <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", "15Y", "20Y")
data <- read.csv(paste0(path,"/Results/Data_Libor_Swap_Rates_88_17.csv"), header=T, sep=",")
data$X <- NULL
data_ts <- ts(data, start=c(1988,1), freq=12)
colnames(data_ts) <- maturities_names

# Method 1: Bootstrap continuously compounded spot rates from libor and swap rates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
data_swaps_mat <- data.matrix(data_ts)

# In order to linearly interpolate between swap rates we define the swap rates as function values and
# the maturitites as arguments. See the File ./R-Scripts/R-Functions/LinearInterpolation.R for more details

# Function values f
f_swaps_mat <- data_swaps_mat[,-(1:5)]
#                1Y        2Y        3Y        4Y        5Y        7Y       10Y       12Y        15Y       20Y
# Jan 1988  0.030600  0.035000  0.040800  0.042400  0.044000  0.046500  0.049000  0.053100  0.059200  0.069400
# Feb 1988  0.030600  0.035000  0.040800  0.042400  0.044000  0.046500  0.049000  0.055000  0.065400  0.081800
# Mar 1988  0.028100  0.034800  0.039800  0.041800  0.044300  0.046300  0.048800  0.054900  0.065600  0.082500

# Argument x
x_maturities <- c(12, 24, 36, 48, 60, 84, 120, 144, 180, 240)
# 12  24  36  48  60  84 120 144 180 240

# Linear Interpolation 
#######################
swap_rates_interpolated <- t(apply(f_swaps_mat, 1, function(f){LinearInterpolation(f,x_maturities)}))

# Bootstrap
###########
# To perform  the bootstrap the values at maturities of 12m, 24m, ... 240m are of interest
swap_rates_interpolated_yearly <- swap_rates_interpolated[,seq(from=12, to=240, by=12)-11]

# The function BootstrapSpotRates in Bootstrap.R returns annualized spot rates  
spot_rates_bootstrapped_tmp <- t(apply(swap_rates_interpolated_yearly, 1, function(x){BootstrapSpotRates(x)}))

# Add the libor rates
spot_rates_bootstrapped <- cbind(data[,1:5], spot_rates_bootstrapped_tmp)

# Continuously compounded spot rates (see e.g. Annaert et al. (2013))
spot_rates_bootstrapped <- t(apply(spot_rates_bootstrapped, 1, function(x){log(1+x)}))

colnames(spot_rates_bootstrapped) <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", 
                                       "6Y", "7Y", "8Y", "9Y", "10Y", "11Y", "12Y", "13Y", "14Y",
                                       "15Y", "16Y", "17Y", "18Y", "19Y", "20Y")

head(spot_rates_bootstrapped)
write.csv(spot_rates_bootstrapped,paste0(path,"/Results/Spot_Rates_Bootstrapped_88_17.csv"))


# # Only 1M, 2M, 3M, 1Y, 2Y, 3Y, 4Y, 5Y, 7Y, 10Y, 12Y, 15Y, 20Y spot rates are used
# spot_rate_mat0 <- cbind(data_swaps_mat[,1:5], spot_rate_mat_tmp[,round(x_maturities/12,0)])
# 
# i <- 200
# plot(y=spot_rate_mat0[i,], x=c(1:5,x_maturities), xlab="Maturity [in Months]", ylab="Spot rate")
# 
# spot_rate_mat <- cbind(data_swaps_mat[,1:3], spot_rate_mat_tmp[,round(x_maturities/12,0)])
# spot_rate_boot <- as.data.frame(spot_rate_mat)
# colnames(spot_rate_boot) <- c("1M", "2M", "3M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", "15Y", "20Y")
# spot_rate_boot_ts <- ts(spot_rate_boot, start=c(1988,1), freq=12)
# 
# i <- 261 # libor and 1 year swap values for rows 240 to 264 (corresponding to 2008/01 to 2009/12) are affected 
# # due to the financial crisis 
# spot_rate_boot_ts[i,]
# plot(y=spot_rate_boot_ts[i,], x=c(1,2,3,x_maturities), xlab="Maturity [in Months]", ylab="Spot rate")


#-------------------------------------------------------------------------------

# Method 2: Continuously compounded spot rates from libor and swap rates using the exact method of
# Filipovic and Willems (2018)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cash Flow Matrix
################################################################################

# Rename the column names to indicate whether it is a libor (L) or a swap rate (S)
colnames(data_ts) <- c("L1M", "L2M", "L3M", "L6M", "L9M", "S1Y", "S2Y", "S3Y", "S4Y", "S5Y", "S7Y", "S10Y", "S12Y", "S15Y", "S20Y") 
# L1M       L2M       L3M       L6M        L9M       S1Y       S2Y       S3Y       S4Y       S5Y       S7Y      S10Y      S12Y      S15Y
# Jan 1988  0.0163000  0.019100  0.020500  0.025800  0.0294000  0.030600  0.035000  0.040800  0.042400  0.044000  0.046500  0.049000  0.053100  0.059200
# Feb 1988  0.0163000  0.019100  0.020500  0.025800  0.0294000  0.030600  0.035000  0.040800  0.042400  0.044000  0.046500  0.049000  0.055000  0.065400

# When creating the cash flow matrices we assume that every month has 30 days. The cash flows are stored in the list 
# 'cashflow_list'. An instrument with maturity 0 is added
instruments_vec <- colnames(data_ts)
rates_vec <- as.numeric(data_ts[1,])
frame <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", "8Y", "9Y", "10Y", 
             "11Y", "12Y", "13Y", "14Y", "15Y", "16Y", "17Y", "18Y", "19Y", "20Y")

cashflow_list <- list()
for (i in 1:nrow(data_ts)){
  c <- Cashflow_matrix(instruments_vec, data_ts[i,], frame)
  c <- cbind(rep(0, length(instruments_vec)), c)
  c <- rbind(c(1,rep(0, length(frame))), c)
  cashflow_list[[i]] <- c
}

#cashflow_list[[360]]
#cashflow_list[[1]]

# Discount curve estimation according to Filipovic and Willems (2018)
#####################################################################
# Cash flow informations
cashflow_dates_vec <- c(0, 1, 2, 3, 6, 9, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120, 
                        132, 144, 156, 168, 180, 192, 204, 216, 228, 240)
present_values_vec <- rep(1, times=length(instruments_vec)+1)

# Defines at which points in time the discount function is evaluated
#t_vec <- c(1,2,3,6,9,seq(from=12, to=240, by=12))
t_vec <- 1:241

spot_rates_exact <- matrix(0, nrow=nrow(data_ts), ncol=length(t_vec))
for (i in 1:nrow(data_ts)){
  discount_curve <- termStructureFW(cashflow_dates_vec, cashflow_list[[i]], present_values_vec, t_vec)
  spot_rate <- -log(discount_curve)/(t_vec/12)
  spot_rates_exact[i,] <- spot_rate
}

#colnames(spot_rates_exact) <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", 
#                                "6Y", "7Y", "8Y", "9Y", "10Y", "11Y", "12Y", "13Y", "14Y",
#                                "15Y", "16Y", "17Y", "18Y", "19Y", "20Y")

#colnames_t_vec <- apply(matrix(t_vec, nrow=length(t_vec)), 1, function(x){paste0(x,"M")})
colnames_t_vec <- paste0(paste0("M", t_vec),"m")

colnames(spot_rates_exact) <- colnames_t_vec
head(spot_rates_exact)
write.csv(spot_rates_exact,paste0(path,"/Results/Spot_Rates_Exact_88_17.csv"))


# # Check
# ########
# i <- 1 #50, 250
# r <- spotRates_mat[i,]
# 
# plot(y=r, x=t_vec, col="blue", lty = 2)
# lines(y=spot_rates_bootstrapped[i,], x=t_vec)

# __________________________________________
# __________________________________________