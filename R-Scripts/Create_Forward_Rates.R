#______________Create_Forward_Rates.R______________
#__________________________________________________
#
# This script creates forward rates from spot
# rates
# 
# _________________________________________________

rm(list = ls())

path <- getwd()

# Imports the variable 'maturities_GLOBAL'
source(paste0(path,"/R-Scripts/Restricted_Maturities.R"))

# DATA
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tau <- 1:240
tau_char <- apply(matrix(tau, nrow=length(tau)), 1, function(x){paste0(x,"M")})

# SNB data
################################################################################
snb_spot_data <- read.csv(paste0(path, "/Results/Data_SNB_Government_Spot_Rates_88_17.csv"))
snb_spot_data$X <- NULL

colnames(snb_spot_data) <- tau_char
snb_spot_data_ts <- ts(snb_spot_data, start=c(1988,1), freq=12)


# Libor and swap rates  
################################################################################
libor_swap_spot_data <- read.csv(paste0(path, "/Results/Spot_Rates_Exact_88_17.csv"))
libor_swap_spot_data$X <- NULL

colnames(libor_swap_spot_data) <- tau_char
libor_swap_spot_data_ts <- ts(libor_swap_spot_data, start=c(1988,1), freq=12)



# FORWARD RATES
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tau1 <- maturities_GLOBAL
tau2 <- tau1 + 1
tau1_y <- tau1/12 # in years
tau2_y <- tau2 / 12 # in years 

# SNB dataset
################################################################################
yield1 <- snb_spot_data[,tau1]
yield2 <- snb_spot_data[,tau2]

forward_snb <- (tau2_y*yield2 - tau1_y*yield1)/(1/12) # t2 - t1 = 1/12 (of a year)
colnames(forward_snb) <- paste0("M",paste0(tau1, "m"))

i <- 1 # 50
plot(y=forward_snb[i,], x=tau1)
plot(y=yield1[i,], x=tau1)

# Save to disk
# ------------------------------------------------------------------------------
write.csv(forward_snb,paste0(path,"/Results/Forward_SNB_88_17_restricted.csv"))

# Libor swap dataset
################################################################################
yield1 <- libor_swap_spot_data[,tau1]
yield2 <- libor_swap_spot_data[,tau2]

forward_libor_swap <- (tau2_y*yield2 - tau1_y*yield1)/(1/12) # t2 - t1 = 1/12 (of a year)
colnames(forward_libor_swap) <- paste0("M",paste0(tau1, "m"))

i <- 1 # 50
plot(y=forward_libor_swap[i,], x=tau1)
plot(y=yield1[i,], x=tau1)

# Save to disk
# ------------------------------------------------------------------------------
write.csv(forward_libor_swap,paste0(path,"/Results/Forward_Libor_Swap_88_17_restricted.csv"))





################################################################################
################################################################################

# # INSTANTANEOUS FORWARD RATE
# 
# rm(list = ls())
# path <- getwd()
# 
# source(paste0(path,"/R-Scripts/R-Functions/CashFlowMatrix.R"))
# source(paste0(path,"/R-Scripts/R-Functions/ExactSmoothTermStructure.R"))
# 
# libor_swap_rate_data <- read.csv(paste0(path, "/Results/Data_Libor_Swap_Rates_88_17.csv"))
# libor_swap_rate_data$X <- NULL
# libor_swap_rate_data_ts <- ts(libor_swap_rate_data, start=c(1988,1), freq=12)
# colnames(libor_swap_rate_data_ts) <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", 
#                                        "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", 
#                                        "15Y", "20Y")
# 
# 
# # Similar to 'Create_Spot_Rates_From_Libor_Swap_Data.R'
# 
# # Distinguish between libor and swap rates
# colnames(libor_swap_rate_data_ts) <- c("L1M", "L2M", "L3M", "L6M", "L9M", "S1Y", 
#                                        "S2Y", "S3Y", "S4Y", "S5Y", "S7Y", "S10Y", 
#                                        "S12Y", "S15Y", "S20Y") 
# 
# 
# # We assume that every month has 30 days. The cash flows are stored in the list 
# # 'cashflow_list'. An instrument with maturity 0 is added
# instruments_vec <- colnames(libor_swap_rate_data_ts)
# frame <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", "6Y", "7Y", 
#            "8Y", "9Y", "10Y", "11Y", "12Y", "13Y", "14Y", "15Y", "16Y", "17Y", 
#            "18Y", "19Y", "20Y")
# 
# cashflow_list <- list()
# for (i in 1:nrow(libor_swap_rate_data_ts)){
#   c <- Cashflow_matrix(instruments_vec, libor_swap_rate_data_ts[i,], frame)
#   c <- cbind(rep(0, length(instruments_vec)), c)
#   c <- rbind(c(1,rep(0, length(frame))), c)
#   cashflow_list[[i]] <- c
# }
# 
# 
# # Forward curve construction according to Filipovic and Willems (2018)
# #-------------------------------------------------------------------------------
# # Numeric cash flow dates as months from now
# cashflow_dates_vec <- c(0, 1, 2, 3, 6, 9, 12, 24, 36, 48, 60, 72, 84, 96, 108, 
#                         120, 132, 144, 156, 168, 180, 192, 204, 216, 228, 240)
# # All the instruments have present value 1
# present_values_vec <- rep(1, times=length(instruments_vec)+1)
# 
# # Defines at which points in time the forward rate curve is evaluated
# t_vec <- c(1,2,3,6,9,seq(from=12, to=240, by=12))
# t_vec <- (1:(240*30))/30
# 
# forward_rates_libor_swap <- matrix(0, nrow=nrow(libor_swap_rate_data_ts), 
#                                    ncol=length(t_vec))
# for (i in 1:nrow(libor_swap_rate_data_ts)){
#   forward_curve <- termStructureForwardFW(cashflow_dates_vec, cashflow_list[[i]], 
#                                           present_values_vec, t_vec)
#   forward_rates_libor_swap[i,] <- forward_curve
# }
# 
# # Plot
# #-------------------------------------------------------------------------------
# plot(y=forward_rates_libor_swap[200,], x=t_vec, type="l")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

