#______________Restrict_Datasets.R______________
#_______________________________________________
#
# This script restricts the datasets to 
# 
# ______________________________________________

rm(list = ls())

path <- getwd()

# Imports the variable 'maturities_GLOBAL'
source(paste0(path,"/R-Scripts/Restricted_Maturities.R"))

# Restrictions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tau <- maturities_GLOBAL

# SNB
# ------------------------------------------------------------------------------
data <- read.csv(paste0(path, "/Results/Data_SNB_Government_Spot_Rates_88_17.csv"))
data$X <- NULL

data_sub <- data[,tau]

write.csv(data_sub,paste0(path,"/Results/Data_SNB_Government_Spot_Rates_88_17_restricted.csv"))

# Libor and swap data
# ------------------------------------------------------------------------------
data <- read.csv(paste0(path, "/Results/Spot_Rates_Exact_88_17.csv"))
data$X <- NULL

data_sub <- data[,tau]

write.csv(data_sub,paste0(path,"/Results/Spot_Rates_Exact_88_17_restricted.csv"))