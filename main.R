#______________Main________________
#__________________________________
#
# This is the main script to create
# the dataset
#
# _________________________________

rm(list = ls())

# Packages
##########
source("./R-Scripts/Packages.R")

# DATASETS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SNB Dataset (Government Bond Yields)
# ------------------------------------------------------------------------------
# Create the dataset with the parameters for the Nelson-Siegel model from the 
# snb homepage (https://data.snb.ch/de/topics/ziredev#!/cube/rendopar). 
# Continuously compounded annualized interest rates between 01/1988 and 12/2017 
# for maturities 1m, 2m, ..., 241m are stored in
# ./Results/Data_SNB_Government_Spot_Rates_88_17.csv
source("./R-Scripts/Create_SNB_Government_Spot_Dataset.R")

# Libor Swap Dataset
# ------------------------------------------------------------------------------
# Create dataset containing libor and swap rates. The dataset contains 
# monthly libor and swap rates between 01/1988 and 12/2017 for maturities
# 1m, 2m, ..., 241m from the snb homepage
# (https://data.snb.ch/de/topics/ziredev#!/cube/zimoma). The data is stored
# in ./Results/Data_Libor_Swap_Rates_88_17.csv
source("./R-Scripts/Create_Libor_Swap_Dataset.R")

# Spot Rates from Libor Swap data
# ------------------------------------------------------------------------------
# In order to get continuously compounded spot from libor and spot
# rates two methods are used:
# - Bootstrap of spot rates
# - The exact method of Filipovic and Willems (2018)
# The dataset with the bootstrapped spot rates contains values for the 
# maturities 1m, 2m, 3m, 6m, 9m, 12m, 24m, 36m, 48m, 60m, 72m, 84m, 96m, 108m,
# 120m, 132m, 144m, 156m, 168m, 180m, 192m, 204m, 216m, 228m, 240m. The dataset
# constructed with the method of Filipovic and Willems (2018) contains rates for
# 1m, 2m, ..., 241m.
source("./R-Scripts/Create_Spot_Rates_From_Libor_Swap_Data.R")

# Restricted datasets
# ------------------------------------------------------------------------------
# The spot rates from the SNB data and from the libor and swap rates calculated 
# with the exact method of Filipovic and Willems (2018) are restricted to 
# 3m, 6m, 9m, 12m, 15m, 18m, 21m, 24m, 30m, 36m, 48m, 60m, 72m, 84m, 96m, 108m,
# 120m, 144m, 180m, 240m. Up to 10 years the maturities equal to the ones used 
# in Diebold and Li (2006) 
source("./R-Scripts/Restrict_Datasets.R")

# Forward Rates
# ------------------------------------------------------------------------------
# Forward rates are constructed from the unrestricted SNB dataset and the dataset
# containing spot rates from libor and swap rates using the method of Filipovic 
# and Willems (2018). The datsets are then restricted to the maturities 3m, 6m, 
# 9m, 12m, 15m, 18m, 21m, 24m, 30m, 36m, 48m, 60m, 72m, 84m, 96m, 108m,
# 120m, 144m, 180m, 240m 
source("./R-Scripts/Create_Forward_Rates.R")


# PRELIMINARY ANALYSIS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Comparing spot rates at different points in time using the unrestricted datasets
source("./R-Scripts/Comparison.R")


# _______________________________
# _______________________________
