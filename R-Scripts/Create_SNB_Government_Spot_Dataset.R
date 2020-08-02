#______________Create_SNB_Government_Spot_Rates_88-17.R______________
#____________________________________________________________________
#
# Creates two dataset of monthly spot rates using the parameters
# of the Nelson-Siegel-Svensson model published by the SNB.
# The dataset is stored in ./Results.
#
# ___________________________________________________________________


rm(list = ls())

# working directory
path <- getwd()

# Import, creation and storage of dataset.
# The dataset of interest are parmaters for the Nelson-Siegel-Svensson
# model estimated by the Swiss National Bank. The dataset contains
# daily data from 88/01 to 17/12. It contains misisng values
data <- read.csv("./Data/Data_SNB_Svensson_Parameters_88_17.csv", sep=",")

data_values_mat <- matrix(data[-(1:3),2], nrow=1) # throw away the header
data_values_mat_char <- as.character(data_values_mat)
values_mat_clean <- matrix(as.numeric(apply(data_values_mat,1,function(x){substr(x,start=18, stop=nchar(x)-1)})), ncol=6, byrow=T)
# [,1]        [,2]        [,3]         [,4]        [,5]        [,6]
# [1,]  4.3860000  -4.1370000  -0.0010000  -1.62900000   0.1820000  2.54900000
# [2,]         NA          NA          NA           NA          NA          NA
# [3,]         NA          NA          NA           NA          NA          NA

# Get the dates
data_dates <- unique(apply(data_values_mat,1,function(x){substr(x,start=1, stop=10)}))

data_rearranged_df <- data.frame(substr(data_dates, start=1, stop=7), substr(data_dates, start=9, stop=10), values_mat_clean)
colnames(data_rearranged_df) <- c("Year_Month", "Day", "B0", "B1", "B2", "B3", "T1", "T2")
# Year_Month Day         B0          B1          B2           B3          T1          T2
# 1       1988-01  04  4.3860000  -4.1370000  -0.0010000  -1.62900000   0.1820000  2.54900000
# 2       1988-01  05         NA          NA          NA           NA          NA          NA

# 18      1988-01  27         NA          NA          NA           NA          NA          NA
# 19      1988-01  28         NA          NA          NA           NA          NA          NA
# 20      1988-01  29  4.4620000  -3.9010000  -0.5910000   4.61700000   1.5220000  0.49900000

# Get rid of NAs and convert Day to 'numeric'
data_rearranged_NAs_removed_df <- data_rearranged_df[!is.na(data_rearranged_df$B0),]
data_rearranged_NAs_removed_df$Day <- as.numeric(as.character(data_rearranged_NAs_removed_df$Day))

# Choose the latest date of each month
dates_of_interest <- aggregate(data_rearranged_NAs_removed_df$Day, by=list(data_rearranged_NAs_removed_df$Year_Month), FUN=max)
dates_of_interest_char <- apply(dates_of_interest, 1, function(x){paste0(paste0(x[1], "-"), x[2])})
dates_of_interest_char_df <- data.frame(Date=dates_of_interest_char)

# Create dates out of column 1 and column 2 of the 'data_rearranged_df' data frame
dates_char <- data_rearranged_df[,1:2]
dates_char <- apply(dates_char,1,function(x){paste0(paste0(x[1], "-"), x[2])})
data_rearranged_dates_as_char_df <- data.frame(Date=dates_char, data_rearranged_df[,-c(1,2)])
# Date         B0          B1          B2           B3          T1          T2
# 1    1988-01-04  4.3860000  -4.1370000  -0.0010000  -1.62900000   0.1820000  2.54900000

# Merge
data_sub_df <- merge(dates_of_interest_char_df, data_rearranged_dates_as_char_df, by="Date")
#str(data_sub_df)

# Date         B0          B1         B2          B3        T1         T2
# 1   1988-01-29  4.4620000  -3.9010000  -0.591000   4.6170000  1.522000  0.4990000
# 2   1988-02-29  4.4650000  -3.9040000  -0.021000   3.5440000  1.466000  0.3570000
# 3   1988-03-31  4.9990000  -3.6330000  -0.823000  -2.5490000  0.728000  6.4350000
# 4   1988-04-29  4.3660000  -2.6310000   0.034000   2.5270000  3.046000  1.1310000
# 5   1988-05-31  4.3110000  -1.5980000  -1.742000  -0.4490000  0.425000  3.6960000
# 6   1988-06-30  4.5330000  -1.3350000  -0.563000   0.8330000  2.820000  0.4980000

# Create spot rates by plugging in Nelson-Siegel-Svensson for maturities 
# 1M, 2M,.., 240M. The result
# is a data frame containing continously compounded interest rates for 
# the maturities given.

# m_char <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", "15Y", "20Y")
# m <- c(1/12, 2/12, 3/12, 6/12, 9/12, 1, 2, 3, 4, 5, 7, 10, 12, 15, 20)

m <- (1:241)/12 
t_vec <- 1:241 
# m_char <- apply(matrix(t_vec, nrow=length(t_vec)), 1, function(x){paste0(x,"M")})
m_char <- paste0(paste0("M", t_vec), "m")
m_char 

svensson <- function(x,m){
  B0 <- x[2]
  B1 <- x[3]
  B2 <- x[4]
  B3 <- x[5]
  T1 <- x[6]
  T2 <- x[7]
  spot <- B0 + B1*((1-exp(-m/T1))/(m/T1)) + B2*((1-exp(-m/T1))/(m/T1)-exp(-m/T1)) + B3*((1-exp(-m/T2))/(m/T2)-exp(-m/T2))
  spot
}


data_spot_rates <- t(apply(data.matrix(data_sub_df), 1, function(x){svensson(x,m)}))
data_spot_rates_df <- data.frame(data_spot_rates)/100
colnames(data_spot_rates_df) <- m_char


# Dataset
head(data_spot_rates_df[1:360,])
write.csv(data_spot_rates_df[1:360,], paste0(path, "/Results/Data_SNB_Government_Spot_Rates_88_17.csv"))

