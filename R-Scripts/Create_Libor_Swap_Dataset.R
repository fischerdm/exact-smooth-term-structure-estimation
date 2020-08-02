#______________Create_libor_swap_dataset.R______________
#_______________________________________________________
#
# Creates the LIBOR and swap dataset and stores
# it in ./Data
#
# ______________________________________________________

# working directory
path <- getwd()

# import, creation and storage of dataset
data <- read.csv("./Data/Data_Libor_Swap_Rates_88_15.csv", sep=";")
rownames(data) <- c("1M", "2M", "3M", "6M", "9M", "1Y", "2Y", "3Y", "4Y", "5Y", "7Y", "10Y", "12Y", "15Y", "20Y")
data <- data[,-1]

# Comment out if the dataset should also contain values from 16/01 to 17/12!
data_part2 <- read.csv("./Data/Data_Libor_Swap_Rates_16_17.csv", sep=",")
data_part2 <- data_part2[,-1]
data <- cbind(data, data_part2/100)
data <- t(data)
rownames(data) <- NULL
head(data)
write.csv(data,paste0(path,"/Results/Data_Libor_Swap_Rates_88_17.csv"))

# # Due to the financial crisis, the dataset is restricted to 88/01-06/12
# # (2006-1988+1)*12 = 228
# data_restricted <- data[,1:228]
# write.csv(data_restricted, paste0(path,"/Results/Data_Libor_Swap_Rates_88_06.csv"))
# ______________________________________________________
# ______________________________________________________