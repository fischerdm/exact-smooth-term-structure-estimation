#______________Preliminary_Analysis.R______________
#__________________________________________________
#
# This script creates plots to compare the bootstrapped 
# spot rates (dots) vs the rates created with the method of 
# Filipovic and Willems (2018) (green line). The government
# bonds yields are also plotted (black line).
# 
# ___________________________________________________

rm(list = ls())

path <- getwd()

# Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# SNB data
################################################################################
snb_data <- read.csv(paste0(path, "/Results/Data_SNB_Government_Spot_Rates_88_17.csv"))
snb_data$X <- NULL
t_vec <- 1:241
colnames_t_vec <- apply(matrix(t_vec, nrow=length(t_vec)), 1, function(x){paste0(x,"M")})
colnames(snb_data) <- paste0(paste0("M", t_vec), "m")
colnames(snb_data) <- colnames_t_vec
snb_data_ts <- ts(snb_data, start=c(1988,1), freq=12)

# Libor swap data
################################################################################

# Bootstrapped spot rates
# ------------------------------------------------------------------------------
libor_swap_data_bootstrapped_tmp <- read.csv(paste0(path, "/Results/Spot_Rates_Bootstrapped_88_17.csv"))
libor_swap_data_bootstrapped_tmp$X <- NULL
#str(libor_swap_data_bootstrapped_tmp)
# colnames(libor_swap_data_bootstrapped) <- c("1M", "2M", "3M", "6M", "9M", "12M", "24M", "36M", 
#                                             "48M", "60M", "84M", "120M", "144M", "180M", "240M")
t_vec <- 1:241
t_vec_boot <- c(1,2,3,6,9,12,24,36,48,60,72,84,96,108,120,132, 144,156,168,180,192,204, 216, 228, 240)
libor_swap_data_bootstrapped <- matrix(NA, nrow=nrow(libor_swap_data_bootstrapped_tmp), ncol=length(t_vec))
libor_swap_data_bootstrapped[,t_vec_boot] <- data.matrix(libor_swap_data_bootstrapped_tmp)

libor_swap_data_bootstrapped <- data.frame(libor_swap_data_bootstrapped)
# colnames_t_vec <- apply(matrix(t_vec, nrow=length(t_vec)), 1, function(x){paste0(x,"M")})
colnames_t_vec <- paste0(paste0("M", t_vec), "m")
colnames(libor_swap_data_bootstrapped) <- colnames_t_vec
libor_swap_data_bootstrapped_ts <- ts(libor_swap_data_bootstrapped, start=c(1988,1), freq=12)

# Exact method of Filipovic and Willems (2018)
# ------------------------------------------------------------------------------
libor_swap_data_exact <- read.csv(paste0(path, "/Results/Spot_Rates_Exact_88_17.csv"))
libor_swap_data_exact$X <- NULL
t_vec <- 1:241
# colnames_t_vec <- apply(matrix(t_vec, nrow=length(t_vec)), 1, function(x){paste0(x,"M")})
colnames_t_vec <- paste0(paste0("M", t_vec), "m")
colnames(libor_swap_data_exact) <- colnames_t_vec
libor_swap_data_exact_ts <- ts(libor_swap_data_exact, start=c(1988,1), freq=12)

# Analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The dataset contains term structures of interest rates over 30 years.
# Here, we will take a closer look at the data
# ------------------------------------------------------------------------------

# Using function of the zoo package
dates <- as.yearmon(time(libor_swap_data_exact_ts))

# Plots 
# -------
# Literature:
# For general concept: http://md.psych.bio.uni-goettingen.de/mv/unit/ggplot2/ggplot2.html
# To format titles: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles 
# To save the plot: http://ggplot2.tidyverse.org/reference/ggsave.html
# To add a legend: https://stackoverflow.com/questions/40833809/add-legend-to-geom-line-graph-in-r

# Remarks:
# blue points: bootstrapped continuously compounded spot rates from libor and swap rates
# green line: continuously compounded spot rates calculated with the exact method of 
#             Filipovic and Willems (2018) from libor and swap rates
# black line: continuously compounded spot rates from government bonds using the factors
#             of the Nelson-Siegel-Svensson model published by the SNB.

for (i in c(90, 180,270, 360)){
  spot_rates_data <- data.frame(y_exact = libor_swap_data_exact_ts[i,], 
                                y_boot = libor_swap_data_bootstrapped_ts[i,],
                                y_snb = snb_data_ts[i,],
                                x=t_vec)
  
  spot_rates_plot <- ggplot(spot_rates_data, aes(x=x)) 
  spot_rates_plot + geom_line(aes(y=y_exact, color='Exact')) +
    geom_point(aes(y=y_boot, color='Bootstrapped')) +
    geom_line(aes(y=y_snb, color='Nelson-Siegel')) +
    ggtitle(paste("Term structure of interest rates at", dates[i])) +
    xlab("Maturity (months)") + 
    ylab("Continuously compounded spot rate") +
    theme(plot.title = element_text(face="bold", hjust = 0.5)) +
    #scale_color_discrete(name = "Methods", labels = c("y_exact", "y_boot", "y_snb"))
    scale_color_manual(values = c('Exact' = 'blue',
                                  'Bootstrapped' = 'green',
                                  'Nelson-Siegel' = 'black')) +
    labs(color = 'Method')
  
  # save it as png
  ggsave(filename=paste0(paste0(paste0(path,"/Results/Plots/Term_structure_"), i),".png")) 
  
}


aes(y=Y1,x= X,colour="darkblue")
