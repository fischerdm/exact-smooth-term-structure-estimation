######################################################
################ Cash flow matrix ####################
######################################################

# Cashflow sequence (helper function)
# ----------------------------------
CashFlowSequence <- function(type, rate, frame){
  # INPUT:
  #  - type: type of the instrument: here Libor, LX(X)M, or swap, SX(X)Y,  or 
  #          Tommorrow Next, TM: Character
  #  - rate: annualized interest rate: Numeric
  #  - frame: vector of characters as e.g. ("TM", "3M", "1Y", ...., "30Y") 

  cashflows <- rep(0, length(frame))
  
  if (type=="TM"){
    ind <- which(frame==type)
    # fraction of the annualized rate
    cashflows[ind] <- 1 + rate/360
    
    
  } else if (substr(type,1,1)=="L"){
    maturity_char <- substr(type, 2, nchar(type))
    maturity_num <- as.numeric(substr(type, 2, nchar(type)-1))
    ind <- which(frame==maturity_char)
    
    # fraction of the annualized rate. It is assumed that a month has 30 days
    cashflows[ind] <- 1 + rate*maturity_num/12
    
    
  } else if (substr(type,1,1)=="S"){
    maturity_end_num <- as.numeric(substr(type, 2, nchar(type)-1))
    maturity_start_num <- 1 # swap cashflows start in year 1
    
    # Extract end date of the cashflow sequence
    frame_last_entry <- frame[length(frame)]
    last_cashflow_date_char <- substr(frame_last_entry, 1, nchar(frame_last_entry))
    
    # Check if it is not a year. If it is not a year, then break
    if (substr(last_cashflow_date_char, nchar(last_cashflow_date_char), nchar(last_cashflow_date_char)) != "Y"){
      break
    }
    
    last_cashflow_date_num <- as.numeric(substr(last_cashflow_date_char, 1, nchar(last_cashflow_date_char)-1))
    for (i in 1:maturity_end_num){
      maturity_num <- maturity_start_num + i-1 
      maturity_char = paste0(maturity_num,"Y")
      ind <- which(frame==maturity_char)
      
      if (maturity_num == maturity_end_num){
        cashflows[ind] <- 1 + rate
      } else {
        cashflows[ind] <- rate  
      }
    }
    
  } else {
    print ("Unknown instrument")
  }
  return(cashflows)  
}


# Example
# frame <- c("TM", "3M", "1Y", "2Y", "3Y")
# 
# CashFlowSequence("TM", 0.03, frame)
# # [1] "Tomorrow Next"
# # [1] 0.03 0.00 0.00 0.00 0.00
# CashFlowSequence("L3M", 0.03, frame)
# # [1] "LIBOR"
# # [1] 0.00 0.03 0.00 0.00 0.00
# CashFlowSequence("S1Y", 0.03, frame)
# # [1] "Swap"
# # [1] 0.00 0.00 1.03 0.00 0.00
# CashFlowSequence("S2Y", 0.03, frame)
# # [1] "Swap"
# # [1] 0.00 0.00 0.03 1.03 0.00
# CashFlowSequence("S3Y", 0.03, frame)
# # [1] "Swap"
# # [1] 0.00 0.00 0.03 0.03 1.03


# Cashflow matrix 

Cashflow_matrix <- function(instruments_vec, rates_vec, frame_vec){
  
  # INPUT: 
  # instrumetns_vec: vector with names of the instruments: TM, LX(X)Y, SX(X)Y.
  # rates_vec: vector of rates belonging to the instruments (numeric). No NA are allowed!
  # frame_vec: vector of dates as character: "TM" (Tomorrow), "3M" (in three months),
  # "1Y", "2Y", ..., "30Y"
  
  # OUTPUT:
  # matrix of cashflows. In the ith row the cashflows of the ith instruments are stored. 
  
  i <- 1
  cashflow_mat <- matrix(0, nrow=length(instruments_vec), ncol=length(frame_vec))
  
  for (instr in instruments_vec){
    rate <- rates_vec[i]
    cashflow_mat[i,] <- CashFlowSequence(instr, rate, frame_vec)
    i <- i + 1
  }
  
  cashflow_mat
  
}
