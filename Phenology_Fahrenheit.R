# This function produces a predicted trajectory of cumulative average counts of 
# codling moth adults captured in pheromone traps.
# The input is a data.frame with n > 3 number of observations with three 
# variables: cumulative degree-days, average moths captured,
# and number of traps. They should be named "DDs", "moths", and "traps", 
# respectively. 

# This version makes all the calculations in Fahrenheit degree-days, although 
# the input could be provided in Celsius, in which case conv.from.cel = TRUE, 
# otherwise should be FALSE. 

# The argument pred.lim stands for prediction limit in Fahrenheit degree-days, 
# which by default is 1039 for the overwintering generation of the codling moth, 
# and greater values are not accepted. The minimum allowed number of cumulative 
# degree days is 126F (70C).

# The output is a data.frame with observed and predicted average counts of 
# codling moths adults with the following columns:

# 1. DDs: Cumulative degree-days
# 2. moths_obs: Observed cumulative mean counts
# 3. moths_model: Predicted cumulative mean counts
# 4. moths_max: Upper limit of the prediction interval
# 5. moths_min: Lower limit of the prediction interval

# This version of the model uses the phenology-based function from 
# Jones & Wiman (2012).

source("./Functions.R")

proc_ph_FinF <- function(data, pred.lim = 1039, conv.from.cel = FALSE) {
  
  pJohnSB_phF <- function(x) {
    gamma = 1.0737
    delta = 1.2394
    xi = 124.2
    lambda = 1039
    pnorm(gamma + delta * (log((x - xi) / (lambda - (x - xi)))), 0 , 1)
  }
  
  rmse <- function (actual, predicted) 
  {
    return(sqrt(mean((actual - predicted)^2)))
  }
  
  dataC <- data
  
  if(conv.from.cel == TRUE) {
    dataC$DDs <- CDD_FDD(dataC$DDs)
  } else {
    dataC$DDs <- dataC$DDs
  }
  
  if((pred.lim > 1039) | (length(dataC$DDs) < 3) | any(dataC$DDs < 126)) {
    stop("You have either too few data, or an incorrect prediction limit, or data below 126 DDs")
  }
  
  ddss <- dataC$DDs
  x <- cumsum(dataC$moths)
  ns <- mean(dataC$traps)
  
  if(sum(x) == 0) {
    ms1 <- rep(0, 901)
    ms_up <- rep(0, length(coll))
    ms_down <- rep(0, length(coll))
    up11 <- rep(0, length(coll))
    lo22 <- rep(0, length(coll))
  } else {
    prop <- pJohnSB_phF(ddss[length(ddss)])
    
    ms1 <- pJohnSB_phF(seq(125, pred.lim)) * (x[length(x)]) / prop
    
    ms_up <- pJohnSB_phF(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                       qnorm(0.9))) / prop
    ms_down <- pJohnSB_phF(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                         qnorm(0.9))) / prop
    
    res_err <- rmse(x, pJohnSB_phF(ddss) * (x[length(x)]) / prop)
    
    desvi1 <- rep(NA, length(ms_up))
    
    for(i in 1:length(ms_up)){
      if (ms_up[i] > 0) {
        desvi1[i] <- (((sqrt(desv(ms_up[i], key1(ms_up[i])) / ns)) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 1039)))
      }
      else desvi1 <- 0
    }
    
    desvi2 <- rep(NA, length(ms_down))
    
    for(i in 1:length(ms_down)){
      if (ms_down[i] > 0) {
        desvi2[i] <- (((sqrt(desv(ms_down[i], key1(ms_down[i])) / ns)) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 1039)))
      }
      else desvi2 <- 0
    }
    
    up11 <- ms_up + desvi1
    lo22 <- ms_down - desvi2
    
    lo22[which(lo22 < x[length(x)])] <- x[length(x)]
    
  }
  
  moths_pred <- ms1[(round(ddss[length(ddss)]) - 124): length(ms1)]
  
  data.frame(DDs = seq(round(ddss[length(ddss)]), pred.lim), 
             moths_avg = moths_pred, 
             moths_max = up11, 
             moths_min = lo22)
}