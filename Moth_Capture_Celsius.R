# This function produces a predicted trajectory of cumulative average counts of 
# codling moth adults captured in pheromone traps.The input is a data.frame 
# with n > 3 number of observations with three variables: cumulative 
# degree-days, average moths captured, and number of traps. They should be named
# "DDs", "moths", and "traps", respectively. 

# This version makes all the calculations in Celsius degree-days, although the 
# input could be provided in Fahrenheit, in which case conv.from.far = TRUE, 
# otherwise should be FALSE. 

# The argument pred.lim stands for prediction limit in Celsius degree-days, 
# which by default is 578 for the overwintering generation of 
# the codling moth, and greater values are not accepted. The minimum allowed 
# number of cumulative degree days is 70C (126F).

# The output is a data.frame with observed and predicted average counts of 
# codling moths adults with the following columns:

# 1. DDs: Cumulative degree-days
# 2. moths_obs: Observed cumulative mean counts
# 3. moths_model: Predicted cumulative mean counts
# 4. moths_max: Upper limit of the prediction interval
# 5. moths_min: Lower limit of the prediction interval

# This version of the model uses an empirical moth capture function.

source("./Functions.R")

proc_cap_FinC <- function(data, pred.lim = 578, conv.from.far = FALSE) {
  
  pJohnSB_cap <- function(x) {
    gamma = 0.4603673
    delta = 0.8674057
    xi = 69.22063
    lambda = 662.5367
    pnorm(gamma + delta * (log((x - xi) / (lambda - (x - xi)))), 0 , 1)
  }
  
  rmse <- function (actual, predicted) 
  {
    return(sqrt(mean((actual - predicted)^2)))
  }
  
  dataC <- data
  
  if(conv.from.far == TRUE) {
    dataC$DDs <- FDD_CDD(dataC$DDs)
  } else {
    dataC$DDs <- dataC$DDs
  }
  
  if((pred.lim > 578) | (length(dataC$DDs) < 3) | any(dataC$DDs < 70)) {
    stop("You have either too few data, or an incorrect prediction limit, or data below 70 DDs")
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
    prop <- pJohnSB_cap(ddss[length(ddss)])
    
    ms1 <- pJohnSB_cap(seq(70, pred.lim)) * (x[length(x)]) / prop
    
    ms_up <- pJohnSB_cap(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                       qnorm(0.9))) / prop
    ms_down <- pJohnSB_cap(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                         qnorm(0.9))) / prop
    
    res_err <- rmse(x, pJohnSB_cap(ddss) * (x[length(x)]) / prop)
    
    
    desvi1 <- rep(NA, length(ms_up))
    
    for(i in 1:length(ms_up)){
      if (ms_up[i] > 0) {
        desvi1[i] <- (((deltamethodV2(seq(round(ddss[length(ddss)]), pred.lim)[i]) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                                        qnorm(0.9))) / prop) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 577.22)))
      }
      else desvi1 <- 0
    }
    
    desvi2 <- rep(NA, length(ms_down))
    
    for(i in 1:length(ms_down)){
      if (ms_down[i] > 0) {
        desvi2[i] <- (((deltamethodV2(seq(round(ddss[length(ddss)]), pred.lim)[i]) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) *
                                                                                                        qnorm(0.9))) / prop) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 577.22)))
      }
      else desvi2 <- 0
    }
    
    up11 <- ms_up + desvi1
    lo22 <- ms_down - desvi2
    
    lo22[which(lo22 < x[length(x)])] <- x[length(x)]
    
  }
  
  moths_pred <- ms1[(round(ddss[length(ddss)]) - 69): length(ms1)]
  
  data.frame(DDs = seq(round(ddss[length(ddss)]), pred.lim), 
             moths_avg = moths_pred, 
             moths_max = up11, 
             moths_min = lo22)
}
