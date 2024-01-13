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
source("./Convert.R")

proc_cap_FinC <- function(data, pred.lim = 578, conv.from.far = TRUE) {
  
  pJohnSB_ph <- function(x) {
    gamma = 0.4603673
    delta = 0.8674057
    xi = 69.22063
    lambda = 662.5367
    pnorm(gamma + delta * (log((x - xi) / (lambda - (x - xi)))), 0 , 1)
  }
  
  xint <- -2.538045
  
  key1 <- function(miu) {
    respu <- rep(NA, length(miu))
    
    for(i in 1: length(miu)) {
      if(miu[i] <= exp(xint)) {
        a <- 2.057018
        b <- 1.160769
      } else {
        a <- 6.411532
        b <- 1.608689
      }
      
      if(miu[i] <= 0) {
        respu[i] <- 0
      } else {
        respu[i] <- (miu[i]^2) / ((a * miu[i]^b) - miu[i])
        if(respu[i] <= 0) respu[i] <- 1e-10
      }
      
    }
    
    respu
    
  }
  
  desv <- function(miu, k) {
    miu + ((miu^2) / k)
  }
  
  deltamethodV2 <- function(x){
    x1 = 0.4603673
    x2 = 0.8674057
    x3 = 69.22063
    x4 = 662.5367
    y = x
    
    e2 <- x3 + x4 - y
    e3 <- y - x3
    e6 <- log(e3) - log(e2)
    e7 <- dnorm(x1 + x2 * e6, 0, 1)
    
    gg <- t(c(x1 = e7, x2 = e7 * e6, x3 = -(x2 * (e3/e2 + 1) * e7/e3), 
              x4 = -(x2 * e7/e2)))
    
    
    vcov = matrix(c(0.02903016, 0.010977323, -0.1683978, 5.746995,
                    0.01097732, 0.008940147, -0.3770118, 2.938212,
                    -0.16839779, -0.377011833, 57.3592901, -99.173763,
                    5.74699489, 2.938211516, -99.1737628, 1676.090297), 4, 4)
    
    var.d <- gg %*% vcov %*% t(gg)
    
    
    sqrt(diag(var.d))
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
    prop <- pJohnSB_ph(ddss[length(ddss)])
    
    ms1 <- pJohnSB_ph(seq(70, pred.lim)) * (x[length(x)]) / prop
    
    ms_up <- pJohnSB_ph(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                       qnorm(0.9))) / prop
    ms_down <- pJohnSB_ph(seq(round(ddss[length(ddss)]), pred.lim)) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                         qnorm(0.9))) / prop
    
    res_err <- rmse(x, pJohnSB_ph(ddss) * (x[length(x)]) / prop)
    
    
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
