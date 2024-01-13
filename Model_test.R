source("./Moth_Capture_Celsius.R")
source("./Moth_Capture_Fahrenheit.R")
source("./Phenology_Celsius.R")
source("./Phenology_Fahrenheit.R")

source("./Convert.R")


# This function generates mean counts of codling moth captures in pheromone 
# traps. DDs is a sequence of celsius degree-days for which counts will be 
# produced, ns is the sample size per time (traps) and m is the total number of 
# moths for the simulated trajecotry.




prod_obs <- function(DDs, ns, m) {
  
  dJohnSB_phC <- function(x) {
    gamma = 1.0737
    delta = 1.2394
    xi = 69
    lambda = 577.22
    z = (x - xi) / lambda
    (delta / (lambda * sqrt(pi*2) * z * (1-z))) * 
      exp(-0.5 * (gamma + delta * log(z / (1 - z)))^2)
  }
  
  xint <- -2.538045
  
  key <- function(miu) {
    respu <- rep(NA, length(miu))
    
    for(i in 1: length(miu)) {
      if(miu[i] <= exp(xint)) {
        a <- 2.057018
        b <- 1.160769
        err <- 0.2939689
      } else {
        a <- 6.411532
        b <- 1.608689
        err <- 0.5904043
      }
      
      if(miu[i] <= 0) {
        respu[i] <- 0
      } else {
        respu[i] <- (miu[i]^2) / ((a * miu[i]^b * exp(rnorm(1, mean = 0, sd = err))) - miu[i])
        if(respu[i] <= 0) respu[i] <- 1e-10
      }
      
    }
    
    respu
    
  }
  
  obstw <- rep(NA, length(DDs) - 1)
  porps <- rep(NA, (length(DDs) - 1))
  
  for(i in 1:(length(DDs) - 1)) {
    porps[i] <- sum(dJohnSB_phC(seq(DDs[i], DDs[i + 1], 1)))
  }
  
  for(i in 1:(length(DDs) - 1)) {
    obstw[i] <- mean(sample(rnbinom(100000, size = key(porps[i]*m), 
                                    mu = porps[i]*m), ns))
  }
  resp <- data.frame(DDs = DDs[2: length(DDs)], moths = obstw, traps = rep(ns, length(obstw)))
  resp
}


# Let's use the first 12 data points of a generated dataset as input, 
# but it can be any number.

# Using the version of the model with the phenology-based function (Celsius)
a <- proc_ph_FinC(prod_obs(seq(70, 578, 20), ns = 5, m = 20)[1:12,])

# plot everything
plot(FDD_CDD(set1[1:15, "DDs"]), cumsum(set1[1:15, "moths"]), xlim = c(70, 578), ylim = c(0, 80), type = "o", lwd = 2)
points(a$DDs, a$moths_avg,  type = "o", lwd = 2, col = "red")

points(a$DDs, a$moths_max,  type = "l", lwd = 2, col = "red")
points(a$DDs, a$moths_min,  type = "l", lwd = 2, col = "red")
# plot the remaining data (optional, not included in the function)
points(FDD_CDD(set1$DDs), cumsum(set1$moths), type = "o")

# Using the version of the model with the phenology-based function (Fahrenheits)
b <- proc_ph_FinF(set1[1:15, ])

# plot everything
plot(set1[1:15, "DDs"], cumsum(set1[1:15, "moths"]), xlim = c(100, 1039), ylim = c(0, 80), type = "o", lwd = 2)
points(b$DDs, b$moths_avg,  type = "o", lwd = 2, col = "red")

points(b$DDs, b$moths_max,  type = "l", lwd = 2, col = "red")
points(b$DDs, b$moths_min,  type = "l", lwd = 2, col = "red")
# plot the remaining data (optional, not included in the function)
points(set1$DDs, cumsum(set1$moths), type = "o")



# Using the version of the model with the moth capture function (Celsius)
c <- proc_cap_FinC(set1[1:15, ])

plot(FDD_CDD(set1[1:15, "DDs"]), cumsum(set1[1:15, "moths"]), xlim = c(70, 578), ylim = c(0, 80), type = "o", lwd = 2)
points(c$DDs, c$moths_avg,  type = "o", lwd = 2, col = "red")

points(c$DDs, c$moths_max,  type = "l", lwd = 2, col = "red")
points(c$DDs, c$moths_min,  type = "l", lwd = 2, col = "red")
# plot the remaining data (optional, not included in the function)
points(FDD_CDD(set1$DDs), cumsum(set1$moths), type = "o")


# Using the version of the model with the moth capture function (Fahrenheits)
d <- proc_cap_FinF(set1[1:15, ])

plot(set1[1:15, "DDs"], cumsum(set1[1:15, "moths"]), xlim = c(100, 1039), ylim = c(0, 80), type = "o", lwd = 2)
points(d$DDs, d$moths_avg,  type = "o", lwd = 2, col = "red")

points(d$DDs, d$moths_max,  type = "l", lwd = 2, col = "red")
points(d$DDs, d$moths_min,  type = "l", lwd = 2, col = "red")
# plot the remaining data (optional, not included in the function)
points((set1$DDs), cumsum(set1$moths), type = "o")
