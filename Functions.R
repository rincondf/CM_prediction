# Required functions to run sensitivity and validation analysis.


# Functions to convert from Celsius degree-days to Fahrenheit degree-days and vice versa.
# From C to F
CDD_FDD <- function(x) {
  (9/5) * x
}

# From F to C
FDD_CDD <- function(x) {
  (5/9) * x
}

# Load variance-mean model
load("lm_varmean.RData")


# Functions run sensitivity analysis
# Delta method to find confidence intervals for the moth-capture model
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

# Functions to find k negative binimial from Taylor's power law parameters.
# Primary function
key1 <- function(miu) {
  respu <- rep(NA, length(miu))
  
  for(i in 1: length(miu)) {
    if(miu[i] <= exp(xint)) {
      a <- exp(coef(vmmod2)[1])
      b <- coef(vmmod2)[2]
    } else {
      a <- exp(coef(vmmod)[1])
      b <- coef(vmmod)[2]
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

# Function including uncertainty
key <- function(miu) {
  respu <- rep(NA, length(miu))
  
  for(i in 1: length(miu)) {
    if(miu[i] <= exp(xint)) {
      a <- exp(coef(vmmod2)[1])
      b <- coef(vmmod2)[2]
      err <- sqrt(mean(vmmod2$residuals^2))
    } else {
      a <- exp(coef(vmmod)[1])
      b <- coef(vmmod)[2]
      err <- sqrt(mean(vmmod$residuals^2))
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

# Function to estimate variance from k and mean
desv <- function(miu, k) {
  miu + ((miu^2) / k)
}

# Function to generate trajectories from a negative binomial
prod_obs2.0 <- function(weeks, ns, m) {
  
  obstw <- rep(NA, length(weeks) - 1)
  porps <- rep(NA, (length(weeks) - 1))
  
  for(i in 1:(length(weeks) - 1)) {
    porps[i] <- sum(dJohnsonSB(seq(weeks[i], weeks[i + 1], 1), 
                               params = list(gamma = 1.0737, 
                                             delta = 1.2394, 
                                             xi = 69, lambda = 577.22)))
  }
  
  for(i in 1:(length(weeks) - 1)) {
    obstw[i] <- mean(sample(rnbinom(100000, size = key(porps[i]*m), 
                                    mu = porps[i]*m), ns))
  }
  resp <- data.frame(DDs = weeks[2: length(weeks)], moths = obstw, traps = rep(ns, length(obstw)))
  resp
}

# Function to test the moth-capture model using computer generated trajectories
test_proc_cap <- function(data, lim, to) {
  dataC <- data
  dataC$DDs <- FDD_CDD(dataC$DDs)
  
  ddss <- dataC$DDs[which(dataC$DDs <= lim)]
  x <- cumsum(dataC$moths[which(dataC$DDs <= lim)])
  ns <- mean(dataC$traps[which(dataC$DDs <= lim)])
  
  inners <- which((dataC$DDs >= ddss[length(ddss)]) & (dataC$DDs <= to))
  coll <- cumsum(dataC$moths)[c(inners, (1 + inners[length(inners)]))]
  
  AF2 <- approxfun(dataC$DDs[c(inners, (1 + inners[length(inners)]))], 
                   coll)
  coll[length(coll)] <- AF2(to)
  
  
  if(sum(x) == 0) {
    ms1 <- rep(0, 901)
    ms_up <- rep(0, length(coll))
    ms_down <- rep(0, length(coll))
    up11 <- rep(0, length(coll))
    lo22 <- rep(0, length(coll))
  } else {
    prop <- pJohnsonSB(ddss[length(ddss)], params = list(gamma = 0.4603673, delta = 0.8674057, xi = 69.22063, 
                                                         lambda = 662.5367))
    
    ms1 <- pJohnsonSB(seq(70, 577.22), 
                      params = list(gamma = 0.4603673, delta = 0.8674057, xi = 69.22063, 
                                    lambda = 662.5367)) * (x[length(x)]) / prop
    
    
    ms_up <- pJohnsonSB(c(dataC$DDs[inners], to), 
                        params = list(gamma = 0.4603673, delta = 0.8674057, xi = 69.22063, 
                                      lambda = 662.5367)) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                               qnorm(0.9))) / prop
    
    ms_down <- pJohnsonSB(c(dataC$DDs[inners], to), 
                          params = list(gamma = 0.4603673, delta = 0.8674057, xi = 69.22063, 
                                        lambda = 662.5367)) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                                 qnorm(0.9))) / prop
    
    res_err <- rmse(x, pJohnsonSB(ddss, 
                                  params = list(gamma = 0.4603673, delta = 0.8674057, xi = 69.22063, 
                                                lambda = 662.5367)) * (x[length(x)]) / prop)
    
    
    
    
    desvi1 <- rep(NA, length(ms_up))
    
    for(i in 1:length(ms_up)){
      if (ms_up[i] > 0) {
        desvi1[i] <- (((deltamethodV2(seq(70, 577.22)[i]) * (x[length(x)] + ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) * 
                                                                               qnorm(0.9))) / prop) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 577.22)))
      }
      else desvi1 <- 0
    }
    
    desvi2 <- rep(NA, length(ms_down))
    
    for(i in 1:length(ms_down)){
      if (ms_down[i] > 0) {
        desvi2[i] <- (((deltamethodV2(seq(70, 577.22)[i]) * (x[length(x)] - ((sqrt(desv(x[length(x)], key1(x[length(x)])) / ns)) *
                                                                               qnorm(0.9))) / prop) + (res_err)) * qnorm(0.9)) * ((1 - (ddss[length(ddss)] / 577.22)))
      }
      else desvi2 <- 0
    }
    
    up11 <- ms_up + desvi1
    lo22 <- ms_down - desvi2
    
    lo22[which(lo22 < x[length(x)])] <- x[length(x)]
    
  }
  
  checks2 <- rep(NA, length(coll))
  
  for(i in 1: length(coll)) {
    checks2[i] <- (coll[i] > up11[i]) | (coll[i] < lo22[i])
  }
  
  c(up11[length(up11)] - lo22[length(lo22)], if(length(which(checks2 == TRUE)) > 3) 1 else 0)
  
}


