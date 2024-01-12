# Variance-mean model

# This function finds the cutoff for a split-domine technique to find Taylor's 
# Power Law parameters of a dataset that shows the Poisson sampling effect.
# x is a matrix with the data col 1 = means, col 2 = variances, and start is a 
# starting value for the cutoff.

findbend <- function(x, start) {
  data1 <- data.frame(x = x[, 1], y = x[, 2])
  data1 <- data1[which(data1$x > 0), ]
  data1$x <- log(data1$x)
  data1$y <- log(data1$y)
  xint <- rep(NA, 1000)
  xint[1] <- start
  
  for(i in 1:1000) {
    vmmod <- lm(y ~ x, data = data1, subset = x > xint[i])
    vmmod2 <- lm(y ~ x, data = data1, subset = x <= xint[i])
    xint[i+1] <- (coef(vmmod2)[1] - coef(vmmod)[1]) / (coef(vmmod)[2] - coef(vmmod2)[2])
    
    if((length(which(!is.na(xint))) > 10)) {
      if(length(unique(xint[length(which(!is.na(xint))):(length(which(!is.na(xint))) - 10)])) == 1) break
      if((length(which(!is.na(xint))) == 999)) stop("no convergence")
    }
    
  }
  xint[which(!is.na(xint))]
}

# The Taylor's Power Law parameteres are then found by:

aa <- findbend(x = cbind(taylor$means, taylor$var), start = -1)

xint <- aa[length(aa)]

vmmod <- lm(lvar ~ lmeans, data = taylor, subset = (lmeans > xint))
summary(vmmod)

vmmod2 <- lm(lvar ~ lmeans, data = taylor, subset = (means > 0) & (lmeans <= xint))
summary(vmmod2)
