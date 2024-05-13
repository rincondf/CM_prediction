source("./Moth_Capture_Celsius.R")
source("./Moth_Capture_Fahrenheit.R")
source("./Phenology_Celsius.R")
source("./Phenology_Fahrenheit.R")

source("./Functions.R")


# This function generates mean counts of codling moth captures in pheromone 
# traps. DDs is a sequence of Celsius degree-days for which counts will be 
# produced, ns is the sample size per time (traps) and m is the total number of 
# moths for the simulated trajectory.

# For this example only data in Celsius is used, but data can also be generated
# in Fahrenheit by changing values for xi and lambda.
# Let's use the first 12 data points of a generated dataset as input, 
# but it can be any number.

set1 <- prod_obs(seq(70, 578, 20), ns = 20, m = 30)[1:12, ]

# Using the version of the model with the phenology-based function (Celsius)
a <- proc_ph_FinC(set1)

# plot everything
plot(set1$DDs, cumsum(set1$moths), xlim = c(70, 578), ylim = c(0, 40), type = "o", 
     lwd = 2, xlab = "Degree-days", ylab = "Cumulative counts")
points(a$DDs, a$moths_avg,  type = "o", lwd = 2, col = "red")

points(a$DDs, a$moths_max,  type = "l", lwd = 2, col = "red")
points(a$DDs, a$moths_min,  type = "l", lwd = 2, col = "red")


# Using the version of the model with the moth capture function (Celsius)
b <- proc_cap_FinC(set1)

plot(set1$DDs, cumsum(set1$moths), xlim = c(70, 578), ylim = c(0, 50), type = "o", 
     lwd = 2, xlab = "Degree-days", ylab = "Cumulative counts")
points(b$DDs, b$moths_avg,  type = "o", lwd = 2, col = "red")

points(b$DDs, b$moths_max,  type = "l", lwd = 2, col = "red")
points(b$DDs, b$moths_min,  type = "l", lwd = 2, col = "red")

