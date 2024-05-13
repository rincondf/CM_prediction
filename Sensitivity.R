# This code runs the sensitivity analysis using computer generated data.
# it is presented with parallel computing to optimize the process

source("./Functions.R")

require(parallel)

n_cores <- 22 # must be < the number of available cores un the computer processor.
cl <- makeCluster(n_cores)

clusterExport(cl, c("deltamethodV2", "test_proc_cap", "key", "key1", "prod_obs", "desv", "vmmod", "vmmod2", "xint", "FDD_CDD"))


# timing plus sample size

fall5_150 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 20), lim = 150, to = 578))
fall5_250 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 20), lim = 250, to = 578))
fall5_350 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 20), lim = 350, to = 578))
fall5_450 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 20), lim = 450, to = 578))

fall10_150 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 20), lim = 150, to = 578))
fall10_250 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 20), lim = 250, to = 578))
fall10_350 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 20), lim = 350, to = 578))
fall10_450 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 20), lim = 450, to = 578))

fall20_150 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 20), lim = 150, to = 578))
fall20_250 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 20), lim = 250, to = 578))
fall20_350 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 20), lim = 350, to = 578))
fall20_450 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 20), lim = 450, to = 578))

fall40_150 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 20), lim = 150, to = 578))
fall40_250 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 20), lim = 250, to = 578))
fall40_350 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 20), lim = 350, to = 578))
fall40_450 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 20), lim = 450, to = 578))



dens5_05 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 0.5), lim = 300, to = 578))
dens5_1 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 1), lim = 300, to = 578))
dens5_5 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 5), lim = 300, to = 578))
dens5_20 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 5, m = 20), lim = 300, to = 578))

dens10_05 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 0.5), lim = 300, to = 578))
dens10_1 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 1), lim = 300, to = 578))
dens10_5 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 5), lim = 300, to = 578))
dens10_20 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 10, m = 20), lim = 300, to = 578))

dens20_05 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 0.5), lim = 300, to = 578))
dens20_1 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 1), lim = 300, to = 578))
dens20_5 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 5), lim = 300, to = 578))
dens20_20 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 20, m = 20), lim = 300, to = 578))

dens40_05 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 0.5), lim = 300, to = 578))
dens40_1 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 1), lim = 300, to = 578))
dens40_5 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 5), lim = 300, to = 578))
dens40_20 <- parSapply(cl, 1:1000, function(x) test_proc_cap(data = prod_obs(seq(70, 578, 20), ns = 40, m = 20), lim = 300, to = 578))


stopCluster(cl)


N5s <- c(sum(fall5_150[2, ])/1000,
         sum(fall5_250[2, ])/1000,
         sum(fall5_350[2, ])/1000,
         sum(fall5_450[2, ])/1000)

N10s <- c(sum(fall10_150[2, ])/1000,
          sum(fall10_250[2, ])/1000,
          sum(fall10_350[2, ])/1000,
          sum(fall10_450[2, ])/1000)

N20s <- c(sum(fall20_150[2, ])/1000,
          sum(fall20_250[2, ])/1000,
          sum(fall20_350[2, ])/1000,
          sum(fall20_450[2, ])/1000)

N40s <- c(sum(fall40_150[2, ])/1000,
          sum(fall40_250[2, ])/1000,
          sum(fall40_350[2, ])/1000,
          sum(fall40_450[2, ])/1000)


