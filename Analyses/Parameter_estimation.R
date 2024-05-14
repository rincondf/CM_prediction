# Procedure to distinguish the overwintering from the first summer generation 
# and parameter estimation of a JohnsonSB pdf.

# To run this code, please refer to the following data files: "DDs.csv", "prp_DDs.csv", "DDsC.csv", "prp_DDsC.csv" and
# "cumprp_DDsC.csv".

# The first contains all the recorded degree-days from 134 trajectories recorded from seven locations.
# The second contains the corresponding proportions of captured codling moths for the entire season.
# The third is the recorded degree-days from the 134 trajectories, constrained to the estimated limit for the overwintering generation.
# The fourth are the corresponding proportions for the overwintering generation.
# Th fifth is the cumulative version of the the forth, only used for plotting.

# Sample sizes <10 and single proportions (i.e., recorded in a single day) >0.6 were excluded from the analysis.

# vector of degree-days repeated in frequencies associated with the respective 
# proportions captured

# The following data files will be provided on request. See readme.

DDs <- read.csv("Data/DDs.csv")
prs <- read.csv("Data/prp_DDs.csv")
DDsC <- read.csv("Data/DDsC.csv")
prsC <- read.csv("Data/prp_DDsC.csv")
cumprsC <- read.csv("Data/cumprp_DDsC.csv")


freqs <- rep(DDs, round(prs*100))

# function for parameter estimation of mixture models, assuming 80% associated 
# with overwintering generation

require(mixtools)
mix_mod <- gammamixEM(freqs, lambda = c(0.8, 0.2), alpha = c(1.4, 15.5),
                      beta = c(0.009, 0.02), k = 2, maxrestarts = 3, maxit = 10000)

# Computing the intersection between both gamma distributions
NormIntersectA <- function(x, shape1, scale1, shape2, scale2) {
  (dgamma(x, shape = shape1, scale = scale1) - dgamma(x, shape = shape2, scale = scale2))^2
}
IntersectFA <- optimize(NormIntersectA, interval =  c(600, 900), shape1 = mix_mod$gamma.pars[1, 1], 
                        scale1 = mix_mod$gamma.pars[2, 1], shape2 = mix_mod$gamma.pars[1, 2], 
                        scale2 = mix_mod$gamma.pars[2, 2])

IntersectFA$minimum # is the DDs at which both distributions intercept

# function for parameter estimation of JohnsonSB from DDs and proportions
require(bbmle)
require(emdbook)
require(ExtDist)
require(SuppDists)

estimat <- function(DDs, prs, method){
  LL1 <- function(gamma, delta, a, b) {
    -sum(pr * log(dJohnsonSB_ab(x = x, gamma = gamma, delta = delta, a = a, b = b) + 0.000001))
  }
  
  if(method == "L-BFGS-B"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                lower = list(gamma = -Inf, delta = 0, a = -Inf, b = max(DDs)), 
                upper = list(gamma = Inf, delta = Inf, a = min(DDs), b = Inf), method = "L-BFGS-B")
  }
  
  if(method == "Nelder-Mead"){
    MLL <- mle2(LL1, start = list(gamma = -0.5, delta = 2, a = min(DDs) - 1, b = max(DDs) + 1), 
                data = list(x = DDs, pr = prs),
                method = "Nelder-Mead")
  }
  
  MLL
}

# Parameter estimation for trajectories constrained to IntersectFA$minimum. Degree-days smaller than
# 55 were removed from the analysis.

mod1C <- estimat(DDs = DDsC[which(DDsC > 55)], prs = prsC[which(DDsC > 55)], method = "Nelder-Mead")
summary(mod1C)