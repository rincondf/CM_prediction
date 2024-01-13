# Procedure to distinguish the overwintering from the first summer generation 
# from dataset and parameter estimation of JohnsonSB

# vector of degree-days repeated in frequencies associated with the respective 
# proportions captured
my_vec <- rep(DDs, round(cummPr*100))

# function for parameter estimation of mixture models, assuming 80% associated 
# with overwintering generation

require(mixtools)
mix_mod <- gammamixEM(my_vec, lambda = c(0.8, 0.2), alpha = c(1.4, 15.5),
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

# parameter estimation for trajectories constrained to IntersectFA$minimum

mod1C <- estimat(DDs = DDs1000FA1, prs = cuFIN, method = "Nelder-Mead")
summary(mod1C)