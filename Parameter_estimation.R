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

DDs <- read.csv("DDs.csv")
prs <- read.csv("prp_DDs.csv")
DDsC <- read.csv("DDsC.csv")
prsC <- read.csv("prp_DDsC.csv")
cumprsC <- read.csv("cumprp_DDsC.csv")


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

# Parameter estimation for trajectories constrained to IntersectFA$minimum


mod1C <- estimat(DDs = DDsC, prs = prsC, method = "Nelder-Mead")
summary(mod1C)


# Code to produce Fig 2.

par(mfrow = c(3, 1), oma = c(0, 0, 1, 0))
par(mar = c(3, 8, 2, 2) + 0.1)
plot(DDs, (prs / 100), xlab = "", 
     ylab = "",  cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")

axis(2, at = seq(0, 0.005, 0.001), labels = c(0, expression("1x10" ^ -3), expression("2x10" ^ -3), expression("3x10" ^ -3),
                                              expression("4x10" ^ -3), expression("5x10" ^ -3)), cex.axis = 1.8, las = 2)

title(ylab = "Proportion captured", line = 6, cex.lab = 2)

lines(seq(0, 1600), dgamma(seq(0, 1600), shape = mix_mod$gamma.pars[1, 1], scale = mix_mod$gamma.pars[2, 1]), lwd = 4, col = "grey38")
lines(seq(0, 1600), dgamma(seq(0, 1600), shape = mix_mod$gamma.pars[1, 2], scale = mix_mod$gamma.pars[2, 2]), lwd = 4, col = "grey38")

abline(v = IntersectFA$minimum, col = "grey38", lwd = 3.5, lty = 3)
abline(v = 577.22, col = "blue", lwd = 3, lty = 3)

mtext("A", side = 3, cex = 2, line = 1, at = -320)


par(mar = c(4, 8, 1, 2) + 0.1)
plot(DDsC, cumprsC, xlab = "", 
     ylab = "",  cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n")

title(ylab = "Cumulative\nproportion captured", line = 4.5, cex.lab = 2)
axis(2, at = seq(0, 1, 0.2), cex.axis = 1.8, las = 2)

lines(seq(0, IntersectFA$minimum), pJohnsonSB(seq(0, IntersectFA$minimum), 
                                              params = list(gamma = coef(mod1C)[1],
                                                            delta = coef(mod1C)[2],
                                                            xi = coef(mod1C)[3],
                                                            lambda = (coef(mod1C)[4] - coef(mod1C)[3]))), lwd = 4, col = "grey51")
lines(seq(0, IntersectFA$minimum), pJohnsonSB(seq(0, IntersectFA$minimum), 
                                              params = list(gamma = 1.0737, delta = 1.2394, xi = 69, 
                                                            lambda = 577.22)), lwd = 3.5, col = "blue")

abline(v = IntersectFA$minimum, col = "grey38", lwd = 3.5, lty = 3)
abline(v = 577.22, col = "blue", lwd = 3, lty = 3)
mtext("B", side = 3, cex = 2, line = 1, at = -130)


load("example.RData")

par(mar = c(5, 8, 0, 2) + 0.1)
plot(ddss_ph, x_ph, type = "o", xlim = c(70, 578), ylim = c(0, 80),
     xlab = "Cumulative degree-days", ylab = "", cex.lab = 2, cex.axis = 1.8, lwd = 2, yaxt = "n", xaxt = "n")

axis(1, at = seq(100, 500, 100), cex.axis = 1.8)
axis(2, seq(0, 80, 20), cex.axis = 1.8, las = 2)

title(ylab = "Cumulative\nmean counts", line = 4.5, cex.lab = 2)

polygon(c(seq(round(ddss_ph[length(ddss_ph)]), 577.22), seq(577.22, round(ddss_ph[length(ddss_ph)]), -1)), 
        c(up11_ph[(round(ddss_ph[length(ddss_ph)])-69): 508], ms1_ph[(round(ddss_ph[length(ddss_ph)])-69): 508][337:1]), col= t_blue,
        border = NA)

polygon(c(seq(round(ddss_ph[length(ddss_ph)]), 577.22), seq(577.22, round(ddss_ph[length(ddss_ph)]), -1)), 
        c(lo22_ph[(round(ddss_ph[length(ddss_ph)])-69): 508], ms1_ph[(round(ddss_ph[length(ddss_ph)])-69): 508][337:1]), col= t_blue,
        border = NA)

polygon(c(seq(round(ddss[length(ddss)]), 577.22), seq(577.22, round(ddss[length(ddss)]), -1)), 
        c(up11[(round(ddss[length(ddss)])-69): 508], ms1[(round(ddss[length(ddss)])-69): 508][337:1]), col= t_grey, border = NA)

polygon(c(seq(round(ddss[length(ddss)]), 577.22), seq(577.22, round(ddss[length(ddss)]), -1)), 
        c(lo22[(round(ddss[length(ddss)])-69): 508], ms1[(round(ddss[length(ddss)])-69): 508][337:1]), col= t_grey, border = NA)

lines(seq(70, 577.22), ms1_ph, lwd = 2, col = "blue")

lines(seq(round(ddss_ph[length(ddss_ph)]), 577.22), ms1_ph[(round(ddss_ph[length(ddss_ph)])-69): 508], col = "blue", lwd = 3)
lines(seq(round(ddss_ph[length(ddss_ph)]), 577.22), up11_ph[(round(ddss_ph[length(ddss_ph)])-69): 508], col = "blue", lty = 2, lwd = 2)
segments(round(ddss_ph[length(ddss_ph)]), x_ph[length(x_ph)], round(ddss_ph[length(ddss_ph)]), up11_ph[(round(ddss_ph[length(ddss_ph)])-69)], col = "blue", lty = 2, lwd =2)
lines(seq(round(ddss_ph[length(ddss_ph)]), 577.22), lo22_ph[(round(ddss_ph[length(ddss_ph)])-69): 508], col = "blue", lty = 2, lwd = 2)

lines(seq(70, to), ms1, lwd = 2, col = "grey38")

lines(seq(round(ddss[length(ddss)]), to), ms1[(round(ddss[length(ddss)])-69): length(ms1)], col = "grey38", lwd = 3)
lines(seq(round(ddss[length(ddss)]), to), up11[(round(ddss[length(ddss)])-69): length(up11)], col = "grey38", lty = 2, lwd = 2)
segments(round(ddss[length(ddss)]), x[length(x)], round(ddss[length(ddss)]), up11[(round(ddss[length(ddss)])-69)], col = "grey38", lty = 2, lwd =2)
lines(seq(round(ddss[length(ddss)]), to), lo22[(round(ddss[length(ddss)])-69): length(lo22)], col = "grey38", lty = 2, lwd = 2)

points(xss, 
       coll, type = "o", col = "brown", lwd = 2)

mtext("C", side = 3, cex = 2, line = 1, at = -35)


