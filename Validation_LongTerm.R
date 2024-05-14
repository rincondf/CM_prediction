# This code runs the validation analysis for long-term predictions using observations not used for parameter estimation.
# It requires an R object provided on request called "Validation_data.RData".
# The object contains six lists (each for a different location) with different number of trajectories (16-20) each in a separate data.frame
# Note that the original degree-days are provided in Fahrenheit and should be converted to C for analysis.
# The test function can deal with F by indicating far = TRUE.

source("./Functions.R")

# Phenology-based model validation

# Predictions made at 250 DDs

res_ph_2501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_2501[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 250, to = 578, far = TRUE)
}

res_ph_2502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_2502[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 250, to = 578, far = TRUE)
}

res_ph_2503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_2503[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 250, to = 578, far = TRUE)
}

res_ph_2504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_2504[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 250, to = 578, far = TRUE)
}

res_ph_2505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_2505[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 250, to = 578, far = TRUE)
}

res_ph_2506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_2506[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 250, to = 578, far = TRUE)
}


t250ph <- (sum(res_ph_2501[, 2]) +
             sum(res_ph_2502[, 2]) +
             sum(res_ph_2503[, 2]) +
             sum(res_ph_2504[, 2]) +
             sum(res_ph_2505[, 2]) +
             sum(res_ph_2506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi250ph <- c(res_ph_2501[, 1], res_ph_2502[, 1], res_ph_2503[, 1], res_ph_2504[, 1], res_ph_2505[, 1], res_ph_2506[, 1])

# Predictions made at 300 DDs

res_ph_3001 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3001[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 300, to = 578, far = TRUE)
}

res_ph_3002 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3002[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 300, to = 578, far = TRUE)
}

res_ph_3003 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3003[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 300, to = 578, far = TRUE)
}

res_ph_3004 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_3004[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 300, to = 578, far = TRUE)
}

res_ph_3005 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3005[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 300, to = 578, far = TRUE)
}

res_ph_3006 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_3006[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 300, to = 578, far = TRUE)
}


t300ph <- (sum(res_ph_3001[, 2]) +
             sum(res_ph_3002[, 2]) +
             sum(res_ph_3003[, 2]) +
             sum(res_ph_3004[, 2]) +
             sum(res_ph_3005[, 2]) +
             sum(res_ph_3006[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi300ph <- c(res_ph_3001[, 1], res_ph_3002[, 1], res_ph_3003[, 1], res_ph_3004[, 1], res_ph_3005[, 1], res_ph_3006[, 1])

# Predictions made at 350 DDs

res_ph_3501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3501[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 350, to = 578, far = TRUE)
}

res_ph_3502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3502[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 350, to = 578, far = TRUE)
}

res_ph_3503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3503[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 350, to = 578, far = TRUE)
}

res_ph_3504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_3504[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 350, to = 578, far = TRUE)
}

res_ph_3505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_3505[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 350, to = 578, far = TRUE)
}

res_ph_3506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_3506[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 350, to = 578, far = TRUE)
}


t350ph <- (sum(res_ph_3501[, 2]) +
             sum(res_ph_3502[, 2]) +
             sum(res_ph_3503[, 2]) +
             sum(res_ph_3504[, 2]) +
             sum(res_ph_3505[, 2]) +
             sum(res_ph_3506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi350ph <- c(res_ph_3501[, 1], res_ph_3502[, 1], res_ph_3503[, 1], res_ph_3504[, 1], res_ph_3505[, 1], res_ph_3506[, 1])

# Predictions made at 400 DDs

res_ph_4001 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4001[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 400, to = 578, far = TRUE)
}

res_ph_4002 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4002[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 400, to = 578, far = TRUE)
}

res_ph_4003 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4003[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 400, to = 578, far = TRUE)
}

res_ph_4004 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_4004[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 400, to = 578, far = TRUE)
}

res_ph_4005 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4005[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 400, to = 578, far = TRUE)
}

res_ph_4006 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_4006[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 400, to = 578, far = TRUE)
}


t400ph <- (sum(res_ph_4001[, 2]) +
             sum(res_ph_4002[, 2]) +
             sum(res_ph_4003[, 2]) +
             sum(res_ph_4004[, 2]) +
             sum(res_ph_4005[, 2]) +
             sum(res_ph_4006[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi400ph <- c(res_ph_4001[, 1], res_ph_4002[, 1], res_ph_4003[, 1], res_ph_4004[, 1], res_ph_4005[, 1], res_ph_4006[, 1])

# Predictions made at 450 DDs

res_ph_4501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4501[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 450, to = 578, far = TRUE)
}

res_ph_4502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4502[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 450, to = 578, far = TRUE)
}

res_ph_4503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4503[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 450, to = 578, far = TRUE)
}

res_ph_4504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_4504[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 450, to = 578, far = TRUE)
}

res_ph_4505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_4505[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 450, to = 578, far = TRUE)
}

res_ph_4506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_4506[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 450, to = 578, far = TRUE)
}


t450ph <- (sum(res_ph_4501[, 2]) +
             sum(res_ph_4502[, 2]) +
             sum(res_ph_4503[, 2]) +
             sum(res_ph_4504[, 2]) +
             sum(res_ph_4505[, 2]) +
             sum(res_ph_4506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi450ph <- c(res_ph_4501[, 1], res_ph_4502[, 1], res_ph_4503[, 1], res_ph_4504[, 1], res_ph_4505[, 1], res_ph_4506[, 1])


# Moth-capture model

# Predictions made at 250 DDs

resuT_2501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_2501[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 250, to = 578, far = TRUE)
}

resuT_2502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_2502[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 250, to = 578, far = TRUE)
}

resuT_2503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_2503[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 250, to = 578, far = TRUE)
}

resuT_2504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_2504[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 250, to = 578, far = TRUE)
}

resuT_2505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_2505[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 250, to = 578, far = TRUE)
}

resuT_2506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_2506[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 250, to = 578, far = TRUE)
}


t250cap <- (sum(resuT_2501[, 2]) +
              sum(resuT_2502[, 2]) +
              sum(resuT_2503[, 2]) +
              sum(resuT_2504[, 2]) +
              sum(resuT_2505[, 2]) +
              sum(resuT_2506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi250cap <- c(resuT_2501[, 1], resuT_2502[, 1], resuT_2503[, 1], resuT_2504[, 1], resuT_2505[, 1], resuT_2506[, 1])


# Predictions made at 300 DDs

resuT_3001 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3001[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 300, to = 578, far = TRUE)
}

resuT_3002 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3002[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 300, to = 578, far = TRUE)
}

resuT_3003 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3003[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 300, to = 578, far = TRUE)
}

resuT_3004 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_3004[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 300, to = 578, far = TRUE)
}

resuT_3005 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3005[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 300, to = 578, far = TRUE)
}

resuT_3006 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_3006[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 300, to = 578, far = TRUE)
}


t300cap <- (sum(resuT_3001[, 2]) +
              sum(resuT_3002[, 2]) +
              sum(resuT_3003[, 2]) +
              sum(resuT_3004[, 2]) +
              sum(resuT_3005[, 2]) +
              sum(resuT_3006[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi300cap <- c(resuT_3001[, 1], resuT_3002[, 1], resuT_3003[, 1], resuT_3004[, 1], resuT_3005[, 1], resuT_3006[, 1])

# Predictions made at 350 DDs

resuT_3501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3501[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 350, to = 578, far = TRUE)
}

resuT_3502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3502[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 350, to = 578, far = TRUE)
}

resuT_3503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3503[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 350, to = 578, far = TRUE)
}

resuT_3504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_3504[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 350, to = 578, far = TRUE)
}

resuT_3505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_3505[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 350, to = 578, far = TRUE)
}

resuT_3506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_3506[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 350, to = 578, far = TRUE)
}


t350cap <- (sum(resuT_3501[, 2]) +
              sum(resuT_3502[, 2]) +
              sum(resuT_3503[, 2]) +
              sum(resuT_3504[, 2]) +
              sum(resuT_3505[, 2]) +
              sum(resuT_3506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi350cap <- c(resuT_3501[, 1], resuT_3502[, 1], resuT_3503[, 1], resuT_3504[, 1], resuT_3505[, 1], resuT_3506[, 1])

# Predictions made at 400 DDs

resuT_4001 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4001[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 400, to = 578, far = TRUE)
}

resuT_4002 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4002[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 400, to = 578, far = TRUE)
}

resuT_4003 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4003[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 400, to = 578, far = TRUE)
}

resuT_4004 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_4004[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 400, to = 578, far = TRUE)
}

resuT_4005 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4005[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 400, to = 578, far = TRUE)
}

resuT_4006 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_4006[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 400, to = 578, far = TRUE)
}


t400cap <- (sum(resuT_4001[, 2]) +
              sum(resuT_4002[, 2]) +
              sum(resuT_4003[, 2]) +
              sum(resuT_4004[, 2]) +
              sum(resuT_4005[, 2]) +
              sum(resuT_4006[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi400cap <- c(resuT_4001[, 1], resuT_4002[, 1], resuT_4003[, 1], resuT_4004[, 1], resuT_4005[, 1], resuT_4006[, 1])

# Predictions made at 450 DDs

resuT_4501 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4501[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 450, to = 578, far = TRUE)
}

resuT_4502 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4502[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 450, to = 578, far = TRUE)
}

resuT_4503 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4503[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 450, to = 578, far = TRUE)
}

resuT_4504 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_4504[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 450, to = 578, far = TRUE)
}

resuT_4505 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_4505[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 450, to = 578, far = TRUE)
}

resuT_4506 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_4506[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 450, to = 578, far = TRUE)
}


t450cap <- (sum(resuT_4501[, 2]) +
              sum(resuT_4502[, 2]) +
              sum(resuT_4503[, 2]) +
              sum(resuT_4504[, 2]) +
              sum(resuT_4505[, 2]) +
              sum(resuT_4506[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi450cap <- c(resuT_4501[, 1], resuT_4502[, 1], resuT_4503[, 1], resuT_4504[, 1], resuT_4505[, 1], resuT_4506[, 1])

# Standardized width of the prediction intervals
# 1. Extraction of total moths captures per location per trajectory

densiR8 <- rep(NA, 16)

for(i in 1:16) {
  densiR8[i] <- sum(A5_a_data[[i]]$moths[which(A5_a_data[[i]]$DDs < 1000)])
}

densiR9 <- rep(NA, 16)

for(i in 1:16) {
  densiR9[i] <- sum(A5_b_data[[i]]$moths[which(A5_b_data[[i]]$DDs < 1000)])
}

densiR10 <- rep(NA, 16)

for(i in 1:16) {
  densiR10[i] <- sum(A7_a_data[[i]]$moths[which(A7_a_data[[i]]$DDs < 1000)])
}

densiR11 <- rep(NA, 20)

for(i in 1:20) {
  densiR11[i] <- sum(A1_c_data[[i]]$moths[which(A1_c_data[[i]]$DDs < 1000)])
}

densiR12 <- rep(NA, 16)

for(i in 1:16) {
  densiR12[i] <- sum(A4_b_data[[i]]$moths[which(A4_b_data[[i]]$DDs < 1000)])
}

densiR13 <- rep(NA, 20)

for(i in 1:20) {
  densiR13[i] <- sum(A3_c_data[[i]]$moths[which(A3_c_data[[i]]$DDs < 1000)])
}

densities <- c(densiR8, densiR9, densiR10, densiR11, densiR12, densiR13)

# 2. Calculation of standardized widths.

wi250capST <- (wi250cap / densities)
wi250phST <- (wi250ph / densities)

wi300capST <- (wi300cap / densities)
wi300phST <- (wi300ph / densities)

wi350capST <- (wi350cap / densities)
wi350phST <- (wi350ph / densities)

wi400capST <- (wi400cap / densities)
wi400phST <- (wi400ph / densities)

wi450capST <- (wi450cap / densities)
wi450phST <- (wi450ph / densities)