# This code runs the validation analysis using observation not used for parameter estimation.
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
