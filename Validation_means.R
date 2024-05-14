# This code runs the validation analysis for the predicted means using observations not used for parameter estimation.
# It requires an R object provided on request called "Validation_data.RData".
# The object contains six lists (each for a different location) with different number of trajectories (16-20) each in a separate data.frame
# Note that the original degree-days are provided in Fahrenheit and should be converted to C for analysis.
# The test function can deal with F by indicating far = TRUE.

source("./Functions.R")

# Phenology-based model validation

res_phM1_3501 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A5_a_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3501[, 1])))
  res_phM1_3501[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3501 <- res_phM1_3501[which(!is.na(res_phM1_3501[, 1])), ]



res_phM1_3502 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A5_b_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3502[, 1])))
  res_phM1_3502[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3502 <- res_phM1_3502[which(!is.na(res_phM1_3502[, 1])), ]


res_phM1_3503 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A7_a_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3503[, 1])))
  res_phM1_3503[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3503 <- res_phM1_3503[which(!is.na(res_phM1_3503[, 1])), ]



res_phM1_3504 <- matrix(NA, 100000, 2)

for(i in 1:20) {
  a <- test_proc_phM1(A1_c_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3504[, 1])))
  res_phM1_3504[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3504 <- res_phM1_3504[which(!is.na(res_phM1_3504[, 1])), ]



res_phM1_3505 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A4_b_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3505[, 1])))
  res_phM1_3505[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3505 <- res_phM1_3505[which(!is.na(res_phM1_3505[, 1])), ]


res_phM1_3506 <- matrix(NA, 10000, 2)

for(i in 1:20) {
  a <- test_proc_phM1(A3_c_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_phM1_3506[, 1])))
  res_phM1_3506[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3506 <- res_phM1_3506[which(!is.na(res_phM1_3506[, 1])), ]




t350phMPr1 <- c((res_phM1_3501[, 1]),
                (res_phM1_3502[, 1]),
                (res_phM1_3503[, 1]),
                (res_phM1_3504[, 1]),
                (res_phM1_3505[, 1]),
                (res_phM1_3506[, 1]))

t350phMOb1 <- c((res_phM1_3501[, 2]),
                (res_phM1_3502[, 2]),
                (res_phM1_3503[, 2]),
                (res_phM1_3504[, 2]),
                (res_phM1_3505[, 2]),
                (res_phM1_3506[, 2]))


# Moth-capture model validation

res_capM1_3501 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A5_a_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3501[, 1])))
  res_capM1_3501[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3501 <- res_capM1_3501[which(!is.na(res_capM1_3501[, 1])), ]



res_capM1_3502 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A5_b_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3502[, 1])))
  res_capM1_3502[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3502 <- res_capM1_3502[which(!is.na(res_capM1_3502[, 1])), ]


res_capM1_3503 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A7_a_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3503[, 1])))
  res_capM1_3503[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3503 <- res_capM1_3503[which(!is.na(res_capM1_3503[, 1])), ]



res_capM1_3504 <- matrix(NA, 100000, 2)

for(i in 1:20) {
  a <- test_proc_capM1(A1_c_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3504[, 1])))
  res_capM1_3504[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3504 <- res_capM1_3504[which(!is.na(res_capM1_3504[, 1])), ]



res_capM1_3505 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A4_b_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3505[, 1])))
  res_capM1_3505[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3505 <- res_capM1_3505[which(!is.na(res_capM1_3505[, 1])), ]


res_capM1_3506 <- matrix(NA, 10000, 2)

for(i in 1:20) {
  a <- test_proc_capM1(A3_c_data[[i]], lim = 300, to = 578, far = TRUE)
  b <- length(which(!is.na(res_capM1_3506[, 1])))
  res_capM1_3506[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3506 <- res_capM1_3506[which(!is.na(res_capM1_3506[, 1])), ]


t350capMPr1 <- c((res_capM1_3501[, 1]),
                 (res_capM1_3502[, 1]),
                 (res_capM1_3503[, 1]),
                 (res_capM1_3504[, 1]),
                 (res_capM1_3505[, 1]),
                 (res_capM1_3506[, 1]))

t350capMOb1 <- c((res_capM1_3501[, 2]),
                 (res_capM1_3502[, 2]),
                 (res_capM1_3503[, 2]),
                 (res_capM1_3504[, 2]),
                 (res_capM1_3505[, 2]),
                 (res_capM1_3506[, 2]))

# Linear models

lim = 3000
mod_val_ph <- lm(t350phMPr1[which(t350phMOb1 < lim)] ~ t350phMOb1[which(t350phMOb1 < lim)])
mod_val_cap <- lm(t350capMPr1[which(t350capMOb1 < lim)] ~ t350capMOb1[which(t350capMOb1 < lim)])
