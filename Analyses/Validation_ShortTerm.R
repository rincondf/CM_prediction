# This code runs the validation analysis for short-term predictions using observations not used for parameter estimation.
# It requires an R object provided on request called "Validation_data.RData".
# The object contains six lists (each for a different location) with different number of trajectories (16-20) each in a separate data.frame
# Note that the original degree-days are provided in Fahrenheit and should be converted to C for analysis.
# The test function can deal with F by indicating far = TRUE.

source("./Functions.R")
load("Data/Validation_data.RData") # Will be provided on request. See readme.

# Phenology-based model validation

# Predictions made at 250 DDs

# 40 DDs-length

res_ph_250A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250A1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 250, to = 290, far = TRUE)
}

res_ph_250A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250A2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 250, to = 290, far = TRUE)
}

res_ph_250A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250A3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 250, to = 290, far = TRUE)
}

res_ph_250A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250A4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 250, to = 290, far = TRUE)
}

res_ph_250A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250A5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 250, to = 290, far = TRUE)
}

res_ph_250A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250A6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 250, to = 290, far = TRUE)
}


t250Aph <- (sum(res_ph_250A1[, 2]) +
              sum(res_ph_250A2[, 2]) +
              sum(res_ph_250A3[, 2]) +
              sum(res_ph_250A4[, 2]) +
              sum(res_ph_250A5[, 2]) +
              sum(res_ph_250A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi250Aph <- c(res_ph_250A1[, 1], res_ph_250A2[, 1], res_ph_250A3[, 1], res_ph_250A4[, 1], res_ph_250A5[, 1], res_ph_250A6[, 1])

# 60 DDs-length

res_ph_250B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250B1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 250, to = 310, far = TRUE)
}

res_ph_250B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250B2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 250, to = 310, far = TRUE)
}

res_ph_250B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250B3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 250, to = 310, far = TRUE)
}

res_ph_250B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250B4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 250, to = 310, far = TRUE)
}

res_ph_250B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250B5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 250, to = 310, far = TRUE)
}

res_ph_250B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250B6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 250, to = 310, far = TRUE)
}


t250Bph <- (sum(res_ph_250B1[, 2]) +
              sum(res_ph_250B2[, 2]) +
              sum(res_ph_250B3[, 2]) +
              sum(res_ph_250B4[, 2]) +
              sum(res_ph_250B5[, 2]) +
              sum(res_ph_250B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi250Bph <- c(res_ph_250B1[, 1], res_ph_250B2[, 1], res_ph_250B3[, 1], res_ph_250B4[, 1], res_ph_250B5[, 1], res_ph_250B6[, 1])

# 120 DDs-length

res_ph_250C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250C1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 250, to = 370, far = TRUE)
}

res_ph_250C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250C2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 250, to = 370, far = TRUE)
}

res_ph_250C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250C3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 250, to = 370, far = TRUE)
}

res_ph_250C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250C4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 250, to = 370, far = TRUE)
}

res_ph_250C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_250C5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 250, to = 370, far = TRUE)
}

res_ph_250C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_250C6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 250, to = 370, far = TRUE)
}


t250Cph <- (sum(res_ph_250C1[, 2]) +
              sum(res_ph_250C2[, 2]) +
              sum(res_ph_250C3[, 2]) +
              sum(res_ph_250C4[, 2]) +
              sum(res_ph_250C5[, 2]) +
              sum(res_ph_250C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi250Cph <- c(res_ph_250C1[, 1], res_ph_250C2[, 1], res_ph_250C3[, 1], res_ph_250C4[, 1], res_ph_250C5[, 1], res_ph_250C6[, 1])


# Predictions made at 300 DDs

# 40 DDs-length

res_ph_300A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300A1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 300, to = 340, far = TRUE)
}

res_ph_300A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300A2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 300, to = 340, far = TRUE)
}

res_ph_300A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300A3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 300, to = 340, far = TRUE)
}

res_ph_300A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300A4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 300, to = 340, far = TRUE)
}

res_ph_300A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300A5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 300, to = 340, far = TRUE)
}

res_ph_300A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300A6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 300, to = 340, far = TRUE)
}


t300Aph <- (sum(res_ph_300A1[, 2]) +
              sum(res_ph_300A2[, 2]) +
              sum(res_ph_300A3[, 2]) +
              sum(res_ph_300A4[, 2]) +
              sum(res_ph_300A5[, 2]) +
              sum(res_ph_300A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi300Aph <- c(res_ph_300A1[, 1], res_ph_300A2[, 1], res_ph_300A3[, 1], res_ph_300A4[, 1], res_ph_300A5[, 1], res_ph_300A6[, 1])

# 60 DDs-length

res_ph_300B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300B1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 300, to = 360, far = TRUE)
}

res_ph_300B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300B2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 300, to = 360, far = TRUE)
}

res_ph_300B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300B3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 300, to = 360, far = TRUE)
}

res_ph_300B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300B4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 300, to = 360, far = TRUE)
}

res_ph_300B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300B5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 300, to = 360, far = TRUE)
}

res_ph_300B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300B6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 300, to = 360, far = TRUE)
}


t300Bph <- (sum(res_ph_300B1[, 2]) +
              sum(res_ph_300B2[, 2]) +
              sum(res_ph_300B3[, 2]) +
              sum(res_ph_300B4[, 2]) +
              sum(res_ph_300B5[, 2]) +
              sum(res_ph_300B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi300Bph <- c(res_ph_300B1[, 1], res_ph_300B2[, 1], res_ph_300B3[, 1], res_ph_300B4[, 1], res_ph_300B5[, 1], res_ph_300B6[, 1])

# 120 DDs-length

res_ph_300C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300C1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 300, to = 420, far = TRUE)
}

res_ph_300C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300C2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 300, to = 420, far = TRUE)
}

res_ph_300C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300C3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 300, to = 420, far = TRUE)
}

res_ph_300C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300C4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 300, to = 420, far = TRUE)
}

res_ph_300C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_300C5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 300, to = 420, far = TRUE)
}

res_ph_300C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_300C6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 300, to = 420, far = TRUE)
}


t300Cph <- (sum(res_ph_300C1[, 2]) +
              sum(res_ph_300C2[, 2]) +
              sum(res_ph_300C3[, 2]) +
              sum(res_ph_300C4[, 2]) +
              sum(res_ph_300C5[, 2]) +
              sum(res_ph_300C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi300Cph <- c(res_ph_300C1[, 1], res_ph_300C2[, 1], res_ph_300C3[, 1], res_ph_300C4[, 1], res_ph_300C5[, 1], res_ph_300C6[, 1])

# Predictions made at 350 DDs

# 40 DDs-length

res_ph_350A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350A1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 350, to = 390, far = TRUE)
}

res_ph_350A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350A2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 350, to = 390, far = TRUE)
}

res_ph_350A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350A3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 350, to = 390, far = TRUE)
}

res_ph_350A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350A4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 350, to = 390, far = TRUE)
}

res_ph_350A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350A5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 350, to = 390, far = TRUE)
}

res_ph_350A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350A6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 350, to = 390, far = TRUE)
}


t350Aph <- (sum(res_ph_350A1[, 2]) +
              sum(res_ph_350A2[, 2]) +
              sum(res_ph_350A3[, 2]) +
              sum(res_ph_350A4[, 2]) +
              sum(res_ph_350A5[, 2]) +
              sum(res_ph_350A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi350Aph <- c(res_ph_350A1[, 1], res_ph_350A2[, 1], res_ph_350A3[, 1], res_ph_350A4[, 1], res_ph_350A5[, 1], res_ph_350A6[, 1])

# 60 DDs-length

res_ph_350B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350B1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 350, to = 410, far = TRUE)
}

res_ph_350B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350B2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 350, to = 410, far = TRUE)
}

res_ph_350B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350B3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 350, to = 410, far = TRUE)
}

res_ph_350B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350B4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 350, to = 410, far = TRUE)
}

res_ph_350B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350B5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 350, to = 410, far = TRUE)
}

res_ph_350B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350B6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 350, to = 410, far = TRUE)
}


t350Bph <- (sum(res_ph_350B1[, 2]) +
              sum(res_ph_350B2[, 2]) +
              sum(res_ph_350B3[, 2]) +
              sum(res_ph_350B4[, 2]) +
              sum(res_ph_350B5[, 2]) +
              sum(res_ph_350B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi350Bph <- c(res_ph_350B1[, 1], res_ph_350B2[, 1], res_ph_350B3[, 1], res_ph_350B4[, 1], res_ph_350B5[, 1], res_ph_350B6[, 1])

# 120 DDs-length

res_ph_350C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350C1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 350, to = 470, far = TRUE)
}

res_ph_350C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350C2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 350, to = 470, far = TRUE)
}

res_ph_350C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350C3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 350, to = 470, far = TRUE)
}

res_ph_350C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350C4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 350, to = 470, far = TRUE)
}

res_ph_350C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_350C5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 350, to = 470, far = TRUE)
}

res_ph_350C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_350C6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 350, to = 470, far = TRUE)
}


t350Cph <- (sum(res_ph_350C1[, 2]) +
              sum(res_ph_350C2[, 2]) +
              sum(res_ph_350C3[, 2]) +
              sum(res_ph_350C4[, 2]) +
              sum(res_ph_350C5[, 2]) +
              sum(res_ph_350C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi350Cph <- c(res_ph_350C1[, 1], res_ph_350C2[, 1], res_ph_350C3[, 1], res_ph_350C4[, 1], res_ph_350C5[, 1], res_ph_350C6[, 1])

# Predictions made at 400 DDs

# 40 DDs-length

res_ph_400A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400A1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 400, to = 440, far = TRUE)
}

res_ph_400A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400A2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 400, to = 440, far = TRUE)
}

res_ph_400A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400A3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 400, to = 440, far = TRUE)
}

res_ph_400A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400A4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 400, to = 440, far = TRUE)
}

res_ph_400A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400A5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 400, to = 440, far = TRUE)
}

res_ph_400A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400A6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 400, to = 440, far = TRUE)
}


t400Aph <- (sum(res_ph_400A1[, 2]) +
              sum(res_ph_400A2[, 2]) +
              sum(res_ph_400A3[, 2]) +
              sum(res_ph_400A4[, 2]) +
              sum(res_ph_400A5[, 2]) +
              sum(res_ph_400A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi400Aph <- c(res_ph_400A1[, 1], res_ph_400A2[, 1], res_ph_400A3[, 1], res_ph_400A4[, 1], res_ph_400A5[, 1], res_ph_400A6[, 1])

# 60 DDs-length

res_ph_400B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400B1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 400, to = 460, far = TRUE)
}

res_ph_400B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400B2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 400, to = 460, far = TRUE)
}

res_ph_400B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400B3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 400, to = 460, far = TRUE)
}

res_ph_400B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400B4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 400, to = 460, far = TRUE)
}

res_ph_400B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400B5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 400, to = 460, far = TRUE)
}

res_ph_400B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400B6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 400, to = 460, far = TRUE)
}


t400Bph <- (sum(res_ph_400B1[, 2]) +
              sum(res_ph_400B2[, 2]) +
              sum(res_ph_400B3[, 2]) +
              sum(res_ph_400B4[, 2]) +
              sum(res_ph_400B5[, 2]) +
              sum(res_ph_400B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi400Bph <- c(res_ph_400B1[, 1], res_ph_400B2[, 1], res_ph_400B3[, 1], res_ph_400B4[, 1], res_ph_400B5[, 1], res_ph_400B6[, 1])

# 120 DDs-length

res_ph_400C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400C1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 400, to = 520, far = TRUE)
}

res_ph_400C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400C2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 400, to = 520, far = TRUE)
}

res_ph_400C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400C3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 400, to = 520, far = TRUE)
}

res_ph_400C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400C4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 400, to = 520, far = TRUE)
}

res_ph_400C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_400C5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 400, to = 520, far = TRUE)
}

res_ph_400C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_400C6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 400, to = 520, far = TRUE)
}


t400Cph <- (sum(res_ph_400C1[, 2]) +
              sum(res_ph_400C2[, 2]) +
              sum(res_ph_400C3[, 2]) +
              sum(res_ph_400C4[, 2]) +
              sum(res_ph_400C5[, 2]) +
              sum(res_ph_400C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi400Cph <- c(res_ph_400C1[, 1], res_ph_400C2[, 1], res_ph_400C3[, 1], res_ph_400C4[, 1], res_ph_400C5[, 1], res_ph_400C6[, 1])

# Predictions made at 450 DDs

# 40 DDs-length

res_ph_450A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450A1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 450, to = 490, far = TRUE)
}

res_ph_450A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450A2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 450, to = 490, far = TRUE)
}

res_ph_450A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450A3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 450, to = 490, far = TRUE)
}

res_ph_450A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450A4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 450, to = 490, far = TRUE)
}

res_ph_450A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450A5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 450, to = 490, far = TRUE)
}

res_ph_450A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450A6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 450, to = 490, far = TRUE)
}


t450Aph <- (sum(res_ph_450A1[, 2]) +
              sum(res_ph_450A2[, 2]) +
              sum(res_ph_450A3[, 2]) +
              sum(res_ph_450A4[, 2]) +
              sum(res_ph_450A5[, 2]) +
              sum(res_ph_450A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi450Aph <- c(res_ph_450A1[, 1], res_ph_450A2[, 1], res_ph_450A3[, 1], res_ph_450A4[, 1], res_ph_450A5[, 1], res_ph_450A6[, 1])

# 60 DDs-length

res_ph_450B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450B1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 450, to = 510, far = TRUE)
}

res_ph_450B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450B2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 450, to = 510, far = TRUE)
}

res_ph_450B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450B3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 450, to = 510, far = TRUE)
}

res_ph_450B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450B4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 450, to = 510, far = TRUE)
}

res_ph_450B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450B5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 450, to = 510, far = TRUE)
}

res_ph_450B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450B6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 450, to = 510, far = TRUE)
}


t450Bph <- (sum(res_ph_450B1[, 2]) +
              sum(res_ph_450B2[, 2]) +
              sum(res_ph_450B3[, 2]) +
              sum(res_ph_450B4[, 2]) +
              sum(res_ph_450B5[, 2]) +
              sum(res_ph_450B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi450Bph <- c(res_ph_450B1[, 1], res_ph_450B2[, 1], res_ph_450B3[, 1], res_ph_450B4[, 1], res_ph_450B5[, 1], res_ph_450B6[, 1])

# 120 DDs-length

res_ph_450C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450C1[i, ] <- test_proc_ph(A5_a_data[[i]], lim = 450, to = 570, far = TRUE)
}

res_ph_450C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450C2[i, ] <- test_proc_ph(A5_b_data[[i]], lim = 450, to = 570, far = TRUE)
}

res_ph_450C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450C3[i, ] <- test_proc_ph(A7_a_data[[i]], lim = 450, to = 570, far = TRUE)
}

res_ph_450C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450C4[i, ] <- test_proc_ph(A1_c_data[[i]], lim = 450, to = 570, far = TRUE)
}

res_ph_450C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  res_ph_450C5[i, ] <- test_proc_ph(A4_b_data[[i]], lim = 450, to = 570, far = TRUE)
}

res_ph_450C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  res_ph_450C6[i, ] <- test_proc_ph(A3_c_data[[i]], lim = 450, to = 570, far = TRUE)
}


t450Cph <- (sum(res_ph_450C1[, 2]) +
              sum(res_ph_450C2[, 2]) +
              sum(res_ph_450C3[, 2]) +
              sum(res_ph_450C4[, 2]) +
              sum(res_ph_450C5[, 2]) +
              sum(res_ph_450C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)


wi450Cph <- c(res_ph_450C1[, 1], res_ph_450C2[, 1], res_ph_450C3[, 1], res_ph_450C4[, 1], res_ph_450C5[, 1], res_ph_450C6[, 1])

# Moth-capture model validation

# Predictions made at 250 DDs

# 40 DDs-length

resuT_250A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250A1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 250, to = 290, far = TRUE)
}

resuT_250A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250A2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 250, to = 290, far = TRUE)
}

resuT_250A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250A3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 250, to = 290, far = TRUE)
}

resuT_250A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250A4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 250, to = 290, far = TRUE)
}

resuT_250A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250A5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 250, to = 290, far = TRUE)
}

resuT_250A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250A6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 250, to = 290, far = TRUE)
}


t250Acap <- (sum(resuT_250A1[, 2]) +
               sum(resuT_250A2[, 2]) +
               sum(resuT_250A3[, 2]) +
               sum(resuT_250A4[, 2]) +
               sum(resuT_250A5[, 2]) +
               sum(resuT_250A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi250Acap <- c(resuT_250A1[, 1], resuT_250A2[, 1], resuT_250A3[, 1], resuT_250A4[, 1], resuT_250A5[, 1], resuT_250A6[, 1])

# 60 DDs-length

resuT_250B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250B1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 250, to = 310, far = TRUE)
}

resuT_250B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250B2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 250, to = 310, far = TRUE)
}

resuT_250B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250B3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 250, to = 310, far = TRUE)
}

resuT_250B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250B4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 250, to = 310, far = TRUE)
}

resuT_250B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250B5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 250, to = 310, far = TRUE)
}

resuT_250B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250B6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 250, to = 310, far = TRUE)
}


t250Bcap <- (sum(resuT_250B1[, 2]) +
               sum(resuT_250B2[, 2]) +
               sum(resuT_250B3[, 2]) +
               sum(resuT_250B4[, 2]) +
               sum(resuT_250B5[, 2]) +
               sum(resuT_250B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi250Bcap <- c(resuT_250B1[, 1], resuT_250B2[, 1], resuT_250B3[, 1], resuT_250B4[, 1], resuT_250B5[, 1], resuT_250B6[, 1])

# 120 DDs-length

resuT_250C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250C1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 250, to = 370, far = TRUE)
}

resuT_250C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250C2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 250, to = 370, far = TRUE)
}

resuT_250C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250C3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 250, to = 370, far = TRUE)
}

resuT_250C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250C4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 250, to = 370, far = TRUE)
}

resuT_250C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_250C5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 250, to = 370, far = TRUE)
}

resuT_250C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_250C6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 250, to = 370, far = TRUE)
}


t250Ccap <- (sum(resuT_250C1[, 2]) +
               sum(resuT_250C2[, 2]) +
               sum(resuT_250C3[, 2]) +
               sum(resuT_250C4[, 2]) +
               sum(resuT_250C5[, 2]) +
               sum(resuT_250C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi250Ccap <- c(resuT_250C1[, 1], resuT_250C2[, 1], resuT_250C3[, 1], resuT_250C4[, 1], resuT_250C5[, 1], resuT_250C6[, 1])


# Predictions made at 300 DDs

# 40 DDs-length

resuT_300A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300A1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 300, to = 340, far = TRUE)
}

resuT_300A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300A2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 300, to = 340, far = TRUE)
}

resuT_300A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300A3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 300, to = 340, far = TRUE)
}

resuT_300A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300A4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 300, to = 340, far = TRUE)
}

resuT_300A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300A5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 300, to = 340, far = TRUE)
}

resuT_300A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300A6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 300, to = 340, far = TRUE)
}


t300Acap <- (sum(resuT_300A1[, 2]) +
               sum(resuT_300A2[, 2]) +
               sum(resuT_300A3[, 2]) +
               sum(resuT_300A4[, 2]) +
               sum(resuT_300A5[, 2]) +
               sum(resuT_300A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi300Acap <- c(resuT_300A1[, 1], resuT_300A2[, 1], resuT_300A3[, 1], resuT_300A4[, 1], resuT_300A5[, 1], resuT_300A6[, 1])

# 60 DDs-length

resuT_300B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300B1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 300, to = 360, far = TRUE)
}

resuT_300B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300B2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 300, to = 360, far = TRUE)
}

resuT_300B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300B3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 300, to = 360, far = TRUE)
}

resuT_300B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300B4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 300, to = 360, far = TRUE)
}

resuT_300B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300B5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 300, to = 360, far = TRUE)
}

resuT_300B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300B6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 300, to = 360, far = TRUE)
}


t300Bcap <- (sum(resuT_300B1[, 2]) +
               sum(resuT_300B2[, 2]) +
               sum(resuT_300B3[, 2]) +
               sum(resuT_300B4[, 2]) +
               sum(resuT_300B5[, 2]) +
               sum(resuT_300B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi300Bcap <- c(resuT_300B1[, 1], resuT_300B2[, 1], resuT_300B3[, 1], resuT_300B4[, 1], resuT_300B5[, 1], resuT_300B6[, 1])

# 120 DDs-length

resuT_300C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300C1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 300, to = 420, far = TRUE)
}

resuT_300C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300C2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 300, to = 420, far = TRUE)
}

resuT_300C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300C3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 300, to = 420, far = TRUE)
}

resuT_300C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300C4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 300, to = 420, far = TRUE)
}

resuT_300C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_300C5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 300, to = 420, far = TRUE)
}

resuT_300C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_300C6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 300, to = 420, far = TRUE)
}


t300Ccap <- (sum(resuT_300C1[, 2]) +
               sum(resuT_300C2[, 2]) +
               sum(resuT_300C3[, 2]) +
               sum(resuT_300C4[, 2]) +
               sum(resuT_300C5[, 2]) +
               sum(resuT_300C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi300Ccap <- c(resuT_300C1[, 1], resuT_300C2[, 1], resuT_300C3[, 1], resuT_300C4[, 1], resuT_300C5[, 1], resuT_300C6[, 1])


# Predictions made at 350 DDs

# 40 DDs-length

resuT_350A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350A1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 350, to = 390, far = TRUE)
}

resuT_350A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350A2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 350, to = 390, far = TRUE)
}

resuT_350A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350A3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 350, to = 390, far = TRUE)
}

resuT_350A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350A4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 350, to = 390, far = TRUE)
}

resuT_350A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350A5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 350, to = 390, far = TRUE)
}

resuT_350A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350A6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 350, to = 390, far = TRUE)
}


t350Acap <- (sum(resuT_350A1[, 2]) +
               sum(resuT_350A2[, 2]) +
               sum(resuT_350A3[, 2]) +
               sum(resuT_350A4[, 2]) +
               sum(resuT_350A5[, 2]) +
               sum(resuT_350A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi350Acap <- c(resuT_350A1[, 1], resuT_350A2[, 1], resuT_350A3[, 1], resuT_350A4[, 1], resuT_350A5[, 1], resuT_350A6[, 1])

# 60 DDs-length

resuT_350B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350B1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 350, to = 410, far = TRUE)
}

resuT_350B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350B2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 350, to = 410, far = TRUE)
}

resuT_350B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350B3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 350, to = 410, far = TRUE)
}

resuT_350B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350B4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 350, to = 410, far = TRUE)
}

resuT_350B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350B5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 350, to = 410, far = TRUE)
}

resuT_350B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350B6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 350, to = 410, far = TRUE)
}


t350Bcap <- (sum(resuT_350B1[, 2]) +
               sum(resuT_350B2[, 2]) +
               sum(resuT_350B3[, 2]) +
               sum(resuT_350B4[, 2]) +
               sum(resuT_350B5[, 2]) +
               sum(resuT_350B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi350Bcap <- c(resuT_350B1[, 1], resuT_350B2[, 1], resuT_350B3[, 1], resuT_350B4[, 1], resuT_350B5[, 1], resuT_350B6[, 1])

# 120 DDs-length

resuT_350C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350C1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 350, to = 470, far = TRUE)
}

resuT_350C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350C2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 350, to = 470, far = TRUE)
}

resuT_350C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350C3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 350, to = 470, far = TRUE)
}

resuT_350C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350C4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 350, to = 470, far = TRUE)
}

resuT_350C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_350C5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 350, to = 470, far = TRUE)
}

resuT_350C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_350C6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 350, to = 470, far = TRUE)
}


t350Ccap <- (sum(resuT_350C1[, 2]) +
               sum(resuT_350C2[, 2]) +
               sum(resuT_350C3[, 2]) +
               sum(resuT_350C4[, 2]) +
               sum(resuT_350C5[, 2]) +
               sum(resuT_350C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi350Ccap <- c(resuT_350C1[, 1], resuT_350C2[, 1], resuT_350C3[, 1], resuT_350C4[, 1], resuT_350C5[, 1], resuT_350C6[, 1])


# Predictions made at 400 DDs

# 40 DDs-length

resuT_400A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400A1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 400, to = 440, far = TRUE)
}

resuT_400A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400A2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 400, to = 440, far = TRUE)
}

resuT_400A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400A3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 400, to = 440, far = TRUE)
}

resuT_400A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400A4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 400, to = 440, far = TRUE)
}

resuT_400A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400A5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 400, to = 440, far = TRUE)
}

resuT_400A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400A6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 400, to = 440, far = TRUE)
}


t400Acap <- (sum(resuT_400A1[, 2]) +
               sum(resuT_400A2[, 2]) +
               sum(resuT_400A3[, 2]) +
               sum(resuT_400A4[, 2]) +
               sum(resuT_400A5[, 2]) +
               sum(resuT_400A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi400Acap <- c(resuT_400A1[, 1], resuT_400A2[, 1], resuT_400A3[, 1], resuT_400A4[, 1], resuT_400A5[, 1], resuT_400A6[, 1])

# 60 DDs-length

resuT_400B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400B1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 400, to = 460, far = TRUE)
}

resuT_400B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400B2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 400, to = 460, far = TRUE)
}

resuT_400B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400B3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 400, to = 460, far = TRUE)
}

resuT_400B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400B4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 400, to = 460, far = TRUE)
}

resuT_400B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400B5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 400, to = 460, far = TRUE)
}

resuT_400B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400B6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 400, to = 460, far = TRUE)
}


t400Bcap <- (sum(resuT_400B1[, 2]) +
               sum(resuT_400B2[, 2]) +
               sum(resuT_400B3[, 2]) +
               sum(resuT_400B4[, 2]) +
               sum(resuT_400B5[, 2]) +
               sum(resuT_400B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi400Bcap <- c(resuT_400B1[, 1], resuT_400B2[, 1], resuT_400B3[, 1], resuT_400B4[, 1], resuT_400B5[, 1], resuT_400B6[, 1])

# 120 DDs-length

resuT_400C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400C1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 400, to = 520, far = TRUE)
}

resuT_400C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400C2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 400, to = 520, far = TRUE)
}

resuT_400C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400C3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 400, to = 520, far = TRUE)
}

resuT_400C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400C4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 400, to = 520, far = TRUE)
}

resuT_400C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_400C5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 400, to = 520, far = TRUE)
}

resuT_400C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_400C6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 400, to = 520, far = TRUE)
}


t400Ccap <- (sum(resuT_400C1[, 2]) +
               sum(resuT_400C2[, 2]) +
               sum(resuT_400C3[, 2]) +
               sum(resuT_400C4[, 2]) +
               sum(resuT_400C5[, 2]) +
               sum(resuT_400C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi400Ccap <- c(resuT_400C1[, 1], resuT_400C2[, 1], resuT_400C3[, 1], resuT_400C4[, 1], resuT_400C5[, 1], resuT_400C6[, 1])


# Predictions made at 450 DDs

# 40 DDs-length

resuT_450A1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450A1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 450, to = 490, far = TRUE)
}

resuT_450A2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450A2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 450, to = 490, far = TRUE)
}

resuT_450A3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450A3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 450, to = 490, far = TRUE)
}

resuT_450A4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450A4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 450, to = 490, far = TRUE)
}

resuT_450A5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450A5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 450, to = 490, far = TRUE)
}

resuT_450A6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450A6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 450, to = 490, far = TRUE)
}


t450Acap <- (sum(resuT_450A1[, 2]) +
               sum(resuT_450A2[, 2]) +
               sum(resuT_450A3[, 2]) +
               sum(resuT_450A4[, 2]) +
               sum(resuT_450A5[, 2]) +
               sum(resuT_450A6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi450Acap <- c(resuT_450A1[, 1], resuT_450A2[, 1], resuT_450A3[, 1], resuT_450A4[, 1], resuT_450A5[, 1], resuT_450A6[, 1])

# 60 DDs-length

resuT_450B1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450B1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 450, to = 510, far = TRUE)
}

resuT_450B2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450B2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 450, to = 510, far = TRUE)
}

resuT_450B3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450B3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 450, to = 510, far = TRUE)
}

resuT_450B4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450B4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 450, to = 510, far = TRUE)
}

resuT_450B5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450B5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 450, to = 510, far = TRUE)
}

resuT_450B6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450B6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 450, to = 510, far = TRUE)
}


t450Bcap <- (sum(resuT_450B1[, 2]) +
               sum(resuT_450B2[, 2]) +
               sum(resuT_450B3[, 2]) +
               sum(resuT_450B4[, 2]) +
               sum(resuT_450B5[, 2]) +
               sum(resuT_450B6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi450Bcap <- c(resuT_450B1[, 1], resuT_450B2[, 1], resuT_450B3[, 1], resuT_450B4[, 1], resuT_450B5[, 1], resuT_450B6[, 1])

# 120 DDs-length

resuT_450C1 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450C1[i, ] <- test_proc_cap(A5_a_data[[i]], lim = 450, to = 570, far = TRUE)
}

resuT_450C2 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450C2[i, ] <- test_proc_cap(A5_b_data[[i]], lim = 450, to = 570, far = TRUE)
}

resuT_450C3 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450C3[i, ] <- test_proc_cap(A7_a_data[[i]], lim = 450, to = 570, far = TRUE)
}

resuT_450C4 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450C4[i, ] <- test_proc_cap(A1_c_data[[i]], lim = 450, to = 570, far = TRUE)
}

resuT_450C5 <- matrix(NA, 16, 2)

for(i in 1:16) {
  resuT_450C5[i, ] <- test_proc_cap(A4_b_data[[i]], lim = 450, to = 570, far = TRUE)
}

resuT_450C6 <- matrix(NA, 20, 2)

for(i in 1:20) {
  resuT_450C6[i, ] <- test_proc_cap(A3_c_data[[i]], lim = 450, to = 570, far = TRUE)
}


t450Ccap <- (sum(resuT_450C1[, 2]) +
               sum(resuT_450C2[, 2]) +
               sum(resuT_450C3[, 2]) +
               sum(resuT_450C4[, 2]) +
               sum(resuT_450C5[, 2]) +
               sum(resuT_450C6[, 2])) / (16 + 16 + 16 + 20 + 16 + 20)

wi450Ccap <- c(resuT_450C1[, 1], resuT_450C2[, 1], resuT_450C3[, 1], resuT_450C4[, 1], resuT_450C5[, 1], resuT_450C6[, 1])


# Compiling data for Figure 3C and 4B)

t_ph <- matrix(c(t250Aph, t250Bph, t250Cph, t300Aph, t300Bph, t300Cph, t350Aph, t350Bph, t350Cph,
                 t400Aph, t400Bph, t400Cph, t450Aph, t450Bph, t450Cph), 3, 5)
t_cap <- matrix(c(t250Acap, t250Bcap, t250Ccap, t300Acap, t300Bcap, t300Ccap, t350Acap, t350Bcap, t350Ccap,
                  t400Acap, t400Bcap, t400Ccap, t450Acap, t450Bcap, t450Ccap), 3, 5)

# Calculating of standardized widths.

wi250AcapST <- (wi250Acap / densities)
wi250BcapST <- (wi250Bcap / densities)
wi250CcapST <- (wi250Ccap / densities)

wi300AcapST <- (wi300Acap / densities)
wi300BcapST <- (wi300Bcap / densities)
wi300CcapST <- (wi300Ccap / densities)

wi350AcapST <- (wi350Acap / densities)
wi350BcapST <- (wi350Bcap / densities)
wi350CcapST <- (wi350Ccap / densities)

wi400AcapST <- (wi400Acap / densities)
wi400BcapST <- (wi400Bcap / densities)
wi400CcapST <- (wi400Ccap / densities)

wi450AcapST <- (wi450Acap / densities)
wi450BcapST <- (wi450Bcap / densities)
wi450CcapST <- (wi450Ccap / densities)


wi250AphST <- (wi250Aph / densities)
wi250BphST <- (wi250Bph / densities)
wi250CphST <- (wi250Cph / densities)

wi300AphST <- (wi300Aph / densities)
wi300BphST <- (wi300Bph / densities)
wi300CphST <- (wi300Cph / densities)

wi350AphST <- (wi350Aph / densities)
wi350BphST <- (wi350Bph / densities)
wi350CphST <- (wi350Cph / densities)

wi400AphST <- (wi400Aph / densities)
wi400BphST <- (wi400Bph / densities)
wi400CphST <- (wi400Cph / densities)

wi450AphST <- (wi450Aph / densities)
wi450BphST <- (wi450Bph / densities)
wi450CphST <- (wi450Cph / densities)


wi250AcapST <- (wi250Acap / densities)
wi250BcapST <- (wi250Bcap / densities)
wi250CcapST <- (wi250Ccap / densities)

wi300AcapST <- (wi300Acap / densities)
wi300BcapST <- (wi300Bcap / densities)
wi300CcapST <- (wi300Ccap / densities)

wi350AcapST <- (wi350Acap / densities)
wi350BcapST <- (wi350Bcap / densities)
wi350BcapST <- (wi350Ccap / densities)

wi400AcapST <- (wi400Acap / densities)
wi400BcapST <- (wi400Bcap / densities)
wi400CcapST <- (wi400Ccap / densities)

wi450AcapST <- (wi450Acap / densities)
wi450BcapST <- (wi450Bcap / densities)
wi450CcapST <- (wi450Ccap / densities)


As_cap <- c(wi250AcapST,
            wi300AcapST,
            wi350AcapST,
            wi400AcapST,
            wi450AcapST)

As_ph <- c(wi250AphST,
           wi300AphST,
           wi350AphST,
           wi400AphST,
           wi450AphST)

Bs_cap <- c(wi250BcapST,
            wi300BcapST,
            wi350BcapST,
            wi400BcapST,
            wi450BcapST)

Bs_ph <- c(wi250BphST,
           wi300BphST,
           wi350BphST,
           wi400BphST,
           wi450BphST)

Cs_cap <- c(wi250CcapST,
            wi300CcapST,
            wi350CcapST,
            wi400CcapST,
            wi450CcapST)

Cs_ph <- c(wi250CphST,
           wi300CphST,
           wi350CphST,
           wi400CphST,
           wi450CphST)