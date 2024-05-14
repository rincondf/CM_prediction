

res_phM1_3501 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A5_a_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_phM1_3501[, 1])))
  res_phM1_3501[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3501 <- res_phM1_3501[which(!is.na(res_phM1_3501[, 1])), ]



res_phM1_3502 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A5_b_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_phM1_3502[, 1])))
  res_phM1_3502[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3502 <- res_phM1_3502[which(!is.na(res_phM1_3502[, 1])), ]


res_phM1_3503 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A7_a_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_phM1_3503[, 1])))
  res_phM1_3503[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3503 <- res_phM1_3503[which(!is.na(res_phM1_3503[, 1])), ]



res_phM1_3504 <- matrix(NA, 100000, 2)

for(i in 1:20) {
  a <- test_proc_phM1(A1_c_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_phM1_3504[, 1])))
  res_phM1_3504[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3504 <- res_phM1_3504[which(!is.na(res_phM1_3504[, 1])), ]



res_phM1_3505 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_phM1(A4_b_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_phM1_3505[, 1])))
  res_phM1_3505[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_phM1_3505 <- res_phM1_3505[which(!is.na(res_phM1_3505[, 1])), ]


res_phM1_3506 <- matrix(NA, 10000, 2)

for(i in 1:20) {
  a <- test_proc_phM1(A3_c_data[[i]], lim = 300, to = 578)
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


res_capM1_3501 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A5_a_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_capM1_3501[, 1])))
  res_capM1_3501[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3501 <- res_capM1_3501[which(!is.na(res_capM1_3501[, 1])), ]



res_capM1_3502 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A5_b_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_capM1_3502[, 1])))
  res_capM1_3502[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3502 <- res_capM1_3502[which(!is.na(res_capM1_3502[, 1])), ]


res_capM1_3503 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A7_a_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_capM1_3503[, 1])))
  res_capM1_3503[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3503 <- res_capM1_3503[which(!is.na(res_capM1_3503[, 1])), ]



res_capM1_3504 <- matrix(NA, 100000, 2)

for(i in 1:20) {
  a <- test_proc_capM1(A1_c_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_capM1_3504[, 1])))
  res_capM1_3504[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3504 <- res_capM1_3504[which(!is.na(res_capM1_3504[, 1])), ]



res_capM1_3505 <- matrix(NA, 1000, 2)

for(i in 1:16) {
  a <- test_proc_capM1(A4_b_data[[i]], lim = 300, to = 578)
  b <- length(which(!is.na(res_capM1_3505[, 1])))
  res_capM1_3505[(b + 1) : (b + length(a[, 1] + 1)), 1:2] <- a
}

res_capM1_3505 <- res_capM1_3505[which(!is.na(res_capM1_3505[, 1])), ]


res_capM1_3506 <- matrix(NA, 10000, 2)

for(i in 1:20) {
  a <- test_proc_capM1(A3_c_data[[i]], lim = 300, to = 578)
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