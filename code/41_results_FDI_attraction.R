load("../result/FDI_attraction04-05_18-02.RData")

str(results, max.level = 2)

# ---- alpha (firms preference) ----

# Convergence
par(mfrow = c(2, 2))
for (i in 1:3) {
  plot(results$asave[, 1], type = "l")
}
par(mfrow = c(1, 1))

# Results alpha
par(mfrow = c(2, 2))
hist(results$asave[, 1])
hist(results$asave[, 2])
hist(results$asave[, 3])
par(mfrow = c(1, 1))

for (i in 1:3) {
  quantile(results$asave[, i], probs = c(0.025, 0.075))
}


# ---- beta ----
nx <- 3

for (i in 1:((dim(results$bsave)[2])/nx)) {
  par(mfrow = c(2, 2))
  plot(results$bsave[, (i - 1) + 1], type = "l")
  plot(results$bsave[, (i - 1) + 2], type = "l")
  plot(results$bsave[, (i - 1) + 3], type = "l")
  par(mfrow = c(1, 1))
}




par(mfrow = c(2, 2))
for (i in 1:3) {
  hist(results$bsave)
}