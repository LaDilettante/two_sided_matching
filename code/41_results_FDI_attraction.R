# load("../result/FDI_attraction04-05_18-02.RData")

str(results, max.level = 2)

# ---- alpha (firms preference) ----
nx <- 4
nw <- 4

# Convergence
par(mfrow = c(2, 2))
for (i in 1:nw) {
  plot(results$asave[, i], type = "l")
}
par(mfrow = c(1, 1))

# Results alpha
par(mfrow = c(2, 2))
for (i in 1:nw) {
  hist(results$asave[, i])
}
par(mfrow = c(1, 1))

for (i in 1:nw) {
  print(quantile(results$asave[, i], probs = c(0.025, 0.975)))
}


# ---- beta ----

for (i in 1:((dim(results$bsave)[2])/nx)) {
  par(mfrow = c(2, 2))
  for (j in 1:nw) {
    plot(results$bsave[, (i - 1) + j], type = "l")
  }
  par(mfrow = c(1, 1))
}




par(mfrow = c(2, 2))
for (i in 1:3) {
  hist(results$bsave)
}