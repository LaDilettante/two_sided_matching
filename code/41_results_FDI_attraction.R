# load("../result/tslogit_0-01-0-05_04-09_12-57.RData")

head(results$dat)
results$acrate
results$mcmc

str(results, max.level = 2)

# ---- alpha (firms preference) ----
nw <- dim(results$asave)[2]
nx <- dim(results$bsave)[2] / length(unique(results$dat$nation_id))

# Convergence
par(mfrow = c(2, 2))
for (i in 1:nw) {
  plot(results$asave[, i], type = "l", ylab = paste(c("alpha", i)))
}
par(mfrow = c(1, 1))

# Results alpha
c_burnin <- 6000
for (i in 1:nw) {
  print(quantile(results$asave[c_burnin:nrow(results$asave), i],
                 probs = c(0.025, 0.5, 0.975)))
}
head(results$dat)

par(mfrow = c(2, 2))
for (i in 1:nw) {
  hist(results$asave[c_burnin:nrow(results$asave), i])
}
par(mfrow = c(1, 1))

# ---- beta ----

for (i in 1:((dim(results$bsave)[2])/nx)) {
  par(mfrow = c(2, 2))
  for (j in 1:nx) {
    plot(results$bsave[, (i - 1) + j], type = "l", ylab = paste(c("beta", i, j)))
  }
  par(mfrow = c(1, 1))
}




par(mfrow = c(2, 2))
for (i in 1:3) {
  hist(results$bsave)
}