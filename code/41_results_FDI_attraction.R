# load("../result/FDI_attraction04-05_18-02.RData")

load("../result/FDI_spillover04-07_03-27.RData")

head(results$dat)
results$acrate
results$mcmc

str(results, max.level = 2)

# ---- alpha (firms preference) ----
<<<<<<< HEAD
nx <- 4
nw <- 4
=======
nw <- dim(results$asave)[2]
nx <- dim(results$bsave)[2] / length(unique(results$dat$nation_id))
>>>>>>> 4da0bd611d8020f526dc088fa5217981b801e9a3

# Convergence
par(mfrow = c(2, 2))
for (i in 1:nw) {
<<<<<<< HEAD
  plot(results$asave[, i], type = "l")
=======
  plot(results$asave[, i], type = "l", ylab = paste(c("alpha", i)))
>>>>>>> 4da0bd611d8020f526dc088fa5217981b801e9a3
}
par(mfrow = c(1, 1))

# Results alpha
<<<<<<< HEAD
par(mfrow = c(2, 2))
for (i in 1:nw) {
  hist(results$asave[, i])
}
par(mfrow = c(1, 1))

for (i in 1:nw) {
  print(quantile(results$asave[, i], probs = c(0.025, 0.975)))
=======
c_burnin <- 6000
for (i in 1:nw) {
  print(quantile(results$asave[c_burnin:nrow(results$asave), i],
                 probs = c(0.025, 0.5, 0.975)))
>>>>>>> 4da0bd611d8020f526dc088fa5217981b801e9a3
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
<<<<<<< HEAD
  for (j in 1:nw) {
    plot(results$bsave[, (i - 1) + j], type = "l")
=======
  for (j in 1:nx) {
    plot(results$bsave[, (i - 1) + j], type = "l", ylab = paste(c("beta", i, j)))
>>>>>>> 4da0bd611d8020f526dc088fa5217981b801e9a3
  }
  par(mfrow = c(1, 1))
}




par(mfrow = c(2, 2))
for (i in 1:3) {
  hist(results$bsave)
}