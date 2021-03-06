library(coda)

# Between 20 and 50 percent is good
load("../result/tslogit_0-05-5e-04_04-12_05-11.RData")
load("../result/tslogit_0-05-0-001_04-12_05-11.RData")
load("../result/tslogit_0-05-0-005_04-12_04-41.RData")

head(results$dat)
results$acrate
results$mcmc

str(results, max.level = 2)

# ---- alpha (firms preference) ----
nw <- dim(results$asave)[2]
nx <- dim(results$bsave)[2] / length(unique(results$dat$nation_id))

dim(results$asave)

asave <- mcmc(results$asave)
plot(asave)
# Convergence
par(mfrow = c(2, 2))
for (i in 1:nw) {
  plot(results$asave[, i], type = "l", ylab = paste(c("alpha", i)))
}
par(mfrow = c(1, 1))

# Results alpha
c_burnin <- 10000
for (i in 1:nw) {
  print(quantile(results$asave[c_burnin:nrow(results$asave), i],
                 probs = c(0.025, 0.05, 0.5, 0.95, 0.975)))
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