# load("../result/FDI_attraction04-05_18-02.RData")

load("../result/FDI_spillover04-06_19-16.RData")

str(results, max.level = 2)

# ---- alpha (firms preference) ----
nw <- dim(results$asave)[2]
nx <- dim(results$bsave)[2] / length(unique(results$dat$nation_id))

# Convergence
par(mfrow = c(2, 2))
for (i in 1:nw) {
  plot(results$asave[, i], type = "l")
}
par(mfrow = c(1, 1))

# Results alpha
c_burnin <- 6000
par(mfrow = c(2, 2))
for (i in 1:nw) {
  hist(results$asave[c_burnin:nrow(results$asave), i])
}
par(mfrow = c(1, 1))

for (i in 1:nw) {
  print(quantile(results$asave[c_burnin:nrow(results$asave), i],
                 probs = c(0.025, 0.5, 0.075)))
}
head(results$dat)


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