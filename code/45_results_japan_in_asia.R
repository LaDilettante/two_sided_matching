rm(list = ls())
library(coda)
library(xtable)
library(psData)

# Between 20 and 50 percent is good
load("../result/tslogit_0-025-0-001_04-12_21-01.RData")
nrow(results$dat) == 4559 # Asia
results$acrate

# ---- alpha ----

colnames(results$asave) <- c("log GDP", "log GDP per cap", "Democracy", "Average Years of Schooling")
asave <- mcmc(results$asave)
pdf("../figure/traceplot_alpha.pdf", w = 5.6, h = 6.2)
plot(asave)
dev.off()

asave_burn <- mcmc(results$asave[10001:nrow(results$asave), ])
summary(asave_burn)
xtable(data.frame(summary(asave)))


# ---- beta ----

bsave <- mcmc(results$bsave)
plot(bsave)

f_getcountrybeta <- function(idx, start = 1) {
  bcountry <- mcmc(bsave[start:nrow(bsave), ((idx-1)*4+1):((idx-1)*4+4) ])
  colnames(bcountry) <- c("intercept", "total emp", "capital", "tech intensity")
  return(bcountry)
}

plot(f_getcountrybeta(which(c_nations == "Poland"))) # Poor trace, 8 firms
plot(f_getcountrybeta(which(c_nations == "Thailand"))) # Lots of country decent trace, 696 firms
plot(f_getcountrybeta(which(c_nations == "China"))) # 1709 firms, very nice trace

b_tech <- bsave[10001:nrow(bsave), seq(4, ncol(bsave), by = 4)]
colnames(b_tech) <- c_nations


d_dpi <- DpiGet()

c_nations <- sort(unique(results$dat$nation))

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