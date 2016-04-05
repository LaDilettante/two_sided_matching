# Modified from R code to run MCMC for the 2-sided marriage model of Logan, Hoff, Newton
#
rm(list = ls())

# time stamp
library(dplyr)
d1 <- date()

#runif(2) ## modify seed for different chain
# Data
dat <- readRDS("../clean_data/JapanFDI_for_analysis.RDdata") %>%
  select(temp, uscptl, nation_id, gdp, gdppc, democracy) %>%
  filter(uscptl > 1000) # Firms with less than 1000 USD capital is 1 pct, likely coding error
dat2 <- read.table("../clean_data/gss18cat.raw", header=T) # for comparison
choice <- dat$nation_id
nfirms <-  length(choice) # Job acceptances (elements in 1:nnations)

nnations <- length(unique(choice))    # includes unemployment
nx <- 3 # number of firm characteristics per firm; including the intercept
nw <- 3  # number of job characteristics per job; in this example, job quality

ww <- matrix(NA,nnations,nw) # ww is a matrix of job characteristics (see below)
for( i in 1:nnations )
{
  ind <- (1:nfirms)[ dat$nation_id == i ] # index of workers whose got job i
  ww[i,1] <- unique( dat$gdp[ind] ) # gdp of nation i
  ww[i,2] <- unique( dat$gdppc[ind] ) # gdppc of nation i
  ww[i,3] <- unique( dat$democracy[ind] ) # democracy of nation i
}
# rescale (for better numerics )
ww[, 1:2] <- log(ww[, 1:2])

one <- rep(1,nfirms)
xx <- cbind( one, dat[,1:(nx-1)] )
# matrix nfirms x nx of firm characteristics
# including column of ones for an intercept

# rescale
xx[,2] <- xx[,2] / 10
xx[,3] <- log(xx[,3])
xx <- as.matrix(xx)



# Run characteristics

mcmc <- list(   nskip=100,    # block size for each saved state
                nsave=100,    # number of saved states
                npar=2,      # skips between parameter updates
                eps1=0.02,    # scale of alpha update
                eps2=1/20  )  #  scale of beta update; reduction of sd

# Initialize key objects and common calculations across cycles

alpha <- rep(0,nw)            # worker preferences (one param for each of nw job characteristics)
beta  <- matrix(0,nx,nnations)   # employer preferences
# each employer gets their own set of preference (hence nnations),
# which comprises of one param for each of nx workers characteristics

# Opportunity sets
opp <- matrix(F,nfirms,nnations)  # The opportunity matrix T=offer,F=no offer
opp[cbind(1:nfirms,choice)] <- T  # firms are offered the countries they are in!

# Visualize the opp set
# library(gplots)
# heatmap.2(head(opp + 0, n = 50), dendrogram = "none", trace = "none",
#   density.info = "none", Rowv = F, Colv = F)

# get 1-sided logit estimates for employer preferences for starting values
# beta for 1st employer type ("unemployed") remains 0
for( j in 1:nnations )
{
  y <- as.numeric( opp[,j] )
  mod <- glm( y ~ one + temp + uscptl - 1, family=binomial,
              data=as.data.frame(xx) )
  beta[,j] <- mod$coef
}
beta0 <- beta ## one-sided logit estimators

bmat <- mcmc$eps2*matrix(1,nx,nnations)

# Why are these codes here? Duplicated from above?
# opp <- matrix(F,nfirms,nnations)  # The opportunity matrix T=offer,F=no offer
# opp[cbind(1:nfirms,choice)] <- T  # people are offered jobs they have!
# opp[,1] <- T                     # Unemployment always offered


tmp <- as.matrix(ww[choice,(1:nw)])
# Anh: tmp IS the characteristics of accepted job.
# It's the same as dat[ , c("presmean", "autmean")] / 10 though...
# all.equal(tmp, as.matrix(dat[ , c("presmean", "autmean")]) / 10, check.attributes = F)
# Anh: don't know what wa is? why take the sum across the (2149) accepted jobs?
wa <- apply(tmp,2,sum) # characteristics of accepted jobs; used in alpha update


avec <- exp(ww%*%alpha) # Anh: right now alpha is [0, 0], so avec is [1, 1]
den <- opp%*%avec          # vector of denominators in acceptance probs
eta <- xx%*%beta           # worker side linear predictors (big matrix)
# nfirms x nnations, same as opp

# Initialize storage
acrate <- rep(0,3)                  # Metropolis acceptance rates
B <- mcmc$nsave*mcmc$nskip          # number of cycles
asave <- matrix(NA,mcmc$nsave,nw)   # saved alphas and betas
bsave <- matrix(NA,mcmc$nsave,nx*(nnations-1) )
logpost1 <- numeric(mcmc$nsave)      # log P(O|beta)
logpost2 <- numeric(mcmc$nsave)      # log P(A|O,alpha)

skipcount <- 0
pskip <- 0
isave <- 1
# Big loop
for( i in 1:B )
{
  # Update counters
  skipcount <- skipcount + 1
  pskip <- pskip + 1
  ###############################################################
  # Update opportunity sets (do things in parallel across workers
  # because of conditional independence)

  # Sample a random job for each  worker and consider switching it
  new <- sample( 1:nnations, size=nfirms, replace=T )
  ind <- cbind( 1:nfirms, new )
  # The offers under consideration are:
  oo <- opp[ind] # A: nfirms logical vector, indicating whether newly sampled job is currently offered
  plusminus <- ifelse(oo,-1,1) # A: convert T/F oo into -1/1
  # Part of MH ratio from P(A|O,alpha)
  denstar <- den+avec[new]*plusminus
  rr1 <- den/denstar
  # Factor in part of MH ratio from P(O|beta)  (logistic model)
  xb <- eta[ind]
  rr2 <- exp( plusminus*xb )

  # Accept or not (in parallel)
  uu <- runif(nfirms)
  ok <- (uu <= rr1*rr2 ) # A: ????
  ok[new == choice] <- F  # don't change an offer for an accepted job
  if( any(ok)  )
  {
    oonew <- oo
    oonew[ok] <- ( !(oo[ok]) ) # Flip offered/unoffered if ok is TRUE
    opp[ind] <- oonew       # Update opportunities
    den[ok] <- denstar[ok]             # Update denominators
  }
  acrate[1] <- acrate[1] + mean(ok)

  # Update parameters alpha and beta, periodically.
  if( pskip == mcmc$npar )
  {
    # First alpha:
    deviation <- mcmc$eps1*runif(nw,min=-1,max=1) # draw a box of +- eps1
    alphastar <- alpha + deviation # sample alphastar from within that box
    # Calculate likelihood ratio
    avecstar <-  exp( ww%*%alphastar )
    denstar <- opp %*% avecstar
    logmh <- sum(wa*deviation) + sum(log(den) - log(denstar)) # logmh is the M-H acceptance ratio
    # A: according to paper this is somehow
    ok <- ifelse( log(runif(1)) <= logmh, T, F )
    ok <- ifelse( is.na(ok), F, ok )
    if(ok)
    {
      alpha <- alphastar
      # Update downstream quantitities
      avec <- avecstar
      den <- denstar
      acrate[2] <- acrate[2]+1
    }
    # Next beta
    whichones <- ifelse( runif(nx*nnations) <= .25, 1, 0 ) # work on a subset of beta's
    rmat <- matrix( runif(nx*nnations,min=-1,max=1)*whichones, nx, nnations )
    deviation <- bmat*rmat
    betastar <- beta + deviation
    etastar <- xx%*%betastar
    # Calculate likelihood ratio (using logistic structure)(don't count unemp.
    lrat <- sum((opp*(etastar-eta)))    # the `canonical' part (unemp. cancels)
    lrat <- lrat + sum(log( 1+exp(eta))-log( 1+exp(etastar))) #unemp. cancels
    # ? prior component; flat for now
    logmh <- lrat
    ok <- ifelse( log(runif(1)) <= logmh, T, F)
    if(ok)
    {
      beta <- betastar
      eta <- etastar
      acrate[3] <- acrate[3]+1
    }
    # reset counter
    pskip <- 0
  }
  # Store output, periodically
  if( skipcount == mcmc$nskip )
  {
    skipcount <- 0
    # evaluate the logposterior
    lp1 <- sum( opp[,2:nnations]*eta[,2:nnations] ) -
      sum( log(1+exp(eta[,2:nnations])) )   # P(O|beta)
    lp2 <-  sum(wa*alpha) - sum( log(den) )       # P(A|O,alpha)
    print( c(isave) )
    logpost1[isave] <- lp1
    logpost2[isave] <- lp2
    asave[isave,] <- c(alpha)
    bsave[isave,] <- c(beta[,2:nnations]) # vectorize
    isave <- isave+1
  }
}
acrate[1] <- acrate[1]/B
acrate[2:3] <- (acrate[2:3]/B)*mcmc$npar

d2 <- date()

results <- list( mcmc=mcmc,  acrate=acrate,
                 asave=asave,bsave=bsave,logpost=cbind(logpost1,logpost2),
                 time=c(d1,d2))

save( results, file="../results/FDI_attraction_wardcluster.RData" )
#save( results, file="RData/test2.RData" )  ## different seed
