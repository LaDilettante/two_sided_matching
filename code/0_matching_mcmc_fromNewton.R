# R code to run MCMC for the 2-sided marriage model of Logan, Hoff, Newton
#

# time stamp
d1 <- date()

#runif(2) ## modify seed for different chain
# Data
dat <- read.table("../clean_data/gss18cat.raw", header=T)
choice <- dat$occ17 + 1 # so 1=unemployment
nworkers <-  length(choice) # Number of workers

njobs <- 18    # includes unemployment
nx <- 4 # number of worker characteristics per worker; including the intercept
              # in this example it measures quality of employee
nw <- 2  # number of job characteristics per job; in this example, job quality
#
 ww <- matrix(NA,njobs,nw) # ww is a matrix of job characteristics (see below)
 for( i in 1:njobs )
  {
   ind <- (1:nworkers)[ dat$occ17==(i-1) ] # index of workers whose got job (i - 1)
   ww[i,1] <- unique( dat$presmean[ind] ) # mean prestige of job (i - 1)
   ww[i,2] <- unique( dat$autmean[ind] ) # mean autonomy of job (i - 1)
   }
# rescale (for better numerics )
ww <- ww/10

one <- rep(1,nworkers)
xx <- cbind( one, dat[,1:3] )
                          # matrix nworkers x nx of worker characteristics
                          # including column of ones for an intercept

# rescale
xx[,2] <- xx[,2]/10
xx[,3] <- xx[,3]/10
xx <- as.matrix(xx)



# Run characteristics

mcmc <- list(   nskip=1000,    # block size for each saved state
                nsave=10000,    # number of saved states
                npar=2,      # skips between parameter updates
                eps1=0.02,    # scale of alpha update
		eps2=1/20  )  #  scale of beta update; reduction of sd

# Initialize key objects and common calculations across cycles

alpha <- rep(0,nw)            # worker preferences (one param for each of nw job characteristics)
beta  <- matrix(0,nx,njobs)   # employer preferences
  # each employer gets their own set of preference (hence njobs),
  # which comprises of one param for each of nx workers characteristics

# Opportunity sets
opp <- matrix(F,nworkers,njobs)  # The opportunity matrix T=offer,F=no offer
opp[cbind(1:nworkers,choice)] <- T  # people are offered jobs they have!
opp[,1] <- T                     # Unemployment always offered

# Visualize the opp set
# library(gplots)
# heatmap.2(head(opp + 0, n = 50), dendrogram = "none", trace = "none",
#   density.info = "none", Rowv = F, Colv = F)

# get 1-sided logit estimates for employer preferences for starting values
# beta for 1st employer type ("unemployed") remains 0
for( j in 2:njobs )
 {
  y <- as.numeric( opp[,j] )
  mod <- glm( y ~ one + educ + age + nonwhite - 1, family=binomial,
			data=as.data.frame(xx) )
  beta[,j] <- mod$coef
 }
beta0 <- beta ## one-sided logit estimators

bmat <- mcmc$eps2*matrix(1,nx,njobs)

# Why are these codes here? Duplicated from above?
# opp <- matrix(F,nworkers,njobs)  # The opportunity matrix T=offer,F=no offer
# opp[cbind(1:nworkers,choice)] <- T  # people are offered jobs they have!
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
                           # nworkers x njobs, same as opp

# Initialize storage
acrate <- rep(0,3)                  # Metropolis acceptance rates
B <- mcmc$nsave*mcmc$nskip          # number of cycles
asave <- matrix(NA,mcmc$nsave,nw)   # saved alphas and betas
bsave <- matrix(NA,mcmc$nsave,nx*(njobs-1) )
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
  new <- sample( 2:njobs, size=nworkers, replace=T ) # don't sample unemp. (1)
  ind <- cbind( 1:nworkers, new )
  # The offers under consideration are:
  oo <- opp[ind] # A: nworkers logical vector, indicating whether newly sampled job is currently offered
  plusminus <- ifelse(oo,-1,1) # A: convert T/F oo into -1/1
  # Part of MH ratio from P(A|O,alpha)
  denstar <- den+avec[new]*plusminus
  rr1 <- den/denstar
  # Factor in part of MH ratio from P(O|beta)  (logistic model)
  xb <- eta[ind]
  rr2 <- exp( plusminus*xb )

  # Accept or not (in parallel)
  uu <- runif(nworkers)
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
     whichones <- ifelse( runif(nx*njobs) <= .25, 1, 0 ) # work on a subset of beta's
     rmat <- matrix( runif(nx*njobs,min=-1,max=1)*whichones, nx, njobs )
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
    lp1 <- sum( opp[,2:njobs]*eta[,2:njobs] ) -
             sum( log(1+exp(eta[,2:njobs])) )   # P(O|beta)
    lp2 <-  sum(wa*alpha) - sum( log(den) )       # P(A|O,alpha)
    print( c(isave) )
    logpost1[isave] <- lp1
    logpost2[isave] <- lp2
    asave[isave,] <- c(alpha)
    bsave[isave,] <- c(beta[,2:njobs]) # vectorize
    isave <- isave+1
   }
  }
acrate[1] <- acrate[1]/B
acrate[2:3] <- (acrate[2:3]/B)*mcmc$npar

d2 <- date()

results <- list( mcmc=mcmc,  acrate=acrate,
                 asave=asave,bsave=bsave,logpost=cbind(logpost1,logpost2),
		 time=c(d1,d2), data="gss18cat.raw" )

save( results, file="../results/test1.RData" )
#save( results, file="RData/test2.RData" )  ## different seed
