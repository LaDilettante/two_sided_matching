# Modified from R code to run MCMC for the 2-sided marriage model of Logan, Hoff, Newton
# Encapsulated into a function

f_tslogit <- function(dat = dat,
                      nskip = 1000, nsave = 10000,
                      eps1=0.02, eps2=1/20,    # scale of alpha / beta update
                      firm_vars = c("temp", "uscptl", "intensity_avg"),
                      country_vars = c("gdp", "gdppc", "democracy", "avg_schooling_years")
              )
{
  library(dplyr)
  d1 <- date() # time-stamp

  class(dat) <- "data.frame" # Remove the dplyr class

  choice <- dat$nation_id
  nfirms <-  length(choice) # Job acceptances (elements in 1:nnations)

  nnations <- length(unique(choice))    # includes unemployment
  # nx <- 3 # number of firm characteristics per firm; including the intercept
  nx <- length(firm_vars) + 1
  # nw <- 3  # number of country characteristics per country;
  nw <- length(country_vars)

  # ww is a matrix of country characteristics
  ww <- dat %>% select(nation_id, match(country_vars, names(dat))) %>%
    distinct() %>%
    arrange(nation_id) %>% select(-nation_id) %>% as.matrix

  # rescale (for better numerics)
  ww[, c("gdp", "gdppc")] <- log(ww[, c("gdp", "gdppc")])

  one <- rep(1,nfirms)
  xx <- cbind( one, dat[, firm_vars] )
  # matrix nfirms x nx of firm characteristics
  # including column of ones for an intercept

  # rescale
  xx[, "temp"] <- xx[, "temp"] / 10
  xx[, "uscptl"] <- log(xx[, "uscptl"])
  xx <- as.matrix(xx)

  # Run characteristics

  mcmc <- list(   nskip=nskip,    # block size for each saved state
                  nsave=nsave,    # number of saved states
                  npar=2,      # skips between parameter updates
                  eps1=eps1,    # scale of alpha update
                  eps2=eps2  )  #  scale of beta update; reduction of sd

  # Initialize key objects and common calculations across cycles

  alpha <- rep(0,nw)            # worker preferences (one param for each of nw job characteristics)
  beta  <- matrix(0,nx,nnations)   # employer preferences
  # each employer gets their own set of preference (hence nnations),
  # which comprises of one param for each of nx workers characteristics

  # Opportunity sets
  opp <- matrix(F,nfirms,nnations)  # The opportunity matrix T=offer,F=no offer
  opp[cbind(1:nfirms,choice)] <- T  # firms are offered the countries they are in!

  # get 1-sided logit estimates for country preferences for starting values
  for( j in 1:nnations )
  {
    y <- as.numeric( opp[,j] )
    mod_formula <- paste("y ~ ", paste(dimnames(xx)[[2]], collapse = " + "), " - 1",
                         collapse = "")
    mod <- glm( as.formula(mod_formula), family=binomial,
                data=as.data.frame(xx) )
    beta[,j] <- mod$coef
  }
  beta0 <- beta ## one-sided logit estimators

  bmat <- mcmc$eps2*matrix(1,nx,nnations)
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
  acrate <- rep(0,3)                  # Metropolis acceptance rates for O, alpha, beta
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

    # Sample a random country offer for each firm and consider switching it
    new <- sample( 1:nnations, size=nfirms, replace=T )
    ind <- cbind( 1:nfirms, new )
    # The offers under consideration are:
    oo <- opp[ind] # A: nfirms logical vector, indicating whether newly sampled offer is currently offered
    plusminus <- ifelse(oo,-1,1) # A: convert T/F oo into -1/1
    # Part of MH ratio from P(A|O,alpha)
    denstar <- den+avec[new]*plusminus
    rr1 <- den/denstar
    # Factor in part of MH ratio from P(O|beta)  (logistic model)
    xb <- eta[ind]
    rr2 <- exp( plusminus*xb )

    # Accept or not (in parallel)
    uu <- runif(nfirms)
    ok <- (uu <= rr1*rr2 )
    ok[new == choice] <- F  # don't change an offer for an accepted location
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
      if (isave %% 5000 == 1) { cat("eps1: ", eps1, "eps2:", eps2,
                                    "iter = ", c(isave), "\n") }
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

  results <- list( mcmc=mcmc, acrate=acrate,
                   asave=asave, bsave=bsave,
                   logpost=cbind(logpost1,logpost2),
                   time=c(d1,d2), data=dat, xx=xx, ww=ww )

  output = paste("../result/tslogit_", gsub("[ \\.]", "-", paste(eps1, eps2)), "_",
                 strftime(Sys.time(), format = "%m-%d_%H-%M"),
                 ".RData", sep = "")
  save(results, file=output)
  cat(output, "done", d1, d2, "\n")
}