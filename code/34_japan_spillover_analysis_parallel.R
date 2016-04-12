rm(list = ls())
source("0_tslogit.R")
library(foreach)
library(doMC)
library(dplyr)
# ---- Load data ----

dat <- readRDS("../clean_data/JapanFDI_for_analysis_withrdintense_humancap.RDdata") %>%
  select(temp, uscptl, intensity_avg, nation, gdp, gdppc, avg_schooling_years, democracy) %>%
  group_by(nation) %>% filter(n() > 5) %>%
  filter(uscptl > 1000)

# Add nation_id for nation

d_nation_id <- data.frame(
  nation = sort(unique(dat$nation)),
  nation_id = 1:length(unique(dat$nation)), stringsAsFactors = F)

dat <- dat %>% inner_join(d_nation_id, by = "nation")

# ---- Create a grid of step sizes ----

eps1 <- c(0.025, 0.03, 0.05)
eps2 <- c(0.0005, 0.00075, 0.0009, 0.001, 0.0015)
eps_grid <- expand.grid(eps1=eps1, eps2=eps2)

# ---- Run the tslogit MCMC ----
firm_vars = c("temp", "uscptl", "intensity_avg")
country_vars = c("gdp", "gdppc", "democracy", "avg_schooling_years")

registerDoMC(cores=detectCores()/2)

logfile <- "tslogit_parallel2.log"
writeLines(c(""), logfile) # Clear out log file
foreach(eps1 = eps_grid$eps1, eps2 = eps_grid$eps2) %dopar% {
  sink(logfile, append=TRUE) # Write to log file
  f_tslogit(dat = dat, nskip = 100, nsave = 20000,
            eps1=eps1, eps2=eps2,
            firm_vars = firm_vars, country_vars = country_vars)
}