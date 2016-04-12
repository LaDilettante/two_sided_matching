rm(list = ls())
source("0_tslogit.R")
library(foreach)
library(doMC)
library(dplyr)
library(countrycode)
# ---- Load data ----

dat <- readRDS("../clean_data/JapanFDI_for_analysis_withrdintense_humancap.RDdata") %>%
  select(temp, uscptl, intensity_avg, nation, gdp, gdppc, avg_schooling_years, democracy) %>%
  mutate(region = countrycode(nation, origin = "country.name", destination = "region")) %>%
  filter(region == "Eastern Asia" | region == "South-Eastern Asia" | nation == "Taiwan") %>%
  group_by(nation) %>% filter(n() > 10) %>% # Effectively remove Laos, Myanmar and Cambodia
  filter(uscptl > 1000) # Remove outliers. 1% of capital already means 38k

# Add nation_id for nation

d_nation_id <- data.frame(
  nation = sort(unique(dat$nation)),
  nation_id = 1:length(unique(dat$nation)), stringsAsFactors = F)

dat <- dat %>% inner_join(d_nation_id, by = "nation")

# ---- Create a grid of step sizes ----

eps1 <- c(0.025, 0.05)
eps2 <- c(0.0005, 0.00075, 0.001)
eps_grid <- expand.grid(eps1=eps1, eps2=eps2)

# ---- Run the tslogit MCMC ----
firm_vars = c("temp", "uscptl", "intensity_avg")
country_vars = c("gdp", "gdppc", "democracy", "avg_schooling_years")

registerDoMC(cores=detectCores()/2)

logfile <- "tslogit_asia_parallel.log"
writeLines(c(""), logfile) # Clear out log file
foreach(eps1 = eps_grid$eps1, eps2 = eps_grid$eps2) %dopar% {
  sink(logfile, append=TRUE) # Write to log file
  f_tslogit(dat = dat, nskip = 100, nsave = 20000,
            eps1=eps1, eps2=eps2,
            firm_vars = firm_vars, country_vars = country_vars)
}