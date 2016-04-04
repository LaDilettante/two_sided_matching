rm(list = ls())
source("0_functions.R")
packs <- c("haven", "dplyr", "WDI", "zoo")
f_install_and_load(packs)

# Note: Merge everything using iso2c

# ---- Constants ----
c_startyear = 1995
c_endyear = 2010

# ---- Get WDI data ----

d_wdi_raw <- WDI(
  country = "all",
  indicator = c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.KD", "SL.TLF.TERT.ZS",
                "SE.SEC.ENRR", "SE.TER.ENRR"),
  start = c_startyear, end = c_endyear, extra = TRUE)

d_wdi <- d_wdi_raw %>%
  filter(region != "Aggregates") %>%
  select(iso2c, country, year,
         gdp = NY.GDP.MKTP.KD,
         gdppc = NY.GDP.PCAP.KD,
         enroll_sec_pct = SE.SEC.ENRR,
         enroll_tert_pct = SE.TER.ENRR) %>%
  arrange(country, year)



# ---- Clean dd data ----

d_dd_raw <- read_dta("../raw_data/GWF Autocratic Regimes 1.2/GWF_AllPoliticalRegimes.dta")

d_dd <- d_dd_raw %>%
  filter(year >= c_startyear, year <= c_endyear) %>%
  mutate(iso2c = countrycode(cowcode, origin="cown", destination="iso2c", warn=TRUE)) %>%
  mutate(democracy = ifelse(gwf_nonautocracy == "democracy", 1, 0)) %>%
  select(iso2c, country = gwf_country, year, democracy, cowcode)

# ---- Save data ----

save(d_dd, d_wdi, file = "../clean_data/country_covariates.RData")

