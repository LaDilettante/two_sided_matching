rm(list = ls())
source("0_functions.R")
packs <- c("plyr", "dplyr", "countrycode")
f_install_and_load(packs)

# ---- Load Japan data ----

d_raw <- readRDS("../clean_data/JapanFDI_long.RData")
d_raw_labels <- readRDS("../clean_data/JapanFDI_labels.RData")
glimpse(d_raw)

# Clean data
d <- d_raw %>%
  select(id, sub_name, year, nation,
         sic_3_1, sic_2_1, jemp, temp, uscptl) %>%
  mutate(year = as.numeric(year), nation = as.character(nation),
         temp = as.numeric(temp), uscptl = as.numeric(uscptl)) %>%
  filter(year == 2001) %>%
  mutate(iso2c = countrycode(nation,
                             origin = "country.name", destination = "iso2c"))

d_nation_with_more_than100firms <- d %>%
  dplyr::count(nation) %>% filter(n >= 100) %>% select(nation)

d <- d %>% inner_join(d_nation_with_more_than100firms, by = "nation")
