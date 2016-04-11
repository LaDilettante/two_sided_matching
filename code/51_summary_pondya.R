rm(list = ls())
source("0_functions.R")
packs <- c("reshape2", "plyr", "dplyr", "ggplot2", "stargazer", "countrycode")
f_install_and_load(packs)

d <- read.csv("../raw_data/fdi.entryres.csv")

d <- d %>%
  mutate(continent = countrycode(d$cty_1, origin = "country.name", destination = "continent"),
         region = countrycode(d$cty_1, origin = "country.name", destination = "region"))

d %>% filter(continent == "Europe") %>% group_by(year) %>%
  summarise(m = mean(entry_res, na.rm = T)) %>% print(n = 100)

d %>% filter(cty == "fin") %>% select(year, entry_res)
