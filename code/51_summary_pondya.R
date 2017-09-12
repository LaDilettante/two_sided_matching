rm(list = ls())
source("0_functions.R")
packs <- c("reshape2", "plyr", "dplyr", "ggplot2", "stargazer", "countrycode")
f_install_and_load(packs)

d <- read.csv("../raw_data/fdi.entryres.csv")

d <- d %>%
  mutate(continent = countrycode(d$cty_1, origin = "country.name", destination = "continent"),
         region = countrycode(d$cty_1, origin = "country.name", destination = "region"))

# ---- Visualize China ----


d_china <- d %>% filter(cty_1 == "china")
pdf("../figure/china_fdi_restriction.pdf", w = 7, h = 3.5)
ggplot(d_china, aes(year, entry_res)) +
  geom_line() + geom_point() +
  labs(y = "Share of Industries with Restrictions",
       x = "Year", title = "China's FDI Ownership Restriction") +
  scale_x_continuous(breaks = seq(1970, 2000, by = 5)) + theme_bw()
dev.off()
