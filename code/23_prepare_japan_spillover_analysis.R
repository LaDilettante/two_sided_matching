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
         sic_3_1, intensity_avg, jemp, temp, uscptl) %>%
  mutate(year = as.numeric(year), nation = as.character(nation),
         temp = as.numeric(temp), uscptl = as.numeric(uscptl)) %>%
  filter(year == 2001) %>%
  mutate(iso2c = countrycode(nation,
                             origin = "country.name", destination = "iso2c"))

d_nation_with_more_than100firms <- d %>%
  dplyr::count(nation) %>% filter(n >= 100) %>% select(nation)

d <- d %>% inner_join(d_nation_with_more_than100firms, by = "nation")

# ---- Merge in country data ----

load("../clean_data/country_covariates.RData")

# Add Taiwan data
d_taiwan <- data.frame(
  iso2c = "TW", year = 2001,
  gdp = 293.68 * 1e9, gdppc = 13107.64, enroll_sec_pct = 93.53)
d_wdi <- rbind.fill(d_wdi, d_taiwan)
d_wdi %>% filter(iso2c == "TW", year == 2001)

# Add Hong Kong data
d_hongkong <- data.frame(iso2c = "HK", year = 2001, democracy = 1)
d_dd <- rbind.fill(d_dd, d_hongkong)

# Add VN data
d_wdi[d_wdi$iso2c == "VN" & d_wdi$year == 201, "enroll_sec_pct"] = 89.94

d_merged <- d %>%
  left_join(d_wdi, by = c("iso2c", "year")) %>%
  left_join(d_dd, by = c("iso2c", "year")) %>%
  select(temp, uscptl, intensity_avg, nation,
         gdp, gdppc, enroll_sec_pct, labor_sec_pct, democracy)

md.pattern(d_merged %>% select(democracy, gdp, gdppc, enroll_sec_pct, labor_sec_pct))
md.pattern(d_merged %>% select(uscptl, temp, intensity_avg))

d_merged_final <- d_merged %>%
  select(temp, uscptl, intensity_avg, nation,
         gdp, gdppc, labor_sec_pct, democracy) %>% na.omit()

# Add nation_id for nation

d_nation_id <- data.frame(
  nation = sort(names(table(d_merged_final$nation))),
  nation_id = 1:length(unique(d_merged_final$nation)))

d_merged_final <- d_merged_final %>% inner_join(d_nation_id, by = "nation")

# ---- Save data ----

saveRDS(d_merged_final, file = "../clean_data/JapanFDI_for_analysis_withrdintense_humancap.RDdata")
