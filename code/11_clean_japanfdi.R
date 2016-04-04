rm(list = ls())
source("0_functions.R")
packs <- c("foreign", "dplyr", "reshape2", "stringr", "tidyr",
           "lubridate")
f_install_and_load(packs)

# ---- read data ----

d_raw <- read.spss("../raw_data/2003_entry_exit_matrix_all_data.sav",
               to.data.frame = TRUE)
d_labels <- data.frame(name = names(d_raw),
                       label = attr(d_raw, "variable.labels"))

# ---- reshape data back to wide format ----
# The dataset is put together by merging a bunch of annual datasets
# Thus, a "minimal dataset" that preserves all the information is in wide format

d <- d_raw %>%
  select(order, sub_name,
         nation, region, province, city, zip_code,
         ind_zone, sta_pro, mode,
         fdtn_yr, fdtn_est, yeardata, yearexit, year_e,
         matches("^gm.*[0-9]{2}$"), # nationality of subsidiary general manager
         matches("^temp.*[0-9]{2}$"), # total employees
         matches("^jemp.*[0-9]{2}$"), # Japanese employees
         matches("^uscptl.*[0-9]{2}$"), # capital invested in subsidiary
         matches("^ussale.*[0-9]{2}$"), # sales of subsidiary
         matches("^prft.*[0-9]{2}$"), # profitability of subsidiary
         matches("ja1.*[0-9]{2}$"), # ownership of primary japanese partnert
         matches("^sic") # SIC code
         ) %>%
  distinct()

if (nrow(d) != length(d$order)) {
  stop("Bad: The data is not yet reduced to its minimal form.
       There are duplications that should not exist!")
} else {
  message("Good: The data is reduced to its minimal form.")
}

# ---- Save wide data ----

saveRDS(d, file = "../clean_data/JapanFDI_wide.RData")
saveRDS(d_labels, file = "../clean_data/JapanFDI_labels.RData")
