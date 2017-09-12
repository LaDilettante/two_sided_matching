rm(list = ls())
library(coda)
library(xtable)
library(psData)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(stargazer)

# Between 20 and 50 percent is good
load("../result/tslogit_0-03-0-00075_04-12_21-01.RData")
nrow(results$dat) == 4559 # Asia
results$acrate

# ---- Summary ----

m <- data.frame(table(results$dat$nation))
m <- rowr::cbind.fill(m[1:20, ], m[21:37, ], fill = NA)
colnames(m) <- NULL
rownames(m) <- NULL

print(xtable(m),
      include.rownames = FALSE, include.colnames = FALSE,
      file = "../table/list_of_countries.tex", floating = FALSE)

# ---- alpha ----

colnames(results$asave) <- c("log GDP", "log GDP per cap", "Democracy", "Average Years of Schooling")
asave <- mcmc(results$asave)
pdf("../figure/traceplot_alpha.pdf", w = 5.6, h = 6.2)
plot(asave)
dev.off()

asave_burn <- mcmc(results$asave[10001:nrow(results$asave), ])
summary(asave_burn)

# ---- beta ----

bsave <- mcmc(results$bsave)
plot(bsave)

f_getcountrybeta <- function(idx, start = 1) {
  bcountry <- mcmc(bsave[start:nrow(bsave), ((idx-1)*4+1):((idx-1)*4+4) ])
  colnames(bcountry) <- c("intercept", "total emp", "capital", "tech intensity")
  return(bcountry)
}

c_nations <- sort(unique(results$dat$nation))

pdf("../figure/traceplot_myanmar.pdf", w = 5.4, h = 4.8)
plot(f_getcountrybeta(which(c_nations == "Myanmar")), density = F) # Poor trace, 8 firms
dev.off()

pdf("../figure/traceplot_thailand.pdf", w = 5.4, h = 4.8)
plot(f_getcountrybeta(which(c_nations == "Thailand")), density = F) # Lots of country decent trace, 696 firms
dev.off()

plot(f_getcountrybeta(which(c_nations == "China")), density = F) # 1709 firms, very nice trace

b_labor <- bsave[10001:nrow(bsave), seq(2, ncol(bsave), by = 4)]
b_tech <- bsave[10001:nrow(bsave), seq(4, ncol(bsave), by = 4)]
colnames(b_labor) <- c_nations
colnames(b_tech) <- c_nations
b_labor <- b_labor[ , colnames(b_labor) != "Hong Kong"] # Remove Hong Kong
b_tech <- b_tech[ , colnames(b_tech) != "Hong Kong"] # Remove Hong Kong

load("../clean_data/country_covariates.RData")

d_developing <- WDI(country = countrycode(c_nations, origin = "country.name", destination = "iso2c"),
                    start = 2003, end = 2003, extra = TRUE) %>%
  select(iso2c, year, income)

d_b_tech <- data.frame(nation = colnames(b_tech),
                       tech_preference = colMeans(b_tech),
                       labor_preference = colMeans(b_labor),
                       year = 2003) %>%
  mutate(iso2c = countrycode(nation, "country.name", "iso2c"),
         tech_over_labor = tech_preference / labor_preference) %>%
  left_join(d_dpi, by = c("iso2c", "year")) %>%
  left_join(d_dd, by = c("iso2c", "year")) %>%
  left_join(d_wdi, by = c("iso2c", "year")) %>%
  left_join(d_developing, by = c("iso2c", "year")) %>%
  mutate(execage = ifelse(execage == -999, NA, execage))

d_b_tech_rich <- filter(d_b_tech, income == "High income: OECD" | income == "High income: nonOECD")
d_b_tech_poor <- filter(d_b_tech, income != "High income: OECD" & income != "High income: nonOECD")

ggplot(data = d_b_tech, aes(execage, tech_over_labor)) +
  geom_point(aes(color = factor(democracy))) +
  geom_text(aes(label = nation))




ggplot(data = d_b_tech_poor, aes(execage, tech_over_labor)) +
  geom_point(aes(color = factor(democracy))) +
  geom_text(aes(label = nation))


p1 <- ggplot(data = d_b_tech_rich,
             aes(execage, tech_preference)) +
  geom_point(aes(color = factor(democracy))) +
  geom_text(aes(label = nation)) + geom_smooth(method = "lm") +
  scale_color_discrete("Democracy") + theme_bw() +
  labs(y = "Preference for high-tech MNCs", x = "Age of Executive's Party")

pdf("../figure/preference_for_tech.pdf", w = 5.5, h = 3.5)
ggplot(data = d_b_tech_poor, aes(execage, tech_preference)) +
  geom_point() +
  geom_text(aes(label = nation)) +
  geom_smooth(method = "lm") +
  scale_color_discrete("Democracy") + theme_bw() +
  labs(y = "Preference for high-tech MNCs", x = "Age of Executive's Party")
dev.off()

ggplot(data = d_b_tech_poor, aes(execage, labor_preference)) +
  geom_point(aes(color = factor(democracy))) +
  geom_text(aes(label = nation)) +
  geom_smooth(method = "lm") +
  scale_color_discrete("Democracy") + theme_bw() +
  labs(y = "Preference for high-tech MNCs", x = "Age of Executive's Party")


ggplot(data = d_b_tech_poor, aes(log(gdppc), tech_over_labor)) +
  geom_point(aes(color = factor(democracy))) +
  geom_text(aes(label = nation))


plot(d_b_tech$execage, d_b_tech$tech_preference)

summary(lm(tech_over_labor ~ execage + spell + log(gdp) + log(gdppc),
           data = d_b_tech_poor))