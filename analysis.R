# CDS 492 capstone (revised analysis): county-level crime, air quality, cost of living
# Austin Yoo
#
# Re-estimates the capstone models at the county level, where crime is measured,
# with state-clustered standard errors and controls. Writes LaTeX tables, figures,
# and a JSON of key numbers used in the paper.
#
# Usage: Rscript analysis.R   (run from this folder)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(fixest))
suppressPackageStartupMessages(library(jsonlite))

# Raw data are not redistributed here. Download them from Kaggle into data/
# (see data/README.md): QOL(County Level).csv and QOL(Public School Level).csv.
DATA <- "data"
OUT  <- "output"
FIG  <- "paper"
dir.create(OUT, showWarnings = FALSE)
dir.create(FIG, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Cleaning helpers
# ---------------------------------------------------------------------------
# '$75,531.37' / '3.21%' / '6,605' -> numeric. '-1' and blanks -> NA.
num <- function(s) {
  x <- suppressWarnings(as.numeric(gsub("[$,%]", "", trimws(s))))
  ifelse(!is.na(x) & x == -1, NA, x)
}

# '  8/1000' -> 8 crimes per 1,000 residents. '0' -> NA (treated as non-reporting).
crime_per_1k <- function(s) {
  s <- trimws(s)
  parts <- strsplit(s, "/", fixed = TRUE)
  sapply(parts, function(v) if (length(v) == 2) as.numeric(v[1]) / as.numeric(v[2]) * 1000 else NA_real_)
}

crime_per_1k_keep_zero <- function(s) {
  r <- crime_per_1k(s)
  ifelse(trimws(s) == "0", 0, r)
}

# ---------------------------------------------------------------------------
# County-level data
# ---------------------------------------------------------------------------
c_raw <- read_csv(file.path(DATA, "QOL(County Level).csv"),
                  col_types = cols(.default = col_character()))
county <- tibble(
  fips     = sprintf("%05d", as.integer(c_raw$FIPS)),
  state    = c_raw$LSTATE,
  crime    = crime_per_1k(c_raw$`2016 Crime Rate`),
  crime0   = crime_per_1k_keep_zero(c_raw$`2016 Crime Rate`),
  pop      = num(c_raw$`2022 Population`),
  unemp    = num(c_raw$Unemployment),
  aqi_good = num(c_raw$`AQI%Good`),
  water_vpv = num(c_raw$WaterQualityVPV),
  col      = num(c_raw$`Cost of Living`),
  income   = num(c_raw$`2022 Median Income`),
  c2i      = num(c_raw$`AVG C2I`),
  party_r  = as.integer(trimws(c_raw$`2020PopulrVoteParty`) == "R")
)

# Urbanicity from NCES school locale codes: share of a county's public schools
# located in a City (11-13) or Suburb (21-23) locale.
s <- read_csv(file.path(DATA, "QOL(Public School Level).csv"),
              col_types = cols(.default = col_character()),
              col_select = c(FIPS, ULOCALE, LSTATE, `2016 Crime Rate`, `AQI%Good`),
              locale = locale(encoding = "latin1"), name_repair = "unique_quiet")
s <- s %>% mutate(fips = sprintf("%05d", as.integer(FIPS)),
                  urban = as.numeric(substr(ULOCALE, 1, 1) %in% c("1", "2")))
loc <- s %>% group_by(fips) %>%
  summarize(urban_share = mean(urban), n_schools = n(), .groups = "drop")
county <- county %>% left_join(loc, by = "fips") %>%
  mutate(log_pop = log(pop), log_income = log(income), col_10k = col / 10000)

# Census regions and divisions, used to check whether air quality only proxies for
# broad geography (e.g., the dry, sunny West).
divisions <- list(
  "New England" = c("CT", "ME", "MA", "NH", "RI", "VT"),
  "Middle Atlantic" = c("NJ", "NY", "PA"),
  "East North Central" = c("IL", "IN", "MI", "OH", "WI"),
  "West North Central" = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  "South Atlantic" = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV"),
  "East South Central" = c("AL", "KY", "MS", "TN"),
  "West South Central" = c("AR", "LA", "OK", "TX"),
  "Mountain" = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
  "Pacific" = c("AK", "CA", "HI", "OR", "WA"))
div_region <- c("New England" = "Northeast", "Middle Atlantic" = "Northeast",
                "East North Central" = "Midwest", "West North Central" = "Midwest",
                "South Atlantic" = "South", "East South Central" = "South",
                "West South Central" = "South", "Mountain" = "West", "Pacific" = "West")
state_div <- setNames(rep(names(divisions), lengths(divisions)), unlist(divisions))
county <- county %>% mutate(division = unname(state_div[state]),
                            region = unname(div_region[division]))
stopifnot(!anyNA(county$region))

# ---------------------------------------------------------------------------
# Structure checks reported in the paper
# ---------------------------------------------------------------------------
facts <- list(
  n_counties_file = nrow(county),
  n_states = n_distinct(county$state),
  aqi_unique_values = n_distinct(county$aqi_good, na.rm = TRUE),
  aqi_max_unique_within_state = max(tapply(county$aqi_good, county$state,
                                           function(x) n_distinct(x, na.rm = TRUE))),
  crime_zero_counties = sum(trimws(c_raw$`2016 Crime Rate`) == "0"),
  n_school_rows = nrow(s),
  states_missing_from_school_file = setdiff(unique(county$state), unique(s$LSTATE))
)

# ---------------------------------------------------------------------------
# Model 0: replicate the original school-level regression (n ~ 87k)
# ---------------------------------------------------------------------------
s0 <- s %>% mutate(crime_rate = crime_per_1k(`2016 Crime Rate`) / 1000,  # original scale
                   aqi_good = num(`AQI%Good`)) %>%
  filter(!is.na(crime_rate), !is.na(aqi_good))
m0  <- feols(crime_rate ~ aqi_good, data = s0, vcov = "iid")
m0c <- feols(crime_rate ~ aqi_good, data = s0, cluster = ~LSTATE)
facts$orig <- list(
  n = nobs(m0), b = unname(coef(m0)["aqi_good"]),
  se_iid = unname(se(m0)["aqi_good"]), se_cluster = unname(se(m0c)["aqi_good"]),
  t_iid = unname(tstat(m0)["aqi_good"]), t_cluster = unname(tstat(m0c)["aqi_good"]),
  p_cluster = unname(pvalue(m0c)["aqi_good"]), r2 = unname(r2(m0, "r2")),
  se_ratio = unname(se(m0c)["aqi_good"] / se(m0)["aqi_good"])
)

# ---------------------------------------------------------------------------
# County models (OLS, standard errors clustered by state)
# ---------------------------------------------------------------------------
need <- c("crime", "aqi_good", "log_pop", "urban_share", "unemp", "log_income", "c2i", "col_10k")
d <- county %>% filter(if_all(all_of(need), ~ !is.na(.x)))
facts$n_analysis <- nrow(d)
facts$n_states_analysis <- n_distinct(d$state)
facts$singleton_states <- names(which(table(d$state) == 1))

controls <- "log_pop + urban_share + unemp + log_income + c2i"
fit <- function(f, data, fe = FALSE, ...) {
  f <- if (fe) paste(f, "| state") else f
  feols(as.formula(f), data = data, cluster = ~state, ...)
}

models <- list(
  M1 = fit("crime ~ aqi_good", d),
  M2 = fit("crime ~ col_10k", d),
  M3 = fit("crime ~ aqi_good + col_10k", d),
  M4 = fit(paste("crime ~ aqi_good +", controls), d),
  M5 = fit(paste("crime ~", controls), d, fe = TRUE)
)

# State-level regression (the level at which air quality actually varies)
st <- d %>% group_by(state) %>%
  summarize(crime = weighted.mean(crime, pop), aqi_good = first(aqi_good),
            urban_share = weighted.mean(urban_share, pop),
            log_income = weighted.mean(log_income, pop), pop = sum(pop),
            region = first(region), .groups = "drop")
ms1 <- feols(crime ~ aqi_good, data = st, vcov = "hetero")
ms2 <- feols(crime ~ aqi_good + urban_share + log_income, data = st, vcov = "hetero")
ms3 <- feols(crime ~ aqi_good | region, data = st, vcov = "hetero")

# Standardized coefficients (1 SD change in X -> SD change in crime)
labels <- c(aqi_good = "Air quality (% good days)", log_pop = "log population",
            urban_share = "Urban share of schools", unemp = "Unemployment rate",
            log_income = "log median income", c2i = "Cost-to-income ratio")
sd_y <- sd(d$crime)
std <- bind_rows(lapply(names(labels), function(v) {
  bind_rows(lapply(c("M4", "M5"), function(mk) {
    m <- models[[mk]]
    if (!v %in% names(coef(m))) return(NULL)
    k <- sd(d[[v]]) / sd_y
    ci <- confint(m)[v, ]
    tibble(var = v, label = labels[[v]], model = mk, beta = coef(m)[[v]] * k,
           lo = ci[[1]] * k, hi = ci[[2]] * k, p = pvalue(m)[[v]])
  }))
}))
write_csv(std, file.path(OUT, "standardized_coefs.csv"))

# ---------------------------------------------------------------------------
# Robustness: one change at a time to Model 4
# ---------------------------------------------------------------------------
f4 <- paste("crime ~ aqi_good +", controls)
d0 <- county %>% filter(if_all(all_of(c("crime0", "aqi_good", "log_pop", "urban_share",
                                        "unemp", "log_income", "c2i")), ~ !is.na(.x))) %>%
  mutate(crime = crime0)
# Illinois has no rows in the school file, so it has no urban share. Dropping that
# control brings Illinois back into the sample.
dil <- county %>% filter(if_all(all_of(c("crime", "aqi_good", "log_pop", "unemp",
                                         "log_income", "c2i")), ~ !is.na(.x)))
rob <- list(
  keep_zero        = fit(f4, d0),
  pop_weighted     = fit(f4, d, weights = ~pop),
  water            = fit(paste(f4, "+ water_vpv"), filter(d, !is.na(water_vpv))),
  party            = fit(paste(f4, "+ party_r"), d),
  col_instead_c2i  = fit("crime ~ aqi_good + log_pop + urban_share + unemp + col_10k", d),
  no_urban_with_il = fit("crime ~ aqi_good + log_pop + unemp + log_income + c2i", dil),
  region_fe        = fit(paste(f4, "| region"), d),
  division_fe      = fit(paste(f4, "| division"), d)
)

# How much of the air-quality variation is left within regions and divisions
facts$aqi_sd_states <- sd(st$aqi_good)
facts$aqi_sd_within_region <- sd(st$aqi_good - ave(st$aqi_good, st$region))
facts$aqi_r2_on_region <- summary(lm(aqi_good ~ region, data = st))$r.squared

# ---------------------------------------------------------------------------
# LaTeX tables
# ---------------------------------------------------------------------------
f3 <- function(x, k = 3) formatC(x, format = "f", digits = k)
pfmt <- function(p) if (p < 0.001) "<0.001" else f3(p)
stars <- function(p) if (p < 0.01) "^{***}" else if (p < 0.05) "^{**}" else if (p < 0.1) "^{*}" else ""
cell <- function(m, v) {
  if (!v %in% names(coef(m))) return(c("", ""))
  c(sprintf("$%s%s$", f3(coef(m)[[v]]), stars(pvalue(m)[[v]])), sprintf("$(%s)$", f3(se(m)[[v]])))
}

rows <- c(aqi_good = "Air quality (\\% good days)", col_10k = "Cost of living (\\$10k)",
          log_pop = "log population", urban_share = "Urban share of schools",
          unemp = "Unemployment rate (\\%)", log_income = "log median income",
          c2i = "Cost-to-income ratio (\\%)")
ord <- names(models)
L <- c(paste0("\\begin{tabular}{@{}l", strrep("c", length(ord)), "@{}}"), "\\toprule",
       paste0(" & ", paste0("(", seq_along(ord), ")", collapse = " & "), " \\\\"), "\\midrule")
for (v in names(rows)) {
  a <- lapply(models, cell, v = v)
  L <- c(L, paste0(rows[[v]], " & ", paste(sapply(a, `[`, 1), collapse = " & "), " \\\\"),
         paste0(" & ", paste(sapply(a, `[`, 2), collapse = " & "), " \\\\[2pt]"))
}
L <- c(L, "\\midrule",
       paste0("State fixed effects & ", paste(ifelse(ord == "M5", "Yes", "No"), collapse = " & "), " \\\\"),
       paste0("Counties & ", paste(sapply(models, function(m) format(nobs(m), big.mark = ",")), collapse = " & "), " \\\\"),
       paste0("States (clusters) & ", paste(sapply(models, function(m) n_distinct(d$state[obs(m)])), collapse = " & "), " \\\\"),
       paste0("$R^2$ & ", paste(sapply(models, function(m) f3(r2(m, "r2"))), collapse = " & "), " \\\\"),
       "\\bottomrule", "\\end{tabular}")
writeLines(L, file.path(OUT, "table_main.tex"))

# Original vs corrected inference for the air-quality slope
o <- facts$orig
m1 <- models$M1
row_inf <- function(lab, b, s, t, p) sprintf("%s & $%s$ & $%s$ & $%s$ & $%s$ \\\\", lab, f3(b), f3(s, 4), f3(t, 2), p)
L <- c("\\begin{tabular}{@{}lcccc@{}}", "\\toprule",
       "Specification & Slope & SE & $t$ & $p$ \\\\", "\\midrule",
       row_inf("School rows, conventional SE (original)", o$b * 1000, o$se_iid * 1000, o$t_iid, "<10^{-16}"),
       row_inf("School rows, SE clustered by state", o$b * 1000, o$se_cluster * 1000, o$t_cluster, pfmt(o$p_cluster)),
       row_inf("Counties, SE clustered by state", coef(m1)[["aqi_good"]], se(m1)[["aqi_good"]],
               tstat(m1)[["aqi_good"]], pfmt(pvalue(m1)[["aqi_good"]])),
       row_inf(sprintf("States (n = %d), robust SE", nobs(ms1)), coef(ms1)[["aqi_good"]], se(ms1)[["aqi_good"]],
               tstat(ms1)[["aqi_good"]], pfmt(pvalue(ms1)[["aqi_good"]])),
       "\\bottomrule", "\\end{tabular}")
writeLines(L, file.path(OUT, "table_inference.tex"))

# Robustness table: air-quality coefficient and its p-value
rob_lab <- c(keep_zero = "Keep counties reporting zero crime",
             pop_weighted = "Weight counties by population",
             water = "Add drinking-water violations",
             party = "Add state 2020 vote winner (R = 1)",
             col_instead_c2i = "Cost of living instead of cost-to-income",
             no_urban_with_il = "Drop urban share (adds Illinois)",
             region_fe = "Add Census region fixed effects (4)",
             division_fe = "Add Census division fixed effects (9)")
L <- c("\\begin{tabular}{@{}lccc@{}}", "\\toprule",
       "Variation of Model (4) & Air quality coef. (SE) & $p$ & Counties \\\\", "\\midrule")
for (k in names(rob_lab)) {
  m <- rob[[k]]
  L <- c(L, sprintf("%s & $%s$ $(%s)$ & $%s$ & %s \\\\", rob_lab[[k]], f3(coef(m)[["aqi_good"]]),
                    f3(se(m)[["aqi_good"]]), pfmt(pvalue(m)[["aqi_good"]]), format(nobs(m), big.mark = ",")))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(L, file.path(OUT, "table_robust.tex"))

# Descriptive statistics
desc_vars <- c(crime = "Crime per 1,000 residents (2016)", aqi_good = "Air quality (\\% good days)",
               col = "Cost of living (\\$)", income = "Median income (\\$)",
               c2i = "Cost-to-income ratio (\\%)", unemp = "Unemployment rate (\\%)",
               pop = "Population (2022)", urban_share = "Urban share of schools",
               water_vpv = "Drinking-water violations")
L <- c("\\begin{tabular}{@{}lrrrrr@{}}", "\\toprule",
       "Variable & N & Mean & SD & Min & Max \\\\", "\\midrule")
for (v in names(desc_vars)) {
  x <- d[[v]][!is.na(d[[v]])]
  k <- if (max(abs(x)) > 1000) 0 else 2
  fm <- function(z) formatC(z, format = "f", digits = k, big.mark = ",")
  L <- c(L, sprintf("%s & %s & %s & %s & %s & %s \\\\", desc_vars[[v]], format(length(x), big.mark = ","),
                    fm(mean(x)), fm(sd(x)), fm(min(x)), fm(max(x))))
}
L <- c(L, "\\bottomrule", "\\end{tabular}")
writeLines(L, file.path(OUT, "table_desc.tex"))

# ---------------------------------------------------------------------------
# Figures
# ---------------------------------------------------------------------------
theme_set(theme_classic(base_size = 10))

# Fig 1: state-level scatter, crime vs air quality
g1 <- ggplot(st, aes(aqi_good, crime)) +
  geom_point(aes(size = pop), alpha = 0.5, color = "#3b6ea5", show.legend = FALSE) +
  geom_text(aes(label = state), size = 2) +
  geom_abline(intercept = coef(ms1)[[1]], slope = coef(ms1)[[2]], color = "#c0504d", linewidth = 0.8) +
  scale_size_area(max_size = 8) +
  labs(x = "Air quality: % of days with a 'good' AQI (state)",
       y = "Crime per 1,000 residents (pop.-weighted)",
       title = sprintf("States (n = %d): slope %s, p = %s", nobs(ms1),
                       f3(coef(ms1)[[2]]), f3(pvalue(ms1)[[2]]))) +
  theme(plot.title = element_text(size = 10, hjust = 0.5))
ggsave(file.path(FIG, "fig_state_aqi.png"), g1, width = 6, height = 4, dpi = 300)

# Fig 2: county binned scatter, crime vs cost of living (20 equal-count bins)
bins <- d %>% mutate(bin = ntile(col, 20)) %>% group_by(bin) %>%
  summarize(col = mean(col), se = sd(crime) / sqrt(n()), crime = mean(crime), .groups = "drop")
g2 <- ggplot() +
  geom_point(data = d, aes(col, crime), size = 0.4, alpha = 0.15, color = "grey40") +
  geom_errorbar(data = bins, aes(col, ymin = crime - 1.96 * se, ymax = crime + 1.96 * se),
                width = 1000, color = "#3b6ea5") +
  geom_point(data = bins, aes(col, crime), color = "#3b6ea5", size = 1.8) +
  scale_x_continuous(labels = function(v) paste0("$", v / 1000, "k")) +
  coord_cartesian(ylim = c(0, quantile(d$crime, 0.995))) +
  labs(x = "Cost of living for a family ($)", y = "Crime per 1,000 residents",
       caption = "Grey: counties. Blue: mean of 20 equal-size bins with 95% CI.")
ggsave(file.path(FIG, "fig_county_col.png"), g2, width = 6, height = 4, dpi = 300)

# Fig 3: standardized coefficients
std_plot <- std %>% mutate(label = factor(label, levels = rev(unname(labels))),
                           model = recode(model, M4 = "Model 4 (no state FE)", M5 = "Model 5 (state FE)"))
g3 <- ggplot(std_plot, aes(beta, label, color = model)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.3) +
  scale_color_manual(values = c("#3b6ea5", "#c0504d")) +
  labs(x = "Standardized coefficient\n(SD change in crime per 1 SD change in the variable)",
       y = NULL, color = NULL) +
  theme(legend.position = "bottom")
ggsave(file.path(FIG, "fig_std_coefs.png"), g3, width = 6.5, height = 4, dpi = 300)

# ---------------------------------------------------------------------------
# Numbers used in the text
# ---------------------------------------------------------------------------
pack <- function(m, v) {
  ci <- confint(m)[v, ]
  list(b = coef(m)[[v]], se = se(m)[[v]], p = pvalue(m)[[v]], lo = ci[[1]], hi = ci[[2]])
}
facts$mean_crime <- mean(d$crime)
facts$sd_crime <- sd(d$crime)
facts$sd_aqi <- sd(d$aqi_good)
facts$corr_col_income <- cor(d$col, d$income)
for (k in names(models)) {
  m <- models[[k]]
  facts[[k]] <- c(list(n = nobs(m), r2 = unname(r2(m, "r2"))),
                  lapply(setNames(nm = intersect(names(rows), names(coef(m)))), function(v) pack(m, v)))
}
facts$state_M1 <- c(list(n = nobs(ms1), r2 = unname(r2(ms1, "r2"))), pack(ms1, "aqi_good"))
facts$state_M2 <- c(list(n = nobs(ms2), r2 = unname(r2(ms2, "r2"))), pack(ms2, "aqi_good"))
facts$state_M3_region <- c(list(n = nobs(ms3), r2 = unname(r2(ms3, "r2"))), pack(ms3, "aqi_good"))
facts$robust <- lapply(rob, function(m) c(list(n = nobs(m)), pack(m, "aqi_good")))
facts$robust$water_coef <- pack(rob$water, "water_vpv")
facts$std <- std
write_json(facts, file.path(OUT, "facts.json"), auto_unbox = TRUE, digits = NA, pretty = TRUE)

etable(models, cluster = ~state)
print(summary(ms1)); print(summary(ms2))
print(std, n = Inf)
str(facts[c("n_analysis", "n_states_analysis", "states_missing_from_school_file", "orig")])
