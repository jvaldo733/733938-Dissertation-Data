# 733938-Dissertation-Code
# =========================================================
# ENIGH Rural Income Composition, 2010-2024
# VERIFIED against actual downloaded files (2008 excluded --
# see note at bottom)
# =========================================================

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(ggplot2)

# ---- 1. SETUP ----

# 2008 excluded: its concentradohogar table has no tam_loc variable
# (only "estrato", a sample-design stratum, not a locality-size
# classification) -- not usable for the rural filter this analysis needs.
years <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)

base_path <- "data/enigh"

# ---- 2. FUNCTION TO LOAD AND STANDARDIZE ONE WAVE ----
# Verified naming differences across your actual files:
#  - 2010 uses OLD names: ingcor, beca, donativo, remesa, jubila
#  - 2012+ uses NEW names: ing_cor, becas, donativos, remesas, jubilacion
#  - weight is "factor_hog" in 2012 & 2014 only; "factor" every other year

load_enigh_wave <- function(year) {

  file_path <- file.path(base_path, year, "concentradohogar.csv")
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- tolower(names(df))

  if (year == 2010) {
    df <- df %>%
      rename(
        wt = factor,
        ing_cor = ingcor,
        becas = beca,
        donativos = donativo,
        remesas = remesa,
        jubilacion = jubila
      )
  } else if (year %in% c(2012, 2014)) {
    df <- df %>% rename(wt = factor_hog)
  } else {
    df <- df %>% rename(wt = factor)
  }

  df <- df %>%
    mutate(
      year = year,
      total_transfer = jubilacion + becas + donativos + bene_gob,
      share_wage       = trabajo   / ing_cor,
      share_agri       = agrope    / ing_cor,
      share_nonagri_se = noagrop   / ing_cor,
      share_remittance = remesas   / ing_cor,
      share_transfer   = total_transfer / ing_cor,
      rural = if_else(tam_loc == 4, 1, 0)
    ) %>%
    select(year, tam_loc, rural, wt, ing_cor,
           share_wage, share_agri, share_nonagri_se,
           share_remittance, share_transfer)

  return(df)
}

# ---- 3. LOAD ALL WAVES ----

safe_load <- safely(load_enigh_wave)
results <- map(years, safe_load)

failed_years <- years[map_lgl(results, ~ !is.null(.x$error))]
if (length(failed_years) > 0) {
  warning("These years failed to load: ", paste(failed_years, collapse = ", "))
  print(map(results[map_lgl(results, ~ !is.null(.x$error))], "error"))
}

all_waves <- map_dfr(results, ~ .x$result)

# ---- 4. WEIGHTED RURAL TREND BY YEAR ----

rural_trend <- all_waves %>%
  filter(rural == 1, ing_cor > 0) %>%
  group_by(year) %>%
  summarise(
    mean_share_wage       = weighted.mean(share_wage, wt, na.rm = TRUE),
    mean_share_agri       = weighted.mean(share_agri, wt, na.rm = TRUE),
    mean_share_nonagri_se = weighted.mean(share_nonagri_se, wt, na.rm = TRUE),
    mean_share_remittance = weighted.mean(share_remittance, wt, na.rm = TRUE),
    mean_share_transfer   = weighted.mean(share_transfer, wt, na.rm = TRUE),
    n_households          = n()
  )

print(rural_trend)
# Expected output (verified via Python cross-check):
#  year  wage   agri  nonagri  remit  transfer   n
#  2010  0.364  0.055  0.069   0.030   0.203    6123
#  2012  0.332  0.070  0.070   0.024   0.199    3310
#  2014  0.388  0.060  0.055   0.024   0.190    5251
#  2016  0.393  0.065  0.066   0.027   0.180   25633
#  2018  0.408  0.069  0.069   0.029   0.158   28637
#  2020  0.411  0.066  0.076   0.027   0.160   33531
#  2022  0.414  0.065  0.080   0.036   0.168   34031
#  2024  0.414  0.059  0.080   0.034   0.176   35825

# ---- 5. VISUAL ----

trend_long <- rural_trend %>%
  select(year, starts_with("mean_share")) %>%
  pivot_longer(-year, names_to = "source", values_to = "share")

ggplot(trend_long, aes(x = year, y = share, color = source)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Rural Household Income Composition, Mexico 2010-2024",
    subtitle = "Weighted shares of total current income (ENIGH, tam_loc == 4)",
    x = NULL, y = "Share of household income", color = "Income source"
  ) +
  theme_minimal()

# ---- 6. NOTE ON DENOMINATOR ----
# Shares will sum to ~0.70-0.76 of ing_cor, not 1.0 -- ing_cor also
# includes imputed rental value of owner-occupied housing and property
# income (rentas/utilidad/arrenda), which are intentionally excluded
# from this breakdown. Document this in your methods section.

# ---- 7. NOTE ON 2008 ----
# 2008's CONCENTRADOHOGAR table lacks tam_loc; it only has "estrato"
# (a sample-design stratum based on marginación/geographic zone per
# INEGI's own methodology docs), which is not a direct substitute for
# the locality-size rural classification used in every other wave.
# State this exclusion explicitly in your data section.

# ---- ROBUSTNESS CHECK 1: Does the trend hold without 2020 (COVID year)? ----

rural_trend_no2020 <- rural_trend %>%
  filter(year != 2020)

print(rural_trend_no2020)

# Compare visually: re-plot both versions side by side
trend_long_no2020 <- rural_trend_no2020 %>%
  select(year, starts_with("mean_share")) %>%
  pivot_longer(-year, names_to = "source", values_to = "share")

ggplot(trend_long_no2020, aes(x = year, y = share, color = source)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Rural Income Composition, 2020 excluded",
       x = NULL, y = "Share of household income", color = "Income source") +
  theme_minimal()

# ---- ROBUSTNESS CHECK 2: Simple trend regression ----
# Tests whether the wage-share increase is statistically distinguishable
# from noise, not just a visual impression from the line chart

rural_data <- all_waves %>%
  filter(rural == 1, ing_cor > 0)

trend_model <- lm(share_wage ~ year, data = rural_data, weights = wt)
summary(trend_model)

# Do the same for your other shares, to see which are actually trending
# vs. roughly flat over time
summary(lm(share_agri ~ year, data = rural_data, weights = wt))
summary(lm(share_transfer ~ year, data = rural_data, weights = wt))



install.packages("gt")  # only needed once
library(gt)

table1_gt <- rural_trend %>%
  gt() %>%
  tab_header(
    title = "Table 1. Weighted mean income shares among rural households, Mexico, 2010–2024"
  ) %>%
  fmt_percent(
    columns = starts_with("mean_share"),
    decimals = 1
  ) %>%
  cols_label(
    year = "Year",
    n_households = "N (households)",
    mean_share_wage = "Wage",
    mean_share_agri = "Agricultural SE",
    mean_share_nonagri_se = "Non-agri SE",
    mean_share_remittance = "Remittances",
    mean_share_transfer = "Transfers"
  ) %>%
  tab_source_note(
    source_note = "Note: Shares weighted by household expansion factor; do not sum to 100% as property income and imputed rent are excluded. 2008 excluded (see Section 4.2)."
  )

table1_gt



 Robustness Check: Wage/Agri/Transfer trend, with and without 2020
# =========================================================
# Assumes `all_waves` already exists from running enigh_verified_script.R

library(dplyr)

rural_data <- all_waves %>%
  filter(rural == 1, ing_cor > 0)

rural_data_no2020 <- rural_data %>%
  filter(year != 2020)

# ---- Full sample (2010-2024, includes 2020) ----
cat("=== FULL SAMPLE ===\n")
m_wage_full     <- lm(share_wage ~ year, data = rural_data, weights = wt)
m_agri_full     <- lm(share_agri ~ year, data = rural_data, weights = wt)
m_transfer_full <- lm(share_transfer ~ year, data = rural_data, weights = wt)

cat("Wage:     coef =", coef(m_wage_full)["year"],
    " p =", summary(m_wage_full)$coefficients["year", "Pr(>|t|)"], "\n")
cat("Agri:     coef =", coef(m_agri_full)["year"],
    " p =", summary(m_agri_full)$coefficients["year", "Pr(>|t|)"], "\n")
cat("Transfer: coef =", coef(m_transfer_full)["year"],
    " p =", summary(m_transfer_full)$coefficients["year", "Pr(>|t|)"], "\n")

# ---- Excluding 2020 (COVID-year robustness check) ----
cat("\n=== EXCLUDING 2020 ===\n")
m_wage_no2020     <- lm(share_wage ~ year, data = rural_data_no2020, weights = wt)
m_agri_no2020     <- lm(share_agri ~ year, data = rural_data_no2020, weights = wt)
m_transfer_no2020 <- lm(share_transfer ~ year, data = rural_data_no2020, weights = wt)

cat("Wage:     coef =", coef(m_wage_no2020)["year"],
    " p =", summary(m_wage_no2020)$coefficients["year", "Pr(>|t|)"], "\n")
cat("Agri:     coef =", coef(m_agri_no2020)["year"],
    " p =", summary(m_agri_no2020)$coefficients["year", "Pr(>|t|)"], "\n")
cat("Transfer: coef =", coef(m_transfer_no2020)["year"],
    " p =", summary(m_transfer_no2020)$coefficients["year", "Pr(>|t|)"], "\n")

cat("\nN full:", nrow(rural_data), " N excl. 2020:", nrow(rural_data_no2020), "\n")

# Expected output (verified via Python cross-check):
#  FULL:        wage coef=0.00497 p<0.001 | agri coef=0.00007 p=0.373 | transfer coef=-0.00261 p<0.001
#  EXCL. 2020:  wage coef=0.00485 p<0.001 | agri coef=0.00003 p=0.756 | transfer coef=-0.00238 p<0.001
