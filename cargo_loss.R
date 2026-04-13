library(tidyverse)
library(stringr)
library(scales)
library(forcats)

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


freq_path <- "datasets/srcsc-2026-claims-cargo-freq.csv"
freq_raw <- read_csv(freq_path, show_col_types = FALSE)

freq <- freq_raw %>%
  mutate(
    policy_id = str_trim(str_replace(as.character(policy_id), "_.*$", "")),
    shipment_id = str_trim(str_replace(as.character(shipment_id), "_.*$", "")),
    cargo_type = str_to_lower(str_trim(str_replace(as.character(cargo_type), "_.*$", ""))),
    container_type = str_trim(str_replace(as.character(container_type), "_.*$", "")),
    
    claim_count = as.integer(claim_count),
    
    cargo_type = as.factor(cargo_type),
    container_type = as.factor(container_type),
    route_risk = as.factor(route_risk)
  )

glimpse(freq)

# Missingness per variable (raw number)
missing_counts <- freq %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

missing_counts

# Missingness per variable (as a percentage of the total data)
missing_perc <- freq %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))

missing_perc


# Missingness & entries that are outside of the data dictionary range
range_flags <- freq %>%
  summarise(
    # exposure & indices should be in [0,1]
    n_bad_exposure = sum(is.na(exposure) | exposure < 0 | exposure > 1),
    n_bad_solar    = sum(is.na(solar_radiation) | solar_radiation < 0 | solar_radiation > 1),
    n_bad_debris   = sum(is.na(debris_density) | debris_density < 0 | debris_density > 1),
    
    # route risk should be 1..5
    n_bad_route_risk = sum(is.na(route_risk) | !(as.character(route_risk) %in% c("1","2","3","4","5"))),
    
    # claim_count should be 0..5
    n_bad_claim_count = sum(is.na(claim_count) | claim_count < 0 | claim_count > 5),
    
    # numeric predictor ranges
    n_bad_distance = sum(is.na(distance) | distance < 1 | distance > 100),
    n_bad_transit  = sum(is.na(transit_duration) | transit_duration < 1 | transit_duration > 60),
    n_bad_pilot    = sum(is.na(pilot_experience) | pilot_experience < 1 | pilot_experience > 30),
    n_bad_vessel   = sum(is.na(vessel_age) | vessel_age < 1 | vessel_age > 50),
    n_bad_weight      = sum(is.na(weight) | weight < 1500 | weight > 250000),
    
    
    n_bad_cargo_value = sum(is.na(cargo_value) | cargo_value < 0)
    
  )

range_flags <- pivot_longer(range_flags, everything(), names_to = "variable", values_to = "Number of bad")
range_flags


freq_flagged <- freq %>%
  mutate(
    bad_row =
      is.na(exposure) | exposure < 0 | exposure > 1 |
      is.na(solar_radiation) | solar_radiation < 0 | solar_radiation > 1 |
      is.na(debris_density) | debris_density < 0 | debris_density > 1 |
      is.na(route_risk) | !(as.character(route_risk) %in% c("1","2","3","4","5")) |
      is.na(claim_count) | claim_count < 0 | claim_count > 5 |
      is.na(distance) | distance < 1 | distance > 100 |
      is.na(transit_duration) | transit_duration < 1 | transit_duration > 60 |
      is.na(pilot_experience) | pilot_experience < 1 | pilot_experience > 30 |
      is.na(vessel_age) | vessel_age < 1 | vessel_age > 50 |
      is.na(cargo_value) | cargo_value <= 0 |
      is.na(weight) | weight < 1500 | weight > 250000
  )

freq_flagged %>% summarise(
  n_total = n(),
  n_bad = sum(bad_row),
  pct_bad = mean(bad_row)
)


# Final cleaning
# Remove bad rows
freq_clean <- freq_flagged %>%
  filter(!bad_row) %>%
  dplyr::select(-bad_row) %>%
  mutate(
    cargo_type = fct_na_value_to_level(cargo_type, level = "unknown"),
    container_type = fct_na_value_to_level(container_type, level = "unknown")
  )


glimpse(freq_clean)

# ---- EDA of Freq Data | Distribution of claim count ----

table(freq_clean$claim_count)

freq_clean %>%
  count(claim_count) %>%
  mutate(prop = n / sum(n))

# Mean, variance, zero proportion, crude annualized claim rate
freq_clean %>%
  summarise(
    mean_claim_count = mean(claim_count),
    var_claim_count = var(claim_count),
    zero_prop = mean(claim_count == 0),
    crude_claim_rate_per_exposure = sum(claim_count) / sum(exposure)
  )

# Plot claim count distribution
ggplot(freq_clean, aes(x = factor(claim_count))) +
  geom_bar() +
  labs(
    title = "Distribution of Claim Count",
    x = "Claim Count",
    y = "Number of Records"
  )

summary(freq_clean$exposure)


freq_clean %>%
  count(cargo_type, sort = TRUE)

freq_clean %>%
  count(container_type, sort = TRUE)

freq_clean %>%
  count(route_risk, sort = TRUE)

# Mean claim count by categorical predictors
freq_clean %>%
  group_by(route_risk) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure)
  ) %>%
  arrange(route_risk)

freq_clean %>%
  group_by(cargo_type) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure)
  ) %>%
  arrange(desc(avg_claims))

freq_clean %>%
  group_by(container_type) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure)
  ) %>%
  arrange(desc(avg_claims))


# Cargo value exploration
summary(freq_clean$cargo_value)
quantile(freq_clean$cargo_value, probs = c(0.90, 0.95, 0.99, 0.999, 0.99999), na.rm = TRUE)

freq_clean %>%
  summarise(
    n_above_680m = sum(cargo_value > 680000000, na.rm = TRUE),
    pct_above_680m = mean(cargo_value > 680000000, na.rm = TRUE),
    n_above_2b = sum(cargo_value > 2000000000, na.rm = TRUE),
    n_above_8b = sum(cargo_value > 8000000000, na.rm = TRUE)
  )

freq_clean <- freq_clean %>%
  mutate(
    precious_flag = case_when(
      cargo_type %in% c("gold", "platinum") ~ "precious",
      TRUE ~ "non_precious"
    ),
    precious_flag = as.factor(precious_flag)
    
  )

# cargo value from precious and non-precious cargo types
freq_clean %>%
  group_by(precious_flag) %>%
  summarise(
    shipments = n(),
    total_value = sum(cargo_value),
    pct_total_value = total_value / sum(freq_clean$cargo_value),
    .groups = "drop"
  )

# cargo weight from precious and non-precious cargo types
freq_clean %>%
  group_by(precious_flag) %>%
  summarise(
    shipments = n(),
    total_weight = sum(weight),
    pct_total_weight = total_weight / sum(freq_clean$weight),
    .groups = "drop"
  )

freq_clean %>%
  group_by(cargo_type) %>%
  summarise(
    avg_value = mean(cargo_value),
    avg_weight = mean(weight),
    avg_value_per_kg = mean(cargo_value / weight),
    median_value_per_kg = median(cargo_value / weight),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_value_per_kg))

continuous_vars <- freq_clean %>%
  dplyr::select(distance, transit_duration, pilot_experience, vessel_age,
                solar_radiation, debris_density, weight, cargo_value, exposure)

summary(continuous_vars)

# Correlation matrix for main frequency predictors
cor(
  freq_clean %>%
    dplyr::select(distance, transit_duration, pilot_experience,
                  vessel_age, solar_radiation, debris_density),
  use = "complete.obs"
)


# Relationship between continuous predictors and claim frequency
# Helper function to create binned summaries
make_binned_summary <- function(data, var, n_bins = 10) {
  var_name <- rlang::ensym(var)
  
  data %>%
    mutate(bin = ntile(!!var_name, n_bins)) %>%
    group_by(bin) %>%
    summarise(
      min_value = min(!!var_name, na.rm = TRUE),
      max_value = max(!!var_name, na.rm = TRUE),
      avg_value = mean(!!var_name, na.rm = TRUE),
      n = n(),
      avg_claims = mean(claim_count),
      claim_rate_per_exposure = sum(claim_count) / sum(exposure),
      zero_rate = mean(claim_count == 0),
      .groups = "drop"
    )
}

# Distance
distance_bins <- make_binned_summary(freq_clean, distance, n_bins = 10)
distance_bins

# Transit duration
transit_bins <- make_binned_summary(freq_clean, transit_duration, n_bins = 10)
transit_bins

# Pilot experience
pilot_bins <- make_binned_summary(freq_clean, pilot_experience, n_bins = 10)
pilot_bins

# Vessel age
vessel_bins <- make_binned_summary(freq_clean, vessel_age, n_bins = 10)
vessel_bins

# Solar radiation
solar_bins <- make_binned_summary(freq_clean, solar_radiation, n_bins = 10)
solar_bins

# Debris density
debris_bins <- make_binned_summary(freq_clean, debris_density, n_bins = 10)
debris_bins


# A few important pairwise checks
# Debris density by route risk
freq_clean %>%
  group_by(route_risk) %>%
  summarise(
    mean_debris = mean(debris_density),
    mean_solar = mean(solar_radiation),
    avg_claims = mean(claim_count)
  ) %>%
  arrange(route_risk)

ggplot(freq_clean, aes(x = route_risk, y = debris_density)) +
  geom_boxplot() +
  labs(
    title = "Debris Density by Route Risk",
    x = "Route Risk",
    y = "Debris Density"
  )

ggplot(freq_clean, aes(x = route_risk, y = solar_radiation)) +
  geom_boxplot() +
  labs(
    title = "Solar Radiation by Route Risk",
    x = "Route Risk",
    y = "Solar Radiation"
  )


# Frequency model readiness checks
# Overdispersion diagnostic inputs
freq_clean %>%
  summarise(
    mean_claim_count = mean(claim_count),
    var_claim_count = var(claim_count),
    variance_to_mean_ratio = var(claim_count) / mean(claim_count)
  )

# Check whether claim count is associated with exposure length
freq_clean %>%
  mutate(exposure_bin = ntile(exposure, 10)) %>%
  group_by(exposure_bin) %>%
  summarise(
    avg_exposure = mean(exposure),
    avg_claims = mean(claim_count),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure),
    .groups = "drop"
  )


# Grouped summaries - contains all the main EDA

route_summary <- freq_clean %>%
  group_by(route_risk) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure),
    avg_distance = mean(distance),
    avg_transit_duration = mean(transit_duration),
    avg_solar_radiation = mean(solar_radiation),
    avg_debris_density = mean(debris_density),
    .groups = "drop"
  )

route_summary

cargo_summary <- freq_clean %>%
  group_by(cargo_type) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure),
    avg_cargo_value = mean(cargo_value),
    avg_weight = mean(weight),
    .groups = "drop"
  )

cargo_summary

container_summary <- freq_clean %>%
  group_by(container_type) %>%
  summarise(
    n = n(),
    avg_claims = mean(claim_count),
    zero_rate = mean(claim_count == 0),
    claim_rate_per_exposure = sum(claim_count) / sum(exposure),
    avg_vessel_age = mean(vessel_age),
    .groups = "drop"
  )

container_summary


freq_clean %>%
  dplyr::select(distance, transit_duration, pilot_experience,
                vessel_age, solar_radiation, debris_density) %>%
  cor(use = "complete.obs")

freq_clean %>%
  group_by(route_risk) %>%
  summarise(
    mean_debris = mean(debris_density),
    mean_solar = mean(solar_radiation),
    avg_claims = mean(claim_count)
  ) %>%
  arrange(route_risk)


# ---- Frequency Modelling ----

library(MASS)

# Baseline frequency summary
freq_clean %>%
  summarise(
    total_claims = sum(claim_count),
    total_exposure = sum(exposure),
    baseline_frequency_per_exposure = total_claims / total_exposure,
    mean_claim_count = mean(claim_count),
    var_claim_count = var(claim_count),
    variance_to_mean_ratio = var(claim_count) / mean(claim_count),
    zero_prop = mean(claim_count == 0)
  )

freq_clean %>%
  summarise(
    mean_exposure = mean(exposure),
    median_exposure = median(exposure),
    min_exposure = min(exposure),
    max_exposure = max(exposure)
  )

# Build modelling dataset
freq_model_df <- freq_clean %>%
  mutate(
    log_cargo_value = log(cargo_value),
    log_weight = log(weight),
    
    # Center continuous predictors so the intercept corresponds
    # to an average shipment in the baseline route risk class
    c_log_cargo_value = log_cargo_value - mean(log_cargo_value),
    c_log_weight      = log_weight - mean(log_weight),
    c_distance        = distance - mean(distance),
    c_transit         = transit_duration - mean(transit_duration),
    c_pilot_exp       = pilot_experience - mean(pilot_experience),
    c_vessel_age      = vessel_age - mean(vessel_age),
    c_solar           = solar_radiation - mean(solar_radiation),
    c_debris          = debris_density - mean(debris_density)
  )


# Poisson model development

# Baseline Poisson with exposure offset
freq_poisson0 <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson0)
exp(coef(freq_poisson0))
deviance(freq_poisson0) / df.residual(freq_poisson0)

# Route-risk-only Poisson
freq_poisson1 <- glm(
  claim_count ~ route_risk + offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson1)
exp(coef(freq_poisson1))
deviance(freq_poisson1) / df.residual(freq_poisson1)

# Full screening model
freq_poisson_full <- glm(
  claim_count ~ route_risk + cargo_type + container_type +
    c_distance + c_transit + c_pilot_exp + c_vessel_age +
    c_solar + c_debris + c_log_cargo_value + c_log_weight +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_full)
exp(coef(freq_poisson_full))
deviance(freq_poisson_full) / df.residual(freq_poisson_full)

# Parsimonious final Poisson model guided by EDA, significance, and interpretability
freq_poisson_pars <- glm(
  claim_count ~ route_risk + c_pilot_exp + c_solar + c_debris +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_pars)
exp(coef(freq_poisson_pars))
deviance(freq_poisson_pars) / df.residual(freq_poisson_pars)

# Stepwise Poisson retained only as a screening benchmark
freq_poisson_step <- step(freq_poisson_full, direction = "backward", trace = 0)

summary(freq_poisson_step)
exp(coef(freq_poisson_step))
deviance(freq_poisson_step) / df.residual(freq_poisson_step)

# Poisson model comparison
AIC(
  freq_poisson0,
  freq_poisson1,
  freq_poisson_pars,
  freq_poisson_full,
  freq_poisson_step
)

# Nested model comparisons
anova(freq_poisson0, freq_poisson1, test = "Chisq")
anova(freq_poisson1, freq_poisson_pars, test = "Chisq")
anova(freq_poisson_pars, freq_poisson_full, test = "Chisq")


# Overdispersion check: quasi-Poisson sensitivity
freq_quasi_pars <- glm(
  claim_count ~ route_risk + c_pilot_exp + c_solar + c_debris +
    offset(log(exposure)),
  family = quasipoisson(link = "log"),
  data = freq_model_df
)

summary(freq_quasi_pars)
exp(coef(freq_quasi_pars))
summary(freq_quasi_pars)$dispersion

# Negative Binomial sensitivity check (not selected as final model)
# Same structure as the parsimonious Poisson model
freq_nb_pars <- glm.nb(
  claim_count ~ route_risk + c_pilot_exp + c_solar + c_debris +
    offset(log(exposure)),
  data = freq_model_df
)

summary(freq_nb_pars)
exp(coef(freq_nb_pars))
AIC(freq_poisson_pars, freq_nb_pars)

# Calibration check: compare fitted aggregate claims
c(
  observed_total_claims = sum(freq_model_df$claim_count),
  poisson_total_fitted = sum(fitted(freq_poisson_pars)),
  nb_total_fitted = sum(fitted(freq_nb_pars))
)

# Route-level calibration check
obs_pred_route_poisson <- freq_model_df %>%
  mutate(pred_claims_pois = fitted(freq_poisson_pars)) %>%
  group_by(route_risk) %>%
  summarise(
    observed_rate = sum(claim_count) / sum(exposure),
    predicted_rate = sum(pred_claims_pois) / sum(exposure),
    .groups = "drop"
  )

obs_pred_route_poisson

obs_pred_route_nb <- freq_model_df %>%
  mutate(pred_claims_nb = fitted(freq_nb_pars)) %>%
  group_by(route_risk) %>%
  summarise(
    observed_rate = sum(claim_count) / sum(exposure),
    predicted_rate = sum(pred_claims_nb) / sum(exposure),
    .groups = "drop"
  )

obs_pred_route_nb


# Diagnostics for selected final model: parsimonious Poisson
# Residual plot
plot(
  fitted(freq_poisson_pars),
  residuals(freq_poisson_pars, type = "pearson"),
  pch = 16, cex = 0.4,
  xlab = "Fitted values",
  ylab = "Pearson residuals",
  main = "Poisson Frequency Model: Pearson Residuals vs Fitted"
)
abline(h = 0, col = "red", lwd = 2)
lines(
  lowess(
    fitted(freq_poisson_pars),
    residuals(freq_poisson_pars, type = "pearson")
  ),
  col = "blue", lwd = 2
)

# Zero-count calibration
obs_zero_rate <- mean(freq_model_df$claim_count == 0)
pred_zero_rate_poisson <- mean(dpois(0, lambda = fitted(freq_poisson_pars)))
pred_zero_rate_nb <- mean(dnbinom(0, mu = fitted(freq_nb_pars), size = freq_nb_pars$theta))

c(
  observed_zero_rate = obs_zero_rate,
  predicted_zero_rate_poisson = pred_zero_rate_poisson,
  predicted_zero_rate_nb = pred_zero_rate_nb
)

# Observed vs predicted claim rate by route risk for final model
ggplot(obs_pred_route_poisson, aes(x = route_risk)) +
  geom_point(aes(y = observed_rate, colour = "Observed"), size = 2) +
  geom_point(aes(y = predicted_rate, colour = "Predicted"), shape = 17, size = 2) +
  geom_line(aes(y = observed_rate, colour = "Observed", group = 1)) +
  geom_line(aes(y = predicted_rate, colour = "Predicted", group = 1)) +
  labs(
    title = "Observed vs Predicted Claim Rate by Route Risk (Poisson Model)",
    x = "Route Risk",
    y = "Claim Rate per Exposure",
    colour = ""
  )


# Final model outputs for reporting
# Final selected model coefficients on multiplicative scale
exp(coef(freq_poisson_pars))

# Final fitted aggregate claim count check
c(
  observed_total_claims = sum(freq_model_df$claim_count),
  fitted_total_claims_poisson = sum(fitted(freq_poisson_pars))
)


# ---- Cleaning of cargo severity data ----

sev_path <- "datasets/srcsc-2026-claims-cargo-sev.csv"
sev_raw <- read_csv(sev_path, show_col_types = FALSE)

sev <- sev_raw %>%
  mutate(
    claim_seq = as.integer(claim_seq),
    claim_id = str_trim(str_replace(as.character(claim_id), "_.*$", "")),
    policy_id = str_trim(str_replace(as.character(policy_id), "_.*$", "")),
    shipment_id = str_trim(str_replace(as.character(shipment_id), "_.*$", "")),
    cargo_type = str_to_lower(str_trim(str_replace(as.character(cargo_type), "_.*$", ""))),
    container_type = str_trim(str_replace(as.character(container_type), "_.*$", "")),
    
    cargo_type = as.factor(cargo_type),
    container_type = as.factor(container_type),
    route_risk = as.factor(route_risk)
  )

glimpse(sev)

# Missingness per variable (raw number)
sev_missing_counts <- sev %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

sev_missing_counts

# Missingness per variable (as a percentage)
sev_missing_perc <- sev %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  arrange(desc(pct_missing))

sev_missing_perc


sev_range_flags <- sev %>%
  summarise(
    # indices / exposure should be in [0,1]
    n_bad_exposure = sum(is.na(exposure) | exposure < 0 | exposure > 1),
    n_bad_solar    = sum(is.na(solar_radiation) | solar_radiation < 0 | solar_radiation > 1),
    n_bad_debris   = sum(is.na(debris_density) | debris_density < 0 | debris_density > 1),
    
    # route risk should be 1..5
    n_bad_route_risk = sum(is.na(route_risk) | !(as.character(route_risk) %in% c("1","2","3","4","5"))),
    
    # numeric predictor ranges
    n_bad_distance = sum(is.na(distance) | distance < 1 | distance > 100),
    n_bad_transit  = sum(is.na(transit_duration) | transit_duration < 1 | transit_duration > 60),
    n_bad_pilot    = sum(is.na(pilot_experience) | pilot_experience < 1 | pilot_experience > 30),
    n_bad_vessel   = sum(is.na(vessel_age) | vessel_age < 1 | vessel_age > 50),
    n_bad_weight   = sum(is.na(weight) | weight < 1500 | weight > 250000),
    
    n_bad_cargo_value = sum(is.na(cargo_value) | cargo_value <= 0),
    
    # severity response must be > 0
    n_bad_claim_amount = sum(is.na(claim_amount) | claim_amount <= 0),
    
    # claim_seq should be >= 1
    n_bad_claim_seq = sum(is.na(claim_seq) | claim_seq < 1)
  )

sev_range_flags <- pivot_longer(sev_range_flags, everything(),
                                names_to = "variable",
                                values_to = "Number of bad")

sev_range_flags

sev_flagged <- sev %>%
  mutate(
    bad_row =
      is.na(claim_amount) | claim_amount <= 0 |
      is.na(route_risk) | !(as.character(route_risk) %in% c("1","2","3","4","5")) |
      is.na(distance) | distance < 1 | distance > 100 |
      is.na(transit_duration) | transit_duration < 1 | transit_duration > 60 |
      is.na(pilot_experience) | pilot_experience < 1 | pilot_experience > 30 |
      is.na(vessel_age) | vessel_age < 1 | vessel_age > 50 |
      is.na(cargo_value) | cargo_value <= 0 |
      is.na(weight) | weight < 1500 | weight > 250000 |
      is.na(solar_radiation) | solar_radiation < 0 | solar_radiation > 1 |
      is.na(debris_density) | debris_density < 0 | debris_density > 1
  )

sev_flagged %>% summarise(
  n_total = n(),
  n_bad = sum(bad_row),
  pct_bad = mean(bad_row)
)

sev_clean <- sev_flagged %>%
  filter(!bad_row) %>%
  dplyr::select(-bad_row) %>%
  mutate(
    cargo_type = fct_na_value_to_level(cargo_type, level = "unknown"),
    container_type = fct_na_value_to_level(container_type, level = "unknown")
  )

glimpse(sev_clean)
summary(sev_clean$claim_amount)


hist(log(sev_clean$claim_amount), breaks = 100)


# claims exceeding cargo value
sev_clean %>%
  summarise(
    n_exceed_value = sum(claim_amount > cargo_value),
    pct_exceed_value = mean(claim_amount > cargo_value)
  )

# loss ratio sanity check
sev_clean %>%
  mutate(loss_ratio = claim_amount / cargo_value) %>%
  summarise(
    max_loss_ratio = max(loss_ratio),
    p99_loss_ratio = quantile(loss_ratio, 0.99)
  )

sev_clean %>%
  mutate(loss_ratio = claim_amount / cargo_value) %>%
  summarise(
    num_loss_gt1 = sum(loss_ratio > 1),
    num_loss_gt2 = sum(loss_ratio > 2),
    num_loss_gt5 = sum(loss_ratio > 5),
    max_loss_ratio = max(loss_ratio)
  )


# ---- EDA of sev dataset ----

sev_clean %>%
  summarise(
    n_claims = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    sd_claim = sd(claim_amount),
    cv = sd_claim / mean_claim,
    min_claim = min(claim_amount),
    p90 = quantile(claim_amount, 0.90),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    p999 = quantile(claim_amount, 0.999),
    p9999 = quantile(claim_amount, 0.9999),
    max_claim = max(claim_amount)
  )

# Claims outside approximate data-dictionary bounds
sev_clean %>%
  summarise(
    n_below_31k = sum(claim_amount < 31000),
    pct_below_31k = mean(claim_amount < 31000),
    n_above_678m = sum(claim_amount > 678000000),
    pct_above_678m = mean(claim_amount > 678000000)
  )

# Inspect very large claims
sev_clean %>%
  filter(claim_amount > 678000000) %>%
  dplyr::select(claim_id, claim_amount, cargo_type, cargo_value, weight, route_risk) %>%
  arrange(desc(claim_amount))

# Severity Distr Plots

ggplot(sev_clean, aes(x = claim_amount)) +
  geom_histogram(bins = 80) +
  labs(
    title = "Claim Amount Distribution (Raw Scale)",
    x = "Claim Amount",
    y = "Count"
  )

ggplot(sev_clean, aes(x = log(claim_amount))) +
  geom_histogram(bins = 100) +
  labs(
    title = "Claim Amount Distribution (Log Scale)",
    x = "log(Claim Amount)",
    y = "Count"
  )

# Severity by key categorical predictors
# Route risk
sev_clean %>%
  group_by(route_risk) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    max_claim = max(claim_amount),
    .groups = "drop"
  ) %>%
  arrange(route_risk)

# Cargo type
sev_clean %>%
  group_by(cargo_type) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    max_claim = max(claim_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_claim))

# Container type
sev_clean %>%
  group_by(container_type) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    max_claim = max(claim_amount),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_claim))


# Sev relationship with cargo value and continuous predictors
# Claim amount vs cargo value
ggplot(sev_clean, aes(x = log(cargo_value), y = log(claim_amount))) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "log(Claim Amount) vs log(Cargo Value)",
    x = "log(Cargo Value)",
    y = "log(Claim Amount)"
  )

summary(lm(log(claim_amount) ~ log(cargo_value), data = sev_clean))

# Correlation matrix for severity-relevant numeric variables
sev_clean %>%
  dplyr::select(
    claim_amount,
    cargo_value,
    weight,
    distance,
    transit_duration,
    pilot_experience,
    vessel_age,
    solar_radiation,
    debris_density
  ) %>%
  cor(use = "complete.obs")


# route_risk vs claim amount
sev_clean %>%
  group_by(route_risk) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    max = max(claim_amount)
  )

# Loss Ratio Exploration
# Keep all observations, including loss ratios > 1
sev_lr <- sev_clean %>%
  mutate(loss_ratio = claim_amount / cargo_value)

sev_lr %>%
  summarise(
    mean_lr = mean(loss_ratio),
    median_lr = median(loss_ratio),
    p95_lr = quantile(loss_ratio, 0.95),
    p99_lr = quantile(loss_ratio, 0.99),
    max_lr = max(loss_ratio),
    n_lr_gt_1 = sum(loss_ratio > 1),
    pct_lr_gt_1 = mean(loss_ratio > 1),
    n_lr_gt_2 = sum(loss_ratio > 2),
    n_lr_gt_5 = sum(loss_ratio > 5)
  )

ggplot(sev_lr, aes(x = loss_ratio)) +
  geom_histogram(bins = 200) +
  coord_cartesian(xlim = c(0, 2)) +
  labs(
    title = "Loss Ratio Distribution",
    x = "Loss Ratio = Claim Amount / Cargo Value",
    y = "Count"
  )

# Inspect rare claims with loss ratio > 1, but do not remove them
sev_lr %>%
  filter(loss_ratio > 1) %>%
  count(cargo_type, sort = TRUE)

sev_lr %>%
  filter(loss_ratio > 1) %>%
  dplyr::select(claim_id, claim_amount, cargo_value, loss_ratio, cargo_type, route_risk) %>%
  arrange(desc(loss_ratio))


# Solar/Debris summaries
sev_clean %>%
  mutate(solar_bin = cut(solar_radiation, breaks = 5)) %>%
  group_by(solar_bin) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    .groups = "drop"
  )

sev_clean %>%
  mutate(debris_bin = cut(debris_density, breaks = 5)) %>%
  group_by(debris_bin) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    .groups = "drop"
  )

# Claim Seq Check
sev_clean %>%
  group_by(claim_seq) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    .groups = "drop"
  ) %>%
  arrange(claim_seq)

# Segmentation into precious and non-precious
sev_clean %>%
  mutate(segment = if_else(cargo_type %in% c("gold", "platinum"), "precious", "other")) %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    min_claim = min(claim_amount),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    max_claim = max(claim_amount),
    .groups = "drop"
  )

sev_model0 <- sev_clean %>%
  mutate(
    segment = if_else(cargo_type %in% c("gold", "platinum"), "precious", "other"),
    segment = factor(segment)
  )

# Segment counts
sev_model0 %>%
  count(segment)

# Segment severity summary
sev_model0 %>%
  group_by(segment) %>%
  summarise(
    n = n(),
    mean_claim = mean(claim_amount),
    median_claim = median(claim_amount),
    p95 = quantile(claim_amount, 0.95),
    p99 = quantile(claim_amount, 0.99),
    max_claim = max(claim_amount),
    .groups = "drop"
  )

# Segment share of total cargo weight
sev_model0 %>%
  group_by(segment) %>%
  summarise(
    total_weight = sum(weight),
    .groups = "drop"
  ) %>%
  mutate(
    pct_weight = total_weight / sum(total_weight)
  )

# Segment share of total cargo value
sev_model0 %>%
  group_by(segment) %>%
  summarise(
    total_value = sum(cargo_value),
    .groups = "drop"
  ) %>%
  mutate(
    pct_value = total_value / sum(total_value)
  )


# ---- Baseline severity models (no predictors) | Subset datasets ----

sev_other <- sev_model0 %>% filter(segment == "other")
sev_precious <- sev_model0 %>% filter(segment == "precious")


# Lognormal baseline
ln_other <- lm(log(claim_amount) ~ 1, data = sev_other)
ln_precious <- lm(log(claim_amount) ~ 1, data = sev_precious)

summary(ln_other)
summary(ln_precious)

# ---- Gamma baseline (log link) ----

gamma_other <- glm(
  claim_amount ~ 1,
  family = Gamma(link = "log"),
  data = sev_other
)

gamma_precious <- glm(
  claim_amount ~ 1,
  family = Gamma(link = "log"),
  data = sev_precious
)

summary(gamma_other)
summary(gamma_precious)

# ---- Compare AIC ----

AIC(ln_other, gamma_other)
AIC(ln_precious, gamma_precious)

# Show that cargo value explains most of the severity scale
# choose lognormal
sev_lognorm1 <- lm(
  log(claim_amount) ~ segment,
  data = sev_model0
)

summary(sev_lognorm1)

# does adding cargo_value do anything meaninful?
sev_lognorm2 <- lm(
  log(claim_amount) ~ segment + log(cargo_value),
  data = sev_model0
)

summary(sev_lognorm2)

# Build loss-ratio modelling dataset
# since cargo value is a proxy for the claim amount,
# should be modelling the loss ratio instead
sev_model <- sev_clean %>%
  mutate(
    segment = if_else(cargo_type %in% c("gold", "platinum"), "precious", "other"),
    segment = factor(segment),
    loss_ratio = claim_amount / cargo_value,
    log_lr = log(loss_ratio)
  )

summary(sev_model$loss_ratio)
summary(sev_model$log_lr)
sd(sev_model$log_lr)

# Keep claims above cargo value in the dataset; do not cap for fitting
sev_model %>%
  summarise(
    pct_lr_gt_1 = mean(loss_ratio > 1),
    max_lr = max(loss_ratio)
  )

# Distribution of log loss ratio
ggplot(sev_model, aes(x = log_lr)) +
  geom_histogram(bins = 100) +
  labs(
    title = "Distribution of log(Loss Ratio)",
    x = "log(Claim Amount / Cargo Value)",
    y = "Count"
  )

# Baseline loss-ratio models
# Intercept-only baseline
sev_lr0 <- lm(log_lr ~ 1, data = sev_model)
summary(sev_lr0)

# Main pooled loss-ratio model
sev_lr1 <- lm(
  log_lr ~ route_risk + solar_radiation + debris_density +
    pilot_experience + vessel_age + container_type + cargo_type,
  data = sev_model
)

summary(sev_lr1)

# Backward-selection screening model
sev_lr_pars <- step(sev_lr1, direction = "backward", trace = 0)
summary(sev_lr_pars)


# Test whether segment adds anything after normalizing by cargo value
sev_lr_seg <- lm(
  log_lr ~ segment + route_risk + solar_radiation + debris_density,
  data = sev_model
)

summary(sev_lr_seg)

# Compare with parsimonious pooled model
AIC(sev_lr0, sev_lr_pars, sev_lr_seg)

anova(sev_lr_pars, sev_lr_seg)


# Segment-specific sensitivity models (not necessarily final)
sev_lr_other <- lm(
  log_lr ~ route_risk + solar_radiation + debris_density,
  data = sev_model %>% filter(segment == "other")
)

sev_lr_precious <- lm(
  log_lr ~ route_risk + solar_radiation + debris_density,
  data = sev_model %>% filter(segment == "precious")
)

summary(sev_lr_other)
summary(sev_lr_precious)

AIC(sev_lr_other, sev_lr_precious)

# Diagnostics for selected pooled loss-ratio model
par(mfrow = c(2, 2))
plot(sev_lr_pars)
par(mfrow = c(1, 1))

sigma(sev_lr_pars)


# Convert fitted log-loss-ratio model to expected loss ratio
sev_sigma <- sigma(sev_lr_pars)

sev_model <- sev_model %>%
  mutate(
    m_lr = predict(sev_lr_pars, newdata = sev_model),   # predicted mean log loss ratio
    elr  = exp(m_lr + 0.5 * sev_sigma^2)                # expected loss ratio under lognormal assumption
  )

summary(sev_model$elr)

# Expected frequency from final Poisson frequency model
freq_model_df <- freq_model_df %>%
  mutate(
    mu_freq = predict(freq_poisson_pars, newdata = freq_model_df, type = "response")
  )

summary(freq_model_df$mu_freq)


# ---- Cosmic Quarry cargo exposure cells and model application | Vessel data ----

cq_vessels <- tribble(
  ~container_type,               ~max_volume_kg, ~Helionis, ~Bayesia, ~Oryn,
  "DeepSpace Haulbox",                 25000,        58,       56,    39,
  "DockArc Freight Case",              50000,       116,      113,    77,
  "HardSeal Transit Crate",           100000,       580,      564,   387,
  "LongHaul Vault Canister",          150000,       232,      226,   155,
  "QuantumCrate Module",              250000,       174,      169,   116
)

cq_cargo_cells <- cq_vessels %>%
  pivot_longer(
    cols = c(Helionis, Bayesia, Oryn),
    names_to = "solar_system",
    values_to = "vessel_count"
  ) %>%
  mutate(
    container_type = factor(container_type, levels = levels(freq_clean$container_type))
  )

# ---- Historical container-capacity mismatch check | (kept as evidence, not used directly for final load factors) ----

container_caps <- cq_vessels %>%
  dplyr::select(container_type, max_volume_kg) %>%
  mutate(
    container_type = factor(container_type, levels = levels(freq_clean$container_type))
  )

hist_load_factors <- freq_clean %>%
  left_join(container_caps, by = "container_type") %>%
  mutate(
    hist_load_factor = pmin(weight / max_volume_kg, 1)
  ) %>%
  group_by(container_type) %>%
  summarise(
    load_factor = median(hist_load_factor, na.rm = TRUE),
    .groups = "drop"
  )

hist_load_factors

# Final judgment-based load assumptions
cq_load_assumptions <- tibble(
  container_type = factor(
    c("DeepSpace Haulbox",
      "DockArc Freight Case",
      "HardSeal Transit Crate",
      "LongHaul Vault Canister",
      "QuantumCrate Module"),
    levels = levels(freq_clean$container_type)
  ),
  load_factor = c(0.95, 0.90, 0.75, 0.50, 0.35)
)

# ---- Solar-system environmental assumptions ----

solar_mu <- mean(freq_clean$solar_radiation)
solar_sd <- sd(freq_clean$solar_radiation)
debris_mu <- mean(freq_clean$debris_density)
debris_sd <- sd(freq_clean$debris_density)
pilot_mean <- mean(freq_clean$pilot_experience)

cq_system_assumptions <- tribble(
  ~solar_system, ~route_risk, ~solar_radiation,        ~debris_density,        ~pilot_experience,
  
  # Helionis: low solar activity, dense asteroid clusters
  "Helionis",    "4",         solar_mu - 0.5 * solar_sd, debris_mu + 0.5 * debris_sd, pilot_mean,
  
  # Bayesia: high radiation spikes, well-mapped asteroid belt
  "Bayesia",     "3",         solar_mu + 0.5 * solar_sd, debris_mu - 0.5 * debris_sd, pilot_mean,
  
  # Oryn: unpredictable flares, dense asymmetric asteroid ring
  "Oryn",        "5",         solar_mu + 0.25 * solar_sd, debris_mu + 1.0 * debris_sd, pilot_mean
) %>%
  mutate(
    solar_radiation = pmin(pmax(solar_radiation, 0), 1),
    debris_density  = pmin(pmax(debris_density, 0), 1),
    route_risk = factor(route_risk, levels = levels(freq_clean$route_risk))
  )

# ---- Value-per-kg table from historical data ----

value_density_tbl <- freq_clean %>%
  mutate(value_per_kg = cargo_value / weight) %>%
  group_by(cargo_type) %>%
  summarise(
    n = n(),
    vpk = median(value_per_kg),
    p25_vpk = quantile(value_per_kg, 0.25),
    p75_vpk = quantile(value_per_kg, 0.75),
    .groups = "drop"
  ) %>%
  arrange(desc(vpk))

value_density_tbl

# ---- Cargo mix scenarios for Cosmic Quarry | Base case: core industrial ore mix ----

base_mix <- tibble(
  cargo_type = c("rare earths", "lithium", "cobalt", "titanium"),
  mix_wt = c(0.35, 0.25, 0.20, 0.20)
)

# Broader operating mix: includes supplies
supplies_mix <- tibble(
  cargo_type = c("rare earths", "lithium", "cobalt", "titanium", "supplies"),
  mix_wt = c(0.30, 0.22, 0.18, 0.15, 0.15)
)

# High-value scenario: small precious-metal concentration
precious_mix <- tibble(
  cargo_type = c("gold", "platinum", "rare earths", "lithium", "cobalt", "titanium"),
  mix_wt = c(0.005, 0.005, 0.34, 0.24, 0.20, 0.21)
)

avg_vpk_base <- value_density_tbl %>%
  inner_join(base_mix, by = "cargo_type") %>%
  summarise(vpk = sum(vpk * mix_wt)) %>%
  pull(vpk)

avg_vpk_supplies <- value_density_tbl %>%
  inner_join(supplies_mix, by = "cargo_type") %>%
  summarise(vpk = sum(vpk * mix_wt)) %>%
  pull(vpk)

avg_vpk_precious <- value_density_tbl %>%
  inner_join(precious_mix, by = "cargo_type") %>%
  summarise(vpk = sum(vpk * mix_wt)) %>%
  pull(vpk)

avg_vpk_base
avg_vpk_supplies
avg_vpk_precious

# ---- Solar-system cargo value assumptions | Oryn uplift for rare-metal extraction focus ----

cq_value_assumptions <- tibble(
  solar_system = c("Helionis", "Bayesia", "Oryn"),
  vpk_multiplier = c(1.00, 1.00, 1.25)
)

# ---- Build rating cells with weights and scenario cargo values ----

cq_cargo_cells2 <- cq_cargo_cells %>%
  left_join(cq_load_assumptions, by = "container_type") %>%
  left_join(cq_system_assumptions, by = "solar_system") %>%
  left_join(cq_value_assumptions, by = "solar_system") %>%
  mutate(
    avg_weight = max_volume_kg * load_factor,
    cargo_value_base = avg_weight * avg_vpk_base * vpk_multiplier,
    cargo_value_supplies = avg_weight * avg_vpk_supplies * vpk_multiplier,
    cargo_value_precious = avg_weight * avg_vpk_precious * vpk_multiplier
  )

# Join sanity check
cq_cargo_cells2 %>%
  summarise(
    any_na_load = any(is.na(load_factor)),
    any_na_route = any(is.na(route_risk)),
    any_na_solar = any(is.na(solar_radiation)),
    any_na_debris = any(is.na(debris_density)),
    any_na_value_base = any(is.na(cargo_value_base)),
    any_na_value_supplies = any(is.na(cargo_value_supplies)),
    any_na_value_precious = any(is.na(cargo_value_precious))
  )

# ---- Calibrate annual shipments to system production targets ----

cq_system_targets <- tibble(
  solar_system = c("Helionis", "Bayesia", "Oryn"),
  target_tonnes = c(375000, 250000, 125000)
)

system_capacity <- cq_cargo_cells2 %>%
  group_by(solar_system) %>%
  summarise(
    tonnes_per_system_voyage = sum(vessel_count * avg_weight) / 1000,
    .groups = "drop"
  )

system_capacity

shipments_per_vessel_by_system <- system_capacity %>%
  left_join(cq_system_targets, by = "solar_system") %>%
  mutate(
    shipments_per_vessel = target_tonnes / tonnes_per_system_voyage
  ) %>%
  dplyr::select(solar_system, shipments_per_vessel)

shipments_per_vessel_by_system

cq_cargo_cells2 <- cq_cargo_cells2 %>%
  left_join(shipments_per_vessel_by_system, by = "solar_system") %>%
  mutate(
    n_shipments = vessel_count * shipments_per_vessel
  )

# ---- Add exposure for frequency pricing ----

avg_exposure <- mean(freq_clean$exposure)

cq_cargo_cells2 <- cq_cargo_cells2 %>%
  mutate(
    exposure = avg_exposure,
    annual_exposure = n_shipments * exposure
  )

# ---- Diagnostics and sanity checks | System production should match targets ----

cq_cargo_cells2 %>%
  group_by(solar_system) %>%
  summarise(
    annual_weight_tonnes = sum(n_shipments * avg_weight) / 1000,
    .groups = "drop"
  )

# Total production should match 750k tonnes
cq_cargo_cells2 %>%
  summarise(
    total_annual_weight_tonnes = sum(n_shipments * avg_weight) / 1000
  )

# Annual insured cargo value under each scenario
cq_cargo_cells2 %>%
  summarise(
    total_annual_value_base = sum(n_shipments * cargo_value_base),
    total_annual_value_supplies = sum(n_shipments * cargo_value_supplies),
    total_annual_value_precious = sum(n_shipments * cargo_value_precious)
  )

cq_cargo_cells2 %>%
  summarise(
    avg_value_per_tonne_base =
      sum(n_shipments * cargo_value_base) / (sum(n_shipments * avg_weight) / 1000),
    avg_value_per_tonne_supplies =
      sum(n_shipments * cargo_value_supplies) / (sum(n_shipments * avg_weight) / 1000),
    avg_value_per_tonne_precious =
      sum(n_shipments * cargo_value_precious) / (sum(n_shipments * avg_weight) / 1000)
  )

# Final exposure cells
cq_cargo_cells2

# ---- Expected frequency for Cosmic Quarry ----

pilot_mean_hist  <- mean(freq_clean$pilot_experience)
solar_mean_hist  <- mean(freq_clean$solar_radiation)
debris_mean_hist <- mean(freq_clean$debris_density)

cq_pred <- cq_cargo_cells2 %>%
  mutate(
    c_pilot_exp = pilot_experience - pilot_mean_hist,
    c_solar     = solar_radiation - solar_mean_hist,
    c_debris    = debris_density - debris_mean_hist
  ) %>%
  mutate(
    # expected claims per shipment (includes exposure offset)
    mu_per_shipment = predict(freq_poisson_pars, newdata = ., type = "response"),
    
    # expected annual claims in the rating cell
    mu_freq = mu_per_shipment * n_shipments
  )

cq_pred %>%
  summarise(
    total_shipments = sum(n_shipments),
    total_expected_claims = sum(mu_freq),
    avg_mu_per_shipment = mean(mu_per_shipment),
    min_mu_per_shipment = min(mu_per_shipment),
    max_mu_per_shipment = max(mu_per_shipment)
  )

cq_pred %>%
  group_by(solar_system) %>%
  summarise(
    shipments = sum(n_shipments),
    expected_claims = sum(mu_freq),
    .groups = "drop"
  )

# ---- Expected severity for Cosmic Quarry cells ----

cq_pred <- cq_pred %>%
  mutate(
    m_lr = predict(sev_lr_pars, newdata = ., type = "response"),
    elr = exp(m_lr + 0.5 * sev_sigma^2)
  )

cq_pred %>%
  summarise(
    avg_elr = mean(elr),
    min_elr = min(elr),
    max_elr = max(elr)
  )

cq_pred %>%
  group_by(solar_system) %>%
  summarise(
    avg_elr = mean(elr),
    min_elr = min(elr),
    max_elr = max(elr),
    .groups = "drop"
  )

# ---- Expected annual loss by CQ cell under each scenario ----

cq_pred <- cq_pred %>%
  mutate(
    exp_loss_base = mu_freq * elr * cargo_value_base,
    exp_loss_supplies = mu_freq * elr * cargo_value_supplies,
    exp_loss_precious = mu_freq * elr * cargo_value_precious
  )

cq_by_system <- cq_pred %>%
  group_by(solar_system) %>%
  summarise(
    shipments = sum(n_shipments),
    expected_claims = sum(mu_freq),
    exp_loss_base = sum(exp_loss_base),
    exp_loss_supplies = sum(exp_loss_supplies),
    exp_loss_precious = sum(exp_loss_precious),
    
    annual_value_base = sum(n_shipments * cargo_value_base),
    annual_value_supplies = sum(n_shipments * cargo_value_supplies),
    annual_value_precious = sum(n_shipments * cargo_value_precious),
    
    loss_cost_rate_base = exp_loss_base / annual_value_base,
    loss_cost_rate_supplies = exp_loss_supplies / annual_value_supplies,
    loss_cost_rate_precious = exp_loss_precious / annual_value_precious,
    .groups = "drop"
  )

cq_by_system

cq_pred %>%
  summarise(
    total_expected_claims = sum(mu_freq),
    total_exp_loss_base = sum(exp_loss_base),
    total_exp_loss_supplies = sum(exp_loss_supplies),
    total_exp_loss_precious = sum(exp_loss_precious),
    
    total_annual_value_base = sum(n_shipments * cargo_value_base),
    total_annual_value_supplies = sum(n_shipments * cargo_value_supplies),
    total_annual_value_precious = sum(n_shipments * cargo_value_precious),
    
    portfolio_loss_cost_rate_base = total_exp_loss_base / total_annual_value_base,
    portfolio_loss_cost_rate_supplies = total_exp_loss_supplies / total_annual_value_supplies,
    portfolio_loss_cost_rate_precious = total_exp_loss_precious / total_annual_value_precious
  )

# ---- 14. Optional cell-level output for later simulation/pricing ----

cq_pred %>%
  dplyr::select(
    solar_system, container_type, vessel_count, n_shipments,
    avg_weight, exposure, annual_exposure,
    route_risk, solar_radiation, debris_density, pilot_experience,
    cargo_value_base, cargo_value_supplies, cargo_value_precious,
    mu_per_shipment, mu_freq, elr,
    exp_loss_base, exp_loss_supplies, exp_loss_precious
  )

## Because Cosmic Quarry’s operations are described as large-scale ore extraction and transport,
## the base pricing scenario uses an industrial mining cargo mix. A broader historical portfolio mix,
## including high-value precious metal shipments, was retained as an upper-bound sensitivity scenario.


# ---- Monte Carlo simulation, pricing, and long-term projection for CQ  cargo portfolio | Monte Carlo simulation, pricing, and long-term projection for CQ cargo portfolio ----

set.seed(123)


# ---- Load macro forecasts ----

inflation_fc <- read_csv("forecasts/inflation.csv", show_col_types = FALSE) %>%
  mutate(
    inflation_rate = inflation_rate / 100
  )

interest_fc <- read_csv("forecasts/interest.csv", show_col_types = FALSE) %>%
  filter(maturity == 1) %>%
  pivot_longer(
    cols = -maturity,
    names_to = "year",
    values_to = "investment_rate"
  ) %>%
  mutate(
    year = as.integer(year),
    investment_rate = investment_rate / 100
  ) %>%
  dplyr::select(year, investment_rate)

macro_fc <- inflation_fc %>%
  left_join(interest_fc, by = "year")

print(macro_fc)

# ---- Product design assumptions ----

deductible_tbl <- tibble(
  scenario = c("base", "supplies", "precious"),
  deductible_lr = c(0.05, 0.05, 0.10)
)

expense_ratio <- 0.10
pricing_quantile <- 0.95
n_sim <- 30000

# ---- Scenario growth assumptions over 10 years ----

growth_targets <- tibble(
  solar_system = c("Helionis", "Bayesia", "Oryn"),
  growth_10y = c(1.25, 1.25, 1.15)
)

make_growth_factor <- function(year_index, growth_10y) {
  1 + (growth_10y - 1) * (year_index - 1) / 9
}

# ---- Helper functions | Cosmic Quarry values are already in 2175 money, so: | 2175 inflation factor = 1 | 2176 onward accumulates inflation from 2176 up to target year ----

get_inflation_factor <- function(year_val, macro_fc) {
  if (year_val == 2175) {
    return(1)
  } else {
    return(prod(1 + macro_fc$inflation_rate[macro_fc$year >= 2176 & macro_fc$year <= year_val]))
  }
}

# Build one projected year of the CQ portfolio
build_projection_year <- function(cq_df, year_val, macro_row, macro_fc) {
  
  year_index <- year_val - 2175 + 1
  
  pilot_mean_hist  <- mean(freq_clean$pilot_experience)
  solar_mean_hist  <- mean(freq_clean$solar_radiation)
  debris_mean_hist <- mean(freq_clean$debris_density)
  
  cq_year <- cq_df %>%
    left_join(growth_targets, by = "solar_system") %>%
    mutate(
      growth_factor = make_growth_factor(year_index, growth_10y),
      
      # Scale shipment volumes through time
      n_shipments_year = n_shipments * growth_factor,
      annual_exposure_year = annual_exposure * growth_factor,
      
      # Inflate cargo values from 2175 base values
      inflation_factor = get_inflation_factor(year_val, macro_fc),
      cargo_value_base_year = cargo_value_base * inflation_factor,
      cargo_value_supplies_year = cargo_value_supplies * inflation_factor,
      cargo_value_precious_year = cargo_value_precious * inflation_factor,
      
      # Recreate centred predictors exactly as in fitted models
      c_pilot_exp = pilot_experience - pilot_mean_hist,
      c_solar     = solar_radiation - solar_mean_hist,
      c_debris    = debris_density - debris_mean_hist
    ) %>%
    mutate(
      # Frequency model
      mu_per_shipment = predict(freq_poisson_pars, newdata = ., type = "response"),
      mu_freq = mu_per_shipment * n_shipments_year,
      
      # Severity model
      m_lr = predict(sev_lr_pars, newdata = ., type = "response"),
      elr = exp(m_lr + 0.5 * sev_sigma^2),
      
      year = year_val,
      inflation_rate = macro_row$inflation_rate,
      investment_rate = macro_row$investment_rate
    )
  
  cq_year
}

# Simulate one year of aggregate insured loss for one scenario
simulate_one_year <- function(cq_df, sev_sigma, value_col, deductible_lr) {
  
  total_loss <- 0
  
  for (i in seq_len(nrow(cq_df))) {
    
    n_claims <- rpois(1, lambda = cq_df$mu_freq[i])
    
    if (n_claims > 0) {
      lr_gross <- exp(rnorm(n_claims, mean = cq_df$m_lr[i], sd = sev_sigma))
      
      # Policy design:
      # 1. cap gross loss ratio at 100% of cargo value
      # 2. apply deductible as a share of cargo value
      lr_net <- pmax(pmin(lr_gross, 1) - deductible_lr, 0)
      
      cell_loss <- sum(lr_net * cq_df[[value_col]][i])
      total_loss <- total_loss + cell_loss
    }
  }
  
  total_loss
}

simulate_loss_distribution <- function(cq_df, sev_sigma, value_col, deductible_lr, n_sim = 100000) {
  replicate(n_sim, simulate_one_year(cq_df, sev_sigma, value_col, deductible_lr))
}

calc_annual_premium <- function(losses, investment_rate, expense_ratio, pricing_quantile = 0.95) {
  
  # Bring losses from mid-year payment timing to end-of-year basis
  losses_eoy <- losses * (1 + investment_rate)^0.5
  
  expected_loss_eoy <- mean(losses_eoy)
  risk_margin_eoy <- as.numeric(quantile(losses_eoy, pricing_quantile) - expected_loss_eoy)
  
  required_eoy_funds <- expected_loss_eoy + risk_margin_eoy
  
  # Premium received at start of year and invested to end of year
  premium_start_net <- required_eoy_funds / (1 + investment_rate)
  
  # Gross up for expenses at policy inception
  premium_start_gross <- premium_start_net / (1 - expense_ratio)
  
  premium_start_gross
}

simulate_net_revenue <- function(losses, premium_start, investment_rate, expense_ratio) {
  
  losses_eoy <- losses * (1 + investment_rate)^0.5
  premium_eoy <- premium_start * (1 + investment_rate)
  investment_return <- premium_eoy - premium_start
  expenses_eoy <- (premium_start * expense_ratio) * (1 + investment_rate)
  total_cost <- losses_eoy + expenses_eoy
  net_revenue <- premium_eoy - total_cost
  
  tibble(
    loss = losses,
    loss_eoy = losses_eoy,
    expenses_eoy = expenses_eoy,
    total_cost = total_cost,
    premium_eoy = premium_eoy,
    investment_return = investment_return,
    net_revenue = net_revenue
  )
}

summarise_distribution <- function(x) {
  tibble(
    mean = mean(x),
    sd = sd(x),
    var = var(x),
    cv = sd(x) / mean(x),
    p1 = quantile(x, 0.01),
    p5 = quantile(x, 0.05),
    p10 = quantile(x, 0.10),
    p50 = quantile(x, 0.50),
    p75 = quantile(x, 0.75),
    p90 = quantile(x, 0.90),
    p95 = quantile(x, 0.95),
    p99 = quantile(x, 0.99),
    max = max(x)
  )
}

# ---- Scenario specifications ----

scenario_specs <- tibble(
  scenario = c("base", "supplies", "precious"),
  value_col = c("cargo_value_base_year", "cargo_value_supplies_year", "cargo_value_precious_year")
) %>%
  left_join(deductible_tbl, by = "scenario")

# ---- Year 1 short-term simulations (2175 annual) ----

cq_2175 <- build_projection_year(
  cq_df = cq_cargo_cells2,
  year_val = 2175,
  macro_row = macro_fc %>% filter(year == 2175),
  macro_fc = macro_fc
)

loss_sims_2175 <- scenario_specs %>%
  mutate(
    losses = pmap(
      list(value_col, deductible_lr),
      ~ simulate_loss_distribution(
        cq_df = cq_2175,
        sev_sigma = sev_sigma,
        value_col = ..1,
        deductible_lr = ..2,
        n_sim = n_sim
      )
    )
  )

cargo_short_term_summary <- loss_sims_2175 %>%
  mutate(summary = map(losses, summarise_distribution)) %>%
  dplyr::select(scenario, summary) %>%
  unnest(summary)

print(cargo_short_term_summary)

# ---- Year 1 premium and net revenue ----

cargo_pricing_2175 <- loss_sims_2175 %>%
  left_join(macro_fc %>% filter(year == 2175), by = character()) %>%
  mutate(
    premium = map_dbl(
      losses,
      ~ calc_annual_premium(
        losses = .x,
        investment_rate = investment_rate[1],
        expense_ratio = expense_ratio,
        pricing_quantile = pricing_quantile
      )
    ),
    net_rev_sims = map2(
      losses, premium,
      ~ simulate_net_revenue(
        losses = .x,
        premium_start = .y,
        investment_rate = investment_rate[1],
        expense_ratio = expense_ratio
      )
    )
  )

cargo_pricing_summary_2175 <- cargo_pricing_2175 %>%
  mutate(
    loss_summary   = map(net_rev_sims, ~ summarise_distribution(.x$loss_eoy)),
    cost_summary   = map(net_rev_sims, ~ summarise_distribution(.x$total_cost)),
    return_summary = map(net_rev_sims, ~ summarise_distribution(.x$investment_return)),
    netrev_summary = map(net_rev_sims, ~ summarise_distribution(.x$net_revenue))
  ) %>%
  transmute(
    scenario,
    premium,
    expected_expense = premium * expense_ratio,
    loss_summary,
    cost_summary,
    return_summary,
    netrev_summary
  ) %>%
  unnest_wider(loss_summary, names_sep = "_loss_") %>%
  unnest_wider(cost_summary, names_sep = "_cost_") %>%
  unnest_wider(return_summary, names_sep = "_return_") %>%
  unnest_wider(netrev_summary, names_sep = "_netrev_")

print(cargo_pricing_summary_2175)

# ---- Correlated stress scenario for Year 1 ----

cq_2175_stress <- cq_2175 %>%
  mutate(
    solar_radiation = case_when(
      solar_system == "Bayesia" ~ pmin(solar_radiation + 0.20, 1),
      solar_system == "Oryn"    ~ pmin(solar_radiation + 0.15, 1),
      TRUE ~ solar_radiation
    ),
    debris_density = case_when(
      solar_system == "Helionis" ~ pmin(debris_density + 0.15, 1),
      solar_system == "Oryn"     ~ pmin(debris_density + 0.20, 1),
      TRUE ~ debris_density
    ),
    route_risk_num = as.numeric(as.character(route_risk)),
    route_risk_num = pmin(route_risk_num + 1, 5),
    route_risk = factor(route_risk_num, levels = levels(freq_clean$route_risk))
  ) %>%
  dplyr::select(-route_risk_num) %>%
  mutate(
    c_pilot_exp = pilot_experience - mean(freq_clean$pilot_experience),
    c_solar     = solar_radiation - mean(freq_clean$solar_radiation),
    c_debris    = debris_density - mean(freq_clean$debris_density),
    mu_per_shipment = predict(freq_poisson_pars, newdata = ., type = "response"),
    mu_freq = mu_per_shipment * n_shipments_year,
    m_lr = predict(sev_lr_pars, newdata = ., type = "response"),
    elr = exp(m_lr + 0.5 * sev_sigma^2)
  )

stress_sims_2175 <- scenario_specs %>%
  mutate(
    losses = pmap(
      list(value_col, deductible_lr),
      ~ simulate_loss_distribution(
        cq_df = cq_2175_stress,
        sev_sigma = sev_sigma,
        value_col = ..1,
        deductible_lr = ..2,
        n_sim = n_sim
      )
    )
  )

cargo_stress_summary_2175 <- stress_sims_2175 %>%
  mutate(summary = map(losses, summarise_distribution)) %>%
  dplyr::select(scenario, summary) %>%
  unnest(summary)

print(cargo_stress_summary_2175)

cargo_stress_compare <- cargo_short_term_summary %>%
  dplyr::select(
    scenario,
    baseline_mean = mean,
    baseline_p95 = p95,
    baseline_p99 = p99
  ) %>%
  left_join(
    cargo_stress_summary_2175 %>%
      dplyr::select(
        scenario,
        stress_mean = mean,
        stress_p95 = p95,
        stress_p99 = p99
      ),
    by = "scenario"
  ) %>%
  mutate(
    mean_ratio = stress_mean / baseline_mean,
    p99_ratio = stress_p99 / baseline_p99
  )

print(cargo_stress_compare)

# ---- Annual yearly projections (kept for trend plots / checks) ----

years_to_model <- 2175:2184

yearly_projection_results <- map_dfr(years_to_model, function(yr) {
  
  macro_row <- macro_fc %>% filter(year == yr)
  
  cq_year <- build_projection_year(
    cq_df = cq_cargo_cells2,
    year_val = yr,
    macro_row = macro_row,
    macro_fc = macro_fc
  )
  
  scenario_specs %>%
    mutate(
      year = yr,
      inflation_rate = macro_row$inflation_rate,
      investment_rate = macro_row$investment_rate,
      losses = pmap(
        list(value_col, deductible_lr),
        ~ simulate_loss_distribution(
          cq_df = cq_year,
          sev_sigma = sev_sigma,
          value_col = ..1,
          deductible_lr = ..2,
          n_sim = n_sim
        )
      ),
      premium = map_dbl(
        losses,
        ~ calc_annual_premium(
          losses = .x,
          investment_rate = macro_row$investment_rate,
          expense_ratio = expense_ratio,
          pricing_quantile = pricing_quantile
        )
      ),
      net_rev_sims = map2(
        losses, premium,
        ~ simulate_net_revenue(
          losses = .x,
          premium_start = .y,
          investment_rate = macro_row$investment_rate,
          expense_ratio = expense_ratio
        )
      )
    ) %>%
    mutate(
      loss_summary   = map(net_rev_sims, ~ summarise_distribution(.x$loss_eoy)),
      cost_summary   = map(net_rev_sims, ~ summarise_distribution(.x$total_cost)),
      return_summary = map(net_rev_sims, ~ summarise_distribution(.x$investment_return)),
      netrev_summary = map(net_rev_sims, ~ summarise_distribution(.x$net_revenue)),
      total_shipments = sum(cq_year$n_shipments_year)
    ) %>%
    transmute(
      year,
      scenario,
      inflation_rate,
      investment_rate,
      total_shipments,
      premium,
      expected_expense = premium * expense_ratio,
      loss_summary,
      cost_summary,
      return_summary,
      netrev_summary
    ) %>%
    unnest_wider(loss_summary, names_sep = "_loss_") %>%
    unnest_wider(cost_summary, names_sep = "_cost_") %>%
    unnest_wider(return_summary, names_sep = "_return_") %>%
    unnest_wider(netrev_summary, names_sep = "_netrev_")
})

print(yearly_projection_results)

# ---- Discount factors to start of 2175 | These apply to END-OF-YEAR quantities ----

discount_tbl <- tibble(year = 2175:2184) %>%
  rowwise() %>%
  mutate(
    pv_factor_eoy = 1 / prod(1 + macro_fc$investment_rate[macro_fc$year >= 2175 & macro_fc$year <= year])
  ) %>%
  ungroup()

print(discount_tbl)

# ---- 10-year cumulative PV distributions (2175–2184) ----

ten_year_pv_sims <- map_dfr(unique(scenario_specs$scenario), function(sc) {
  
  scenario_years <- map_dfr(years_to_model, function(yr) {
    
    macro_row <- macro_fc %>% filter(year == yr)
    
    cq_year <- build_projection_year(
      cq_df = cq_cargo_cells2,
      year_val = yr,
      macro_row = macro_row,
      macro_fc = macro_fc
    )
    
    spec_row <- scenario_specs %>% filter(scenario == sc)
    
    losses <- simulate_loss_distribution(
      cq_df = cq_year,
      sev_sigma = sev_sigma,
      value_col = spec_row$value_col[[1]],
      deductible_lr = spec_row$deductible_lr[[1]],
      n_sim = n_sim
    )
    
    premium <- calc_annual_premium(
      losses = losses,
      investment_rate = macro_row$investment_rate,
      expense_ratio = expense_ratio,
      pricing_quantile = pricing_quantile
    )
    
    net_rev_tbl <- simulate_net_revenue(
      losses = losses,
      premium_start = premium,
      investment_rate = macro_row$investment_rate,
      expense_ratio = expense_ratio
    )
    
    pv_factor <- discount_tbl %>%
      filter(year == yr) %>%
      pull(pv_factor_eoy)
    
    tibble(
      scenario = sc,
      year = yr,
      sim_id = seq_len(n_sim),
      pv_loss = net_rev_tbl$loss_eoy * pv_factor,
      pv_cost = net_rev_tbl$total_cost * pv_factor,
      pv_netrev = net_rev_tbl$net_revenue * pv_factor
    )
  })
  
  scenario_years %>%
    group_by(scenario, sim_id) %>%
    summarise(
      pv_loss_10y = sum(pv_loss),
      pv_cost_10y = sum(pv_cost),
      pv_netrev_10y = sum(pv_netrev),
      .groups = "drop"
    )
})

print(ten_year_pv_sims)

# ---- Present-value adjustment for short-term (2175 annual) | Discount end-of-year 2175 quantities back to start of 2175 ----

pv_factor_2175 <- discount_tbl %>%
  filter(year == 2175) %>%
  pull(pv_factor_eoy)

short_term_pv_sims <- cargo_pricing_2175 %>%
  transmute(
    scenario,
    pv_loss = map(net_rev_sims, ~ .x$loss_eoy * pv_factor_2175),
    pv_cost = map(net_rev_sims, ~ .x$total_cost * pv_factor_2175),
    pv_net_revenue = map(net_rev_sims, ~ .x$net_revenue * pv_factor_2175),
    premium,
    expected_expense = premium * expense_ratio
  )

cargo_management_short_term <- short_term_pv_sims %>%
  mutate(
    loss_summary = map(pv_loss, summarise_distribution),
    cost_summary = map(pv_cost, summarise_distribution),
    netrev_summary = map(pv_net_revenue, summarise_distribution)
  ) %>%
  transmute(
    scenario,
    premium,
    expected_expense,
    loss_summary,
    cost_summary,
    netrev_summary
  ) %>%
  unnest_wider(loss_summary, names_sep = "_loss_") %>%
  unnest_wider(cost_summary, names_sep = "_cost_") %>%
  unnest_wider(netrev_summary, names_sep = "_netrev_") %>%
  transmute(
    scenario,
    premium,
    expected_expense,
    mean_loss = loss_summary_loss_mean,
    sd_loss = loss_summary_loss_sd,
    p95_loss = loss_summary_loss_p95,
    p99_loss = loss_summary_loss_p99,
    mean_cost = cost_summary_cost_mean,
    sd_cost = cost_summary_cost_sd,
    p95_cost = cost_summary_cost_p95,
    p99_cost = cost_summary_cost_p99,
    mean_net_revenue = netrev_summary_netrev_mean,
    sd_net_revenue = netrev_summary_netrev_sd,
    p5_net_revenue = netrev_summary_netrev_p5,
    p1_net_revenue = netrev_summary_netrev_p1,
    p95_net_revenue = netrev_summary_netrev_p95,
    p99_net_revenue = netrev_summary_netrev_p99
  )

print(cargo_management_short_term)

# ---- Long-term (10-year cumulative PV) summary ----

cargo_management_long_term_10y <- ten_year_pv_sims %>%
  group_by(scenario) %>%
  summarise(
    mean_loss = mean(pv_loss_10y),
    sd_loss = sd(pv_loss_10y),
    p95_loss = quantile(pv_loss_10y, 0.95),
    p99_loss = quantile(pv_loss_10y, 0.99),
    
    mean_cost = mean(pv_cost_10y),
    sd_cost = sd(pv_cost_10y),
    p95_cost = quantile(pv_cost_10y, 0.95),
    p99_cost = quantile(pv_cost_10y, 0.99),
    
    mean_net_revenue = mean(pv_netrev_10y),
    sd_net_revenue = sd(pv_netrev_10y),
    p5_net_revenue = quantile(pv_netrev_10y, 0.05),
    p1_net_revenue = quantile(pv_netrev_10y, 0.01),
    p95_net_revenue = quantile(pv_netrev_10y, 0.95),
    p99_net_revenue = quantile(pv_netrev_10y, 0.99),
    .groups = "drop"
  )

print(cargo_management_long_term_10y)

# ---- Final Section 3 summary table ----

section3_summary_table <- bind_rows(
  cargo_management_short_term %>%
    transmute(
      scenario,
      horizon = "Short term (2175 annual PV)",
      mean_loss,
      sd_loss,
      p99_loss,
      mean_cost,
      sd_cost,
      p99_cost,
      mean_net_revenue,
      sd_net_revenue,
      p1_net_revenue
    ),
  cargo_management_long_term_10y %>%
    transmute(
      scenario,
      horizon = "Long term (10-year cumulative PV)",
      mean_loss,
      sd_loss,
      p99_loss,
      mean_cost,
      sd_cost,
      p99_cost,
      mean_net_revenue,
      sd_net_revenue,
      p1_net_revenue
    )
)

print(section3_summary_table)

# ---- Diagnostic checks on long-term distributions ----

long_term_diag <- ten_year_pv_sims %>%
  group_by(scenario) %>%
  summarise(
    n_sims = n(),
    min_loss = min(pv_loss_10y),
    max_loss = max(pv_loss_10y),
    sd_loss = sd(pv_loss_10y),
    min_cost = min(pv_cost_10y),
    max_cost = max(pv_cost_10y),
    sd_cost = sd(pv_cost_10y),
    min_netrev = min(pv_netrev_10y),
    max_netrev = max(pv_netrev_10y),
    sd_netrev = sd(pv_netrev_10y),
    .groups = "drop"
  )

print(long_term_diag)

# ---- Individual histogram plots ----

report_blue_fill <- "#4F81BD"
report_blue_line <- "#2F5D8A"

plot_hist_with_lines <- function(df, xvar, title_text, x_text, scale_divisor = 1, bins = 50) {
  mean_val <- mean(df[[xvar]]) / scale_divisor
  p99_val <- quantile(df[[xvar]], 0.99) / scale_divisor
  p1_val <- quantile(df[[xvar]], 0.01) / scale_divisor
  
  ggplot(df, aes(x = .data[[xvar]] / scale_divisor)) +
    geom_histogram(bins = bins, fill = report_blue_fill, colour = report_blue_line) +
    geom_vline(xintercept = mean_val, linetype = "solid", linewidth = 0.8) +
    geom_vline(xintercept = p99_val, linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = p1_val, linetype = "dotted", linewidth = 0.8) +
    labs(
      title = title_text,
      x = x_text,
      y = "Frequency"
    ) +
    theme_minimal(base_size = 11)
}

# ---- Build short-term plotting datasets ----

short_base <- short_term_pv_sims %>% filter(scenario == "base")
short_supplies <- short_term_pv_sims %>% filter(scenario == "supplies")
short_precious <- short_term_pv_sims %>% filter(scenario == "precious")

short_base_df <- tibble(
  pv_loss = short_base$pv_loss[[1]],
  pv_cost = short_base$pv_cost[[1]],
  pv_net_revenue = short_base$pv_net_revenue[[1]]
)

short_supplies_df <- tibble(
  pv_loss = short_supplies$pv_loss[[1]],
  pv_cost = short_supplies$pv_cost[[1]],
  pv_net_revenue = short_supplies$pv_net_revenue[[1]]
)

short_precious_df <- tibble(
  pv_loss = short_precious$pv_loss[[1]],
  pv_cost = short_precious$pv_cost[[1]],
  pv_net_revenue = short_precious$pv_net_revenue[[1]]
)

# ---- Short-term histograms (annual PV, 2175) | Base ----

plot_hist_with_lines(
  short_base_df,
  "pv_loss",
  "Base Scenario: Short-Term Aggregate Loss Distribution",
  "Aggregate Loss (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_base_df,
  "pv_cost",
  "Base Scenario: Short-Term Total Cost Distribution",
  "Total Cost (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_base_df,
  "pv_net_revenue",
  "Base Scenario: Short-Term Net Revenue Distribution",
  "Net Revenue (Đ millions)",
  scale_divisor = 1e6
)

# Supplies
plot_hist_with_lines(
  short_supplies_df,
  "pv_loss",
  "Supplies Scenario: Short-Term Aggregate Loss Distribution",
  "Aggregate Loss (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_supplies_df,
  "pv_cost",
  "Supplies Scenario: Short-Term Total Cost Distribution",
  "Total Cost (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_supplies_df,
  "pv_net_revenue",
  "Supplies Scenario: Short-Term Net Revenue Distribution",
  "Net Revenue (Đ millions)",
  scale_divisor = 1e6
)

# Precious
plot_hist_with_lines(
  short_precious_df,
  "pv_loss",
  "Precious Scenario: Short-Term Aggregate Loss Distribution",
  "Aggregate Loss (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_precious_df,
  "pv_cost",
  "Precious Scenario: Short-Term Total Cost Distribution",
  "Total Cost (Đ millions)",
  scale_divisor = 1e6
)

plot_hist_with_lines(
  short_precious_df,
  "pv_net_revenue",
  "Precious Scenario: Short-Term Net Revenue Distribution",
  "Net Revenue (Đ millions)",
  scale_divisor = 1e6
)

# ---- Long-term histograms (10-year cumulative PV) ----

long_base_df <- ten_year_pv_sims %>% filter(scenario == "base")
long_supplies_df <- ten_year_pv_sims %>% filter(scenario == "supplies")
long_precious_df <- ten_year_pv_sims %>% filter(scenario == "precious")

# Base
plot_hist_with_lines(
  long_base_df,
  "pv_loss_10y",
  "Base Scenario: Long-Term Aggregate Loss Distribution",
  "10-Year Cumulative PV Loss (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_base_df,
  "pv_cost_10y",
  "Base Scenario: Long-Term Total Cost Distribution",
  "10-Year Cumulative PV Cost (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_base_df,
  "pv_netrev_10y",
  "Base Scenario: Long-Term Net Revenue Distribution",
  "10-Year Cumulative PV Net Revenue (Đ billions)",
  scale_divisor = 1e9
)

# Supplies
plot_hist_with_lines(
  long_supplies_df,
  "pv_loss_10y",
  "Supplies Scenario: Long-Term Aggregate Loss Distribution",
  "10-Year Cumulative PV Loss (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_supplies_df,
  "pv_cost_10y",
  "Supplies Scenario: Long-Term Total Cost Distribution",
  "10-Year Cumulative PV Cost (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_supplies_df,
  "pv_netrev_10y",
  "Supplies Scenario: Long-Term Net Revenue Distribution",
  "10-Year Cumulative PV Net Revenue (Đ billions)",
  scale_divisor = 1e9
)

# Precious
plot_hist_with_lines(
  long_precious_df,
  "pv_loss_10y",
  "Precious Scenario: Long-Term Aggregate Loss Distribution",
  "10-Year Cumulative PV Loss (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_precious_df,
  "pv_cost_10y",
  "Precious Scenario: Long-Term Total Cost Distribution",
  "10-Year Cumulative PV Cost (Đ billions)",
  scale_divisor = 1e9
)

plot_hist_with_lines(
  long_precious_df,
  "pv_netrev_10y",
  "Precious Scenario: Long-Term Net Revenue Distribution",
  "10-Year Cumulative PV Net Revenue (Đ billions)",
  scale_divisor = 1e9
)


# ---- 10-year line plots: base vs supplies only ----

trend_plot_df <- yearly_projection_results %>%
  filter(scenario %in% c("base", "supplies")) %>%
  mutate(
    scenario = factor(scenario, levels = c("base", "supplies"))
  )

report_blue_dark <- "#2F5D8A"
report_blue_light <- "#4F81BD"

scenario_cols <- c(
  "base" = report_blue_dark,
  "supplies" = report_blue_light
)

# 1. Annual premium
ggplot(trend_plot_df, aes(x = year, y = premium, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = scenario_cols) +
  labs(
    title = "Annual Premium by Scenario",
    x = "Year",
    y = "Premium"
  ) +
  theme_minimal(base_size = 11)

# 2. Expected annual loss
ggplot(trend_plot_df, aes(x = year, y = loss_summary_loss_mean, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = scenario_cols) +
  labs(
    title = "Expected Annual Cargo Loss by Scenario",
    x = "Year",
    y = "Expected Annual Loss"
  ) +
  theme_minimal(base_size = 11)

# 3. Expected annual total insurer cost
ggplot(trend_plot_df, aes(x = year, y = cost_summary_cost_mean, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = scenario_cols) +
  labs(
    title = "Expected Annual Total Cost by Scenario",
    x = "Year",
    y = "Expected Annual Total Cost"
  ) +
  theme_minimal(base_size = 11)

# 4. Expected annual net revenue
ggplot(trend_plot_df, aes(x = year, y = netrev_summary_netrev_mean, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = scenario_cols) +
  labs(
    title = "Expected Annual Net Revenue by Scenario",
    x = "Year",
    y = "Expected Annual Net Revenue"
  ) +
  theme_minimal(base_size = 11)


# ---- Cargo threat table by solar system and portfolio ----

cargo_threat_table <- tibble(
  threat_rank = 1:6,
  threat = c(
    "Oryn asymmetric ring instability",
    "Bayesia radiation spike events",
    "Helionis debris fragmentation and clutter",
    "Portfolio exposure growth over 10 years",
    "High-value precious cargo concentration",
    "Cross-system correlated event"
  ),
  most_affected_system = c(
    "Oryn",
    "Bayesia",
    "Helionis",
    "All systems",
    "All systems",
    "All systems"
  ),
  hazard_driver = c(
    "High route risk, orbital shear, low visibility, communication inconsistency",
    "Binary-star radiation spikes",
    "Dense asteroid clusters and shifting debris clouds",
    "25% growth in Helionis/Bayesia and 15% in Oryn",
    "Very high declared value per shipment",
    "Simultaneous deterioration in debris/radiation/routing conditions"
  ),
  evidence_from_model = c(
    "Oryn has the highest expected loss ratio and highest loss cost rate",
    "Stress scenario materially increases losses when radiation is shocked upward",
    "Helionis base assumptions already embed higher debris than Bayesia",
    "Premiums and losses rise steadily from 2175 to 2184 under annual repricing",
    "Precious scenario premium and loss are much larger relative to CQ financial scale",
    "Stress scenario lifts mean and p99 losses materially across all product scenarios"
  ),
  underwriting_response = c(
    "Higher rating tier, stricter underwriting terms, route controls",
    "Radiation-related operational conditions and monitoring requirements",
    "Debris-related navigation controls and operational safeguards",
    "Annual repricing and exposure-based scaling",
    "Remove from pooled cover; underwrite separately shipment by shipment",
    "Capital buffer, stress testing, possible facultative reinsurance"
  )
)

cargo_threat_table

# ---- Solar-system risk narrative support table ----

cargo_system_risk_table <- cq_by_system %>%
  transmute(
    solar_system,
    shipments,
    expected_claims,
    annual_value_base,
    exp_loss_base,
    loss_cost_rate_base,
    qualitative_risk_view = case_when(
      solar_system == "Helionis" ~ "Moderate-high: stable star but elevated debris hazard from asteroid clusters",
      solar_system == "Bayesia"  ~ "Moderate: higher radiation risk offset by standardized routes and infrastructure",
      solar_system == "Oryn"     ~ "Highest: asymmetric ring, low visibility, unstable routes, rare-metal expansion"
    )
  )

cargo_system_risk_table


# Create tidy year-1 simulation results for plotting
cargo_sim_2175_tidy <- cargo_pricing_2175 %>%
  dplyr::select(scenario, premium, net_rev_sims) %>%
  mutate(sim_id = map(net_rev_sims, ~ seq_len(nrow(.x)))) %>%
  unnest(c(net_rev_sims, sim_id)) %>%
  rename(
    loss_nominal = loss,
    loss_eoy = loss_eoy,
    net_revenue = net_revenue
  )

cargo_sim_2175_tidy <- cargo_sim_2175_tidy %>%
  mutate(
    profitable = net_revenue > 0
  )

ggplot(cargo_sim_2175_tidy, aes(x = loss_eoy, colour = scenario, fill = scenario)) +
  geom_density(alpha = 0.15) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution",
    x = "Annual Loss (end-of-year basis)",
    y = "Density"
  )


ggplot(filter(cargo_sim_2175_tidy, scenario == "base"), aes(x = loss_eoy)) +
  geom_density(fill = "lightblue", alpha = 0.4) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution - Base",
    x = "Annual Loss (end-of-year basis)",
    y = "Density"
  )

ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"), aes(x = loss_eoy)) +
  geom_density(fill = "lightblue", alpha = 0.4) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution - Supplies",
    x = "Annual Loss (end-of-year basis)",
    y = "Density"
  )

ggplot(filter(cargo_sim_2175_tidy, scenario == "precious"), aes(x = loss_eoy)) +
  geom_density(fill = "lightblue", alpha = 0.4) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution - Precious",
    x = "Annual Loss (end-of-year basis)",
    y = "Density"
  )


ggplot(filter(cargo_sim_2175_tidy, scenario == "base"), aes(x = net_revenue)) +
  geom_density(fill = "lightgreen", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Year 2175 Net Revenue Distribution - Base",
    x = "Net Revenue (end-of-year basis)",
    y = "Density"
  )

ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"), aes(x = net_revenue)) +
  geom_density(fill = "lightgreen", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Year 2175 Net Revenue Distribution - Supplies",
    x = "Net Revenue (end-of-year basis)",
    y = "Density"
  )

ggplot(filter(cargo_sim_2175_tidy, scenario == "precious"), aes(x = net_revenue)) +
  geom_density(fill = "lightgreen", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Year 2175 Net Revenue Distribution - Precious",
    x = "Net Revenue (end-of-year basis)",
    y = "Density"
  )


cargo_sim_2175_tidy %>%
  group_by(scenario) %>%
  summarise(
    prob_negative_net_revenue = mean(net_revenue < 0),
    prob_positive_net_revenue = mean(net_revenue > 0)
  )


ggplot(yearly_projection_results,
       aes(x = year, colour = scenario)) +
  geom_line(aes(y = loss_summary_loss_mean), linewidth = 1) +
  geom_line(aes(y = premium), linetype = "dashed") +
  labs(
    title = "Expected Loss and Premium Path Over Time",
    x = "Year",
    y = "Amount"
  )


ggplot(yearly_projection_results,
       aes(x = year, y = netrev_summary_netrev_mean,
           colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Expected Net Revenue Over Time",
    x = "Year",
    y = "Expected Net Revenue"
  )


cargo_stress_compare_long <- cargo_stress_compare %>%
  pivot_longer(
    cols = c(mean_ratio, p99_ratio),
    names_to = "metric",
    values_to = "ratio"
  )

ggplot(cargo_stress_compare_long,
       aes(x = scenario, y = ratio, fill = metric)) +
  geom_col(position = "dodge") +
  labs(
    title = "Stress Scenario Loss Uplift",
    x = "Scenario",
    y = "Stress / Baseline"
  )


ggplot(cargo_system_risk_table,
       aes(x = solar_system, y = loss_cost_rate_base)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Loss Cost Rate by Solar System",
    x = "Solar System",
    y = "Loss Cost Rate"
  )


projection_plot_df <- yearly_projection_results %>%
  filter(scenario %in% c("base", "supplies"))

ggplot(projection_plot_df, aes(x = year, colour = scenario)) +
  geom_line(aes(y = loss_summary_loss_mean), linewidth = 1) +
  geom_line(aes(y = premium), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = loss_summary_loss_mean)) +
  geom_point(aes(y = premium), shape = 1) +
  labs(
    title = "Expected Loss and Premium Path Over Time",
    x = "Year",
    y = "Amount"
  )


projection_loss_premium_long <- yearly_projection_results %>%
  filter(scenario %in% c("base", "supplies")) %>%
  transmute(
    year,
    scenario,
    expected_loss = loss_summary_loss_mean,
    premium = premium
  ) %>%
  pivot_longer(
    cols = c(expected_loss, premium),
    names_to = "metric",
    values_to = "amount"
  )

ggplot(projection_loss_premium_long,
       aes(x = year, y = amount, colour = scenario, linetype = metric)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Expected Loss and Premium Path Over Time",
    x = "Year",
    y = "Amount",
    linetype = ""
  )


ggplot(projection_plot_df,
       aes(x = year, y = netrev_summary_netrev_mean, colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Expected Net Revenue Over Time",
    x = "Year",
    y = "Expected Net Revenue"
  )


projection_netrev_long <- yearly_projection_results %>%
  filter(scenario %in% c("base", "supplies")) %>%
  transmute(
    year,
    scenario,
    mean_net_revenue = netrev_summary_netrev_mean,
    adverse_net_revenue = netrev_summary_netrev_p1
  ) %>%
  pivot_longer(
    cols = c(mean_net_revenue, adverse_net_revenue),
    names_to = "metric",
    values_to = "amount"
  )

ggplot(projection_netrev_long,
       aes(x = year, y = amount, colour = scenario, linetype = metric)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Expected and Adverse Net Revenue Over Time",
    x = "Year",
    y = "Net Revenue",
    linetype = ""
  )


# corr stress test

base_baseline_losses <- loss_sims_2175 %>%
  filter(scenario == "base") %>%
  pull(losses) %>%
  .[[1]]

base_stress_losses <- stress_sims_2175 %>%
  filter(scenario == "base") %>%
  pull(losses) %>%
  .[[1]]

base_stress_plot_df <- tibble(
  loss = c(base_baseline_losses, base_stress_losses),
  case = rep(c("Baseline", "Stress"),
             each = length(base_baseline_losses))
)

ggplot(base_stress_plot_df, aes(x = loss, colour = case, fill = case)) +
  geom_density(alpha = 0.20) +
  labs(
    title = "Base Cargo: Baseline vs Correlated Stress Loss Distribution",
    x = "Annual Loss",
    y = "Density"
  )


supplies_baseline_losses <- loss_sims_2175 %>%
  filter(scenario == "supplies") %>%
  pull(losses) %>%
  .[[1]]

supplies_stress_losses <- stress_sims_2175 %>%
  filter(scenario == "supplies") %>%
  pull(losses) %>%
  .[[1]]

supplies_stress_plot_df <- tibble(
  loss = c(supplies_baseline_losses, supplies_stress_losses),
  case = rep(c("Baseline", "Stress"),
             each = length(supplies_baseline_losses))
)

ggplot(supplies_stress_plot_df, aes(x = loss, colour = case, fill = case)) +
  geom_density(alpha = 0.20) +
  labs(
    title = "Cargo + Supplies: Baseline vs Correlated Stress Loss Distribution",
    x = "Annual Loss",
    y = "Density"
  )


cargo_negative_netrev_table_2175 <- cargo_pricing_2175 %>%
  transmute(
    scenario,
    prob_negative_net_revenue = map_dbl(net_rev_sims, ~ mean(.x$net_revenue < 0)),
    prob_positive_net_revenue = map_dbl(net_rev_sims, ~ mean(.x$net_revenue > 0)),
    prob_break_even_or_better = map_dbl(net_rev_sims, ~ mean(.x$net_revenue >= 0))
  )

cargo_negative_netrev_table_2175


ggplot(filter(cargo_sim_2175_tidy, scenario == "base"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black", alpha = 0.8) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Base Scenario",
    x = "Annual Loss (end-of-year basis)",
    y = "Frequency"
  )


ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "darkorange", colour = "black", alpha = 0.8) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Cargo + Supplies",
    x = "Annual Loss (end-of-year basis)",
    y = "Frequency"
  )

ggplot(filter(cargo_sim_2175_tidy, scenario == "precious"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "purple", colour = "black", alpha = 0.8) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Precious Cargo",
    x = "Annual Loss (end-of-year basis)",
    y = "Frequency"
  )


# net rev plots
ggplot(filter(cargo_sim_2175_tidy, scenario == "base"),
       aes(x = net_revenue)) +
  geom_histogram(bins = 80, fill = "darkgreen", colour = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Year 2175 Net Revenue Distribution – Base Cargo Scenario",
    x = "Net Revenue (end-of-year basis)",
    y = "Frequency"
  )


ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"),
       aes(x = net_revenue)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Year 2175 Net Revenue Distribution – Cargo + Supplies Scenario",
    x = "Net Revenue (end-of-year basis)",
    y = "Frequency"
  )


theme_report <- theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.margin = margin(10, 15, 10, 15)
  )


ggplot(filter(cargo_sim_2175_tidy, scenario == "base"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black", alpha = 0.85) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Base Scenario",
    x = "Annual Loss (End-of-Year Basis)",
    y = "Frequency"
  ) +
  theme_report

ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "darkorange", colour = "black", alpha = 0.85) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Cargo + Supplies",
    x = "Annual Loss (End-of-Year Basis)",
    y = "Frequency"
  ) +
  theme_report

ggplot(filter(cargo_sim_2175_tidy, scenario == "precious"),
       aes(x = loss_eoy)) +
  geom_histogram(bins = 80, fill = "purple", colour = "black", alpha = 0.85) +
  labs(
    title = "Year 2175 Aggregate Annual Cargo Loss Distribution – Precious Cargo",
    x = "Annual Loss (End-of-Year Basis)",
    y = "Frequency"
  ) +
  theme_report

ggplot(filter(cargo_sim_2175_tidy, scenario == "base"),
       aes(x = net_revenue)) +
  geom_histogram(bins = 80, fill = "darkgreen", colour = "black", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Year 2175 Net Revenue Distribution – Base Cargo Scenario",
    x = "Net Revenue (End-of-Year Basis)",
    y = "Frequency"
  ) +
  theme_report

ggplot(filter(cargo_sim_2175_tidy, scenario == "supplies"),
       aes(x = net_revenue)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "red", linewidth = 1) +
  labs(
    title = "Year 2175 Net Revenue Distribution – Cargo + Supplies Scenario",
    x = "Net Revenue (End-of-Year Basis)",
    y = "Frequency"
  ) +
  theme_report

