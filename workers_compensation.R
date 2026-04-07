## Install packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("stringr")
install.packages("MASS")
install.packages("evir")
install.packages("readxl")

library(dplyr)
library(tidyverse)
library(stringr)
library(MASS)
library(evir)
library(readxl)


## Read Worker's Compensation data
workcomp_freq_raw <- read.csv("/Users/irisbaek/actl4001-26t1/datasets/srcsc-2026-claims-workers-comp-freq.csv")
workcomp_sev_raw <- read.csv("/Users/irisbaek/actl4001-26t1/datasets/srcsc-2026-claims-workers-comp-sev.csv")

## Function to clean names
clean_id <- function(x) str_trim(str_replace(as.character(x), "_.*$", ""))
clean_cat <- function(x) str_trim(str_replace(as.character(x), "_.*$", ""))

## Fix types and clean names
workcomp_freq <- workcomp_freq_raw %>%
  mutate(
    policy_id       = clean_id(policy_id),
    worker_id       = clean_id(worker_id),
    solar_system    = clean_cat(solar_system),
    station_id      = clean_cat(station_id),
    occupation      = clean_cat(occupation),
    employment_type = clean_cat(employment_type)
  )

workcomp_sev <- workcomp_sev_raw %>%
  mutate(
    policy_id       = clean_id(policy_id),
    worker_id       = clean_id(worker_id),
    solar_system    = clean_cat(solar_system),
    station_id      = clean_cat(station_id),
    occupation      = clean_cat(occupation),
    employment_type = clean_cat(employment_type)
  )

## Fix variables
id_cols     <- c("policy_id","worker_id")
cat_cols    <- c("solar_system","station_id","occupation","employment_type","injury_type","injury_cause")
num_cols    <- c("experience_yrs","hours_per_week","supervision_level","gravity_level","base_salary","exposure",
                 "claim_length","claim_amount")
int_cols    <- c("claim_count")
factor_cols <- c("accident_history_flag","psych_stress_index","safety_training_index","protective_gear_quality")

workcomp_freq <- workcomp_freq %>%
  mutate(
    across(any_of(id_cols), as.character),
    across(any_of(cat_cols), as.factor),
    across(any_of(num_cols), as.numeric),
    across(any_of(int_cols), as.integer),
    across(any_of(factor_cols), as.factor)
  )

workcomp_sev <- workcomp_sev %>%
  mutate(
    across(any_of(id_cols), as.character),
    across(any_of(cat_cols), as.factor),
    across(any_of(num_cols), as.numeric),
    across(any_of(int_cols), as.integer),
    across(any_of(factor_cols), as.factor)
  )

glimpse(workcomp_freq)
glimpse(workcomp_sev)

## Missingness
missing_summary <- function(df) {
  out <- data.frame(
    variable    = names(df),
    n_missing   = colSums(is.na(df)),
    pct_missing = colMeans(is.na(df)),
    row.names = NULL
  )
  out[order(-out$n_missing), ]
}

miss_freq <- missing_summary(workcomp_freq)
miss_sev  <- missing_summary(workcomp_sev)

miss_freq
miss_sev

## Set ranges
range_spec <- data.frame(
  var = c("experience_yrs", "supervision_level", "gravity_level", "base_salary", "exposure",
          "claim_count", "claim_length", "claim_amount"),
  min = c(0.2, 0, 0.75, 20000, 0, 0, 3, 0),
  max = c(40, 1, 1.50, 130000, 1, 2, 1000, Inf),
  stringsAsFactors = FALSE
)

allowed_spec <- list(
  hours_per_week          = c(20,25,30,35,40),
  accident_history_flag   = c("0","1"),
  psych_stress_index      = as.character(1:5),
  safety_training_index   = as.character(1:5),
  protective_gear_quality = as.character(1:5)
)

## Ranges and allowed values check
range_bad_vec <- function(df, var, min, max) {
  if (!var %in% names(df)) return(rep(FALSE, nrow(df)))
  x <- df[[var]]
  is.na(x) | x < min | x > max
}

allowed_bad_vec <- function(df, var, allowed) {
  if (!var %in% names(df)) return(rep(FALSE, nrow(df)))
  x <- df[[var]]
  is.na(x) | !(as.character(x) %in% allowed)
}

build_check_matrix <- function(df) {
  checks <- list()
  
  for (i in seq_len(nrow(range_spec))) {
    v <- range_spec$var[i]
    checks[[paste0("bad_", v)]] <- range_bad_vec(df, v, range_spec$min[i], range_spec$max[i])
  }
  
  for (v in names(allowed_spec)) {
    checks[[paste0("bad_", v)]] <- allowed_bad_vec(df, v, allowed_spec[[v]])
  }
  
  as.data.frame(checks)
}

flag_and_report <- function(df) {
  check_mat <- build_check_matrix(df)
  bad_row <- Reduce(`|`, check_mat)
  
  check_counts <- sort(colSums(check_mat), decreasing = TRUE)
  report <- data.frame(
    check = names(check_counts),
    n_bad = as.integer(check_counts),
    row.names = NULL
  )
  
  list(check_mat = check_mat, bad_row = bad_row, report = report)
}

freq_qc <- flag_and_report(workcomp_freq)
sev_qc  <- flag_and_report(workcomp_sev)

freq_qc$report
sev_qc$report

## Overall summary
c(n_total = nrow(workcomp_freq), n_bad = sum(freq_qc$bad_row), pct_bad = mean(freq_qc$bad_row))
c(n_total = nrow(workcomp_sev),  n_bad = sum(sev_qc$bad_row),  pct_bad = mean(sev_qc$bad_row))

## Remove bad rows
workcomp_freq_flagged <- workcomp_freq
workcomp_freq_flagged$bad_row <- freq_qc$bad_row

workcomp_sev_flagged <- workcomp_sev
workcomp_sev_flagged$bad_row <- sev_qc$bad_row

workcomp_freq_clean <- workcomp_freq_flagged[!workcomp_freq_flagged$bad_row, ]
workcomp_sev_clean  <- workcomp_sev_flagged[!workcomp_sev_flagged$bad_row, ]

## Drop flag column
workcomp_freq_clean$bad_row <- NULL
workcomp_sev_clean$bad_row  <- NULL

## Checking
dim(workcomp_freq)
dim(workcomp_freq_clean)

dim(workcomp_sev)
dim(workcomp_sev_clean)


##---------------------------------------------------------------------------------
##--------------------------------- Modelling -------------------------------------
##--------------------------------- Frequency ------------------------------------
freq_df <- workcomp_freq_clean %>%
  filter(!is.na(exposure), exposure > 0) %>%
  mutate(
    log_base_salary = log(base_salary),
    c_experience_yrs    = experience_yrs - mean(experience_yrs, na.rm = TRUE),
    c_hours_per_week    = hours_per_week - mean(hours_per_week, na.rm = TRUE),
    c_supervision_level = supervision_level - mean(supervision_level, na.rm = TRUE),
    c_gravity_level     = gravity_level - mean(gravity_level, na.rm = TRUE),
    c_log_base_salary   = log_base_salary - mean(log_base_salary, na.rm = TRUE)
  )

glimpse(freq_df)

## Baseline Poisson
pois0 <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_df
)

summary(pois0)
exp(coef(pois0))

## Full Poisson model
pois_full <- glm(
  claim_count ~ occupation + employment_type +
    c_experience_yrs + c_hours_per_week + c_supervision_level + c_gravity_level +
    accident_history_flag + psych_stress_index +
    safety_training_index + protective_gear_quality +
    c_log_base_salary + solar_system + offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_df
)

summary(pois_full)

## Check for overdispersion
dispersion_pois <- sum(residuals(pois_full, type = "pearson")^2) / df.residual(pois_full)
dispersion_pois

## Backward selection
pois_back <- step(pois_full, direction = "backward", trace = 1)
summary(pois_back)

AIC(pois0, pois_full, pois_back)

## Zero check
obs_zero <- mean(freq_df$claim_count == 0)
mu_back <- fitted(pois_back)
exp_zero <- mean(exp(-mu_back))

c(obs_zero = obs_zero, exp_zero = exp_zero, diff = obs_zero - exp_zero)


##------------------------------------------------------------------------------
##--------------------------------- Severity ------------------------------------
##------------------------------------------------------------------------------
sev_df <- workcomp_sev_clean %>%
  mutate(
    claim_amount_use = claim_amount,
    log_claim_amount = log(claim_amount_use),
    log_base_salary = log(base_salary),
    c_experience_yrs    = experience_yrs - mean(experience_yrs, na.rm = TRUE),
    c_hours_per_week    = hours_per_week - mean(hours_per_week, na.rm = TRUE),
    c_supervision_level = supervision_level - mean(supervision_level, na.rm = TRUE),
    c_gravity_level     = gravity_level - mean(gravity_level, na.rm = TRUE),
    c_log_base_salary   = log_base_salary - mean(log_base_salary, na.rm = TRUE)
  )

glimpse(sev_df)
summary(sev_df$claim_amount_use)

## Distributional characteristic
ggplot(sev_df, aes(x = log_claim_amount)) +
  geom_histogram(bins = 100) +
  theme_minimal()

## Lognormal body model
lnorm_mod <- lm(
  log_claim_amount ~ occupation + employment_type +
    c_experience_yrs + c_hours_per_week + c_supervision_level + c_gravity_level +
    accident_history_flag + psych_stress_index +
    safety_training_index + protective_gear_quality +
    c_log_base_salary + solar_system,
  data = sev_df
)

summary(lnorm_mod)

## EVT tail fit
sev_df2 <- sev_df %>%
  filter(!is.na(claim_amount_use), claim_amount_use > 0)

u <- as.numeric(quantile(sev_df2$claim_amount_use, 0.95))
u

gpd_fit <- evir::gpd(sev_df2$claim_amount_use, threshold = u)
gpd_fit

xi   <- unname(gpd_fit$par.ests["xi"])
beta <- unname(gpd_fit$par.ests["beta"])
c(xi = xi, beta = beta)

p_tail <- mean(sev_df2$claim_amount_use > u)
p_tail

## Strict body-tail splice helpers
## Truncated lognormal sampler on (0, upper]
rlnorm_trunc_upper <- function(n, meanlog, sdlog, upper) {
  if (n <= 0) return(numeric(0))
  
  if (length(meanlog) == 1) {
    meanlog <- rep(meanlog, n)
  }
  
  p_upper <- plnorm(upper, meanlog = meanlog, sdlog = sdlog)
  p_upper <- pmin(pmax(p_upper, .Machine$double.eps), 1 - .Machine$double.eps)
  
  u_rand <- runif(n)
  qlnorm(u_rand * p_upper, meanlog = meanlog, sdlog = sdlog)
}

## Draw meanlog values
draw_meanlog_source <- function(n, meanlog_source) {
  if (length(meanlog_source) == 1) {
    rep(meanlog_source, n)
  } else {
    sample(meanlog_source, size = n, replace = TRUE)
  }
}

## Strict spliced severity sampler
rsev_spliced <- function(n,
                         meanlog_source,
                         sdlog,
                         p_tail,
                         u,
                         xi,
                         beta,
                         severity_scale = 1) {
  if (n <= 0) return(numeric(0))
  
  is_tail <- runif(n) < p_tail
  n_tail <- sum(is_tail)
  n_body <- n - n_tail
  
  out <- numeric(n)
  
  if (n_body > 0) {
    meanlog_body <- draw_meanlog_source(n_body, meanlog_source)
    out[!is_tail] <- rlnorm_trunc_upper(
      n = n_body,
      meanlog = meanlog_body,
      sdlog = sdlog,
      upper = u
    )
  }
  
  if (n_tail > 0) {
    out[is_tail] <- u + evir::rgpd(n_tail, xi = xi, beta = beta)
  }
  
  out * severity_scale
}

## Severity simulation
sdlog <- summary(lnorm_mod)$sigma
meanlog_i <- predict(lnorm_mod, newdata = sev_df2)

set.seed(123)

sim_sev <- rsev_spliced(
  n = 5000,
  meanlog_source = meanlog_i,
  sdlog = sdlog,
  p_tail = p_tail,
  u = u,
  xi = xi,
  beta = beta
)

## Check body truncation
sim_body_only <- rlnorm_trunc_upper(
  n = 5000,
  meanlog = draw_meanlog_source(5000, meanlog_i),
  sdlog = sdlog,
  upper = u
)

max(sim_body_only)
all(sim_body_only <= u)

quantile(sev_df2$claim_amount_use, probs = c(.9, .95, .99))
quantile(sim_sev, probs = c(.9, .95, .99))

hist(sim_sev,
     breaks = 60,
     main = "Simulated severity (Truncated Lognormal body + GPD tail)",
     xlab = "Claim amount")

## Tail QQ plot
tail_data <- sev_df2$claim_amount_use[sev_df2$claim_amount_use > u]
excess <- tail_data - u

q <- seq(0.05, 0.95, by = 0.05)
emp_ex <- quantile(excess, q, na.rm = TRUE)
gpd_ex <- evir::qgpd(q, xi = xi, beta = beta)

plot(gpd_ex, emp_ex,
     main = "Tail QQ: Empirical exceedances vs GPD",
     xlab = "GPD exceedance quantiles",
     ylab = "Empirical exceedance quantiles",
     asp = 1)
abline(0, 1, col = "red")



##------------------------------------------------------------------------------
##-------------------------------- Synthetic data -------------------------------
##------------------------------------------------------------------------------
## Simple CQ model (occupation + employment type + salary + experience)
freq_train <- workcomp_freq_clean %>%
  filter(!is.na(exposure), exposure > 0) %>%
  mutate(
    log_base_salary = log(base_salary),
    occupation = droplevels(occupation),
    employment_type = droplevels(employment_type),
    experience_yrs = as.numeric(experience_yrs)
  )

pois_cq <- glm(
  claim_count ~ occupation + employment_type + log_base_salary + experience_yrs +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_train
)
summary(pois_cq)

sev_train <- workcomp_sev_clean %>%
  filter(!is.na(claim_amount), claim_amount > 0) %>%
  mutate(
    claim_amount_use = claim_amount,
    log_claim_amount = log(claim_amount_use),
    log_base_salary = log(base_salary),
    occupation = droplevels(occupation),
    employment_type = droplevels(employment_type),
    experience_yrs = as.numeric(experience_yrs)
  )

lnorm_cq <- lm(
  log_claim_amount ~ occupation + employment_type + log_base_salary + experience_yrs,
  data = sev_train
)
summary(lnorm_cq)

sdlog_cq <- summary(lnorm_cq)$sigma

u_cq <- as.numeric(quantile(sev_train$claim_amount_use, 0.95))
gpd_cq <- evir::gpd(sev_train$claim_amount_use, threshold = u_cq)

xi_cq   <- unname(gpd_cq$par.ests["xi"])
beta_cq <- unname(gpd_cq$par.ests["beta"])
p_tail_cq <- mean(sev_train$claim_amount_use > u_cq)

## Read Cosmic Quarry data
pers_path <- "/Users/irisbaek/actl4001-26t1/datasets/srcsc-2026-cosmic-quarry-personnel.xlsx"

pers_raw <- read_excel(pers_path, sheet = "Personnel", skip = 2)
names(pers_raw) <- c("role", "n_total", "n_fulltime", "n_contract", "avg_salary", "avg_age")

pers <- pers_raw %>%
  filter(!is.na(n_total), !is.na(avg_salary)) %>%
  mutate(
    role = as.character(role),
    n_fulltime = as.integer(n_fulltime),
    n_contract = as.integer(n_contract),
    avg_salary = as.numeric(avg_salary)
  )

occ_levels <- levels(freq_train$occupation)
emp_levels <- levels(freq_train$employment_type)

## Map CQ roles -> historical occupations
role_to_occ <- function(role) {
  dplyr::case_when(
    role == "Engineers" ~ "Engineer",
    role == "Drilling operators" ~ "Drill Operator",
    role == "Maintenance" ~ "Maintenance Staff",
    role %in% c("Director") ~ "Manager",
    role %in% c("Executive", "Vice President") ~ "Executive",
    role %in% c("HR","Legal","Finance & Accounting","Steward","Galleyhand","Medical Personel") ~ "Administrator",
    role %in% c("IT","Robotics technician") ~ "Technology Officer",
    role %in% c("Environmental Scientists","Geoligist","Scientist") ~ "Scientist",
    role == "Safety Officer" ~ "Safety Officer",
    role %in% c("Field technician","Security personel") ~ "Planetary Operations",
    role %in% c("Freight operators","Navigation officers") ~ "Spacecraft Operator",
    TRUE ~ "Administrator"
  )
}

pers <- pers %>%
  mutate(
    occupation = role_to_occ(role),
    occupation = if_else(!(occupation %in% occ_levels), occ_levels[1], occupation),
    occupation = factor(occupation, levels = occ_levels)
  )


##------------------------------------------------------------------------------
##---------------------------- Build risk groups -------------------------------
##------------------------------------------------------------------------------

start_age <- 25

## Deployment frequency training data
freq_train2 <- workcomp_freq_clean %>%
  dplyr::filter(!is.na(exposure), exposure > 0) %>%
  dplyr::mutate(
    log_base_salary = log(base_salary),
    psych_stress_index      = factor(as.character(psych_stress_index), levels = as.character(1:5)),
    safety_training_index   = factor(as.character(safety_training_index), levels = as.character(1:5)),
    occupation      = droplevels(occupation),
    employment_type = droplevels(employment_type),
    experience_yrs  = as.numeric(experience_yrs)
  ) %>%
  dplyr::filter(
    !is.na(claim_count),
    !is.na(base_salary),
    !is.na(experience_yrs),
    !is.na(psych_stress_index),
    !is.na(safety_training_index)
  )

pois_cq2 <- glm(
  claim_count ~ occupation + employment_type + log_base_salary + experience_yrs +
    psych_stress_index + safety_training_index +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_train2
)
summary(pois_cq2)

## Deployment severity training data
sev_train2 <- workcomp_sev_clean %>%
  dplyr::filter(!is.na(claim_amount), claim_amount > 0) %>%
  dplyr::mutate(
    claim_amount_use = claim_amount,
    log_claim_amount = log(claim_amount_use),
    log_base_salary = log(base_salary),
    psych_stress_index      = factor(as.character(psych_stress_index), levels = as.character(1:5)),
    safety_training_index   = factor(as.character(safety_training_index), levels = as.character(1:5)),
    occupation      = droplevels(occupation),
    employment_type = droplevels(employment_type),
    experience_yrs  = as.numeric(experience_yrs)
  ) %>%
  dplyr::filter(
    !is.na(log_claim_amount),
    !is.na(base_salary),
    !is.na(experience_yrs),
    !is.na(psych_stress_index),
    !is.na(safety_training_index)
  )

lnorm_cq2 <- lm(
  log_claim_amount ~ occupation + employment_type + log_base_salary + experience_yrs +
    psych_stress_index + safety_training_index,
  data = sev_train2
)
summary(lnorm_cq2)

sdlog_cq2 <- summary(lnorm_cq2)$sigma

## Historical median psych/safety by risk group
freq_idx <- freq_train2 %>%
  dplyr::mutate(
    psych_num  = as.numeric(as.character(psych_stress_index)),
    safety_num = as.numeric(as.character(safety_training_index))
  )

hist_idx_by_group <- freq_idx %>%
  dplyr::group_by(occupation, employment_type) %>%
  dplyr::summarise(
    n_policies = dplyr::n(),
    psych_median  = median(psych_num,  na.rm = TRUE),
    safety_median = median(safety_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    psych_assign  = pmin(5L, pmax(1L, as.integer(round(psych_median)))),
    safety_assign = pmin(5L, pmax(1L, as.integer(round(safety_median))))
  )

hist_idx_by_occ <- freq_idx %>%
  dplyr::group_by(occupation) %>%
  dplyr::summarise(
    psych_occ_median  = median(psych_num,  na.rm = TRUE),
    safety_occ_median = median(safety_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    psych_occ_assign  = pmin(5L, pmax(1L, as.integer(round(psych_occ_median)))),
    safety_occ_assign = pmin(5L, pmax(1L, as.integer(round(safety_occ_median))))
  )

psych_overall  <- pmin(5L, pmax(1L, as.integer(round(median(freq_idx$psych_num,  na.rm = TRUE)))))
safety_overall <- pmin(5L, pmax(1L, as.integer(round(median(freq_idx$safety_num, na.rm = TRUE)))))

psych_overall
safety_overall

## Build CQ workers' compensation risk groups
emp_levels <- levels(freq_train2$employment_type)
occ_levels <- levels(freq_train2$occupation)

cq_wc_groups2 <- pers %>%
  dplyr::mutate(
    occupation = factor(occupation, levels = occ_levels)
  ) %>%
  dplyr::group_by(occupation) %>%
  dplyr::summarise(
    n_fulltime = sum(n_fulltime, na.rm = TRUE),
    n_contract = sum(n_contract, na.rm = TRUE),
    avg_salary = stats::weighted.mean(avg_salary, w = n_total, na.rm = TRUE),
    avg_age    = stats::weighted.mean(avg_age,    w = n_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c(n_fulltime, n_contract),
    names_to = "type_col",
    values_to = "n_employees"
  ) %>%
  dplyr::filter(!is.na(n_employees), n_employees > 0) %>%
  dplyr::mutate(
    employment_type = dplyr::if_else(type_col == "n_fulltime", "Full-time", "Contract"),
    employment_type = factor(employment_type, levels = emp_levels),
    base_salary = avg_salary,
    log_base_salary = log(base_salary),
    experience_yrs = pmax(0.2, pmin(40, avg_age - start_age))
  ) %>%
  dplyr::select(occupation, employment_type, n_employees, avg_salary, avg_age,
                base_salary, log_base_salary, experience_yrs)

## Assign psych/safety using historical medians
cq_wc_groups2 <- cq_wc_groups2 %>%
  dplyr::left_join(
    hist_idx_by_group %>% dplyr::select(occupation, employment_type, psych_assign, safety_assign),
    by = c("occupation", "employment_type")
  ) %>%
  dplyr::left_join(
    hist_idx_by_occ %>% dplyr::select(occupation, psych_occ_assign, safety_occ_assign),
    by = "occupation"
  ) %>%
  dplyr::mutate(
    psych_assign  = dplyr::coalesce(psych_assign,  psych_occ_assign,  psych_overall),
    safety_assign = dplyr::coalesce(safety_assign, safety_occ_assign, safety_overall),
    psych_stress_index    = factor(as.character(psych_assign),  levels = as.character(1:5)),
    safety_training_index = factor(as.character(safety_assign), levels = as.character(1:5))
  ) %>%
  dplyr::select(-psych_occ_assign, -safety_occ_assign)

cq_wc_groups2 %>%
  dplyr::summarise(
    any_na_psych  = any(is.na(psych_stress_index)),
    any_na_safety = any(is.na(safety_training_index))
  )

## Predict frequency and severity for each group
cq_pred_df <- cq_wc_groups2 %>%
  dplyr::mutate(exposure = 1)

cq_wc_groups2 <- cq_wc_groups2 %>%
  dplyr::mutate(
    lambda_group = predict(pois_cq2, newdata = cq_pred_df, type = "response"),
    mu_group     = lambda_group * n_employees,
    meanlog_group = predict(lnorm_cq2, newdata = cq_pred_df, type = "response")
  )

cq_wc_groups2 %>%
  dplyr::arrange(dplyr::desc(lambda_group)) %>%
  dplyr::select(occupation, employment_type, n_employees,
                psych_stress_index, safety_training_index,
                lambda_group, mu_group, meanlog_group) %>%
  print(n = Inf)

cq_wc_groups2 %>%
  dplyr::summarise(
    total_employees = sum(n_employees),
    total_expected_claims = sum(mu_group),
    avg_lambda = weighted.mean(lambda_group, w = n_employees)
  )


##------------------------------------------------------------------------------
##--------------------------- Monte Carlo simulation ----------------------------
##------------------------------------------------------------------------------

u_cq2 <- as.numeric(quantile(sev_train2$claim_amount_use, 0.95))
gpd_cq2 <- evir::gpd(sev_train2$claim_amount_use, threshold = u_cq2)

xi_cq2   <- unname(gpd_cq2$par.ests["xi"])
beta_cq2 <- unname(gpd_cq2$par.ests["beta"])
p_tail_cq2 <- mean(sev_train2$claim_amount_use > u_cq2)

c(u_cq2 = u_cq2, xi_cq2 = xi_cq2, beta_cq2 = beta_cq2, p_tail_cq2 = p_tail_cq2)

## One-year simulator with strict splice
simulate_one_year_wc <- function(cq_groups,
                                 sdlog,
                                 p_tail,
                                 u,
                                 xi,
                                 beta,
                                 per_employee = FALSE) {
  total_loss <- 0
  total_N <- 0
  
  for (g in seq_len(nrow(cq_groups))) {
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    if (per_employee) {
      N <- sum(rpois(n_g, lam))
    } else {
      N <- rpois(1, n_g * lam)
    }
    
    total_N <- total_N + N
    if (N == 0) next
    
    sev_g <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta
    )
    
    total_loss <- total_loss + sum(sev_g)
  }
  
  list(loss = total_loss, N = total_N)
}

## Run Monte Carlo Simulation
set.seed(123)
n_sim <- 100000

S <- numeric(n_sim)
N_total <- integer(n_sim)

per_employee_flag <- FALSE

for (sim in seq_len(n_sim)) {
  res <- simulate_one_year_wc(
    cq_groups = cq_wc_groups2,
    sdlog = sdlog_cq2,
    p_tail = p_tail_cq2,
    u = u_cq2,
    xi = xi_cq2,
    beta = beta_cq2,
    per_employee = per_employee_flag
  )
  S[sim] <- res$loss
  N_total[sim] <- res$N
}

summary(N_total)
head(N_total, 20)

summary(S)
quantile(S, probs = c(0.50, 0.75, 0.90, 0.95, 0.99))

hist(S, breaks = 60,
     main = "Simulated Aggregate Loss for Workers' Compensation",
     xlab = "Aggregate loss",
     col = 'steelblue',
     cex.main=0.9)

## Aggregate loss distribution summary
agg_loss_summary <- function(S) {
  VaR_95 <- as.numeric(quantile(S, 0.95, na.rm = TRUE))
  VaR_99 <- as.numeric(quantile(S, 0.99, na.rm = TRUE))
  TVaR_99 <- mean(S[S >= VaR_99], na.rm = TRUE)
  
  tibble(
    expected_loss = mean(S, na.rm = TRUE),
    sd_loss       = sd(S, na.rm = TRUE),
    p50_loss      = as.numeric(quantile(S, 0.50, na.rm = TRUE)),
    p75_loss      = as.numeric(quantile(S, 0.75, na.rm = TRUE)),
    p90_loss      = as.numeric(quantile(S, 0.90, na.rm = TRUE)),
    p95_loss      = VaR_95,
    p99_loss      = VaR_99,
    TVaR_99       = TVaR_99,
    min_loss      = min(S, na.rm = TRUE),
    max_loss      = max(S, na.rm = TRUE)
  )
}

wc_agg_loss_tbl <- agg_loss_summary(S)
wc_agg_loss_tbl


##------------------------------------------------------------------------------
##------------------------------ Stress Testing --------------------------------
##------------------------------------------------------------------------------

summarise_loss <- function(S) {
  VaR_95  <- as.numeric(quantile(S, 0.95))
  VaR_99  <- as.numeric(quantile(S, 0.99))
  TVaR_99 <- mean(S[S >= VaR_99])
  
  tibble(
    mean    = mean(S),
    sd      = sd(S),
    p50     = as.numeric(quantile(S, 0.50)),
    p95     = VaR_95,
    p99     = VaR_99,
    TVaR_99 = TVaR_99,
    max     = max(S)
  )
}

simulate_wc_portfolio <- function(
    n_sim,
    cq_groups,
    sdlog,
    p_tail,
    u,
    xi,
    beta,
    stress = list(
      lambda_mult = 1,
      sev_mult    = 1,
      p_tail_mult = 1,
      beta_mult   = 1,
      xi_add      = 0,
      sdlog_mult  = 1,
      catastrophe_prob  = 0,
      cat_lambda_mult   = 1,
      cat_sev_mult      = 1,
      cat_p_tail_mult   = 1
    ),
    seed = 123,
    per_employee = FALSE
) {
  set.seed(seed)
  
  lambda_mult <- ifelse(is.null(stress$lambda_mult), 1, stress$lambda_mult)
  sev_mult    <- ifelse(is.null(stress$sev_mult), 1, stress$sev_mult)
  p_tail_mult <- ifelse(is.null(stress$p_tail_mult), 1, stress$p_tail_mult)
  beta_mult   <- ifelse(is.null(stress$beta_mult), 1, stress$beta_mult)
  xi_add      <- ifelse(is.null(stress$xi_add), 0, stress$xi_add)
  sdlog_mult  <- ifelse(is.null(stress$sdlog_mult), 1, stress$sdlog_mult)
  
  cat_prob      <- ifelse(is.null(stress$catastrophe_prob), 0, stress$catastrophe_prob)
  cat_lam_mult  <- ifelse(is.null(stress$cat_lambda_mult), 1, stress$cat_lambda_mult)
  cat_sev_mult  <- ifelse(is.null(stress$cat_sev_mult), 1, stress$cat_sev_mult)
  cat_tail_mult <- ifelse(is.null(stress$cat_p_tail_mult), 1, stress$cat_p_tail_mult)
  
  S <- numeric(n_sim)
  N_total <- integer(n_sim)
  
  for (sim in seq_len(n_sim)) {
    is_cat <- (runif(1) < cat_prob)
    
    lam_cat_mult  <- if (is_cat) cat_lam_mult  else 1
    sev_cat_mult2 <- if (is_cat) cat_sev_mult  else 1
    tail_cat_mult <- if (is_cat) cat_tail_mult else 1
    
    sdlog_eff     <- sdlog * sdlog_mult
    xi_eff        <- min(0.99, max(-0.5, xi + xi_add))
    beta_eff      <- beta * beta_mult
    p_tail_eff    <- pmin(0.99, p_tail * p_tail_mult * tail_cat_mult)
    sev_scale_eff <- sev_mult * sev_cat_mult2
    
    total_loss <- 0
    total_N    <- 0
    
    for (g in seq_len(nrow(cq_groups))) {
      n_g  <- cq_groups$n_employees[g]
      lam  <- cq_groups$lambda_group[g] * lambda_mult * lam_cat_mult
      mlog <- cq_groups$meanlog_group[g]
      
      if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
      
      if (per_employee) {
        N <- sum(rpois(n_g, lam))
      } else {
        N <- rpois(1, n_g * lam)
      }
      
      total_N <- total_N + N
      if (N == 0) next
      
      sev_g <- rsev_spliced(
        n = N,
        meanlog_source = mlog,
        sdlog = sdlog_eff,
        p_tail = p_tail_eff,
        u = u,
        xi = xi_eff,
        beta = beta_eff,
        severity_scale = sev_scale_eff
      )
      
      total_loss <- total_loss + sum(sev_g)
    }
    
    S[sim] <- total_loss
    N_total[sim] <- total_N
  }
  
  list(S = S, N_total = N_total)
}

## Define stress scenarios
scenarios <- list(
  Base = list(),
  Freq_Up_30pct = list(lambda_mult = 1.30),
  Sev_Up_20pct  = list(sev_mult = 1.20),
  Tail_Worse    = list(p_tail_mult = 2.0, beta_mult = 1.30, xi_add = 0.05),
  Combined      = list(lambda_mult = 1.30, sev_mult = 1.20,
                       p_tail_mult = 2.0, beta_mult = 1.30, xi_add = 0.05),
  Cat_1in100    = list(catastrophe_prob = 0.01,
                       cat_lambda_mult = 2.0,
                       cat_sev_mult    = 1.50,
                       cat_p_tail_mult = 3.0)
)

## Stress summary table
loss_table_metrics <- function(S) {
  tibble::tibble(
    expected_loss = mean(S, na.rm = TRUE),
    sd_loss       = sd(S, na.rm = TRUE),
    p95_loss      = as.numeric(quantile(S, 0.95, na.rm = TRUE)),
    p99_loss      = as.numeric(quantile(S, 0.99, na.rm = TRUE))
  )
}

scenario_labels <- c(
  Freq_Up_30pct = "Frequency stress (incident surge)",
  Sev_Up_20pct  = "Severity inflation stress",
  Tail_Worse    = "Tail-worsening stress",
  Combined      = "Combined adverse scenario",
  Cat_1in100    = "1-in-100 catastrophe year stress"
)

stress_names <- names(scenarios)
stress_names <- stress_names[stress_names != "Base"]

n_sim_stress <- 100000
seed_stress  <- 123

stress_table_raw <- dplyr::bind_rows(lapply(stress_names, function(nm) {
  sim_out <- simulate_wc_portfolio(
    n_sim     = n_sim_stress,
    cq_groups = cq_wc_groups2,
    sdlog     = sdlog_cq2,
    p_tail    = p_tail_cq2,
    u         = u_cq2,
    xi        = xi_cq2,
    beta      = beta_cq2,
    stress    = scenarios[[nm]],
    seed      = seed_stress,
    per_employee = FALSE
  )
  
  loss_table_metrics(sim_out$S) %>%
    dplyr::mutate(
      Scenario = dplyr::coalesce(scenario_labels[[nm]], nm),
      .before = 1
    )
}))

stress_table_raw <- stress_table_raw %>%
  dplyr::select(Scenario, expected_loss, sd_loss, p95_loss, p99_loss)

stress_table_raw

## Visual comparison
base_S <- simulate_wc_portfolio(
  n_sim_stress, cq_wc_groups2, sdlog_cq2, p_tail_cq2, u_cq2, xi_cq2, beta_cq2,
  stress = scenarios$Base, seed = 123
)$S

comb_S <- simulate_wc_portfolio(
  n_sim_stress, cq_wc_groups2, sdlog_cq2, p_tail_cq2, u_cq2, xi_cq2, beta_cq2,
  stress = scenarios$Combined, seed = 123
)$S

cat_S <- simulate_wc_portfolio(
  n_sim_stress, cq_wc_groups2, sdlog_cq2, p_tail_cq2, u_cq2, xi_cq2, beta_cq2,
  stress = scenarios$Cat_1in100, seed = 123
)$S

par(mfrow = c(1,3))
hist(base_S, breaks = 60, main = "Base", xlab = "Aggregate loss")
hist(comb_S, breaks = 60, main = "Combined stress", xlab = "Aggregate loss")
hist(cat_S,  breaks = 60, main = "1-in-100 stress-year", xlab = "Aggregate loss")
par(mfrow = c(1,1))


##------------------------------------------------------------------------------
##----------------------------- Deductible Selection ---------------------------
##------------------------------------------------------------------------------

ded_probs <- c(0.01, 0.02, 0.05, 0.10, 0.20)
raw_deds <- as.numeric(quantile(sev_train2$claim_amount_use, probs = ded_probs, na.rm = TRUE))

nice_round <- function(x) {
  if (x < 2000) return(round(x / 100) * 100)
  if (x < 10000) return(round(x / 500) * 500)
  return(round(x / 1000) * 1000)
}

ded_levels <- sapply(raw_deds, nice_round)
ded_levels <- sort(unique(pmax(0, ded_levels)))
ded_levels <- sort(unique(c(0, ded_levels)))

ded_levels

## One-year simulation with deductible
simulate_one_year_wc_ded <- function(cq_groups,
                                     sdlog,
                                     p_tail,
                                     u,
                                     xi,
                                     beta,
                                     ded = 0,
                                     per_employee = FALSE) {
  insurer_loss <- 0
  employer_retained <- 0
  total_claims <- 0
  paid_claims <- 0
  
  for (g in seq_len(nrow(cq_groups))) {
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    if (per_employee) {
      N <- sum(rpois(n_g, lam))
    } else {
      N <- rpois(1, n_g * lam)
    }
    
    total_claims <- total_claims + N
    if (N == 0) next
    
    sev <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta
    )
    
    insurer_pay <- pmax(sev - ded, 0)
    retained    <- pmin(sev, ded)
    
    insurer_loss      <- insurer_loss + sum(insurer_pay)
    employer_retained <- employer_retained + sum(retained)
    paid_claims       <- paid_claims + sum(sev > ded)
  }
  
  list(
    insurer_loss = insurer_loss,
    employer_retained = employer_retained,
    total_claims = total_claims,
    paid_claims = paid_claims
  )
}

summarise_vec <- function(x) {
  tibble(
    mean = mean(x),
    sd   = sd(x),
    p95  = as.numeric(quantile(x, 0.95)),
    p99  = as.numeric(quantile(x, 0.99)),
    max  = max(x)
  )
}

set.seed(123)
n_sim_ded <- 100000

ded_results <- lapply(ded_levels, function(ded) {
  S_ins <- numeric(n_sim_ded)
  S_ret <- numeric(n_sim_ded)
  N_tot <- integer(n_sim_ded)
  N_paid <- integer(n_sim_ded)
  
  for (i in seq_len(n_sim_ded)) {
    res <- simulate_one_year_wc_ded(
      cq_groups = cq_wc_groups2,
      sdlog = sdlog_cq2,
      p_tail = p_tail_cq2,
      u = u_cq2,
      xi = xi_cq2,
      beta = beta_cq2,
      ded = ded,
      per_employee = FALSE
    )
    S_ins[i] <- res$insurer_loss
    S_ret[i] <- res$employer_retained
    N_tot[i] <- res$total_claims
    N_paid[i] <- res$paid_claims
  }
  
  pct_hist_below <- mean(sev_train2$claim_amount_use <= ded, na.rm = TRUE)
  
  tibble(
    deductible = ded,
    expected_loss = mean(S_ins),
    sd_loss = sd(S_ins),
    p95_loss = as.numeric(quantile(S_ins, 0.95)),
    p99_loss = as.numeric(quantile(S_ins, 0.99)),
    expected_retained = mean(S_ret),
    p95_retained = as.numeric(quantile(S_ret, 0.95)),
    p99_retained = as.numeric(quantile(S_ret, 0.99)),
    avg_total_claims = mean(N_tot),
    avg_paid_claims  = mean(N_paid),
    pct_paid_claims  = mean(N_paid / pmax(1, N_tot)),
    pct_hist_sev_below_ded = pct_hist_below
  )
})

ded_tbl <- bind_rows(ded_results) %>%
  arrange(deductible)

base_EL <- ded_tbl$expected_loss[ded_tbl$deductible == 0][1]

ded_tbl <- ded_tbl %>%
  mutate(
    EL_relativity = expected_loss / base_EL,
    EL_reduction_pct = 1 - EL_relativity
  )

ded_tbl

ded_table_report <- ded_tbl %>%
  transmute(
    Scenario = paste0("Deductible = ", format(deductible, big.mark = ",")),
    `Expected Loss (Ð)` = round(expected_loss),
    `Standard Deviation` = round(sd_loss),
    `P95 Loss` = round(p95_loss),
    `P99 Loss` = round(p99_loss)
  )

ded_table_report %>% print(n = Inf)

ded_best <- ded_tbl %>%
  filter(pct_hist_sev_below_ded >= 0.10, EL_reduction_pct >= 0.03) %>%
  arrange(deductible) %>%
  slice(1)

ded_best


##------------------------------------------------------------------------------
##----------------------------- Retention Selection ----------------------------
##------------------------------------------------------------------------------

## We use per-claim Excess of Loss (XoL) reinsurance:
## insurer pays min(X, retention)
## reinsurer pays max(X - retention, 0)

## Candidate retentions based on upper severity quantiles
ri_probs <- c(0.80, 0.90, 0.95, 0.975, 0.99)
raw_retentions <- as.numeric(quantile(sev_train2$claim_amount_use, probs = ri_probs, na.rm = TRUE))

## Use same rounding style as deductible section
ri_retentions <- sapply(raw_retentions, nice_round)
ri_retentions <- sort(unique(pmax(0, ri_retentions)))

ri_retentions

## One-year simulation with per-claim XoL reinsurance
simulate_one_year_wc_reinsurance <- function(cq_groups,
                                             sdlog,
                                             p_tail,
                                             u,
                                             xi,
                                             beta,
                                             retention = 0,
                                             per_employee = FALSE) {
  gross_loss <- 0
  insurer_net_loss <- 0
  reinsurer_loss <- 0
  total_claims <- 0
  ceded_claims <- 0
  
  for (g in seq_len(nrow(cq_groups))) {
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    ## Frequency
    if (per_employee) {
      N <- sum(rpois(n_g, lam))
    } else {
      N <- rpois(1, n_g * lam)
    }
    
    total_claims <- total_claims + N
    if (N == 0) next
    
    ## Claim severities
    sev <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta
    )
    
    ## Per-claim XoL split
    insurer_part   <- pmin(sev, retention)
    reinsurer_part <- pmax(sev - retention, 0)
    
    gross_loss       <- gross_loss + sum(sev)
    insurer_net_loss <- insurer_net_loss + sum(insurer_part)
    reinsurer_loss   <- reinsurer_loss + sum(reinsurer_part)
    ceded_claims     <- ceded_claims + sum(sev > retention)
  }
  
  list(
    gross_loss = gross_loss,
    insurer_net_loss = insurer_net_loss,
    reinsurer_loss = reinsurer_loss,
    total_claims = total_claims,
    ceded_claims = ceded_claims
  )
}

## Run candidate retentions
set.seed(123)
n_sim_ri <- 100000

ri_results <- lapply(ri_retentions, function(ret) {
  gross_vec <- numeric(n_sim_ri)
  net_vec   <- numeric(n_sim_ri)
  ceded_vec <- numeric(n_sim_ri)
  N_tot     <- integer(n_sim_ri)
  N_ceded   <- integer(n_sim_ri)
  
  for (i in seq_len(n_sim_ri)) {
    res <- simulate_one_year_wc_reinsurance(
      cq_groups = cq_wc_groups2,
      sdlog = sdlog_cq2,
      p_tail = p_tail_cq2,
      u = u_cq2,
      xi = xi_cq2,
      beta = beta_cq2,
      retention = ret,
      per_employee = FALSE
    )
    
    gross_vec[i] <- res$gross_loss
    net_vec[i]   <- res$insurer_net_loss
    ceded_vec[i] <- res$reinsurer_loss
    N_tot[i]     <- res$total_claims
    N_ceded[i]   <- res$ceded_claims
  }
  
  pct_hist_above_ret <- mean(sev_train2$claim_amount_use > ret, na.rm = TRUE)
  
  tibble(
    retention = ret,
    
    gross_expected_loss = mean(gross_vec),
    gross_sd_loss       = sd(gross_vec),
    gross_p95_loss      = as.numeric(quantile(gross_vec, 0.95)),
    gross_p99_loss      = as.numeric(quantile(gross_vec, 0.99)),
    
    net_expected_loss   = mean(net_vec),
    net_sd_loss         = sd(net_vec),
    net_p95_loss        = as.numeric(quantile(net_vec, 0.95)),
    net_p99_loss        = as.numeric(quantile(net_vec, 0.99)),
    
    expected_ceded_loss = mean(ceded_vec),
    ceded_p95_loss      = as.numeric(quantile(ceded_vec, 0.95)),
    ceded_p99_loss      = as.numeric(quantile(ceded_vec, 0.99)),
    
    avg_total_claims    = mean(N_tot),
    avg_ceded_claims    = mean(N_ceded),
    pct_ceded_claims    = mean(N_ceded / pmax(1, N_tot)),
    pct_hist_sev_above_ret = pct_hist_above_ret
  )
})

ri_tbl <- bind_rows(ri_results) %>%
  arrange(retention)

## Compare against no reinsurance gross portfolio
gross_EL_base  <- mean(S)
gross_p95_base <- as.numeric(quantile(S, 0.95))
gross_p99_base <- as.numeric(quantile(S, 0.99))

ri_tbl <- ri_tbl %>%
  mutate(
    EL_relativity   = net_expected_loss / gross_EL_base,
    EL_reduction_pct = 1 - EL_relativity,
    
    p95_reduction_pct = 1 - net_p95_loss / gross_p95_base,
    p99_reduction_pct = 1 - net_p99_loss / gross_p99_base,
    
    ceded_share_expected = expected_ceded_loss / gross_EL_base
  )

ri_tbl

## Clean report table
ri_table_report <- ri_tbl %>%
  transmute(
    Scenario = paste0("XoL retention = ", format(retention, big.mark = ",")),
    `Expected Net Loss (Ð)` = round(net_expected_loss),
    `Standard Deviation`    = round(net_sd_loss),
    `P95 Net Loss`          = round(net_p95_loss),
    `P99 Net Loss`          = round(net_p99_loss),
    `Expected Ceded Loss`   = round(expected_ceded_loss),
    `P99 Reduction %`       = round(100 * p99_reduction_pct, 1)
  )

ri_table_report %>% print(n = Inf)


## choose the smallest retention that:
## - cedes no more than 10% of historical claims, and
## - reduces P99 by at least 10%
ri_best <- ri_tbl %>%
  filter(pct_hist_sev_above_ret <= 0.10, p99_reduction_pct >= 0.10) %>%
  arrange(retention) %>%
  slice(1)

ri_best

##------------------------------------------------------------------------------
##----------------------------- Capital Modelling -------------------------------
##-------------------------------- Short term -----------------------------------
## One-year premium calculation with deductible, XoL reinsurance,
## expenses, brokerage, capital cost and target loss probability

## Assumptions
n_sim_capital <- 100000
expense_loading     <- 0.10
brokerage_loading   <- 0.15
reinsurance_loading <- 0.25
capital_cost_rate   <- 0.08
target_loss_prob    <- 0.01

## 1-year interest rate for 2175
interest_rate <- 0.0362

## Use selected deductible and retention from earlier sections
deductible <- ded_best
retention  <- ri_best

deductible
retention
interest_rate

## Helper summary function
summarise_capital_dist <- function(x) {
  VaR_95  <- as.numeric(quantile(x, 0.95, na.rm = TRUE))
  VaR_99  <- as.numeric(quantile(x, 0.99, na.rm = TRUE))
  TVaR_99 <- mean(x[x >= VaR_99], na.rm = TRUE)
  
  tibble(
    mean    = mean(x, na.rm = TRUE),
    sd      = sd(x, na.rm = TRUE),
    p01     = as.numeric(quantile(x, 0.01, na.rm = TRUE)),
    p05     = as.numeric(quantile(x, 0.05, na.rm = TRUE)),
    p50     = as.numeric(quantile(x, 0.50, na.rm = TRUE)),
    p95     = VaR_95,
    p99     = VaR_99,
    TVaR_99 = TVaR_99,
    min     = min(x, na.rm = TRUE),
    max     = max(x, na.rm = TRUE)
  )
}

## One-year claim simulator for pricing
## simulate claims -> apply deductible -> apply XoL reinsurance
simulate_one_year_wc_pricing <- function(cq_groups,
                                         sdlog,
                                         p_tail,
                                         u,
                                         xi,
                                         beta,
                                         deductible,
                                         retention,
                                         per_employee = FALSE) {
  gross_loss_mid <- 0
  insured_loss_after_ded_mid <- 0
  insurer_claims_mid <- 0
  reinsurer_recoveries_mid <- 0
  total_claims <- 0
  
  for (g in seq_len(nrow(cq_groups))) {
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    ## Frequency
    if (per_employee) {
      N <- sum(rpois(n_g, lam))
    } else {
      N <- rpois(1, n_g * lam)
    }
    
    total_claims <- total_claims + N
    if (N == 0) next
    
    ## Gross severities
    sev <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta
    )
    
    ## Apply deductible first
    claims_after_ded <- pmax(sev - deductible, 0)
    
    ## Apply per-claim XoL reinsurance
    insurer_part   <- pmin(claims_after_ded, retention)
    reinsurer_part <- pmax(claims_after_ded - retention, 0)
    
    ## Accumulate
    gross_loss_mid              <- gross_loss_mid + sum(sev)
    insured_loss_after_ded_mid  <- insured_loss_after_ded_mid + sum(claims_after_ded)
    insurer_claims_mid          <- insurer_claims_mid + sum(insurer_part)
    reinsurer_recoveries_mid    <- reinsurer_recoveries_mid + sum(reinsurer_part)
  }
  
  list(
    gross_loss_mid = gross_loss_mid,
    insured_loss_after_ded_mid = insured_loss_after_ded_mid,
    insurer_claims_mid = insurer_claims_mid,
    reinsurer_recoveries_mid = reinsurer_recoveries_mid,
    total_claims = total_claims
  )
}

## Step 1: simulate one-year losses 
set.seed(2026)

gross_loss_mid_vec             <- numeric(n_sim_capital)
insured_loss_after_ded_mid_vec <- numeric(n_sim_capital)
insurer_claims_mid_vec         <- numeric(n_sim_capital)
reinsurer_recoveries_mid_vec   <- numeric(n_sim_capital)
claim_count_vec                <- integer(n_sim_capital)




# Keep deductible/retention to be numeric scalars
as_scalar_num <- function(x) {
  if (is.data.frame(x)) x <- x[[1]]     # pull first column if tibble/data.frame
  x <- as.numeric(x)
  x[1]
}

deductible <- as_scalar_num(deductible)
retention  <- as_scalar_num(retention)

str(deductible); str(retention)




for (i in seq_len(n_sim_capital)) {
  res <- simulate_one_year_wc_pricing(
    cq_groups   = cq_wc_groups2,
    sdlog       = sdlog_cq2,
    p_tail      = p_tail_cq2,
    u           = u_cq2,
    xi          = xi_cq2,
    beta        = beta_cq2,
    deductible  = deductible,
    retention   = retention,
    per_employee = FALSE
  )
  
  gross_loss_mid_vec[i]             <- res$gross_loss_mid
  insured_loss_after_ded_mid_vec[i] <- res$insured_loss_after_ded_mid
  insurer_claims_mid_vec[i]         <- res$insurer_claims_mid
  reinsurer_recoveries_mid_vec[i]   <- res$reinsurer_recoveries_mid
  claim_count_vec[i]                <- res$total_claims
}

## Step 2: move claims to end-year value
## claim payments occur on average mid-year, so accumulate for half a year
gross_loss_end_year             <- gross_loss_mid_vec * (1 + interest_rate)^0.5
insured_loss_after_ded_end_year <- insured_loss_after_ded_mid_vec * (1 + interest_rate)^0.5
insurer_claims_end_year         <- insurer_claims_mid_vec * (1 + interest_rate)^0.5
reinsurer_recoveries_end_year   <- reinsurer_recoveries_mid_vec * (1 + interest_rate)^0.5

## Aggregate loss summaries
gross_agg_summary <- summarise_capital_dist(gross_loss_end_year) %>%
  mutate(distribution = "Gross aggregate loss", .before = 1)

insured_after_ded_summary <- summarise_capital_dist(insured_loss_after_ded_end_year) %>%
  mutate(distribution = "Aggregate insured loss after deductible", .before = 1)

insurer_net_claims_summary <- summarise_capital_dist(insurer_claims_end_year) %>%
  mutate(distribution = "Insurer net claims after reinsurance", .before = 1)

aggregate_loss_summary_pricing <- bind_rows(
  gross_agg_summary,
  insured_after_ded_summary,
  insurer_net_claims_summary
)

aggregate_loss_summary_pricing

## Step 3: expected costs 
expected_claim_cost <- mean(insurer_claims_end_year)

## Reinsurance premium is based on expected ceded loss
expected_reinsurance_loss <- mean(reinsurer_recoveries_mid_vec)

reinsurance_premium_start <- expected_reinsurance_loss * (1 + reinsurance_loading)
reinsurance_premium_end   <- reinsurance_premium_start * (1 + interest_rate)

## Step 4: capital requirement
## use aggregate insured loss after deductible (before reinsurance) for VaR99
VaR_99 <- as.numeric(quantile(insured_loss_after_ded_end_year, 0.99, na.rm = TRUE))
capital_cost <- capital_cost_rate * VaR_99

## Step 5: initial premium guess 
## Premium at end of year must cover:
## insurer claims + reinsurance premium + capital cost + expenses + brokerage
## where expenses and brokerage are percentages of premium
premium_end_year_guess <-
  (expected_claim_cost + reinsurance_premium_end + capital_cost) /
  (1 - expense_loading - brokerage_loading)

premium_start_year_guess <- premium_end_year_guess / (1 + interest_rate)

premium_start_year_guess

## Step 6: profit function
profit_function <- function(premium_start) {
  premium_end <- premium_start * (1 + interest_rate)
  
  expenses_end  <- premium_end * expense_loading
  brokerage_end <- premium_end * brokerage_loading
  
  profit <- premium_end -
    insurer_claims_end_year -
    reinsurance_premium_end -
    expenses_end -
    brokerage_end -
    capital_cost
  
  profit
}

loss_probability <- function(premium_start) {
  profit <- profit_function(premium_start)
  mean(profit < 0)
}

## Step 7: find premium matching target loss probability
premium_grid <- seq(
  premium_start_year_guess * 0.50,
  premium_start_year_guess * 2.00,
  length.out = 300
)

loss_probs <- sapply(premium_grid, loss_probability)

idx_best <- which.min(abs(loss_probs - target_loss_prob))
premium_target <- premium_grid[idx_best]

premium_target

## Step 8: final profit / net revenue distribution
profit_simulation <- profit_function(premium_target)

mean_profit <- mean(profit_simulation)
probability_of_loss <- mean(profit_simulation < 0)

profit_quantiles <- quantile(
  profit_simulation,
  probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99),
  na.rm = TRUE
)

profit_quantiles

net_revenue_summary <- summarise_capital_dist(profit_simulation) %>%
  mutate(
    probability_of_loss = probability_of_loss,
    expected_profit = mean_profit,
    target_loss_probability = target_loss_prob,
    .before = 1
  )

net_revenue_summary

## Step 9: plot profit distribution 
ggplot(data.frame(profit = profit_simulation), aes(x = profit)) +
  geom_histogram(
    bins = 50,
    fill = "lightblue",
    colour = "black"
  ) +
  labs(
    title = "Year 1 Insurer Profit Distribution",
    x = "Profit",
    y = "Simulation Count"
  ) +
  theme_minimal()

## Step 10: pricing summary 
pricing_summary <- tibble(
  deductible                  = deductible,
  retention                   = retention,
  interest_rate               = interest_rate,
  expense_loading             = expense_loading,
  brokerage_loading           = brokerage_loading,
  reinsurance_loading         = reinsurance_loading,
  capital_cost_rate           = capital_cost_rate,
  target_loss_probability     = target_loss_prob,
  expected_claim_count        = mean(claim_count_vec),
  expected_claim_cost         = expected_claim_cost,
  expected_reinsurance_loss   = expected_reinsurance_loss,
  reinsurance_premium_start   = reinsurance_premium_start,
  reinsurance_premium_end     = reinsurance_premium_end,
  capital_requirement_VaR99   = VaR_99,
  capital_cost                = capital_cost,
  premium_guess_start_year    = premium_start_year_guess,
  premium_charged_start_year  = premium_target,
  premium_income_end_year     = premium_target * (1 + interest_rate),
  expected_profit             = mean_profit,
  probability_of_loss         = probability_of_loss
)

pricing_summary

## Per-employee premium
total_employees_y1 <- sum(cq_wc_groups2$n_employees, na.rm = TRUE)

premium_per_employee <- premium_target / total_employees_y1

premium_per_employee



## RISK-GROUP PREMIUM RATING (occupation x employment_type)
## --- Safety checks ---
stopifnot(all(c("n_employees","lambda_group","meanlog_group") %in% names(cq_wc_groups2)))
stopifnot(is.numeric(deductible), length(deductible) == 1)
stopifnot(is.numeric(retention),  length(retention)  == 1)

n_groups <- nrow(cq_wc_groups2)


## 1) Simulate 1-year losses BY GROUP (mid-year payment basis)
simulate_one_year_by_group <- function(cq_groups,
                                       sdlog, p_tail, u, xi, beta,
                                       deductible, retention) {
  
  ins_claims_mid_g   <- numeric(n_groups)  # insurer payments after retention (mid-year)
  reins_rec_mid_g    <- numeric(n_groups)  # reinsurer recoveries (mid-year)
  insured_after_ded_mid_g <- numeric(n_groups) # after deductible, before retention (mid-year)
  
  total_claims <- 0L
  
  for (g in seq_len(nrow(cq_groups))) {
    
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    ## aggregate Poisson is equivalent to per-employee, but faster:
    N <- rpois(1, n_g * lam)
    total_claims <- total_claims + N
    if (N == 0) next
    
    ## severities (gross, before deductible)
    sev <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta
    )
    
    ## apply deductible per claim
    after_ded <- pmax(sev - deductible, 0)
    
    ## insurer pays up to retention; reinsurer pays excess
    ins_part <- pmin(after_ded, retention)
    re_part  <- pmax(after_ded - retention, 0)
    
    insured_after_ded_mid_g[g] <- insured_after_ded_mid_g[g] + sum(after_ded)
    ins_claims_mid_g[g]        <- ins_claims_mid_g[g]        + sum(ins_part)
    reins_rec_mid_g[g]         <- reins_rec_mid_g[g]         + sum(re_part)
  }
  
  list(
    insured_after_ded_mid_g = insured_after_ded_mid_g,
    ins_claims_mid_g        = ins_claims_mid_g,
    reins_rec_mid_g         = reins_rec_mid_g,
    total_claims            = total_claims
  )
}


## 2) Run Monte Carlo to estimate group cost components
set.seed(2026)
n_sim_group_rating <- 50000  # adjust if you want (20k–100k)

insured_after_ded_mid_mat <- matrix(0, nrow = n_sim_group_rating, ncol = n_groups)
ins_claims_mid_mat        <- matrix(0, nrow = n_sim_group_rating, ncol = n_groups)
reins_rec_mid_mat         <- matrix(0, nrow = n_sim_group_rating, ncol = n_groups)

for (i in seq_len(n_sim_group_rating)) {
  res <- simulate_one_year_by_group(
    cq_groups   = cq_wc_groups2,
    sdlog       = sdlog_cq2,
    p_tail      = p_tail_cq2,
    u           = u_cq2,
    xi          = xi_cq2,
    beta        = beta_cq2,
    deductible  = deductible,
    retention   = retention
  )
  
  insured_after_ded_mid_mat[i, ] <- res$insured_after_ded_mid_g
  ins_claims_mid_mat[i, ]        <- res$ins_claims_mid_g
  reins_rec_mid_mat[i, ]         <- res$reins_rec_mid_g
}

## Convert MID-year payments to END-of-year value (mid-year -> end-year accrual)
acc_mid_to_end <- (1 + interest_rate)^(0.5)

insured_after_ded_end_mat <- insured_after_ded_mid_mat * acc_mid_to_end
ins_claims_end_mat        <- ins_claims_mid_mat        * acc_mid_to_end
reins_rec_end_mat         <- reins_rec_mid_mat         * acc_mid_to_end

## Totals (portfolio)
insured_after_ded_end_total <- rowSums(insured_after_ded_end_mat)
ins_claims_end_total        <- rowSums(ins_claims_end_mat)
reins_rec_end_total         <- rowSums(reins_rec_end_mat)


## 3) Portfolio capital cost (based on 99% VaR of insured-after-ded)
VaR99_insured_after_ded <- as.numeric(quantile(insured_after_ded_end_total, 0.99, na.rm = TRUE))
capital_cost_total_end  <- capital_cost_rate * VaR99_insured_after_ded

mean_insured_after_ded_by_group <- colMeans(insured_after_ded_end_mat)
mean_insured_after_ded_total    <- mean(insured_after_ded_end_total)

cap_wt <- if (mean_insured_after_ded_total > 0) {
  mean_insured_after_ded_by_group / mean_insured_after_ded_total
} else {
  rep(1 / n_groups, n_groups)
}
capital_cost_end_by_group <- capital_cost_total_end * cap_wt


## 4) Reinsurance premium (loaded) — allocate by expected ceded amount
EL_reins_end_by_group <- colMeans(reins_rec_end_mat)
reins_prem_end_by_group <- EL_reins_end_by_group * (1 + reinsurance_loading)


## 5) Group premium calculation
EL_claims_end_by_group <- colMeans(ins_claims_end_mat)

denom <- 1 - (expense_loading + brokerage_loading)
if (denom <= 0) stop("expense_loading + brokerage_loading must be < 1")

premium_end_by_group <- (EL_claims_end_by_group + reins_prem_end_by_group + capital_cost_end_by_group) / denom
premium_start_by_group <- premium_end_by_group / (1 + interest_rate)


## 6) OPTIONAL: rescale so group premiums sum to your portfolio premium_target
if (exists("premium_target")) {
  scale_factor <- premium_target / sum(premium_start_by_group)
  premium_start_by_group <- premium_start_by_group * scale_factor
  premium_end_by_group   <- premium_end_by_group   * scale_factor
  
  ## expenses/brokerage are % of premium -> scaling is consistent
}


## 7) Build a neat table (premium per employee + relativities)
total_employees <- sum(cq_wc_groups2$n_employees)
avg_prem_per_emp_portfolio <- sum(premium_start_by_group) / total_employees

wc_group_premium_tbl <- cq_wc_groups2 %>%
  dplyr::mutate(
    group_id = dplyr::row_number(),
    premium_start = premium_start_by_group[group_id],
    premium_end   = premium_end_by_group[group_id],
    premium_per_employee = premium_start / n_employees,
    premium_relativity   = premium_per_employee / avg_prem_per_emp_portfolio
  ) %>%
  dplyr::select(
    occupation, employment_type, n_employees,
    premium_start, premium_per_employee, premium_relativity
  ) %>%
  dplyr::arrange(dplyr::desc(premium_per_employee))

wc_group_premium_tbl %>% print(n = Inf)

cat("\nCheck totals:\n")
cat("Sum(group premium_start) = ", format(sum(premium_start_by_group), big.mark=","), "\n")
if (exists("premium_target")) {
  cat("premium_target          = ", format(premium_target, big.mark=","), "\n")
}
cat("Portfolio avg premium per employee = ", round(avg_prem_per_emp_portfolio, 2), "\n")



##------------------------------------------------------------------------------
##---------------------------------- Long term -----------------------------------
##------------------------------------------------------------------------------

inflation_10 <- read.csv("/Users/irisbaek/actl4001-26t1/forecasts/inflation.csv")
interest_10  <- read.csv("/Users/irisbaek/actl4001-26t1/forecasts/interest.csv")

## 0) Parameters (aligned with short-term)
n_years         <- 10
n_sim_10y       <- 20000       # increase later if you want smoother tails
target_loss_prob <- 0.01

# loadings (use the same as short term; fallback if not defined)
expense_loading     <- if (exists("expense_loading")) expense_loading else 0.10
brokerage_loading   <- if (exists("brokerage_loading")) brokerage_loading else 0.15
reinsurance_loading <- if (exists("reinsurance_loading")) reinsurance_loading else 0.25
capital_cost_rate   <- if (exists("capital_cost_rate")) capital_cost_rate else 0.08

# claims paid "mid-year" assumption
midyear_claims <- TRUE

# make sure deductible/retention bases are numeric scalars
as_scalar_num <- function(x) {
  if (is.data.frame(x)) x <- x[[1]]
  x <- as.numeric(x)
  x[1]
}

# Base WC terms (use your selected short-term scalars if they exist)
ded_base <- if (exists("deductible")) as_scalar_num(deductible) else 600
ret_base <- if (exists("retention"))  as_scalar_num(retention)  else 48000

stopifnot(is.numeric(ded_base), length(ded_base) == 1)
stopifnot(is.numeric(ret_base), length(ret_base) == 1)

## Sanity: required objects must exist from earlier sections
stopifnot(exists("cq_wc_groups2"))
stopifnot(all(c("n_employees","lambda_group","meanlog_group") %in% names(cq_wc_groups2)))
stopifnot(exists("rsev_spliced"))
stopifnot(exists("sdlog_cq2"), exists("p_tail_cq2"), exists("u_cq2"), exists("xi_cq2"), exists("beta_cq2"))

n_groups <- nrow(cq_wc_groups2)


## 1) Parse inflation vector (10 years)

stopifnot(all(c("year","inflation_rate") %in% names(inflation_10)))

years_vec <- inflation_10$year[1:n_years]
infl_rate <- inflation_10$inflation_rate[1:n_years] / 100  # decimal

infl_factor <- numeric(n_years)
infl_factor[1] <- 1
if (n_years >= 2) {
  infl_factor[2:n_years] <- cumprod(1 + infl_rate[1:(n_years-1)])
}

infl_tbl <- tibble::tibble(
  year = years_vec,
  infl_rate = infl_rate,
  infl_factor = infl_factor
)

infl_tbl


## 2) Interest rates (maturity = 1)
get_interest_vec <- function(interest_df, years_vec, maturity = 1) {
  stopifnot("maturity" %in% names(interest_df))
  row <- interest_df[interest_df$maturity == maturity, , drop = FALSE]
  if (nrow(row) != 1) stop("interest_10 must have exactly one row for maturity=", maturity)
  
  cols <- paste0("X", years_vec)
  if (!all(cols %in% names(row))) {
    stop("interest_10 missing columns: ", paste(setdiff(cols, names(row)), collapse = ", "))
  }
  as.numeric(row[1, cols]) / 100
}

r_year <- get_interest_vec(interest_10, years_vec, maturity = 1)


## 3) Exposure growth factors (mine expansion proxy)
# Case study: +25% Helionis & Bayesia, +15% Oryn over 10 years
mines_0      <- c(Helionis = 30, Bayesia = 15, Oryn = 10)
target_mult  <- c(Helionis = 1.25, Bayesia = 1.25, Oryn = 1.15)

# Smooth compounding so year 1 = 1 and year 10 = target_mult
t_grid <- seq(0, 1, length.out = n_years)
sys_factor <- sapply(names(mines_0), function(s) target_mult[s]^t_grid)
colnames(sys_factor) <- names(mines_0)

total_mines_t   <- as.numeric(sys_factor %*% mines_0)
growth_overall  <- total_mines_t / sum(mines_0)

growth_tbl <- tibble::tibble(
  year = years_vec,
  growth_overall = growth_overall,
  Helionis = sys_factor[, "Helionis"],
  Bayesia  = sys_factor[, "Bayesia"],
  Oryn     = sys_factor[, "Oryn"]
)

growth_tbl


## 4) Helpers: distribution summary
summarise_dist <- function(x) {
  tibble::tibble(
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x, na.rm = TRUE),
    p95  = as.numeric(quantile(x, 0.95, na.rm = TRUE, type = 7)),
    p99  = as.numeric(quantile(x, 0.99, na.rm = TRUE, type = 7))
  )
}


## 5) Simulate ONE YEAR (portfolio vectors + group expected components)
##    - Uses inflation to scale severities and also inflates deductible/retention
##    - Uses growth_overall[t] to scale frequency
##    - Claims assumed paid mid-year -> accumulated to end-year by (1+r)^(0.5)

simulate_one_year_wc_stats <- function(
    n_sim,
    cq_groups,
    growth_t,
    infl_t,
    r_t,
    sdlog, p_tail, u, xi, beta,
    ded_base, ret_base,
    midyear_claims = TRUE,
    seed = 123
) {
  set.seed(seed)
  
  ded_t <- ded_base * infl_t
  ret_t <- ret_base * infl_t
  
  # mid-year -> end-year accumulation
  claim_mult <- if (midyear_claims) (1 + r_t)^(0.5) else 1
  
  # portfolio vectors (end-year value)
  insured_after_ded_end <- numeric(n_sim)   # BEFORE reinsurance
  insurer_claims_end    <- numeric(n_sim)   # AFTER retention (insurer layer)
  reins_recovery_end    <- numeric(n_sim)   # ceded layer
  
  # group sums (end-year), accumulated to compute means
  sum_insured_after_ded_by_g <- numeric(n_groups)
  sum_insurer_claims_by_g    <- numeric(n_groups)
  sum_reins_recovery_by_g    <- numeric(n_groups)
  
  for (i in seq_len(n_sim)) {
    
    insured_after_ded_g <- numeric(n_groups)
    insurer_claims_g    <- numeric(n_groups)
    reins_recovery_g    <- numeric(n_groups)
    
    for (g in seq_len(nrow(cq_groups))) {
      
      n_g  <- cq_groups$n_employees[g]
      lam  <- cq_groups$lambda_group[g]
      mlog <- cq_groups$meanlog_group[g]
      
      if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
      
      # growth scales expected claim count
      mu_g <- (n_g * lam) * growth_t
      N <- rpois(1, mu_g)
      if (N == 0) next
      
      # severities: strict splice, inflation applied as multiplicative scale
      sev <- rsev_spliced(
        n = N,
        meanlog_source = mlog,
        sdlog = sdlog,
        p_tail = p_tail,
        u = u,
        xi = xi,
        beta = beta,
        severity_scale = infl_t
      )
      
      # deductible then per-claim XoL retention
      after_ded <- pmax(sev - ded_t, 0)
      ins_part  <- pmin(after_ded, ret_t)
      re_part   <- pmax(after_ded - ret_t, 0)
      
      insured_after_ded_g[g] <- insured_after_ded_g[g] + sum(after_ded)
      insurer_claims_g[g]    <- insurer_claims_g[g]    + sum(ins_part)
      reins_recovery_g[g]    <- reins_recovery_g[g]    + sum(re_part)
    }
    
    # end-year value
    insured_after_ded_g <- insured_after_ded_g * claim_mult
    insurer_claims_g    <- insurer_claims_g    * claim_mult
    reins_recovery_g    <- reins_recovery_g    * claim_mult
    
    insured_after_ded_end[i] <- sum(insured_after_ded_g)
    insurer_claims_end[i]    <- sum(insurer_claims_g)
    reins_recovery_end[i]    <- sum(reins_recovery_g)
    
    sum_insured_after_ded_by_g <- sum_insured_after_ded_by_g + insured_after_ded_g
    sum_insurer_claims_by_g    <- sum_insurer_claims_by_g    + insurer_claims_g
    sum_reins_recovery_by_g    <- sum_reins_recovery_by_g    + reins_recovery_g
  }
  
  list(
    ded_t = ded_t,
    ret_t = ret_t,
    
    insured_after_ded_end = insured_after_ded_end,
    insurer_claims_end    = insurer_claims_end,
    reins_recovery_end    = reins_recovery_end,
    
    EL_insured_after_ded_by_g = sum_insured_after_ded_by_g / n_sim,
    EL_insurer_claims_by_g    = sum_insurer_claims_by_g    / n_sim,
    EL_reins_recovery_by_g    = sum_reins_recovery_by_g    / n_sim
  )
}



## 6) Price ONE YEAR using your short-term mechanism (grid search on loss prob)
price_one_year_grid <- function(
    insurer_claims_end,
    insured_after_ded_end,
    reins_recovery_end,
    r,
    expense_loading,
    brokerage_loading,
    reinsurance_loading,
    capital_cost_rate,
    target_loss_prob
) {
  # reinsurance premium (end-year) based on expected ceded
  EL_reins_end <- mean(reins_recovery_end, na.rm = TRUE)
  reins_prem_end <- EL_reins_end * (1 + reinsurance_loading)
  
  # capital cost (end-year) on VaR99 of insured-after-ded (pre-reinsurance)
  VaR99 <- as.numeric(quantile(insured_after_ded_end, 0.99, na.rm = TRUE, type = 7))
  capital_cost_end <- capital_cost_rate * VaR99
  
  # expected insurer claims (end-year)
  EL_claim_end <- mean(insurer_claims_end, na.rm = TRUE)
  
  # initial premium guess (same algebra as short-term)
  premium_end_guess <-
    (EL_claim_end + reins_prem_end + capital_cost_end) /
    (1 - expense_loading - brokerage_loading)
  
  premium_start_guess <- premium_end_guess / (1 + r)
  
  profit_end_given_premstart <- function(prem_start) {
    prem_end <- prem_start * (1 + r)
    expenses_end  <- prem_end * expense_loading
    brokerage_end <- prem_end * brokerage_loading
    
    prem_end -
      insurer_claims_end -
      reins_prem_end -
      expenses_end -
      brokerage_end -
      capital_cost_end
  }
  
  loss_prob <- function(prem_start) mean(profit_end_given_premstart(prem_start) < 0)
  
  grid  <- seq(premium_start_guess * 0.50, premium_start_guess * 2.00, length.out = 300)
  probs <- sapply(grid, loss_prob)
  
  prem_star <- grid[which.min(abs(probs - target_loss_prob))]
  profit_end <- profit_end_given_premstart(prem_star)
  
  list(
    premium_start = prem_star,
    premium_end   = prem_star * (1 + r),
    reins_prem_end = reins_prem_end,
    expenses_end  = (prem_star * (1 + r)) * expense_loading,
    brokerage_end = (prem_star * (1 + r)) * brokerage_loading,
    capital_cost_end = capital_cost_end,
    loss_prob = mean(profit_end < 0),
    profit_end = profit_end,
    VaR99_insured_after_ded = VaR99
  )
}



## 7) Run 10 years: simulate + price + compute GROUP-SPECIFIC premiums
set.seed(2026)

# portfolio outputs by year
premium_start_vec    <- numeric(n_years)
premium_end_vec      <- numeric(n_years)
expenses_end_vec     <- numeric(n_years)
broker_end_vec       <- numeric(n_years)
reins_prem_end_vec   <- numeric(n_years)
cap_cost_end_vec     <- numeric(n_years)
lossprob_vec         <- numeric(n_years)
VaR99_vec            <- numeric(n_years)

# store deductible/retention paths
ded_vec <- numeric(n_years)
ret_vec <- numeric(n_years)

# store distributions for plots / PV
ins_claims_end_mat <- matrix(0, nrow = n_sim_10y, ncol = n_years)
profit_end_mat     <- matrix(0, nrow = n_sim_10y, ncol = n_years)

# store group premiums (start-year) each year: n_groups x n_years
group_prem_start_mat <- matrix(0, nrow = n_groups, ncol = n_years)
group_prem_end_mat   <- matrix(0, nrow = n_groups, ncol = n_years)

# also store group premium per employee and relativity
group_prem_per_emp_mat <- matrix(0, nrow = n_groups, ncol = n_years)
group_rel_mat          <- matrix(0, nrow = n_groups, ncol = n_years)

for (t in seq_len(n_years)) {
  
  sim_t <- simulate_one_year_wc_stats(
    n_sim = n_sim_10y,
    cq_groups = cq_wc_groups2,
    growth_t = growth_overall[t],
    infl_t   = infl_factor[t],
    r_t      = r_year[t],
    sdlog = sdlog_cq2,
    p_tail = p_tail_cq2,
    u = u_cq2,
    xi = xi_cq2,
    beta = beta_cq2,
    ded_base = ded_base,
    ret_base = ret_base,
    midyear_claims = midyear_claims,
    seed = 1000 + t
  )
  
  ded_vec[t] <- sim_t$ded_t
  ret_vec[t] <- sim_t$ret_t
  
  ins_claims_end_mat[, t] <- sim_t$insurer_claims_end
  
  # Price the PORTFOLIO for this year (target loss probability)
  pr_t <- price_one_year_grid(
    insurer_claims_end      = sim_t$insurer_claims_end,
    insured_after_ded_end   = sim_t$insured_after_ded_end,
    reins_recovery_end      = sim_t$reins_recovery_end,
    r = r_year[t],
    expense_loading = expense_loading,
    brokerage_loading = brokerage_loading,
    reinsurance_loading = reinsurance_loading,
    capital_cost_rate = capital_cost_rate,
    target_loss_prob = target_loss_prob
  )
  
  premium_start_vec[t]   <- pr_t$premium_start
  premium_end_vec[t]     <- pr_t$premium_end
  expenses_end_vec[t]    <- pr_t$expenses_end
  broker_end_vec[t]      <- pr_t$brokerage_end
  reins_prem_end_vec[t]  <- pr_t$reins_prem_end
  cap_cost_end_vec[t]    <- pr_t$capital_cost_end
  lossprob_vec[t]        <- pr_t$loss_prob
  VaR99_vec[t]           <- pr_t$VaR99_insured_after_ded
  
  profit_end_mat[, t] <- pr_t$profit_end
  
  ## ---- GROUP-SPECIFIC premium allocation (year t) ----
  EL_ins_g    <- sim_t$EL_insurer_claims_by_g
  EL_reins_g  <- sim_t$EL_reins_recovery_by_g
  EL_insd_g   <- sim_t$EL_insured_after_ded_by_g
  
  # allocate capital cost to groups by expected insured-after-ded share
  cap_wt <- if (sum(EL_insd_g) > 0) EL_insd_g / sum(EL_insd_g) else rep(1 / n_groups, n_groups)
  cap_cost_end_g <- cap_cost_end_vec[t] * cap_wt
  
  # group reinsurance premium (end-year, loaded)
  reins_prem_end_g <- EL_reins_g * (1 + reinsurance_loading)
  
  denom <- 1 - (expense_loading + brokerage_loading)
  if (denom <= 0) stop("expense_loading + brokerage_loading must be < 1")
  
  # unscaled group premiums
  prem_end_g_unscaled   <- (EL_ins_g + reins_prem_end_g + cap_cost_end_g) / denom
  prem_start_g_unscaled <- prem_end_g_unscaled / (1 + r_year[t])
  
  # rescale so group premiums sum to the portfolio premium for this year
  scale_factor <- premium_start_vec[t] / sum(prem_start_g_unscaled)
  prem_start_g <- prem_start_g_unscaled * scale_factor
  prem_end_g   <- prem_end_g_unscaled   * scale_factor
  
  group_prem_start_mat[, t] <- prem_start_g
  group_prem_end_mat[, t]   <- prem_end_g
  
  # per-employee + relativity vs portfolio avg per-employee
  prem_per_emp_g <- prem_start_g / pmax(1, cq_wc_groups2$n_employees)
  port_avg_per_emp <- sum(prem_start_g) / sum(cq_wc_groups2$n_employees)
  
  group_prem_per_emp_mat[, t] <- prem_per_emp_g
  group_rel_mat[, t]          <- prem_per_emp_g / port_avg_per_emp
}


## 8) PV of 10-year profit at time 0 (end-year cashflows)
disc_end <- 1 / cumprod(1 + r_year)               # discount end of year t to time 0
pv_profit_10y <- as.numeric(profit_end_mat %*% disc_end)

# 10-year aggregate insurer loss (nominal, end-year basis)
insurer_loss_10y <- rowSums(ins_claims_end_mat)


## 9) Year-by-year summary tables (portfolio)
year_summary_tbl <- dplyr::bind_rows(lapply(seq_len(n_years), function(t) {
  tibble::tibble(
    year = years_vec[t],
    inflation_rate = infl_rate[t],
    interest_rate  = r_year[t],
    growth_factor  = growth_overall[t],
    deductible     = ded_vec[t],
    retention      = ret_vec[t],
    
    premium_start  = premium_start_vec[t],
    premium_end    = premium_end_vec[t],
    expenses_end   = expenses_end_vec[t],
    brokerage_end  = broker_end_vec[t],
    reins_prem_end = reins_prem_end_vec[t],
    capital_cost_end = cap_cost_end_vec[t],
    VaR99_insured_after_ded = VaR99_vec[t],
    
    target_loss_prob = target_loss_prob,
    achieved_loss_prob = lossprob_vec[t]
  ) %>%
    dplyr::bind_cols(
      summarise_dist(ins_claims_end_mat[, t]) %>% dplyr::rename_with(~paste0("claims_", .x)),
      summarise_dist(profit_end_mat[, t])     %>% dplyr::rename_with(~paste0("profit_", .x))
    )
}))

year_summary_tbl


## 10) Group premium table (ALL years, occupation x employment_type)
group_premium_tbl_all <- dplyr::bind_rows(lapply(seq_len(n_years), function(t) {
  port_avg_per_emp <- sum(group_prem_start_mat[, t]) / sum(cq_wc_groups2$n_employees)
  
  cq_wc_groups2 %>%
    dplyr::mutate(
      year = years_vec[t],
      premium_start = group_prem_start_mat[, t],
      premium_end   = group_prem_end_mat[, t],
      premium_per_employee = group_prem_per_emp_mat[, t],
      premium_relativity   = group_rel_mat[, t],
      portfolio_avg_premium_per_employee = port_avg_per_emp
    ) %>%
    dplyr::select(
      year, occupation, employment_type, n_employees,
      premium_start, premium_per_employee, premium_relativity,
      portfolio_avg_premium_per_employee
    )
}))

group_premium_tbl_all %>% print(n = Inf)



## 11) Compact annual cashflow table (portfolio)
annual_cashflow_tbl <- year_summary_tbl %>%
  dplyr::transmute(
    year,
    inflation_rate,
    interest_rate,
    growth_factor,
    annual_premium_start = premium_start,
    annual_premium_end   = premium_end,
    annual_expenses_end  = expenses_end,
    annual_brokerage_end = brokerage_end,
    annual_reins_prem_end = reins_prem_end,
    annual_deductible = deductible,
    annual_retention  = retention,
    annual_claim_payout_mean = claims_mean,
    annual_net_revenue_mean  = profit_mean,
    achieved_loss_prob
  )

annual_cashflow_tbl



## 12) PV summary + plots
pv_summary_tbl <- summarise_dist(pv_profit_10y) %>%
  dplyr::mutate(metric = "PV of 10y profit at time 0 (claims paid mid-year)") %>%
  dplyr::select(metric, dplyr::everything())

pv_summary_tbl

par(mfrow = c(1,2))
hist(insurer_loss_10y, breaks = 60,
     main = "10-year Aggregate Insurer Loss (end-year basis)",
     xlab = "Total insurer loss over 10 years")
hist(pv_profit_10y, breaks = 60,
     main = "10-year Aggregate Profit (PV at time 0)",
     xlab = "PV of total profit over 10 years")
par(mfrow = c(1,1))

# sanity checks
loss_prob_by_year <- colMeans(profit_end_mat < 0)
loss_prob_by_year
mean(loss_prob_by_year)
mean(pv_profit_10y < 0)



##------------------------------------------------------------------------------
##------------------------------- Summary Table --------------------------------
##------------------------------------------------------------------------------
## Helper functions
summary_mean_sd_var99 <- function(x) {
  tibble(
    Mean = mean(x, na.rm = TRUE),
    `Std Dev` = sd(x, na.rm = TRUE),
    `VaR(99%)` = as.numeric(quantile(x, 0.99, na.rm = TRUE, type = 7))
  )
}

summary_mean_sd_p1 <- function(x) {
  tibble(
    Mean = mean(x, na.rm = TRUE),
    `Std Dev` = sd(x, na.rm = TRUE),
    P1 = as.numeric(quantile(x, 0.01, na.rm = TRUE, type = 7))
  )
}


## 1) SHORT-TERM (2175)
## Annual Loss = aggregate loss distribution you simulated earlier
short_annual_loss <- S

## Total Insurer Cost = Annual Loss + 10% premium + reinsurance price
## (using your group's definition)
short_expense_cost <- rep(0.10 * premium_target * (1 + interest_rate), length(S))
short_reins_cost   <- rep(reinsurance_premium_end, length(S))

short_total_insurer_cost <- short_annual_loss + short_expense_cost + short_reins_cost

## Net Revenue = already computed
short_net_revenue <- profit_simulation


## 2) LONG-TERM (10-year total, present value)
## Long-term Annual Loss = PV of total aggregate loss over 10 years
simulate_one_year_wc_gross_long <- function(
    cq_groups,
    growth_t,
    infl_t,
    r_t,
    sdlog, p_tail, u, xi, beta,
    per_employee = FALSE,
    midyear_claims = TRUE
) {
  total_gross_mid <- 0
  
  for (g in seq_len(nrow(cq_groups))) {
    n_g  <- cq_groups$n_employees[g]
    lam  <- cq_groups$lambda_group[g]
    mlog <- cq_groups$meanlog_group[g]
    
    if (is.na(n_g) || n_g <= 0 || is.na(lam) || lam < 0 || is.na(mlog)) next
    
    mu_g <- (n_g * lam) * growth_t
    
    if (per_employee) {
      N <- sum(rpois(n_g, lam * growth_t))
    } else {
      N <- rpois(1, mu_g)
    }
    
    if (N == 0) next
    
    sev <- rsev_spliced(
      n = N,
      meanlog_source = mlog,
      sdlog = sdlog,
      p_tail = p_tail,
      u = u,
      xi = xi,
      beta = beta,
      severity_scale = infl_t
    )
    
    total_gross_mid <- total_gross_mid + sum(sev)
  }
  
  ## move claims to end-year value if paid mid-year
  total_gross_end <- if (midyear_claims) total_gross_mid * (1 + r_t)^0.5 else total_gross_mid
  
  total_gross_end
}

## Simulate yearly gross aggregate losses across all 10 years
set.seed(4040)
gross_loss_end_mat <- matrix(0, nrow = n_sim_10y, ncol = n_years)

for (i in seq_len(n_sim_10y)) {
  for (t in seq_len(n_years)) {
    gross_loss_end_mat[i, t] <- simulate_one_year_wc_gross_long(
      cq_groups = cq_wc_groups2,
      growth_t  = growth_overall[t],
      infl_t    = infl_factor[t],
      r_t       = r_year[t],
      sdlog     = sdlog_cq2,
      p_tail    = p_tail_cq2,
      u         = u_cq2,
      xi        = xi_cq2,
      beta      = beta_cq2,
      per_employee = FALSE,
      midyear_claims = TRUE
    )
  }
}

## PV of total aggregate loss over 10 years
pv_annual_loss_10y <- as.numeric(gross_loss_end_mat %*% disc_end)

## Long-term Total Insurer Cost = PV of [Annual Loss + 10% premium + reinsurance price]
expense_end_vec_simple <- 0.10 * premium_end_vec

expense_end_mat_simple <- matrix(
  rep(expense_end_vec_simple, each = n_sim_10y),
  nrow = n_sim_10y,
  ncol = n_years
)

reins_prem_end_mat <- matrix(
  rep(reins_prem_end_vec, each = n_sim_10y),
  nrow = n_sim_10y,
  ncol = n_years
)

long_total_cost_mat <- gross_loss_end_mat +
  expense_end_mat_simple +
  reins_prem_end_mat

pv_total_insurer_cost_10y <- as.numeric(long_total_cost_mat %*% disc_end)

## Long-term Net Revenue = PV of total 10-year net revenue
long_net_revenue_pv <- pv_profit_10y



## 3) Aggregate Loss & Insurer Cost Distribution Summary
agg_cost_summary_tbl <- bind_rows(
  summary_mean_sd_var99(short_annual_loss) %>%
    mutate(Item = "Annual Loss", .before = 1) %>%
    rename(
      `Short Term Mean (Ð)` = Mean,
      `Short Term Std Dev` = `Std Dev`,
      `Short Term VaR(99%)` = `VaR(99%)`
    ) %>%
    bind_cols(
      summary_mean_sd_var99(pv_annual_loss_10y) %>%
        rename(
          `Long Term Mean (PV)` = Mean,
          `Long Term Std Dev` = `Std Dev`,
          `Long Term VaR(99%)` = `VaR(99%)`
        )
    ),
  
  summary_mean_sd_var99(short_total_insurer_cost) %>%
    mutate(Item = "Total Insurer Cost", .before = 1) %>%
    rename(
      `Short Term Mean (Ð)` = Mean,
      `Short Term Std Dev` = `Std Dev`,
      `Short Term VaR(99%)` = `VaR(99%)`
    ) %>%
    bind_cols(
      summary_mean_sd_var99(pv_total_insurer_cost_10y) %>%
        rename(
          `Long Term Mean (PV)` = Mean,
          `Long Term Std Dev` = `Std Dev`,
          `Long Term VaR(99%)` = `VaR(99%)`
        )
    )
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

agg_cost_summary_tbl



## 4) Net Revenue Distribution Summary
net_revenue_summary_tbl <- summary_mean_sd_p1(short_net_revenue) %>%
  rename(
    `Short Term Mean` = Mean,
    `Short Term Std Dev` = `Std Dev`,
    `Short Term P1` = P1
  ) %>%
  bind_cols(
    summary_mean_sd_p1(long_net_revenue_pv) %>%
      rename(
        `Long Term Mean (PV)` = Mean,
        `Long Term Std Dev` = `Std Dev`,
        `Long Term P1` = P1
      )
  ) %>%
  mutate(Item = "Net Revenue", .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

net_revenue_summary_tbl



##------------------------------------------------------------------------------
##--------------------------------- EDA ----------------------------------------
##------------------------------------------------------------------------------
##############################################################
# Exploratory Data Analysis (EDA)
##############################################################

par(mfrow=c(2,2))

# Histogram of claim amounts (severity)
hist(workcomp_sev$claim_amount[workcomp_sev$claim_amount <= 50000],
     breaks = 50,
     col = "steelblue",
     border = "white",
     main = "Distribution of Claim Amount",
     xlab = "Claim Amount",
     xlim = c(0, 50000))

## Summary statistics for claim amount (excluding negative values)

claim_amount_summary <- workcomp_sev %>%
  filter(claim_amount >= 0) %>%
  summarise(
    n      = n(),
    mean   = mean(claim_amount, na.rm = TRUE),
    sd     = sd(claim_amount, na.rm = TRUE),
    min    = min(claim_amount, na.rm = TRUE),
    q25    = quantile(claim_amount, 0.25, na.rm = TRUE),
    median = median(claim_amount, na.rm = TRUE),
    q75    = quantile(claim_amount, 0.75, na.rm = TRUE),
    p90    = quantile(claim_amount, 0.90, na.rm = TRUE),
    p95    = quantile(claim_amount, 0.95, na.rm = TRUE),
    p99    = quantile(claim_amount, 0.99, na.rm = TRUE),
    max    = max(claim_amount, na.rm = TRUE)
  )

claim_amount_summary

