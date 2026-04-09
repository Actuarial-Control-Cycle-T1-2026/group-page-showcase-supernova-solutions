#install.packages("MASS")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("pscl")
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyverse)
library(MASS)
library(pscl)

## Read Business interruption data

busIntFreqData <- read.csv("srcsc-2026-claims-business-interruption-freq.csv")

busIntSevData <- read.csv("srcsc-2026-claims-business-interruption-sev.csv")


## Clean data
busIntFreqData$solar_system <- as.character(busIntFreqData$solar_system)
busIntSevData$solar_system <- as.character(busIntSevData$solar_system)


busIntFreqData$solar_system <- sub("_\\?\\?\\?[0-9]+$", "", busIntFreqData$solar_system)
busIntSevData$solar_system <- sub("_\\?\\?\\?[0-9]+$", "", busIntSevData$solar_system)

busIntFreqData$station_id <- sub("_\\?\\?\\?[0-9]+$", "", busIntFreqData$station_id)
busIntSevData$station_id <- sub("_\\?\\?\\?[0-9]+$", "", busIntSevData$station_id)
busIntSevData$claim_id <- sub("_\\?\\?\\?[0-9]+$", "", busIntSevData$claim_id)


busIntFreqData$policy_id <- sub("_\\?\\?\\?[0-9]+$", "", busIntFreqData$policy_id)
busIntSevData$policy_id <- sub("_\\?\\?\\?[0-9]+$", "", busIntSevData$policy_id)


# Initial Sample Statistics for Frequency and Severity Data

SampleStats <- busIntFreqData %>% 
  filter(production_load >= 0) %>%
  filter(production_load <= 1) %>%
  group_by(solar_system) %>%
  summarise(avg_production_load = mean(production_load))

SampleStats <- busIntFreqData %>% 
  filter(claim_count <= 4)  %>% 
  filter(claim_count >= 0) %>%
  filter(exposure >= 0) %>%
  filter(exposure <= 1) %>%
  group_by(solar_system) %>%
  summarise(tot_claims = sum(claim_count), 
            avg_claim_count = mean(claim_count), 
            exposureTime = sum(exposure),
            claimsperexposure = tot_claims/exposureTime)

SampleStats <- busIntSevData %>% 
  filter(claim_amount <= 1426000)  %>% 
  filter(claim_amount >= 28000) %>%
  filter(exposure >= 0) %>%
  filter(exposure <= 1) %>%
  group_by(solar_system) %>%
  summarise(tot_claim_amt = sum(claim_amount), 
            avg_claim_amt = mean(claim_amount), 
            exposureTime = sum(exposure),
            claimsperexposure = tot_claim_amt/exposureTime)


# Complete cleaned frequency data

cleanedFreqData <- busIntFreqData %>% 
  filter(claim_count <= 4)  %>% 
  filter(claim_count >= 0) %>%
  filter(exposure > 0) %>%
  filter(exposure <= 1) %>%
  filter(production_load >= 0) %>%
  filter(production_load <= 1) %>%
  filter(energy_backup_score >= 1) %>%
  filter(energy_backup_score <= 5) %>%
  filter(supply_chain_index <= 1) %>%
  filter(supply_chain_index >= 0) %>%
  filter(avg_crew_exp >= 1) %>%
  filter(avg_crew_exp <= 30) %>%
  filter(maintenance_freq <= 6) %>%
  filter(maintenance_freq >= 0) %>%
  filter(safety_compliance <= 5) %>%
  filter(safety_compliance >= 1) %>%
  filter(solar_system != "")

## Get removed data

missingRows <- anti_join(busIntFreqData, cleanedFreqData)

cleanedFreqData$station_id <- as.factor(cleanedFreqData$station_id)
cleanedFreqData$solar_system <- as.factor(cleanedFreqData$solar_system)
cleanedFreqData$energy_backup_score <- as.factor(cleanedFreqData$energy_backup_score)
cleanedFreqData$safety_compliance <- as.factor(cleanedFreqData$safety_compliance)

## Complete frequency data sample statistics for all fields

SampleStats <- cleanedFreqData %>% 
  group_by(solar_system) %>%
  summarise(tot_claims = sum(claim_count), 
            avg_claim_count = mean(claim_count), 
            exposureTime = sum(exposure),
            claimsperexposure = tot_claims/exposureTime,
            production_load = mean(production_load),
            supplyindex = mean(supply_chain_index), averagecrewexp = mean(avg_crew_exp),
            maintenance_freq = median(maintenance_freq))


# Claim Count Histogram
x <- cleanedFreqData$claim_count

hist(x, probability = TRUE, breaks = 20)



# Missing data mean and variance
mean(missingRows$claim_count, na.rm = TRUE)
var(missingRows$claim_count, na.rm = TRUE)


# KS test to see if missing data is statistically different
ks.test(missingRows$claim_count, cleanedFreqData$claim_count)

meanFreq <- sum(cleanedFreqData$claim_count)/sum(cleanedFreqData$exposure)

cleanedFreqData$freq <- cleanedFreqData$claim_count/cleanedFreqData$exposure
varFreq <- weighted.mean((cleanedFreqData$freq - meanFreq)^2, cleanedFreqData$exposure)


#################
# Model Testing
#################

###########################
# Frequency Data Modelling
###########################

# Negative Binomial Model

# Ensure exposure is logged for the offset

cleanedFreqData$log_exp <- log(cleanedFreqData$exposure)

# Add exposure to frequency model
freq_model <- glm.nb(claim_count ~ production_load + 
                       as.factor(station_id) +
                       as.factor(solar_system) +
                       as.factor(energy_backup_score) + 
                       supply_chain_index + 
                       avg_crew_exp + 
                       maintenance_freq + 
                       as.factor(safety_compliance) +
                       offset(log_exp),
                     data = cleanedFreqData)

summary(freq_model)

# Check AIC
AIC(freq_model)


# Cleaned Model using stepwise selection
#step_model <- stepAIC(hurdle_model, direction = "both")

cleaned_freq_model <- glm.nb(claim_count ~
                               as.factor(energy_backup_score) + 
                               supply_chain_index + 
                               maintenance_freq +
                               offset(log_exp),
                             data = cleanedFreqData)

summary(cleaned_freq_model)
AIC(cleaned_freq_model)

mean(cleanedFreqData$claim_count)
var(cleanedFreqData$claim_count)

# Fit Poisson Model
pois <- fitdistr(cleanedFreqData$claim_count, "Poisson")

# Compare AIC
AIC(pois)
AIC(freq_model)


# Create the Hurdle Negative Binomial Model
hurdle_model <- hurdle(
  claim_count ~ production_load + 
    as.factor(station_id) +
    as.factor(solar_system) +
    as.factor(energy_backup_score) +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    as.factor(safety_compliance)
  |   # zero-hurdle part
    production_load + 
    as.factor(station_id) +
    as.factor(solar_system) +
    as.factor(energy_backup_score) +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    as.factor(safety_compliance),
  data = cleanedFreqData,
  dist = "negbin",        # negative binomial for counts
  zero.dist = "binomial", # logistic regression for zeros
  link = "logit",          # default for zero part
  offset = log_exp         # exposure offset
)

# Inspect the model
summary(hurdle_model)
AIC(hurdle_model)

# Perform stepwise selection
#step_model <- stepAIC(hurdle_model, direction = "both")


#cleanedFreqData$station_id <- as.factor(cleanedFreqData$station_id)
#cleanedFreqData$solar_system <- as.factor(cleanedFreqData$solar_system)
#cleanedFreqData$energy_backup_score <- as.factor(cleanedFreqData$energy_backup_score)
#cleanedFreqData$safety_compliance <- as.factor(cleanedFreqData$safety_compliance)

# collapse rare station_id levels to reduce noise
# (combine stations with very few observations into "Other")
station_counts <- table(cleanedFreqData$station_id)
rare_stations <- names(station_counts[station_counts < 20])
cleanedFreqData$station_id <- factor(
  ifelse(cleanedFreqData$station_id %in% rare_stations, "Other", cleanedFreqData$station_id)
)


# Cleaned Hurdle Negative Binomial Model
hurdle_model_clean <- hurdle(
  claim_count ~ maintenance_freq
  |  # Zero-hurdle part
   maintenance_freq + supply_chain_index + as.factor(energy_backup_score),
  data = cleanedFreqData,
  dist = "negbin",        # NB for count part
  zero.dist = "binomial", # logistic for zero-hurdle
  link = "logit",         # default
  offset = log(exposure)  # exposure offset
)


# Inspect the model
summary(hurdle_model_clean)
AIC(hurdle_model_clean)


# Simulate claim counts using the cleaned hurdle model
n <- nrow(cleanedFreqData)

p_zero <- predict(hurdle_model_clean, type = "prob")[,1]
is_zero <- rbinom(n, 1, p_zero)

mu_count <- predict(hurdle_model_clean, type = "count")
theta <- hurdle_model_clean$theta

# truncated NB
p0 <- dnbinom(0, mu = mu_count, size = theta)
u <- runif(n, p0, 1)
sim_counts <- qnbinom(u, mu = mu_count, size = theta)

# combine
sim_claims <- ifelse(is_zero == 1, 0, sim_counts)
sim_claims <- pmin(sim_claims, 4)

# Table of Simulated Claim counts
table(sim_claims)

# Production Risk Test
# cleanedFreqData$production_risk <- cleanedFreqData$production_load/cleanedFreqData$safety_compliance
# Created Production Risk variable is statistically insignificant.


# Poisson Model

# Fit Poisson Model
freq_model_poi <- glm(claim_count ~ production_load + 
                       as.factor(station_id) +
                       as.factor(solar_system) +
                       as.factor(energy_backup_score) + 
                       supply_chain_index + 
                       avg_crew_exp + 
                       maintenance_freq + 
                       as.factor(safety_compliance) + 
                       offset(log_exp), family = poisson(link = "log"),
                     data = cleanedFreqData)

# Inspect Model
summary(freq_model_poi)
AIC(freq_model_poi)

# Fit Cleaned Poisson Model (removed insignificant predictors)
cleaned_freq_model_poi <- glm(claim_count ~
                    
                    as.factor(solar_system) +
                    as.factor(energy_backup_score) + 
                    supply_chain_index + 
                    maintenance_freq + 
                    offset(log_exp), family = poisson(link = "log"),
                  data = cleanedFreqData)

# Inspect cleaned poisson model
summary(cleaned_freq_model_poi)
AIC(cleaned_freq_model_poi)


# Zero Inflation Model

model <- zeroinfl(
  claim_count ~ production_load + as.factor(station_id) + as.factor(solar_system) + 
    as.factor(energy_backup_score) + supply_chain_index + 
    avg_crew_exp + maintenance_freq + as.factor(safety_compliance) + 
    offset(log_exp) | 1,
  data = cleanedFreqData,
  dist = "negbin"
)

# Inspect Model
summary(model)


# Cleaned zero inflation model
cleaned_model <- zeroinfl(
  claim_count ~ 
    as.factor(energy_backup_score) + supply_chain_index + maintenance_freq +
    offset(log_exp) | 1, # <--- The pipe goes HERE
  data = cleanedFreqData,
  dist = "negbin"
)

# Inspect Model
summary(cleaned_model)
AIC(cleaned_model)

##########################
## Severity Data Modelling
###########################

# Clean Claim Severity Data
cleanedSevData <- busIntSevData %>% 
  filter(claim_amount > 0) %>%
  filter(exposure > 0) %>%
  filter(exposure <= 1) %>%
  filter(production_load >= 0) %>%
  filter(production_load <= 1) %>%
  filter(energy_backup_score >= 1) %>%
  filter(energy_backup_score <= 5) %>%
  filter(safety_compliance <= 5) %>%
  filter(safety_compliance >= 1) %>%
  filter(solar_system != "") %>%
  filter(claim_seq >= 1) %>%
  filter(claim_seq <= 4)

# Summary of claim amounts
summary(cleanedSevData$claim_amount)

# Claim Amount Box Plot
boxplot(cleanedSevData$claim_amount, col = "steelblue")

# Claim Amount Histogram
hist(log(cleanedSevData$claim_amount), 
     breaks = 50, 
     probability = TRUE)

lines(density(log(cleanedSevData$claim_amount)), col = "blue")

# Sample statistics for Severity Data
SampleStats <- cleanedSevData %>% 
  group_by(solar_system) %>%
  summarise(tot_claim_amt = sum(claim_amount), 
            avg_claim_amt = mean(claim_amount), 
            exposureTime = sum(exposure),
            claimsperexposure = tot_claim_amt/exposureTime)


## QQ plot of claim amounts
qqnorm(log(cleanedSevData$claim_amount))
qqline(log(cleanedSevData$claim_amount), col = "red")

# Mean Claim Amounts by Solar System
SampleStats <- cleanedSevData %>% 
  group_by(solar_system) %>%
  summarise(claim_amt = mean(claim_amount))

# Gamma Model
sev_model_gamma <- glm(
  claim_amount ~ production_load + 
    as.factor(station_id) +
    as.factor(solar_system) +
    as.factor(energy_backup_score) + 
    as.factor(safety_compliance) +
    as.factor(claim_seq),
  family = Gamma(link = "log"),
  data = cleanedSevData
)

# Inspect Model
summary(sev_model_gamma)


# Cleaned Gamma Model
sev_model_gamma <- glm(
  claim_amount ~
    as.factor(solar_system) +
    as.factor(energy_backup_score) + 
    as.factor(claim_seq),
  family = Gamma(link = "log"),
  data = cleanedSevData
)

# Inspect Model
summary(sev_model_gamma)

# Calculate dispersion ratio
pearson_resid <- residuals(sev_model_gamma, type = "pearson")
dispersion_ratio <- sum(pearson_resid^2) / sev_model_gamma$df.residual


# Fit Lognormal Model
sev_model_lognorm_all <- glm(
  log(claim_amount) ~ production_load + 
    as.factor(station_id) +
    as.factor(solar_system) +
    as.factor(energy_backup_score) + 
    as.factor(safety_compliance) +
    as.factor(claim_seq),
  family = gaussian(link = "identity"),
  data = cleanedSevData
)

# Inspect Log-normal Model
summary(sev_model_lognorm_all)


# Cleaned Log-normal Model
sev_model_lognorm <- glm(
  log(claim_amount) ~
    as.factor(energy_backup_score) + 
    as.factor(claim_seq),
  family = gaussian(link = "identity"),
  data = cleanedSevData
)

# Residual graphs
par(mfrow = c(2,2))
plot(sev_model_lognorm)

# AIC comparison between Gamma and Log-normal
AIC(sev_model_gamma, sev_model_lognorm)

# Predict from the fitted log-normal
pred_lognorm <- 
  exp(predict(sev_model_lognorm) + 
        0.5 * summary(sev_model_lognorm)$dispersion)

# Compare Predicted vs Observed Quantiles
cleanedSevData %>%
  mutate(decile = ntile(pred_lognorm, 10)) %>%
  group_by(decile) %>%
  summarise(
    avg_obs = mean(claim_amount),
    avg_pred = mean(pred_lognorm)
  )

# Fit inverse Gaussian
sev_model_ig <- glm(
  claim_amount ~ 
    as.factor(station_id) +
    as.factor(solar_system) +
    as.factor(energy_backup_score) + 
    as.factor(claim_seq),
  family = inverse.gaussian(link = "log"),
  data = cleanedSevData
)

# Inspect Model
summary(sev_model_ig)

# Compare Predicted vs Observed quantiles
cleanedSevData$pred_ig <- 
  predict(sev_model_ig, type = "response")

cleanedSevData %>%
  mutate(decile = ntile(pred_ig, 10)) %>%
  group_by(decile) %>%
  summarise(
    avg_obs = mean(claim_amount),
    avg_pred = mean(pred_ig)
  )


# Compute residuals on log scale
log_resid <- residuals(sev_model_lognorm, type = "pearson")

# Q-Q plots
qqnorm(log_resid, main = "Q-Q Plot: Lognormal Residuals")
qqline(log_resid, col = "red")

gamma_resid <- residuals(sev_model_gamma, type = "deviance")

qqnorm(gamma_resid, main = "Q-Q Plot: Gamma Deviance Residuals")
qqline(gamma_resid, col = "red")

ig_resid <- residuals(sev_model_ig, type = "deviance")

qqnorm(ig_resid, main = "Q-Q Plot: Inverse Gaussian Deviance Residuals")
qqline(ig_resid, col = "red")


# Claim Amount log-normal mean and variance
sev_mean <- mean(log(cleanedSevData$claim_amount))
sev_var <- var(log(cleanedSevData$claim_amount))

# Log-normal parameters
LN_mu <- exp(sev_mean+0.5*sev_var)
LN_sigma_squared <- exp(2*sev_mean+sev_var)*(exp(sev_var)-1)



# Helonis Cluster Specific Severity Data
helionOnly <- cleanedSevData %>%
  filter(solar_system == "Helionis Cluster")

# Helonis Cluster Log-normal Parameters
sev_mean_hel <- mean(log(helionOnly$claim_amount))
sev_var_hel <- var(log(helionOnly$claim_amount))

LN_mu_hel <- exp(sev_mean_hel+0.5*sev_var_hel)
LN_sigma_squared_hel <- exp(2*sev_mean_hel+sev_var_hel)*(exp(sev_var_hel)-1)


# Zeta Specific Severity Data and Log-normal Parameters
zetaOnly <- cleanedSevData %>%
  filter(solar_system == "Zeta")

sev_mean_zet <- mean(log(zetaOnly$claim_amount))
sev_var_zet <- var(log(zetaOnly$claim_amount))

LN_mu_zet <- exp(sev_mean_zet+0.5*sev_var_zet)
LN_sigma_squared_zet <- exp(2*sev_mean_zet+sev_var_zet)*(exp(sev_var_zet)-1)


# Epsilon Specific Severity Data and Log-normal Parameters
epsilonOnly <- cleanedSevData %>%
  filter(solar_system == "Epsilon")


sev_mean_ep <- mean(log(epsilonOnly$claim_amount))
sev_var_ep <- var(log(epsilonOnly$claim_amount))

LN_mu_ep <- exp(sev_mean_ep+0.5*sev_var_ep)
LN_sigma_squared_ep <- exp(2*sev_mean_ep+sev_var_ep)*(exp(sev_var_ep)-1)


# Subset data into non Helonis Cluster solar systems
others <- cleanedSevData %>%
  filter(solar_system != "Helionis Cluster")

# Test if the mean claim amount is statistically different 
t.test(
  log(helionOnly$claim_amount),
  log(others$claim_amount)
)

# Test if the variance of claim amounts is statistically different 
var.test(
  log(helionOnly$claim_amount),
  log(others$claim_amount)
)

#######################
# Simulating Frequency
#######################

# Cleaned Hurdle Negative Binomial Model
hurdle_model_clean <- hurdle(
  claim_count ~ maintenance_freq
  |  # Zero-hurdle part
    maintenance_freq + supply_chain_index + as.factor(energy_backup_score) + offset(log(exposure)),
  data = cleanedFreqData,
  dist = "negbin",        # NB for count part
  zero.dist = "binomial", # logistic for zero-hurdle
  link = "logit"          # exposure offset
)

# Simulate Claims Counts from Hurdle NB model
n <- nrow(cleanedFreqData)

p_zero <- predict(hurdle_model_clean, type = "prob")[,1]
is_zero <- rbinom(n, 1, p_zero)

mu_count <- predict(hurdle_model_clean, type = "count")
theta <- hurdle_model_clean$theta

# truncated NB
p0 <- dnbinom(0, mu = mu_count, size = theta)
u <- runif(n, p0, 1)
sim_counts <- qnbinom(u, mu = mu_count, size = theta)

# combine
sim_claims <- ifelse(is_zero == 1, 0, sim_counts)
sim_claims <- pmin(sim_claims, 4)
table(sim_claims)


## Simulate Claim Counts for 1 year by setting exposure to 1.

newdata <- cleanedFreqData %>%
  filter(solar_system == "Helionis Cluster")
newdata$exposure <- 1   # simulate annual claims
n <- nrow(newdata)

p_zero <- predict(hurdle_model_clean, newdata=newdata, type="prob")[,1]
mu <- predict(hurdle_model_clean, newdata=newdata, type="count")
theta <- hurdle_model_clean$theta

sim_claims <- ifelse(runif(n) < p_zero, 0,
                     rnbinom(n, mu=mu, size=theta) + 1)
sim_claims <- pmin(sim_claims, 4)


table(sim_claims)

############################
# Severity Model Simulation
############################

# Severity Simulation

n <- nrow(newdata)
sev_mean <- mean(log(cleanedSevData$claim_amount))
sev_var <- var(log(cleanedSevData$claim_amount))

LN_mu <- exp(sev_mean+0.5*sev_var)
LN_sigma_squared <- exp(2*sev_mean+sev_var)*(exp(sev_var)-1)

sim_sev <- rlnorm(n, meanlog = sev_mean, sdlog = sqrt(sev_var))

head(sim_sev)

summary(sim_sev)
summary(cleanedSevData$claim_amount)



n <- length(sim_claims)

max_claims <- 4

# create empty claim matrix
claims_mat <- matrix(0, nrow = n, ncol = max_claims)

colnames(claims_mat) <- paste0("Claim", 1:max_claims)

# simulate severities row by row
for(i in 1:n){
  
  freq <- sim_claims[i]
  
  if(freq > 0){
    claims_mat[i,1:freq] <- rlnorm(freq, meanlog = sev_mean, sdlog = sqrt(sev_var))
  }
  
}

# Build final table
loss_table <- data.frame(
  LossID = 1:n,
  Frequency = sim_claims,
  claims_mat
)

# total loss
loss_table$TotalLoss <- rowSums(loss_table[,3:6])

# Inspect Table of Losses
summary(loss_table$TotalLoss)



## Portfolio Simulation Function
simulate_portfolio <- function(n_policies = 55, alt_variance){
  
  # frequency simulation
  rows <- sample(1:nrow(newdata), n_policies, replace = TRUE)
  sample_data <- newdata[rows, ]
  
  p_zero <- predict(hurdle_model_clean, newdata=sample_data, type="prob")[,1]
  mu <- predict(hurdle_model_clean, newdata=sample_data, type="count")
  theta <- hurdle_model_clean$theta
  
  
  sim_claims <- ifelse(runif(n_policies) < p_zero, 0,
                       rnbinom(n_policies, mu = mu, size = theta) + 1)
  
  sim_claims <- pmin(sim_claims, 4)
  
  max_claims <- 4
  
  claims_mat <- matrix(0, nrow = n_policies, ncol = max_claims)
  
  for(i in 1:n_policies){
    
    freq <- sim_claims[i]
    
    # Use adjusted variance for solar system risk.
    if(freq > 0){
      claims_mat[i,1:freq] <- rlnorm(freq,
                                     meanlog = sev_mean,
                                     sdlog = sqrt(alt_variance))
      claims_mat[i, 1:freq][claims_mat[i, 1:freq] < 30000] <- 0 # Remove Deductibles
    }
    
  }
  
  total_loss <- sum(claims_mat)
  
  return(total_loss)
}


### Simulate Aggregate Losses for Helionis (1 Year)

sev_var_hel <- var(log(helionOnly$claim_amount))

n_sim <- 10000

aggregate_losses_hel <- replicate(n_sim, simulate_portfolio(55, sev_var_hel))

hist(aggregate_losses_hel,
     breaks = 100,
     col = "skyblue",
     main = "Aggregate Loss Distribution (55 Policies)",
     xlab = "Total Portfolio Loss")

summary(aggregate_losses_hel)

## Simulate Aggregate Losses for Bayesia (1 Year)

sev_var_bay <- var(log(helionOnly$claim_amount)) * 0.86

n_sim <- 10000

aggregate_losses_bay <- replicate(n_sim, simulate_portfolio(55, sev_var_bay))
summary(aggregate_losses_bay)


hist(aggregate_losses_bay,
     breaks = 100,
     col = "skyblue",
     main = "Aggregate Loss Distribution (55 Policies)",
     xlab = "Total Portfolio Loss")

### Simulate Aggregate Losses for Oryn Delta (1 Year)

sev_var_orn <- var(log(helionOnly$claim_amount)) * 1.025

n_sim <- 10000

aggregate_losses_orn <- replicate(n_sim, simulate_portfolio(55, sev_var_orn))

summary(aggregate_losses_orn)

hist(aggregate_losses_orn,
     breaks = 100,
     col = "skyblue",
     main = "Aggregate Loss Distribution (55 Policies)",
     xlab = "Total Portfolio Loss")


# AGGREGATE LOSS Simulation (all solar systems)

## Mixed portfolio simulation function
simulate_mixed_portfolio <- function(n_hel = 30, n_bay = 15, n_orn = 10,
                                     sev_var_hel, sev_var_bay, sev_var_orn){
  
  # Helionis
  loss_hel <- simulate_portfolio(n_policies = n_hel, alt_variance = sev_var_hel)
  
  # Bayesia
  loss_bay <- simulate_portfolio(n_policies = n_bay, alt_variance = sev_var_bay)
  
  # Orynn Delta
  loss_orn <- simulate_portfolio(n_policies = n_orn, alt_variance = sev_var_orn)
  
  # Total portfolio loss
  total_loss <- loss_hel + loss_bay + loss_orn
  
  return(total_loss)
}

# Simulate Portfolio of Losses
n_sim <- 10000

aggregate_losses_mixed <- replicate(
  n_sim,
  simulate_mixed_portfolio(
    n_hel = 30, n_bay = 15, n_orn = 10,
    sev_var_hel = sev_var_hel,
    sev_var_bay = sev_var_bay,
    sev_var_orn = sev_var_orn
  )
)

summary(aggregate_losses_mixed)

# Histogram of Losses
hist(aggregate_losses_mixed,
     breaks = 100,
     col = "skyblue",
     main = "Aggregate Loss Distribution (Mixed Portfolio)",
     xlab = "Total Portfolio Loss")

# Cleaned Histogram of Losses
summary(aggregate_losses_mixed)
hist(aggregate_losses_mixed[aggregate_losses_mixed <2*10^8],
     breaks = 100,
     col = "skyblue",
     main = "Aggregate Loss Distribution",
     xlab = "Total Portfolio Loss")

agg_data_cdf <- ecdf(aggregate_losses_mixed)
VaR95 <- quantile(agg_data_cdf, 0.95)
VaR99 <- quantile(agg_data_cdf, 0.99)

# Calculate Pure Premium
#losses <- aggregate_losses_mixed
#attachment <- 110e6

#recoveries <- pmax(losses - attachment, 0)

#pure_premium <- mean(recoveries)


## Simulate Portfolio of Losses over 10 Years

years <- 10

# Expansion of mines over 10 years causes increasing exposure.
hel_exposure <- round(seq(30, 30*1.25, length.out = years))
bay_exposure <- round(seq(15, 15*1.25, length.out = years))
orn_exposure <- round(seq(10, 10*1.15, length.out = years))

# 10 year loss simulation function
simulate_10yr_portfolio <- function(sev_var_hel, sev_var_bay, sev_var_orn){
  
  total_loss <- 0
  
  for(y in 1:10){
    
    yearly_loss <- simulate_mixed_portfolio(
      n_hel = hel_exposure[y],
      n_bay = bay_exposure[y],
      n_orn = orn_exposure[y],
      sev_var_hel = sev_var_hel,
      sev_var_bay = sev_var_bay,
      sev_var_orn = sev_var_orn
    )
    
    total_loss <- total_loss + yearly_loss
  }
  
  return(total_loss)
}

# Simulate 10 year Losses 10000 times.
n_sim <- 10000

aggregate_losses_10yr <- replicate(
  n_sim,
  simulate_10yr_portfolio(
    sev_var_hel = sev_var_hel,
    sev_var_bay = sev_var_bay,
    sev_var_orn = sev_var_orn
  )
)


summary(aggregate_losses_10yr)


## Simulation function accounting for inflation
simulate_portfolio <- function(n_policies = 55, alt_variance, infl){
  
  # frequency simulation
  rows <- sample(1:nrow(newdata), n_policies, replace = TRUE)
  sample_data <- newdata[rows, ]
  
  p_zero <- predict(hurdle_model_clean, newdata=sample_data, type="prob")[,1]
  mu <- predict(hurdle_model_clean, newdata=sample_data, type="count")
  theta <- hurdle_model_clean$theta
  
  
  sim_claims <- ifelse(runif(n_policies) < p_zero, 0,
                       rnbinom(n_policies, mu = mu, size = theta) + 1)
  
  sim_claims <- pmin(sim_claims, 4)
  
  max_claims <- 4
  
  claims_mat <- matrix(0, nrow = n_policies, ncol = max_claims)
  
  for(i in 1:n_policies){
    
    freq <- sim_claims[i]
    
    if(freq > 0){
      claims_mat[i,1:freq] <- rlnorm(freq,
                                     meanlog = sev_mean,
                                     sdlog = sqrt(alt_variance)) * infl #inflation factor
      claims_mat[i, 1:freq][claims_mat[i, 1:freq] < 30000] <- 0 # Remove Deductibles
    }
    
  }
  
  total_loss <- sum(claims_mat)
  
  return(total_loss)
}

## Simulation of 10 year portfolio with 10% frequency shock and 10% severity shock
simulate_portfolio_shock <- function(n_policies = 55, alt_variance, infl){
  
  # frequency simulation
  rows <- sample(1:nrow(newdata), n_policies, replace = TRUE)
  sample_data <- newdata[rows, ]
  
  p_zero <- predict(hurdle_model_clean, newdata=sample_data, type="prob")[,1]*0.9
  mu <- predict(hurdle_model_clean, newdata=sample_data, type="count")
  theta <- hurdle_model_clean$theta
  
  
  sim_claims <- ifelse(runif(n_policies) < p_zero, 0,
                       rnbinom(n_policies, mu = mu, size = theta) + 1)
  
  sim_claims <- pmin(sim_claims, 4)
  
  max_claims <- 4
  
  claims_mat <- matrix(0, nrow = n_policies, ncol = max_claims)
  
  for(i in 1:n_policies){
    
    freq <- sim_claims[i]
    
    if(freq > 0){
      claims_mat[i,1:freq] <- rlnorm(freq,
                                     meanlog = sev_mean,
                                     sdlog = sqrt(alt_variance)) * infl
      claims_mat[i, 1:freq][claims_mat[i, 1:freq] < 30000] <- 0
    }
    
  }
  
  total_loss <- sum(claims_mat)
  
  return(total_loss)
}

# Read forecasted inflation data
inflation_data <- read.csv("forecasts/inflation.csv")
interest_data <- read.csv("forecasts/interest.csv")

infl_factor <- as.vector(inflation_data$inflation_rate/100+1)

# Create cumulative inflation vector
for (i in 2:10) {
  infl_factor[i] <- infl_factor[i - 1] * infl_factor[i]
}

# 10 year losses adjusted for inflation and company growth
simulate_10yr_losses <- function(){
  
  yearly_losses <- numeric(years)
  
  for(y in 1:years){
    
    loss_hel <- simulate_portfolio(hel_exposure[y], sev_var_hel, infl_factor[y])
    loss_bay <- simulate_portfolio(bay_exposure[y], sev_var_bay, infl_factor[y])
    loss_orn <- simulate_portfolio(orn_exposure[y], sev_var_orn, infl_factor[y])
    
    yearly_losses[y] <- loss_hel + loss_bay + loss_orn
  }
  
  yearly_losses
}

# Simulation function adjusted for inflation, 10% severity-frequency shocks and company growth.
simulate_10yr_losses_shock <- function(){
  
  yearly_losses <- numeric(years)
  
  for(y in 1:years){
    
    loss_hel <- simulate_portfolio_shock(hel_exposure[y], sev_var_hel*1.1, infl_factor[y])
    loss_bay <- simulate_portfolio_shock(bay_exposure[y], sev_var_bay*1.1, infl_factor[y])
    loss_orn <- simulate_portfolio_shock(orn_exposure[y], sev_var_orn*1.1, infl_factor[y])
    
    yearly_losses[y] <- loss_hel + loss_bay + loss_orn
  }
  
  yearly_losses
}

n_sim <- 10000

# Simulate 10 year losses accounting for inflation 
loss_matrix <- replicate(n_sim, simulate_10yr_losses())

# Simulate 10 year losses accounting for inflation and 10% shocks to frequency and severity
loss_matrix_shock <- replicate(n_sim, simulate_10yr_losses_shock())

# Each row corresponds to the simulated year. 
# Each column is a simulation of the losses.

expected_losses <- rowMeans(loss_matrix)

# Calculate semi annual interest rate for the first year
interest <- as.vector(interest_data[2,])
interest_vec <- unlist(interest)
semi_annual_interest <- vector(length = 10)
semi_annual_interest <- (1+interest_vec[2])^(1/2)-1

# Find the yearly present value of losses for each year.

expected_loss_year <- vector(length = 10)
median_loss_year <- vector(length = 10)

for (i in 1:10) {
  semi_annual_interest <- (1+interest_vec[i+1]/100)^(1/2)-1
  
  expected_loss_year[i] <- mean(loss_matrix[i,])/(semi_annual_interest+1)
  median_loss_year[i] <- median(loss_matrix[i,])/(semi_annual_interest+1)
}


# Create cumulative interest rate vector
pvinterestrate <- vector(length = 10)
pvinterestrate[1] <- 1+interest_vec[2]/100

for (i in 2:10) {
  pvinterestrate[i] <- (pvinterestrate[i-1])*(1+interest_vec[i+1]/100)
}

# Create matrix for present value of losses
presentvaluelossmatrix <- loss_matrix
presentvaluelossmatrix_shock <- loss_matrix_shock

semi_annual_interest_num <- (1+interest_vec[1+1]/100)^(1/2)-1

# Calculate present values for all simulated losses
presentvaluelossmatrix[1,] <- presentvaluelossmatrix[1,]/(semi_annual_interest_num+1)
presentvaluelossmatrix_shock[1,] <- presentvaluelossmatrix_shock[1,]/(semi_annual_interest_num+1)

for (i in 2:10) { 
  
  semi_annual_interest_num <- (1+interest_vec[i+1]/100)^(1/2)-1
  
  presentvaluelossmatrix[i, ] <- loss_matrix[i, ]/(pvinterestrate[i-1]*(semi_annual_interest_num+1))
  presentvaluelossmatrix_shock[i, ] <- loss_matrix_shock[i, ]/(pvinterestrate[i-1]*(semi_annual_interest_num+1))
}


hist(presentvaluelossmatrix[presentvaluelossmatrix < 2.5e8]/1000000,
     breaks = 80,
     col = "skyblue",
     main = "One Year Aggregate Loss Distribution",
     xlab = "Total Loss ($ millions)")


PVtenYearLossDist <- colSums(presentvaluelossmatrix)

# Plot the Aggregate loss for 10 years in present value
hist(PVtenYearLossDist[PVtenYearLossDist < 1.25e9]/1000000,
     breaks = 60,
     col = "steelblue",
     main = "10 Year Aggregate Loss Distribution (Present Value)",
     xlab = "Total Loss (Đ millions)")


# Plot the Aggregate loss for One year in present value
plotting <- loss_matrix[1,]
plotting <- (plotting[plotting < 2.45e8]/1000000)/((1+interest_vec[1+1]/100)^(1/2))
hist(plotting,
     breaks = 80,
     col = "steelblue",
     main = "One Year Aggregate Loss Distribution (Present Value)",
     xlab = "Total Loss (Đ millions)")

#######################
# Revenue Simulation
#######################

## Standard deviation principle to calculate premium with 0.5 loading factor 
## and expense profit loading of 25%
final_premium <- (mean(presentvaluelossmatrix[1,])+0.5*sqrt(var(presentvaluelossmatrix[1,])))/(1-0.25)

final_premium*(1+interest_vec[2]/100)-final_premium

# Calculate reinsurance premium
losses <- presentvaluelossmatrix[1,]
attachment <- VaR95

recoveries <- pmax(losses - attachment, 0)

# Reinsurance premium
pure_premium <- mean(recoveries)


# Calculate the charged premium and reinsurance cost for each year
yearly_charged_premiums <- vector(length = 10)
reinsurance_charges <-  vector(length = 10)

for (i in 1:10) {
  losses <- presentvaluelossmatrix[i,]
  attachment <- quantile(presentvaluelossmatrix[i,],0.95)
  
  recoveries <- pmax(losses - attachment, 0)
  
  pure_premium <- mean(recoveries)
  reinsurance_charges[i] <- pure_premium
  
  
  yearly_charged_premiums[i] <- (mean(presentvaluelossmatrix[i,])+0.5*sqrt(var(presentvaluelossmatrix[i,])))/(1-0.25)
}

# Vector of forecasted interest rates
real_interest_vec <- c(3.924167,3.109941, 2.479714 ,
                       2.306360 ,2.395142, 2.514046, 
                       2.565149, 2.550692, 2.511367, 2.477747)

real_interest_vec <- 1+real_interest_vec/100

# Calculate yearly profit matrix
# Profit = Annual Premium - Present Value of Loss - Reinsurance Cost - Expenses (10% of premium) + interest earned on premium
profit_matrix <- yearly_charged_premiums -
  presentvaluelossmatrix -
  reinsurance_charges -
  0.1 * yearly_charged_premiums + yearly_charged_premiums*real_interest_vec-yearly_charged_premiums

# Revenue is the sum of columns (sum of revenue for each simulated year)
revenue <- colSums(profit_matrix)

# Probability of Loss over 10 years
revenue_edcf <- ecdf(revenue)
Probability_of_Loss <- revenue_edcf(0)


# 10 year profit matrix with 10% shock to frequency and severity
profit_matrix <- yearly_charged_premiums -
  presentvaluelossmatrix_shock -
  reinsurance_charges -
  0.1 * yearly_charged_premiums + yearly_charged_premiums*real_interest_vec-yearly_charged_premiums

# Probability of loss over 10 years year due to 10% frequency-severity shock.
revenue_shock <- colSums(profit_matrix)
shock_edcf <- ecdf(revenue_shock)
Probability_of_Loss_shock <- shock_edcf(0)


