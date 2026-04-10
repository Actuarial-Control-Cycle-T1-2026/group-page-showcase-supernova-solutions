#Kaleb's R Code for ACTL4001 Assignment
#Loading Libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(MASS)
library(evir)
library(ismev)
library(splines)
library(reshape2)


#Setting Working directory
setwd("C:/Users/vegkj/OneDrive/University/Fourth Year/Term 1/ACTL4001/Case Study/Git Hub Stuff/actl4001-26t1/datasets")

#Loading datasets
claims_equipment_failure_freq <- read_csv("srcsc-2026-claims-equipment-failure-freq.csv")
claims_equipment_failure_sev <- read_csv("srcsc-2026-claims-equipment-failure-sev.csv")

#############################################################################################################
#Cleaning Data
#Identifying Valid Variables
validation_rules <- list(
  policy_id = list(type = "pattern",
                pattern = "^EF-[0-9]{6}$"),
  equipment_id = list(type = "pattern",
                     pattern = "^EQ-[0-9]{6}$"),
  equipment_type = list(type = "categorical",
                        valid = c("Quantum Bore", "Graviton Extractor", "FexStram Carrier", "Ion Pulverizer", "Flux Rider", "ReglAggregators")),  
  equipment_age = list(type = "range",
                range = c(0,30)),  
  solar_system = list(type = "categorical",
                      valid = c("Epsilon", "Zeta", "Helionis Cluster")),
  maintenance_int = list(type = "range",
                         range = c(100,5000)),
  usage_int = list(type = "range",
                         range = c(0,24)),
  exposure = list(type = "range",
                  range = c(0,1)),
  claim_count = list(type = "range",
                     range = c(0,3)),
  claim_amount = list(type = "range", 
                      range = c(10000, 790000))
  
)
#The data description file had inaccurate categories so I used the unique function to determine the true categories to use
#Equipment age range had to be wrong because approximately half the dataset would have been invalid if that was correct. Used the box plot and quantiles to land at a range of 0-30 years
ggplot(claims_equipment_failure_sev, aes(x = equipment_age)) +
     geom_boxplot() +
     coord_cartesian(xlim = c(0, 30))

quantile(claims_equipment_failure_sev$equipment_age, 
                    probs = c(0.05, 0.95), 
                    na.rm = TRUE)


#Creating function to count the amount of invalid variables
count_issues <- function(x, rule) {
  
  # Define missing values
  is_missing <- is.na(x) | x == ""
  missing_count <- sum(is_missing)
  
  # Initialize full-length invalid vector
  invalid_vec <- rep(FALSE, length(x))
  
  non_missing <- !is_missing
  
  if (rule$type == "categorical") {
    invalid_vec[non_missing] <- !(x[non_missing] %in% rule$valid)
    
  } else if (rule$type == "range") {
    invalid_vec[non_missing] <- 
      x[non_missing] < rule$range[1] |
      x[non_missing] > rule$range[2]
    
  } else if (rule$type == "pattern") {
    invalid_vec[non_missing] <- 
      !grepl(rule$pattern, x[non_missing])
  }
  
  invalid_count <- sum(invalid_vec)
  
  c(
    Missing_Count = missing_count,
    Invalid_Count = invalid_count
  )
}

#Applying above function to the claims frequency
result_claims_freq <- sapply(names(validation_rules), function(var) {
  if (var %in% names(claims_equipment_failure_freq)) {
    count_issues(claims_equipment_failure_freq[[var]], validation_rules[[var]])
  } else {
    c(Missing_Count = NA, Invalid_Count = NA)
  }
})

claims_freq_table <- as.data.frame(result_claims_freq)
claims_freq_table

#Applying above function to the claims severity
result_claims_sev <- sapply(names(validation_rules), function(var) {
  if (var %in% names(claims_equipment_failure_sev)) {
    count_issues(claims_equipment_failure_sev[[var]], validation_rules[[var]])
  } else {
    c(Missing_Count = NA, Invalid_Count = NA)
  }
})

claims_sev_table <- as.data.frame(result_claims_sev)
claims_sev_table


#Inspecting invalid categorical data for possible correction
# List of datasets to check
datasets <- list(
  claims_freq = claims_equipment_failure_freq,
  claims_sev  = claims_equipment_failure_sev
)

# Identify all categorical variables from your validation rules
# List of datasets to check
datasets <- list(
  claims_freq = claims_equipment_failure_freq,
  claims_sev  = claims_equipment_failure_sev
)

# Loop over each dataset
for (ds_name in names(datasets)) {
  cat("\n==============================\n")
  cat("Dataset:", ds_name, "\n")
  cat("==============================\n")
  
  data <- datasets[[ds_name]]
  
  # Loop over each variable in validation_rules
  for (var in names(validation_rules)) {
    
    rule <- validation_rules[[var]]
    
    # Only check categorical or pattern variables
    if (rule$type %in% c("categorical", "pattern")) {
      
      non_missing <- !is.na(data[[var]])
      
      if (rule$type == "categorical") {
        invalid_values <- data[[var]][non_missing & !(data[[var]] %in% rule$valid)]
      } else if (rule$type == "pattern") {
        invalid_values <- data[[var]][non_missing & !grepl(rule$pattern, data[[var]])]
      }
      
      if (length(invalid_values) > 0) {
        cat("\nInvalid values for", var, "(", length(invalid_values), "rows ):\n")
        print(unique(invalid_values))  # just list each invalid value once
      } else {
        cat("\nNo invalid values for", var, "\n")
      }
    }
  }
}
  

#Correcting Categorical errors
# Function to clean categorical errors of form "Category_???1234"
clean_systematic_suffix <- function(data) {
  
  for (var in names(data)) {
    
    if (is.character(data[[var]])) {
      
      error_pattern <- grepl("_\\?\\?\\?[0-9]{4}$", data[[var]])
      
      data[[var]][error_pattern] <- sub("_.*$", "", data[[var]][error_pattern])
    }
  }
  
  return(data)
}

claims_equipment_failure_freq <- clean_systematic_suffix(claims_equipment_failure_freq)
claims_equipment_failure_sev  <- clean_systematic_suffix(claims_equipment_failure_sev)


#Converting all remaining invalid data into NA's
for (var in names(validation_rules)) {
  
  rule <- validation_rules[[var]]
  
  if (rule$type == "range") {
    
    # FREQUENCY DATASET
    if (var %in% names(claims_equipment_failure_freq)) {
      
      claims_equipment_failure_freq[[var]] <- 
        as.numeric(claims_equipment_failure_freq[[var]])
      
      claims_equipment_failure_freq[[var]][
        !is.na(claims_equipment_failure_freq[[var]]) &
          (claims_equipment_failure_freq[[var]] < rule$range[1] |
             claims_equipment_failure_freq[[var]] > rule$range[2])
      ] <- NA
    }
    
    # SEVERITY DATASET
    if (var %in% names(claims_equipment_failure_sev)) {
      
      claims_equipment_failure_sev[[var]] <- 
        as.numeric(claims_equipment_failure_sev[[var]])
      
      claims_equipment_failure_sev[[var]][
        !is.na(claims_equipment_failure_sev[[var]]) &
          (claims_equipment_failure_sev[[var]] < rule$range[1] |
             claims_equipment_failure_sev[[var]] > rule$range[2])
      ] <- NA
    }
  }
}


#Dealing with missingness
#Identifying proprtion of missing values
missing_summary <- function(data) {
  
  data.frame(
    Variable = names(data),
    Missing_Count = sapply(data, function(x) sum(is.na(x))),
    Missing_Percent = sapply(data, function(x) mean(is.na(x)) * 100)
  )
}

missing_freq <- missing_summary(claims_equipment_failure_freq)
missing_sev  <- missing_summary(claims_equipment_failure_sev)

missing_freq
missing_sev

mean(is.na(claims_equipment_failure_freq)) * 100
mean(is.na(claims_equipment_failure_sev)) * 100

#Determining if no claims are linked to missingness
claims_equipment_failure_freq %>%
  mutate(has_claim = ifelse(claim_count > 0, "Claim", "No Claim")) %>%
  group_by(has_claim) %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100))


#Removing Missing values
claims_equipment_failure_freq <- na.omit(claims_equipment_failure_freq)
claims_equipment_failure_sev  <- na.omit(claims_equipment_failure_sev)



#Checking Cleaning worked
#Claims frequency
result_claims_freq <- sapply(names(validation_rules), function(var) {
  if (var %in% names(claims_equipment_failure_freq)) {
    count_issues(claims_equipment_failure_freq[[var]], validation_rules[[var]])
  } else {
    c(Missing_Count = NA, Invalid_Count = NA)
  }
})

claims_freq_table <- as.data.frame(result_claims_freq)
claims_freq_table

#Claims severity
result_claims_sev <- sapply(names(validation_rules), function(var) {
  if (var %in% names(claims_equipment_failure_sev)) {
    count_issues(claims_equipment_failure_sev[[var]], validation_rules[[var]])
  } else {
    c(Missing_Count = NA, Invalid_Count = NA)
  }
})

claims_sev_table <- as.data.frame(result_claims_sev)
claims_sev_table


#Duplicate Checks
#frequency

# Total rows
nrow(claims_equipment_failure_freq)

# Number of unique equipment
nrow(
  distinct(
    claims_equipment_failure_freq,
    equipment_type,
    equipment_id,
    policy_id
  )
)

#Severity
nrow(claims_equipment_failure_sev)

nrow(
  distinct(claims_equipment_failure_sev, claim_id, claim_seq)
)
#############################################################################################################
#EDA
#Claim Count
# --- Identify variable types ---
numeric_vars <- names(claims_equipment_failure_freq)[sapply(claims_equipment_failure_freq, is.numeric)]
integer_vars <- numeric_vars[sapply(claims_equipment_failure_freq[numeric_vars], function(x) all(x %% 1 == 0))]
continuous_vars <- setdiff(numeric_vars, integer_vars)

categorical_vars <- names(validation_rules)[sapply(validation_rules, function(x) x$type == "categorical")]

# --- 1. Boxplots for continuous numeric variables ---
for (var in continuous_vars) {
  p <- ggplot(claims_equipment_failure_freq, aes_string(x = "1", y = var)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var), x = "", y = var) +
    theme_minimal()
  print(p)
}

# --- 2. Histograms for integer/count variables ---
for (var in integer_vars) {
  p <- ggplot(claims_equipment_failure_freq, aes_string(x = var)) +
    geom_histogram(binwidth = 1, color = "black", fill = "steelblue") +
    labs(title = paste("Histogram of", var), x = var, y = "Count") +
    theme_minimal()
  print(p)
}

# --- 3. Bar charts for categorical variables ---
for (var in categorical_vars) {
  p <- ggplot(claims_equipment_failure_freq, aes_string(x = var)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = paste("Count of", var), x = var, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# --- 4. Plot each variable vs claim_count (integer outcome) ---
for (var in c(continuous_vars, integer_vars, categorical_vars)) {
  
  # Continuous numeric -> boxplot vs claim_count
  if (var %in% continuous_vars) {
    p <- ggplot(claims_equipment_failure_freq, aes_string(x = "factor(claim_count)", y = var)) +
      geom_boxplot() +
      labs(title = paste(var, "vs claim_count"), x = "Claim Count", y = var) +
      theme_minimal()
    print(p)
    
    # Integer variables (other counts) -> histogram faceted by claim_count
  } else if (var %in% integer_vars) {
    p <- ggplot(claims_equipment_failure_freq, aes_string(x = var)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
      facet_wrap(~claim_count, scales = "free_y") +
      labs(title = paste("Histogram of", var, "faceted by claim_count"), x = var, y = "Count") +
      theme_minimal()
    print(p)
    
    # Categorical variables -> barplot faceted by claim_count
  } else if (var %in% categorical_vars) {
    p <- ggplot(claims_equipment_failure_freq, aes_string(x = var)) +
      geom_bar(fill = "steelblue", color = "black") +
      facet_wrap(~claim_count, scales = "free_y") +
      labs(title = paste(var, "by claim_count"), x = var, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}

# --- Optional: plot vs claim_amount if present ---
if ("claim_amount" %in% names(claims_equipment_failure_freq)) {
  for (var in c(continuous_vars, integer_vars, categorical_vars)) {
    if (var == "claim_amount") next
    if (var %in% continuous_vars | var %in% integer_vars) {
      p <- ggplot(claims_equipment_failure_freq, aes_string(x = var, y = "claim_amount")) +
        geom_point(alpha = 0.3) +
        labs(title = paste(var, "vs claim_amount"), x = var, y = "Claim Amount") +
        theme_minimal()
      print(p)
    } else if (var %in% categorical_vars) {
      p <- ggplot(claims_equipment_failure_freq, aes_string(x = var, y = "claim_amount")) +
        geom_boxplot() +
        labs(title = paste(var, "vs claim_amount"), x = var, y = "Claim Amount") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
    }
  }
}


#Severity
data <- claims_equipment_failure_sev

# Identify numeric variables
numeric_vars <- names(data)[sapply(data, is.numeric)]

# Identify integer variables
integer_vars <- numeric_vars[
  sapply(data[numeric_vars], function(x) all(x %% 1 == 0))
]

# Continuous numeric (exclude integers)
continuous_vars <- setdiff(numeric_vars, integer_vars)

# Remove outcome from predictors
continuous_vars <- setdiff(continuous_vars, "claim_amount")
integer_vars <- setdiff(integer_vars, "claim_amount")

# Categorical variables
categorical_vars <- names(validation_rules)[
  sapply(validation_rules, function(x) x$type == "categorical")
]

### -------------------------------
### 1. Continuous Numeric Variables
### -------------------------------

for (var in continuous_vars) {
  
  # Standalone boxplot
  p1 <- ggplot(data, aes_string(x = "1", y = var)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", var),
         x = "", y = var) +
    theme_minimal()
  print(p1)
  
  # Scatter vs claim_amount
  p2 <- ggplot(data, aes_string(x = var, y = "claim_amount")) +
    geom_point(alpha = 0.3) +
    labs(title = paste(var, "vs Claim Amount"),
         x = var,
         y = "Claim Amount") +
    theme_minimal()
  print(p2)
}

### -------------------------------
### 2. Integer Variables (including claim_seq)
### -------------------------------

for (var in integer_vars) {
  
  # Standalone histogram
  p1 <- ggplot(data, aes_string(x = var)) +
    geom_histogram(binwidth = 1,
                   color = "black",
                   fill = "skyblue") +
    labs(title = paste("Histogram of", var),
         x = var, y = "Count") +
    theme_minimal()
  print(p1)
  
  # Boxplot vs claim_amount
  p2 <- ggplot(data,
               aes_string(x = paste0("factor(", var, ")"),
                          y = "claim_amount")) +
    geom_boxplot() +
    labs(title = paste(var, "vs Claim Amount"),
         x = var,
         y = "Claim Amount") +
    theme_minimal()
  print(p2)
}

### -------------------------------
### 3. Categorical Variables
### -------------------------------

for (var in categorical_vars) {
  
  # Standalone barplot
  p1 <- ggplot(data, aes_string(x = var)) +
    geom_bar(fill = "lightgreen",
             color = "black") +
    labs(title = paste("Count of", var),
         x = var,
         y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p1)
  
  # Boxplot vs claim_amount
  p2 <- ggplot(data,
               aes_string(x = var,
                          y = "claim_amount")) +
    geom_boxplot() +
    labs(title = paste(var, "vs Claim Amount"),
         x = var,
         y = "Claim Amount") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p2)
}


#Summary statistics
summary(claims_equipment_failure_freq)
summary(claims_equipment_failure_sev)



###############################################################
###############################################################
# Equipment Failure – Frequency Modelling (Poisson GLM)
###############################################################
###############################################################
# 1. Baseline portfolio frequency
###############################################################

claims_equipment_failure_freq %>%
  summarise(
    total_claims   = sum(claim_count),
    total_exposure = sum(exposure),
    frequency      = total_claims / total_exposure
  )

###############################################################
# 2. Check dispersion
###############################################################

mean_count <- mean(claims_equipment_failure_freq$claim_count)
var_count  <- var(claims_equipment_failure_freq$claim_count)
disp_ratio <- var_count / mean_count
disp_ratio  # ~1 = Poisson appropriate

###############################################################
# 3. Intercept-only Poisson model (exposure offset)
###############################################################

freq_poisson0 <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = claims_equipment_failure_freq
)

summary(freq_poisson0)
exp(coef(freq_poisson0))             # baseline claim rate
deviance(freq_poisson0) / df.residual(freq_poisson0)

###############################################################
# 4. Prepare modelling dataset
###############################################################

freq_model_df <- claims_equipment_failure_freq %>%
  mutate(
    age_band = cut(
      equipment_age,
      breaks = c(0, 5, 10, 15, 20, Inf),
      right = FALSE,
      labels = c("0-4", "5-9", "10-14", "15-19", "20+")
    ),
    # Factor categorical variables
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system),
    age_band       = factor(age_band),
    # Center continuous variables
    c_maintenance  = maintenance_int - mean(maintenance_int, na.rm = TRUE),
    c_usage        = usage_int - mean(usage_int, na.rm = TRUE)
  )

###############################################################
# 5. Main-effects Poisson GLM
###############################################################

freq_poisson_main <- glm(
  claim_count ~ equipment_type +
    age_band +
    solar_system +
    c_maintenance +
    c_usage +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_main)
deviance(freq_poisson_main) / df.residual(freq_poisson_main)
exp(coef(freq_poisson_main))

###############################################################
# 6. Plausible two-way interactions
###############################################################

freq_poisson_inter <- glm(
  claim_count ~ equipment_type +
    age_band * solar_system +
    equipment_type:age_band +
    c_maintenance:c_usage +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_inter)

###############################################################
# 7. Add splines for continuous variables
###############################################################

freq_poisson_spline <- glm(
  claim_count ~ equipment_type +
    age_band +
    solar_system +
    ns(c_maintenance, df = 3) +
    ns(c_usage, df = 3) +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_spline)

###############################################################
# 8. Stepwise selection (main + interactions)
###############################################################

freq_poisson_step <- stepAIC(
  freq_poisson_main, 
  scope = list(
    lower = ~1 + offset(log(exposure)),
    upper = ~ equipment_type + age_band + solar_system +
      c_maintenance + c_usage +
      equipment_type:age_band +
      age_band:solar_system +
      c_maintenance:c_usage
  ),
  direction = "both",
  trace = TRUE
)

summary(freq_poisson_step)
AIC(freq_poisson_step)

###############################################################
# 9. Compare AICs
###############################################################

AIC(freq_poisson0,
    freq_poisson_main,
    freq_poisson_inter,
    freq_poisson_spline,
    freq_poisson_step)

###############################################################
# 10. Pearson residual diagnostic plot
###############################################################

plot(freq_poisson_main$fitted.values,
     residuals(freq_poisson_main, type = "pearson"),
     xlab = "Fitted values",
     ylab = "Pearson residuals",
     main = "Residuals vs Fitted")

###############################################################
# 11. Zero-inflation check
###############################################################

mean(freq_model_df$claim_count == 0)
mean(exp(-fitted(freq_poisson_main)))

###############################################################
# 12. Deployment model (no solar_system)
###############################################################

freq_poisson_deploy <- glm(
  claim_count ~ equipment_type +
    age_band +
    c_maintenance +
    c_usage +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_deploy)
deviance(freq_poisson_deploy) / df.residual(freq_poisson_deploy)
AIC(freq_poisson_main, freq_poisson_deploy)

###############################################################
# 13. Compare predictions: full vs deployment
###############################################################

freq_model_df$pred_full   <- predict(freq_poisson_main, type = "response")
freq_model_df$pred_deploy <- predict(freq_poisson_deploy, type = "response")

summary(freq_model_df$pred_full / freq_model_df$pred_deploy)

###############################################################
# 14. Multiplicative effects table for deployment model
###############################################################

coef_df <- data.frame(
  Predictor = names(coef(freq_poisson_deploy)),
  Coef      = coef(freq_poisson_deploy)
)

coef_df$Multiplicative_Effect <- exp(coef_df$Coef)

# Clean table of Predictor vs Effect:
coef_df <- coef_df %>% dplyr::select(Predictor, Multiplicative_Effect)
coef_df


##############################################################
#Adding Multiplicative Affect (Solar System)
##############################################################
# 15. Fit Poisson model including solar_system
# Helionis Cluster is the baseline
##############################################################

# Ensure Helionis is the reference level
freq_model_df$solar_system <- relevel(freq_model_df$solar_system, ref = "Helionis Cluster")

# Fit GLM
freq_poisson_full <- glm(
  claim_count ~ equipment_type + age_band + solar_system +
    c_maintenance + c_usage + offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_df
)

summary(freq_poisson_full)

# Multiplicative relativity
exp(coef(freq_poisson_full))


##############################################################
# 16. Historical solar system distribution
##############################################################

solar_dist <- freq_model_df %>%
  group_by(solar_system) %>%
  summarise(
    total_exposure = sum(exposure),
    total_claims   = sum(claim_count),
    freq           = total_claims / total_exposure,
    n_obs          = n()
  ) %>%
  arrange(desc(total_exposure))

solar_dist


##############################################################
# 17. Generate Helionis baseline predictions
# Predict ALL observations as if they are Helionis
##############################################################

helionis_df <- freq_model_df
helionis_df$solar_system <- "Helionis Cluster"

freq_model_df$pred_helionis <- predict(
  freq_poisson_full,
  newdata = helionis_df,
  type = "response"
)

summary(freq_model_df$pred_helionis)

# Total predicted claims if everything operated in Helionis
total_helionis_claims <- sum(freq_model_df$pred_helionis)

total_helionis_claims


##############################################################
# 18. Define solar system risk multipliers
# (Relative to Helionis Cluster)
##############################################################

solar_adjustments <- c(
  
  "Helionis Cluster" = 1.00,
  
  # Model-based relativities
  "Epsilon" = exp(coef(freq_poisson_full)["solar_systemEpsilon"]),
  "Zeta"    = exp(coef(freq_poisson_full)["solar_systemZeta"]),
  
  # ---------------------------------------------------------
  # PROFESSIONAL ACTUARIAL JUDGEMENT REQUIRED
  # Inserted assumed risk level relative to Helionis
  # ---------------------------------------------------------
  
  "Bayesia System" = 0.75,  # <-- Adjusted based on qualitative info
  "Oryn Delta"      = 0.70   # <-- Adjusted based on qualitative info
)

solar_adjustments


##############################################################
# 19. Generate predictions for each solar system scenario
##############################################################

scenario_results <- data.frame(
  solar_system = names(solar_adjustments),
  multiplier   = solar_adjustments,
  predicted_claims = total_helionis_claims * solar_adjustments
)

scenario_results


##############################################################
# 20. Sensitivity analysis for Bayesia System
##############################################################

Bayesia_range <- seq(0.6, 1.1, by = 0.05)

Bayesia_sensitivity <- data.frame(
  multiplier = Bayesia_range,
  predicted_claims = total_helionis_claims * Bayesia_range
)

Bayesia_sensitivity


##############################################################
# 21. Sensitivity analysis for Oryn Delta
##############################################################

oryn_range <- seq(0.6, 1.1, by = 0.05)

oryn_sensitivity <- data.frame(
  multiplier = oryn_range,
  predicted_claims = total_helionis_claims * oryn_range
)

oryn_sensitivity


##############################################################
# End of solar system scenario modelling
##############################################################

##############################################################
# Equipment Failure Severity Modelling
##############################################################
##############################################################
# 1. Baseline Severity Statistics
##############################################################

claims_equipment_failure_sev %>%
  summarise(
    avg_severity = mean(claim_amount),
    sd_severity  = sd(claim_amount),
    median_sev   = median(claim_amount)
  )

##############################################################
# 2. Prepare Severity Dataset
##############################################################

sev_model_df <- claims_equipment_failure_sev %>%
  filter(claim_amount > 0) %>%
  mutate(
    
    age_band = cut(
      equipment_age,
      breaks = c(0,5,10,15,20,Inf),
      right = FALSE,
      labels = c("0-4","5-9","10-14","15-19","20+")
    ),
    
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system),
    age_band       = factor(age_band),
    
    # Set Helionis as baseline
    solar_system = relevel(solar_system, ref = "Helionis Cluster"),
    
    # Centered variables
    c_maintenance = maintenance_int - mean(maintenance_int),
    c_usage       = usage_int - mean(usage_int)
    
  )

##############################################################
# 3. Main Lognormal Severity Model
##############################################################

sev_lognorm_main <- lm(
  
  log(claim_amount) ~
    
    equipment_type +
    age_band +
    solar_system +
    c_maintenance +
    c_usage,
  
  data = sev_model_df
)

summary(sev_lognorm_main)

##############################################################
# 4. Interaction Model
##############################################################

sev_lognorm_inter <- lm(
  
  log(claim_amount) ~
    
    equipment_type +
    age_band * solar_system +
    equipment_type:age_band +
    c_maintenance:c_usage,
  
  data = sev_model_df
)

summary(sev_lognorm_inter)

##############################################################
# 5. Spline Model
##############################################################

sev_lognorm_spline <- lm(
  
  log(claim_amount) ~
    
    equipment_type +
    age_band +
    solar_system +
    ns(c_maintenance, df = 3) +
    ns(c_usage, df = 3),
  
  data = sev_model_df
)

summary(sev_lognorm_spline)

##############################################################
# 6. Stepwise Model Selection
##############################################################

sev_lognorm_step <- stepAIC(
  
  sev_lognorm_main,
  
  scope = list(
    
    lower = ~1,
    
    upper = ~
      
      equipment_type +
      age_band +
      solar_system +
      c_maintenance +
      c_usage +
      equipment_type:age_band +
      age_band:solar_system +
      c_maintenance:c_usage
  ),
  
  direction = "both",
  trace = TRUE
)

summary(sev_lognorm_step)

##############################################################
# 7. Compare Model Fits
##############################################################

AIC(
  sev_lognorm_main,
  sev_lognorm_inter,
  sev_lognorm_spline,
  sev_lognorm_step
)

##############################################################
# 8. Deployment Model (No Solar System)
##############################################################

sev_lognorm_deploy <- lm(
  
  log(claim_amount) ~
    
    equipment_type +
    age_band +
    c_maintenance +
    c_usage,
  
  data = sev_model_df
)

summary(sev_lognorm_deploy)

AIC(sev_lognorm_step, sev_lognorm_deploy)

##############################################################
# 9. Back-Transform Lognormal Predictions
##############################################################

sigma_ln <- summary(sev_lognorm_step)$sigma

sev_model_df$pred_lognorm <- exp(
  predict(sev_lognorm_step) + 0.5 * sigma_ln^2
)

sev_model_df$pred_severity <- sev_model_df$pred_lognorm
##############################################################
# 10. Diagnostic Plots
##############################################################

par(mfrow=c(2,2))
plot(sev_lognorm_step)
par(mfrow=c(1,1))


##############################################################
# 11. Generate Helionis Baseline Severity
##############################################################

helionis_df <- sev_model_df
helionis_df$solar_system <- "Helionis Cluster"

sev_model_df$pred_helionis <- exp(
  predict(sev_lognorm_step, newdata = helionis_df) +
    0.5 * sigma_ln^2
)

mean_helionis_severity <- mean(sev_model_df$pred_helionis)

mean_helionis_severity

##############################################################
# 12. Multiplicative Severity Relativities
##############################################################

severity_relativities <- exp(coef(sev_lognorm_step))

severity_relativities

##############################################################
# 13. Solar System Severity Multipliers
##############################################################

solar_sev_adjustments <- c(
  
  "Helionis Cluster" = 1.00,
  
  "Epsilon" =
    exp(coef(sev_lognorm_step)["solar_systemEpsilon"]),
  
  "Zeta" =
    exp(coef(sev_lognorm_step)["solar_systemZeta"]),
  
  # Actuarial judgement based on similarity
  "Bayesia System" = 1.02,
  "Oryn Delta"      = 1.13
)

solar_sev_adjustments

##############################################################
# 14. Scenario Severity Estimates
##############################################################

severity_scenarios <- data.frame(
  
  solar_system = names(solar_sev_adjustments),
  
  multiplier = solar_sev_adjustments,
  
  predicted_severity =
    mean_helionis_severity * solar_sev_adjustments
)

severity_scenarios


##############################################################
# 15. Actual vs Predicted Plot (Report Quality)
##############################################################

ggplot(sev_model_df,
       aes(x = pred_lognorm, y = claim_amount)) +
  
  geom_point(alpha = 0.2) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    colour = "blue"
  ) +
  
  labs(
    title = "Actual vs Predicted Claim Severity",
    x = "Predicted Severity",
    y = "Observed Claim Severity"
  ) +
  
  theme_minimal()


##############################################################
# 16. Sensitivity Analysis for Bayesia System Severity
##############################################################

Bayesia_range <- seq(0.8, 1.3, by = 0.05)

Bayesia_sev_sensitivity <- data.frame(
  
  multiplier = Bayesia_range,
  
  predicted_severity =
    mean_helionis_severity * Bayesia_range
  
)

Bayesia_sev_sensitivity


##############################################################
# 17. Sensitivity Analysis for Oryn Delta Severity
##############################################################

oryn_range <- seq(0.8, 1.3, by = 0.05)

oryn_sev_sensitivity <- data.frame(
  
  multiplier = oryn_range,
  
  predicted_severity =
    mean_helionis_severity * oryn_range
  
)

oryn_sev_sensitivity
##############################################################
# End of Severity Modelling
##############################################################

##############################################################
##############################################################
# Aggregate Loss Simulation – Equipment Failure
##############################################################
##############################################################

##############################################################
# FINAL DEPLOYMENT MODELLING + MONTE CARLO SIMULATION
##############################################################

##############################################################
# 1. Rebuild modelling datasets (non-centered)
##############################################################

freq_model_nc <- claims_equipment_failure_freq %>%
  mutate(
    age_band = cut(
      equipment_age,
      breaks = c(0,5,10,15,20,Inf),
      right = FALSE,
      labels = c("0-4","5-9","10-14","15-19","20+")
    ),
    
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system),
    age_band       = factor(age_band)
  )

sev_model_nc <- claims_equipment_failure_sev %>%
  filter(claim_amount > 0) %>%
  mutate(
    age_band = cut(
      equipment_age,
      breaks = c(0,5,10,15,20,Inf),
      right = FALSE,
      labels = c("0-4","5-9","10-14","15-19","20+")
    ),
    
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system),
    age_band       = factor(age_band)
  )

##############################################################
# 2. Fit NON-CENTERED frequency model
##############################################################

freq_poisson_noncenter <- glm(
  claim_count ~
    equipment_type +
    age_band +
    solar_system +
    maintenance_int +
    usage_int +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = freq_model_nc
)

summary(freq_poisson_noncenter)


##############################################################
# 3. Fit NON-CENTERED severity model
##############################################################

sev_lognorm_noncenter <- lm(
  
  log(claim_amount) ~
    
    equipment_type +
    age_band +
    solar_system +
    maintenance_int +
    usage_int,
  
  data = sev_model_nc
)

summary(sev_lognorm_noncenter)

sigma_ln <- summary(sev_lognorm_noncenter)$sigma


##############################################################
# 4. Load portfolio
##############################################################

portfolio_mining <- read_csv("Equipment_Failure_Mining_Data.csv")


##############################################################
# 5. Prepare portfolio variables
##############################################################

portfolio_df <- portfolio_mining %>%
  mutate(
    
    equipment_type = factor(
      equipment_type,
      levels = levels(freq_model_nc$equipment_type)
    ),
    
    age_band = factor(
      age_band,
      levels = levels(freq_model_nc$age_band)
    ),
    exposure = 1
    
  )


##############################################################
# 6. Helionis baseline dataset
##############################################################

helionis_portfolio <- portfolio_df
helionis_portfolio$solar_system <- "Helionis Cluster"


##############################################################
# 7. Predict baseline frequency
##############################################################

portfolio_df$freq_base <- predict(
  freq_poisson_noncenter,
  newdata = helionis_portfolio,
  type = "response"
)


##############################################################
# 8. Apply solar frequency multipliers
##############################################################

portfolio_df$solar_freq_mult <-
  solar_adjustments[portfolio_df$solar_system]

portfolio_df$lambda <-
  portfolio_df$freq_base *
  portfolio_df$solar_freq_mult


##############################################################
# 9. Predict baseline severity
##############################################################

portfolio_df$sev_base <- exp(
  
  predict(
    sev_lognorm_noncenter,
    newdata = helionis_portfolio
  ) + 0.5 * sigma_ln^2
  
)


##############################################################
# 10. Apply solar severity multipliers
##############################################################

portfolio_df$solar_sev_mult <-
  solar_sev_adjustments[portfolio_df$solar_system]

portfolio_df$severity_mean <-
  portfolio_df$sev_base *
  portfolio_df$solar_sev_mult


##############################################################
# 11. Vectorised Monte Carlo Simulation
##############################################################

set.seed(123)

n_sim <- 100000
n_risks <- nrow(portfolio_df)

lambda_vec <- portfolio_df$lambda * portfolio_df$n_units
sev_mean_vec <- portfolio_df$severity_mean

aggregate_losses <- numeric(n_sim)
claims_store <- vector("list",n_sim)

for(i in 1:n_sim){
  
  claim_counts <- rpois(n_risks, lambda_vec)
  
  total_claims <- sum(claim_counts)
  
  if(total_claims > 0){
    
    sev_expanded <- rep(sev_mean_vec, claim_counts)
    
    claim_sizes <- rlnorm(
      total_claims,
      meanlog = log(sev_expanded),
      sdlog = sigma_ln
    )
    
    aggregate_losses[i] <- sum(claim_sizes)
    claims_store[[i]] <- claim_sizes
    
  } else {
    
    aggregate_losses[i] <- 0
    claims_store[[i]] <- numeric(0)
  }
}


##############################################################
# 12. Aggregate Loss Distribution
##############################################################

summary(aggregate_losses)

quantile(
  aggregate_losses,
  probs = c(0.5,0.75,0.9,0.95,0.99)
)


##############################################################
# 13. Risk Metrics
##############################################################

expected_loss <- mean(aggregate_losses)

VaR_99 <- quantile(aggregate_losses,0.99)

TVaR_99 <- mean(
  aggregate_losses[aggregate_losses > VaR_99]
)

expected_loss
VaR_99
TVaR_99


##############################################################
# 14. Plot Loss Distribution
##############################################################

ggplot(
  data.frame(loss = aggregate_losses),
  aes(x = loss)
) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    colour = "black"
  ) +
  labs(
    title = "Aggregate Loss Distribution",
    x = "Total Portfolio Loss",
    y = "Simulation Count"
  ) +
  theme_minimal()



##############################################################
# 15.Set Function for Calculating Deductibles
##############################################################

apply_insurance <- function(
    claims_list, 
    deductible = 0, 
    policy_limit = Inf, 
    retention = Inf, 
    re_limit = 0
){
  n_sim <- length(claims_list)
  net_losses <- numeric(n_sim)
  
  for(i in 1:n_sim){
    claims <- claims_list[[i]]
    if(length(claims) > 0){
      # apply deductible
      claims <- pmax(claims - deductible, 0)
      # apply policy limit
      claims <- pmin(claims, policy_limit)
      # reinsurance recovery
      recovery <- pmin(pmax(claims - retention, 0), re_limit)
      net_losses[i] <- sum(claims - recovery)
    } else {
      net_losses[i] <- 0
    }
  }
  
  return(net_losses)
}

##############################################################
# 16. Running Analysis on Deductibles
##############################################################
#Finding a reasonable range to test over for deductibles
mean_claim <- mean(claims_equipment_failure_sev$claim_amount)
median_claim <- median(claims_equipment_failure_sev$claim_amount)
min_claim <- min(claims_equipment_failure_sev$claim_amount)
max_claim <- max(claims_equipment_failure_sev$claim_amount)

deductibles <- seq(
  from = min_claim * 0.5,   # can't be smaller than actual claims
  to   = median_claim,       # don't want >50% of typical claim
  by   = (median_claim - min_claim*0.5)/10  # 10 points
)
results <- data.frame(
  deductible = deductibles,
  expected_loss = NA,
  VaR_95 = NA,
  VaR_99 = NA,
  TVaR_99 = NA
)

for(i in seq_along(deductibles)){
  net_loss_sim <- apply_insurance(
    claims_store,
    deductible = deductibles[i],
    policy_limit = 2000000,
    retention = 1000000,
    re_limit = 4000000
  )
  
  results$expected_loss[i] <- mean(net_loss_sim)
  results$VaR_95[i] <- quantile(net_loss_sim, 0.95)
  results$VaR_99[i] <- quantile(net_loss_sim, 0.99)
  results$TVaR_99[i] <- mean(net_loss_sim[net_loss_sim > quantile(net_loss_sim, 0.99)])
}

# View results
print(results)

# Plot deductible curve
ggplot(results, aes(x = deductible, y = expected_loss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Expected Net Loss vs Deductible",
    x = "Deductible",
    y = "Expected Net Loss"
  ) +
  theme_minimal()


# Flatten the list of claims across all simulations
all_claims <- unlist(claims_store)

# Quick summary stats
summary(all_claims)
quantile(all_claims, probs = c(0.01,0.05,0.1,0.15,0.25, 0.5, 0.75, 0.9, 0.95, 0.99))


#ggplot of severity of claims
ggplot(data.frame(claim = all_claims), aes(x = claim)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  scale_x_continuous(limits = c(0, 200000), labels = scales::comma) +
  labs(
    title = "Distribution of Small Claim Severities",
    x = "Claim Size",
    y = "Count"
  ) +
  theme_minimal()



# Deductibles to test
deductibles <- c(10000, 15000, 20000)

# Create a data frame to store results
deductible_results <- data.frame(
  deductible = deductibles,
  expected_loss = NA,
  VaR_95 = NA,
  VaR_99 = NA,
  TVaR_99 = NA
)

# Loop through deductibles and calculate net losses
for(i in seq_along(deductibles)){
  d <- deductibles[i]
  
  # Apply deductible to each claim
  net_claims <- pmax(all_claims - d, 0)
  
  # Store metrics
  deductible_results$expected_loss[i] <- mean(net_claims)
  deductible_results$VaR_95[i] <- quantile(net_claims, 0.95)
  deductible_results$VaR_99[i] <- quantile(net_claims, 0.99)
  deductible_results$TVaR_99[i] <- mean(net_claims[net_claims > quantile(net_claims,0.99)])
}

deductible_results




##############################
# Function to evaluate excess-of-loss reinsurance
test_reinsurance <- function(claims_store, retention_levels) {
  
  n_sim <- length(claims_store)
  n_levels <- length(retention_levels)
  
  results <- data.frame(
    retention = retention_levels,
    expected_loss = numeric(n_levels),
    VaR_95 = numeric(n_levels),
    VaR_99 = numeric(n_levels),
    TVaR_99 = numeric(n_levels)
  )
  
  for(i in seq_along(retention_levels)){
    retention <- retention_levels[i]
    
    # calculate net losses after reinsurance
    net_losses <- numeric(n_sim)
    
    for(sim in 1:n_sim){
      claims <- claims_store[[sim]]
      if(length(claims) > 0){
        # Only pay up to retention per claim
        net_claims <- pmin(claims, retention)
        net_losses[sim] <- sum(net_claims)
      } else {
        net_losses[sim] <- 0
      }
    }
    
    # store summary stats
    results$expected_loss[i] <- mean(net_losses)
    results$VaR_95[i] <- quantile(net_losses, 0.95)
    results$VaR_99[i] <- quantile(net_losses, 0.99)
    results$TVaR_99[i] <- mean(net_losses[net_losses > results$VaR_99[i]])
  }
  
  return(results)
}





# Example retention levels (adjust depending on portfolio size)
retentions <- seq(100000, 150000, 200000)

reinsurance_results <- test_reinsurance(claims_store, retentions)

print(reinsurance_results)





ggplot(reinsurance_results, aes(x = retention, y = expected_loss)) +
  geom_line(color="darkgreen") +
  geom_point(color="darkgreen") +
  labs(
    title = "Expected Net Loss vs Reinsurance Retention",
    x = "Retention per Claim",
    y = "Expected Net Loss"
  ) +
  theme_minimal()




ggplot(data.frame(claim = all_claims), aes(x = claim)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black") +
  scale_x_log10(labels = scales::comma) +
  geom_vline(xintercept = c(100000,150000,200000),
             colour = c("red","orange","darkgreen"),
             linetype = "dashed",
             size = 1) +
  labs(
    title = "Claim Severity Distribution with Reinsurance Attachment Points",
    x = "Claim Size (log scale)",
    y = "Number of Claims"
  ) +
  theme_minimal()


ggplot(data.frame(claim = all_claims), aes(x = claim)) +
  geom_histogram(bins = 80, fill = "steelblue", colour = "black") +
  geom_vline(
    xintercept = c(100000,150000,200000),
    colour = c("red","orange","darkgreen"),
    linetype = "dashed",
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(0,300000)) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Claim Severity Distribution (Zoomed, Linear Scale)",
    x = "Claim Size",
    y = "Number of Claims"
  ) +
  theme_minimal()



#######################################################################
#Applying deductible and reinsurance
#######################################################################
deductible <- 20000
retention  <- 200000

n_sim <- length(claims_store)

insurer_losses <- numeric(n_sim)
reinsurer_losses <- numeric(n_sim)

for(i in 1:n_sim){
  
  claims <- claims_store[[i]]
  
  if(length(claims) > 0){
    
    # Apply deductible
    claims_after_ded <- pmax(claims - deductible, 0)
    
    # Insurer pays up to retention
    insurer_claims <- pmin(claims_after_ded, retention)
    
    # Reinsurer pays excess
    reinsurer_claims <- pmax(claims_after_ded - retention, 0)
    
    insurer_losses[i] <- sum(insurer_claims)
    reinsurer_losses[i] <- sum(reinsurer_claims)
    
  } else {
    
    insurer_losses[i] <- 0
    reinsurer_losses[i] <- 0
  }
}


#Insurer risk
expected_loss <- mean(insurer_losses)

VaR_95 <- quantile(insurer_losses,0.95)

VaR_99 <- quantile(insurer_losses,0.99)

TVaR_99 <- mean(insurer_losses[insurer_losses > VaR_99])

expected_loss
VaR_95
VaR_99
TVaR_99



#reinsurer costs
expected_reinsurance_loss <- mean(reinsurer_losses)

expected_reinsurance_loss

reinsurance_premium <- expected_reinsurance_loss * 1.3



ggplot(data.frame(loss = reinsurer_losses), aes(x = loss)) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    colour = "black"
  ) +
  labs(
    title = "Reinsurer Loss Distribution",
    x = "Reinsurance Payment",
    y = "Simulation Count"
  ) +
  theme_minimal()



####################################################
#Reading in datasets
####################################################
#temporarily change working directory
setwd("C:/Users/vegkj/OneDrive/University/Fourth Year/Term 1/ACTL4001/Case Study/Git Hub Stuff/actl4001-26t1/forecasts")

inflation <- read.csv("inflation.csv")
interest <- read.csv("interest.csv")


setwd("C:/Users/vegkj/OneDrive/University/Fourth Year/Term 1/ACTL4001/Case Study/Git Hub Stuff/actl4001-26t1/datasets")




##############################################################
# ONE-YEAR PREMIUM CALCULATION WITH FULL ACTUARIAL EXPENSES
##############################################################

# --- Parameters ---
interest_rate        <- interest$X2175[1] / 100
expense_loading      <- 0.10     # operating expenses
brokerage_loading    <- 0.15     # broker commission
reinsurance_loading  <- 0.25     # reinsurer profit margin
capital_cost_rate    <- 0.08     # cost of capital (8%) #Consistent with peers)

deductible <- 20000
retention  <- 200000

target_loss_prob <- 0.01

# --- Step 1: Apply deductible and reinsurance to simulated claims ---

n_sim <- length(claims_store)

claim_value_end_year <- numeric(n_sim)
reinsurance_cost_end_year <- numeric(n_sim)

for(i in 1:n_sim){
  
  claims <- claims_store[[i]]
  
  if(length(claims) > 0){
    
    claims_after_ded <- pmax(claims - deductible, 0)
    
    insurer_claims <- pmin(claims_after_ded, retention)
    
    reinsurer_claims <- pmax(claims_after_ded - retention, 0)
    
    claim_value_end_year[i] <- sum(insurer_claims)
    
    reinsurance_cost_end_year[i] <- sum(reinsurer_claims)
    
  } else {
    
    claim_value_end_year[i] <- 0
    reinsurance_cost_end_year[i] <- 0
    
  }
}

# --- Step 2: Expected losses ---

expected_claim_cost <- mean(claim_value_end_year)

expected_reinsurance_loss <- mean(reinsurance_cost_end_year)

reinsurance_premium <- expected_reinsurance_loss * (1 + reinsurance_loading)

# --- Step 3: Capital requirement (risk capital) ---

loss_distribution <- claim_value_end_year + reinsurance_cost_end_year

VaR_99 <- quantile(loss_distribution, 0.99)

capital_cost <- capital_cost_rate * VaR_99

# --- Step 4: Base premium (present value) ---

base_cost <- expected_claim_cost +
  reinsurance_premium +
  capital_cost

premium_start_year <- base_cost / (1 + interest_rate/2)

# --- Step 5: Profit simulation with all expenses ---

profit_function <- function(premium_start){
  
  premium_end <- premium_start * (1 + interest_rate/2)
  
  expenses  <- premium_end * expense_loading
  brokerage <- premium_end * brokerage_loading
  
  profit <- premium_end -
    claim_value_end_year -
    reinsurance_premium -
    expenses -
    brokerage -
    capital_cost
  
  return(profit)
}

# --- Step 6: Find premium achieving target probability of loss ---

loss_probability <- function(premium_start){
  
  profit <- profit_function(premium_start)
  
  mean(profit < 0)
}

premium_grid <- seq(premium_start_year*0.5,
                    premium_start_year*2,
                    length.out = 200)

loss_probs <- sapply(premium_grid, loss_probability)

index <- which.min(abs(loss_probs - target_loss_prob))

premium_target <- premium_grid[index]

# --- Step 7: Profit distribution at target premium ---

profit_simulation <- profit_function(premium_target)

mean_profit <- mean(profit_simulation)

probability_of_loss <- mean(profit_simulation < 0)

profit_quantiles <- quantile(
  profit_simulation,
  probs = c(0.01,0.05,0.25,0.5,0.75,0.95,0.99)
)

discount_factor_1yr <- 1 / (1 + interest_rate/2)

claim_value_pv <- claim_value_end_year * discount_factor_1yr
returns_1yr <- rep(premium_target, n_sim)

# --- Step 8: ONE YEAR SUMMARY RISK METRICS ---


reinsurance_premium_pv <- reinsurance_premium 

expenses_1yr  <- returns_1yr * expense_loading
brokerage_1yr <- returns_1yr * brokerage_loading

costs_1yr <- claim_value_pv +
  reinsurance_premium_pv +
  expenses_1yr +
  brokerage_1yr

net_revenue_1yr <- returns_1yr - costs_1yr

# Claim costs already in claim_value_pv
summary_table_1yr <- data.frame(
  Metric = c("Claims","Costs","Returns","Net Revenue"),
  Expected_Value = c(
    mean(claim_value_pv),
    mean(costs_1yr),
    mean(returns_1yr),
    mean(net_revenue_1yr)
  ),
  sd = c(
    sd(claim_value_pv),
    sd(costs_1yr),
    sd(returns_1yr),
    sd(net_revenue_1yr)
  ),
  VaR_95 = c(
    quantile(claim_value_pv,0.95),
    quantile(costs_1yr,0.95),
    quantile(returns_1yr,0.05),
    quantile(net_revenue_1yr,0.05)
  ),
  VaR_99 = c(
    quantile(claim_value_pv,0.99),
    quantile(costs_1yr,0.99),
    quantile(returns_1yr,0.01),
    quantile(net_revenue_1yr,0.01)
  ),
  VaR_1 = c(
    quantile(claim_value_pv,0.01),      # very low claims
    quantile(costs_1yr,0.01),           # very low costs
    quantile(returns_1yr,0.99),         # very high returns
    quantile(net_revenue_1yr,0.01)      # very low net revenue
  )
)

print(summary_table_1yr)

# --- Step 9: ONE YEAR STRESS TESTING ---


# Frequency stress
freq_losses_1yr <- claim_value_end_year * 1.30

# Severity stress
sev_losses_1yr <- claim_value_end_year * 1.20

# Combined stress
combined_losses_1yr <- claim_value_end_year * 1.50

# Catastrophe scenario
cat_losses_1yr <- claim_value_end_year
cat_event <- rbinom(n_sim,1,0.01)

cat_losses_1yr[cat_event == 1] <- cat_losses_1yr[cat_event == 1] * 2

stress_table_1yr <- data.frame(
  
  Scenario = c(
    "Baseline",
    "Frequency +30%",
    "Severity +20%",
    "Combined Shock",
    "1-in-100 Catastrophe"
  ),
  
  Mean_Loss = c(
    mean(claim_value_end_year),
    mean(freq_losses_1yr),
    mean(sev_losses_1yr),
    mean(combined_losses_1yr),
    mean(cat_losses_1yr)
  ),
  
  VaR95 = c(
    quantile(claim_value_end_year,0.95),
    quantile(freq_losses_1yr,0.95),
    quantile(sev_losses_1yr,0.95),
    quantile(combined_losses_1yr,0.95),
    quantile(cat_losses_1yr,0.95)
  ),
  
  VaR99 = c(
    quantile(claim_value_end_year,0.99),
    quantile(freq_losses_1yr,0.99),
    quantile(sev_losses_1yr,0.99),
    quantile(combined_losses_1yr,0.99),
    quantile(cat_losses_1yr,0.99)
  )
)

print(stress_table_1yr)

# --- Step 10: Plot profit distribution ---


ggplot(data.frame(profit = profit_simulation),
       aes(x = profit)) +
  geom_histogram(bins = 50,
                 fill = "steelblue",
                 colour = "black") +
  labs(title = "Insurer Profit Distribution",
       x = "Profit",
       y = "Simulation Count") +
  theme_minimal()













###########################################################################
#10 Year Pricing
##########################################################################
##########################################################################
# 1: Paramaters and Datasets
##########################################################################

# --- Parameters ---
interest_rate_LT <-  as.numeric(interest[1,-1]) / 100
expense_loading      <- 0.10     # operating expenses
brokerage_loading    <- 0.15     # broker commission
reinsurance_loading  <- 0.25     # reinsurer profit margin
capital_cost_rate    <- 0.08    # cost of capital (8%)

base_deductible <- 20000 
base_retention  <- 200000
# Initialize vectors
inflation_vec <- inflation$inflation_rate/100
inflation_factor <- cumprod(1 + inflation_vec)
n_years <- length(inflation_vec)
deductible <- numeric(n_years)
retention  <- numeric(n_years)

# Year 1 is just base values
deductible[1] <- base_deductible * (1 + inflation_vec[1])
retention[1]  <- base_retention  * (1 + inflation_vec[1])

# Compute cumulative increase for subsequent years
for(y in 2:n_years){
  deductible[y] <- deductible[y-1] * (1 + inflation_vec[y])
  retention[y]  <- retention[y-1]  * (1 + inflation_vec[y])
}

# View results
deductible
retention

target_loss_prob <- 0.01


# --- Read in Dataset ---
portfolio_mining_LT <- read_csv("Equipment_Failure_Mining_Data_LT.csv")

##########################################################################
# 2: Monte Carlo Methods
##########################################################################
# --- Set up ---
set.seed(123)

n_sim <- 100000
n_risks <- nrow(portfolio_mining_LT)
n_years <- 10

# storage matrices
insurer_loss_matrix <- matrix(0, nrow = n_sim, ncol = n_years)
reinsurer_loss_matrix <- matrix(0, nrow = n_sim, ncol = n_years)


sev_mean_vec <- portfolio_df$severity_mean
sigma_ln <- summary(sev_lognorm_noncenter)$sigma

# --- Monte Carlo Simulations ---

for(i in 1:n_sim){
  
  for(y in 1:n_years){
    
    # number of machines in that year
    units_year <- portfolio_mining_LT[[paste0("n_units_y",y)]]
    
    # expected claims
    lambda_vec <- portfolio_df$lambda * units_year
    
    # simulate frequency
    claim_counts <- rpois(n_risks, lambda_vec)
    
    total_claims <- sum(claim_counts)
    
    if(total_claims > 0){
      
      sev_expanded <- rep(sev_mean_vec, claim_counts)
      
      claim_sizes <- rlnorm(
        total_claims,
        meanlog = log(sev_expanded),
        sdlog = sigma_ln
      ) * inflation_factor[y]
      
      ################################################
      # Apply deductible and reinsurance (inflation-adjusted)
      ################################################
      
      after_ded <- pmax(claim_sizes - deductible[y], 0)
      
      insurer_part <- pmin(after_ded, retention[y])
      
      reinsurer_part <- pmax(after_ded - retention[y], 0)
      
      insurer_loss_matrix[i,y] <- sum(insurer_part)
      reinsurer_loss_matrix[i,y] <- sum(reinsurer_part)
      
    } else {
      
      insurer_loss_matrix[i,y] <- 0
      reinsurer_loss_matrix[i,y] <- 0
      
    }
    
  }
}


##########################################################################
# 3: Multi-Year Pricing, Premiums, and Profit Calculations (Capital Included)
##########################################################################

# Storage vectors
premium_start_year <- numeric(n_years)
expected_claim_cost <- numeric(n_years)
reinsurance_premium <- numeric(n_years)
reinsurance_cost_end_year <- numeric(n_years)
mean_profit <- numeric(n_years)
probability_of_loss <- numeric(n_years)
capital_cost_vec <- numeric(n_years)

# Function to calculate probability of loss for a given premium for year y
loss_probability_year <- function(premium_start, y) {
  ir <- interest_rate_LT[y]
  claim_values <- insurer_loss_matrix[,y]
  reinsurer_values <- reinsurer_loss_matrix[,y]
  
  # Move claims to end-of-year (mid-year payment → compound half year)
  claim_value_end_year <- claim_values * (1 + ir)^0.5
  
  # Reinsurance premium → grows 1 year
  reinsurance_premium_tmp <- mean(reinsurer_values) * (1 + reinsurance_loading)
  reinsurance_cost_end_year_tmp <- reinsurance_premium_tmp * (1 + ir)
  
  # Capital requirement
  loss_distribution <- claim_value_end_year + reinsurer_values
  capital_cost <- capital_cost_rate * quantile(loss_distribution, 0.99)
  
  # Profit simulation
  profit_sim <- premium_start * (1 + ir) -
    claim_value_end_year -
    reinsurance_cost_end_year_tmp -
    capital_cost
  
  mean(profit_sim < 0)
}

# Loop over years
for(y in 1:n_years){
  
  ir <- interest_rate_LT[y]
  claim_values <- insurer_loss_matrix[,y]
  reinsurer_values <- reinsurer_loss_matrix[,y]
  
  # Move claims to end-of-year
  claim_value_end_year <- claim_values * (1 + ir)^0.5
  expected_claim_cost[y] <- mean(claim_value_end_year)
  
  # Reinsurance premium and cost
  reinsurance_premium[y] <- mean(reinsurer_values) * (1 + reinsurance_loading)
  reinsurance_cost_end_year[y] <- reinsurance_premium[y] * (1 + ir)
  
  # Capital cost
  loss_distribution <- claim_value_end_year + reinsurer_values
  capital_cost_vec[y] <- capital_cost_rate * quantile(loss_distribution, 0.99)
  
  # Base cost including capital
  base_cost <- expected_claim_cost[y] + reinsurance_cost_end_year[y] + capital_cost_vec[y]
  
  # Include expense and brokerage loading
  premium_end_year_required <- base_cost * (1 + expense_loading + brokerage_loading)
  
  # Start-of-year premium
  premium_start_year[y] <- premium_end_year_required / (1 + ir)
  
  # Profit simulation
  profit_simulation <- premium_start_year[y] * (1 + ir) -
    claim_value_end_year -
    reinsurance_cost_end_year[y] -
    capital_cost_vec[y]
  
  mean_profit[y] <- mean(profit_simulation)
  probability_of_loss[y] <- mean(profit_simulation < 0)
}

# Adjust premiums year by year to hit target loss probability
premium_target <- numeric(n_years)

for(y in 1:n_years){
  
  premium_grid <- seq(
    premium_start_year[y]*0.5,
    premium_start_year[y]*2,
    length.out = 200
  )
  
  loss_probs <- sapply(premium_grid, loss_probability_year, y = y)
  
  index <- which.min(abs(loss_probs - target_loss_prob))
  premium_target[y] <- premium_grid[index]
  
  # Update premium_start_year to target
  premium_start_year[y] <- premium_target[y]
}

# Recompute profits with adjusted premiums
for(y in 1:n_years){
  ir <- interest_rate_LT[y]
  claim_values <- insurer_loss_matrix[,y]
  reinsurer_values <- reinsurer_loss_matrix[,y]
  
  claim_value_end_year <- claim_values * (1 + ir)^0.5
  reinsurance_premium[y] <- mean(reinsurer_values) * (1 + reinsurance_loading)
  reinsurance_cost_end_year[y] <- reinsurance_premium[y] * (1 + ir)
  capital_cost_vec[y] <- capital_cost_rate * quantile(claim_value_end_year + reinsurer_values, 0.99)
  
  profit_simulation <- premium_start_year[y] * (1 + ir) -
    claim_value_end_year -
    reinsurance_cost_end_year[y] -
    capital_cost_vec[y]
  
  mean_profit[y] <- mean(profit_simulation)
  probability_of_loss[y] <- mean(profit_simulation < 0)
}

# Pricing summary
pricing_summary <- data.frame(
  year = 1:n_years,
  deductible = deductible,
  retention  = retention,
  interest_rate = interest_rate_LT,
  expected_claim_cost = expected_claim_cost,
  reinsurance_premium = reinsurance_premium,
  capital_cost = capital_cost_vec,
  premium_start_year = premium_start_year,
  mean_profit = mean_profit,
  probability_of_loss = probability_of_loss
)



print(pricing_summary)
##########################################################################
# 4: Pricing Summary
##########################################################################

pricing_summary <- data.frame(
  year = 1:n_years,
  deductible = deductible,
  retention  = retention,
  interest_rate = interest_rate_LT,
  expected_claim_cost = expected_claim_cost,
  reinsurance_premium = reinsurance_premium,
  premium_start_year = premium_start_year,
  mean_profit = mean_profit,
  probability_of_loss = probability_of_loss
)

print(pricing_summary)

##########################################################################
# 5: Plot Start-of-Year Premium Over Time
##########################################################################

ggplot(pricing_summary, aes(x = year, y = premium_start_year)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Start-of-Year Premium Over 10 Years",
    x = "Year",
    y = "Premium Charged (Start of Year)"
  ) +
  theme_minimal()



##########################################################################
# 6: Aggregate PV Profits and Risk Metrics (Capital Included)
##########################################################################

# Claims occur mid-year
discount_claims <- 1 / (1 + interest_rate_LT)^(1:n_years - 0.5)

# Premiums and capital occur at start of year
discount_premium <- 1 / (1 + interest_rate_LT)^(0:(n_years-1))

# Present value of insurer losses (mid-year payment)
insurer_loss_matrix_pv <- sweep(insurer_loss_matrix, 2, discount_claims, "*")
total_insurer_losses_pv <- rowSums(insurer_loss_matrix_pv)

# Present value of reinsurance premiums (paid start of year)
reinsurance_pv <- reinsurance_premium * discount_premium
total_reinsurance_pv <- rowSums(matrix(rep(reinsurance_pv, each = n_sim), nrow = n_sim))

# Present value of capital costs
capital_cost_pv <- capital_cost_vec * discount_premium
total_capital_cost_pv <- rowSums(matrix(rep(capital_cost_pv, each = n_sim), nrow = n_sim))

# Present value of premium income
premium_start_matrix <- matrix(rep(premium_start_year, each = n_sim), nrow = n_sim)
premium_pv <- sweep(premium_start_matrix, 2, discount_premium, "*")
total_premiums_pv <- rowSums(premium_pv)

# Total costs including capital
total_costs_pv <- total_insurer_losses_pv +
  total_capital_cost_pv +
  total_reinsurance_pv

# Net present value profits
total_profits_pv <- total_premiums_pv - total_costs_pv

# --- SUMMARY RISK METRICS ---
# Present value of claims only (mid-year)
total_claims_pv <- rowSums(insurer_loss_matrix_pv)

summary_table <- data.frame(
  Metric = c("Claims","Costs","Returns","Net Revenue"),
  Expected_Value = c(
    mean(total_claims_pv),
    mean(total_costs_pv),
    mean(total_premiums_pv),
    mean(total_profits_pv)
  ),
  Sd = c(
    sd(total_claims_pv),
    sd(total_costs_pv),
    sd(total_premiums_pv),
    sd(total_profits_pv)
  ),
  VaR_95 = c(
    quantile(total_claims_pv, 0.95),
    quantile(total_costs_pv, 0.95),
    quantile(total_premiums_pv, 0.05),
    quantile(total_profits_pv, 0.05)
  ),
  VaR_99 = c(
    quantile(total_claims_pv, 0.99),
    quantile(total_costs_pv, 0.99),
    quantile(total_premiums_pv, 0.01),
    quantile(total_profits_pv, 0.01)
  ),
  VaR_1 = c(
    quantile(total_claims_pv, 0.01),      # very low claims
    quantile(total_costs_pv, 0.01),       # very low costs
    quantile(total_premiums_pv, 0.99),    # very high premiums
    quantile(total_profits_pv, 0.01)      # very low net revenue
  )
)

print(summary_table)

# --- HISTOGRAMS ---
ggplot(data.frame(loss = total_insurer_losses_pv), aes(x = loss)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Aggregate Insurer Losses PV Over 10 Years", x = "Loss (PV)", y = "Simulations") +
  theme_minimal()


ggplot(data.frame(total_profits = total_profits_pv), aes(x = total_profits)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Total Profit PV Over 10 Years", x = "Profit (PV)", y = "Simulations") +
  theme_minimal()


###################################################
# Stress test
##################################################

# Frequency shock (+30%)
freq_stress_losses <- total_insurer_losses_pv * 1.30

# Severity shock (+20%)
sev_stress_losses <- total_insurer_losses_pv * 1.20

# Combined shock
combined_stress_losses <- total_insurer_losses_pv * 1.30 * 1.20

# Catastrophe scenario (1% probability of loss doubling)

cat_losses <- total_insurer_losses_pv
cat_event <- rbinom(n_sim, 1, 0.01)
cat_losses[cat_event == 1] <- cat_losses[cat_event == 1] * 2

# Summary
stress_results <- data.frame(
  Scenario = c("Baseline","Frequency +30%","Severity +20%","Combined","Catastrophe"),
  Mean = c(
    mean(total_insurer_losses_pv),
    mean(freq_stress_losses),
    mean(sev_stress_losses),
    mean(combined_stress_losses),
    mean(cat_losses)
  ),
  VaR_95 = c(
    quantile(total_insurer_losses_pv,0.95),
    quantile(freq_stress_losses,0.95),
    quantile(sev_stress_losses,0.95),
    quantile(combined_stress_losses,0.95),
    quantile(cat_losses,0.95)
  ),
  VaR_99 = c(
    quantile(total_insurer_losses_pv,0.99),
    quantile(freq_stress_losses,0.99),
    quantile(sev_stress_losses,0.99),
    quantile(combined_stress_losses,0.99),
    quantile(cat_losses,0.99)
  )
)

print(stress_results)

