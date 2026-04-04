library(vars)
library(BVAR)
library(dplyr)
library(scales)
library(plotly)
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
library(purrr)
library(lubridate)

theme_set(theme_linedraw())

# ─────────────────────────────── Data Cleaning ────────────────────────────────
# Provided data from SOA
soa_data <- read.csv("datasets/srcsc-2026-interest-and-inflation.csv", header = T) %>%
  setNames(c("year","inflation_rate","overnight_rate","spot_1y","spot_10y")) %>%
  mutate(across(-year, ~ as.numeric(gsub("%", "", .)) / 100),
         term_spread = spot_10y - spot_1y)

# Monthly US CPI data
us_cpi_data <- read.csv("datasets/CPIAUCSL.csv", header = T) %>%
  mutate(observation_date = as.Date(observation_date),
         cpi = na.approx(as.numeric(CPIAUCSL))) %>%
  select(-CPIAUCSL)

# Monthly US Fed Rate data
us_fed_rate_data <- read.csv("datasets/FEDFUNDS.csv", header = T)%>%
  mutate(observation_date = as.Date(observation_date),
         fed_rate = na.approx(as.numeric(FEDFUNDS))) %>%
  select(-FEDFUNDS)

# Monthly US 1Y Spot Rate data
us_spot_1y_data <- read.csv("datasets/DGS1.csv", header = T)%>%
  mutate(observation_date = as.Date(observation_date),
         spot_1y = na.approx(as.numeric(DGS1))) %>%
  select(-DGS1) %>%
  group_by(year = year(observation_date), month = month(observation_date)) %>%
  slice_min(observation_date, n = 1) %>%
  ungroup() %>%
  mutate(observation_date = make_date(year, month, 1)) %>%
  select(observation_date, spot_1y)

# Monthly US 10Y Spot rate data
us_spot_10y_data <- read.csv("datasets/DGS10.csv", header = T)%>%
  mutate(observation_date = as.Date(observation_date),
         spot_10y = na.approx(as.numeric(DGS10))) %>%
  select(-DGS10) %>%
  group_by(year = year(observation_date), month = month(observation_date)) %>%
  slice_min(observation_date, n = 1) %>%
  ungroup() %>%
  mutate(observation_date = make_date(year, month, 1)) %>%
  select(observation_date, spot_10y)

# Use monthly cpi to calculate annual inflation
us_inflation_data <- us_cpi_data %>%
  mutate(inflation_rate = (cpi / lag(cpi, 12) - 1) * 100) %>%
  select(observation_date, inflation_rate) %>%
  filter(!is.na(inflation_rate))

# Combine us rates
us_data <- us_inflation_data %>%
  inner_join(us_fed_rate_data, by = "observation_date") %>%
  inner_join(us_spot_1y_data, by = "observation_date") %>%
  inner_join(us_spot_10y_data, by = "observation_date")

# ──────────────────────────────── Rate Trends ─────────────────────────────────
ggplot(soa_data, aes(x = year)) +
  geom_line(aes(y = inflation_rate, colour = "Inflation Rate"), linewidth = 1) +
  geom_line(aes(y = overnight_rate, colour = "Overnight Rate"), linewidth = 1) +
  geom_line(aes(y = spot_1y, colour = "1Y Spot Rate"), linewidth = 1) +
  geom_line(aes(y = spot_10y, colour = "10Y Spot Rate"), linewidth = 1) +
  labs(
    title = "Rate Trends (SOA Provided)",
    x = "Year",
    y = "Rate (%)",
    colour = "Rate"
  ) +
  scale_colour_manual(values = c(
    "Inflation Rate" = "red",
    "Overnight Rate" = "green",
    "1Y Spot Rate" = "blue",
    "10Y Spot Rate" = "orange"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(),
    legend.title = element_blank(),
    legend.key.spacing.x = unit(0.6, "cm")
  )

ggplot(us_data %>% filter(observation_date >= as.Date("2011-03-01")),
       aes(x = observation_date)) +
  geom_line(aes(y = inflation_rate, colour = "Inflation Rate"), linewidth = 0.8) +
  geom_line(aes(y = fed_rate, colour = "Fed Rate"), linewidth = 0.8) +
  geom_line(aes(y = spot_1y, colour = "1Y Spot Rate"), linewidth = 0.8) +
  geom_line(aes(y = spot_10y, colour = "10Y Spot Rate"), linewidth = 0.8) +
  labs(
    title = "Rate Trends (US Market)",
    x = "Year",
    y = "Rate (%)",
    colour = "Rate"
  ) +
  scale_colour_manual(values = c(
    "Inflation Rate" = "red",
    "Fed Rate" = "green",
    "1Y Spot Rate" = "blue",
    "10Y Spot Rate" = "orange"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.margin = margin(),
    legend.title = element_blank(),
    legend.key.spacing.x = unit(0.6, "cm")
  )

# ──────────────────────────── Stationarity Checks ─────────────────────────────
start_date <- as.Date("1990-01-01")

us_inflation_data <- us_inflation_data %>% filter(observation_date >= start_date)
us_fed_rate_data  <- us_fed_rate_data %>% filter(observation_date >= start_date)
us_spot_10y_data  <- us_spot_10y_data %>% filter(observation_date >= start_date)
us_spot_1y_data   <- us_spot_1y_data %>% filter(observation_date >= start_date)

adf.test(us_inflation_data$inflation_rate)       # p-value = <0.01 (significant)
adf.test(diff(us_inflation_data$inflation_rate)) # p-value = <0.01 (significant)

adf.test(us_fed_rate_data$fed_rate)       # p-value = 0.03658
adf.test(diff(us_fed_rate_data$fed_rate)) # p-value = <0.01 (significant)

adf.test(us_spot_10y_data$spot_10y)       # p-value = 0.5853
adf.test(diff(us_spot_10y_data$spot_10y)) # p-value = <0.01 (significant)

adf.test(us_spot_1y_data$spot_1y)       # p-value = 0.2448
adf.test(diff(us_spot_1y_data$spot_1y)) # p-value = <0.01 (significant)

# Differencing required for stationarity

# ──────────────────────────── ARIMA Model Fitting ─────────────────────────────
# Forecast horizon (10 years = 120 months)
h <- 12 * 10

fit_arima <- function(series, name) {
  model <- auto.arima(series, allowdrift = TRUE)
  cat("\n================================================\n")
  cat("Model for:", name, "\n")
  cat("================================================\n")
  print(summary(model))
  checkresiduals(model)
  model
}

# Fit ARIMA models automatically
arima_inflation_rate <- fit_arima(us_inflation_data$inflation_rate, "Inflation Rate")
arima_fed_rate       <- fit_arima(us_fed_rate_data$fed_rate, "Fed Rate")
arima_spot_1y        <- fit_arima(us_spot_1y_data$spot_1y, "1Y Spot Rate")
arima_spot_10y       <- fit_arima(us_spot_10y_data$spot_10y, "10Y Spot Rate")

# Plot rate forecasts
par(mfrow = c(2,2), mar = c(2.5,3,4,1))
plot(forecast(arima_inflation_rate, h), main = "Inflation Rate Forecast\nARIMA(0,1,1)")
plot(forecast(arima_fed_rate, h), main = "Fed Rate Forecast\nARIMA(1,1,2)")
plot(forecast(arima_spot_1y, h), main = "1Y Spot Rate Forecast\nARIMA(1,1,1)")
plot(forecast(arima_spot_10y, h), main = "10Y Spot Rate Forecast\nARIMA(0,1,0)")

# ───────────────────────────── BVAR Model Fitting ─────────────────────────────
us_data <- us_data %>%
  rename(date = observation_date) %>%
  filter(date >= start_date)

us_ts <- ts(us_data %>% select(-c(date)),
            frequency = 12,
            start = c(year(start_date), month(start_date)))

# Find optimal lag length
VARselect(us_ts, lag.max = 50, type = "const")
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 16      2      2     16 

# Fit bayesian vector autoregression 
model <- bvar(us_ts, lags = 16)
summary(model)

# Residual acf plots
res <- resid(model)
par(mfrow = c(2,2), mar = c(4,4,4,1))
acf(res[,1], main = "ACF Residuals: Inflation")
acf(res[,2], main = "ACF Residuals: Fed Rate")
acf(res[,3], main = "ACF Residuals: 1Y Spot Rate")
acf(res[,4], main = "ACF Residuals: 10Y Spot Rate")

# ─────────────────────────────── BVAR Forecast ────────────────────────────────
# Forecast 10 years into the future, with 95% CI
forecast <- predict(model, horizon = 10 * 12, conf_bands = c(0.05, 0.95))

# Plot forecast with last 15 years historical data
plot(forecast, t_back = 15 * 12)

# Extract forecast means
forecast_means <- apply(forecast$fcast, c(2,3), mean)

forecast_dates <- seq.Date(
  from = max(us_data$date) %m+% months(1),
  by = "month",
  length.out = 120
)

forecast_data <- data.frame(
  date           = forecast_dates,
  inflation_rate = forecast_means[,1],
  fed_rate       = forecast_means[,2],
  spot_1y        = forecast_means[,3],
  spot_10y       = forecast_means[,4]
)

# Combine forecasts and historicals together, and map US dates to case study
# dates using: 2025 -> 2175
mapped_data <- bind_rows(us_data, forecast_data) %>%
  mutate(mapped_date = date %m+% years(150))

# Keep 2175-2184 (10 year) forecasts and convert monthly rates to annual
# by taking average across months
annual_forecast <- mapped_data %>%
  filter(
    year(mapped_date) >= 2175 &
    year(mapped_date) <= 2184
  ) %>%
  mutate(year = year(mapped_date)) %>%
  group_by(year) %>%
  summarise(
    inflation_rate = mean(inflation_rate),
    fed_rate       = mean(fed_rate),
    spot_1y        = mean(spot_1y),
    spot_10y       = mean(spot_10y)
  ) %>%
  ungroup()
annual_forecast

# ──────────────────────────── Yield Curve Fitting ─────────────────────────────
fit_yield_curve <- function(fed_rate, spot_1y, spot_10y){
  x <- c(0, 1, 10)
  y <- c(fed_rate, spot_1y, spot_10y)
  # Use spline for interpolation
  spline_fun <- splinefun(x, y, method = "natural")
  data.frame(
    maturity = 0:10,
    yield = spline_fun(0:10)
  )
}

# Create a table for all forecasted years
yield_curve_table <- lapply(1:nrow(annual_forecast), function(i) {
  yield_curve <- fit_yield_curve(
    fed_rate = annual_forecast$fed_rate[i],
    spot_1y = annual_forecast$spot_1y[i],
    spot_10y = annual_forecast$spot_10y[i]
  )
  colnames(yield_curve)[2] <- as.character(annual_forecast$year[i])
  yield_curve
})

# Merge into one table
yield_curve_final <- reduce(yield_curve_table, left_join, by = "maturity")
yield_curve_final

# 3d plot
plot_ly(
  x = yield_curve_final$maturity,
  y = as.numeric(colnames(yield_curve_final)[-1]),
  z = as.matrix(yield_curve_final[,-1])
) |>
  add_surface(
    contours = list(
      x = list(show = TRUE, color = "#242526", width = 1),
      y = list(show = TRUE, color = "#242526", width = 1)
    )
  ) |>
  layout(
    scene = list(
      xaxis = list(title = "Maturity (Years)"),
      yaxis = list(title = "Forecast Year"),
      zaxis = list(title = "Yield Rate (%)")
    )
  )

# ────────────────────────────── Export Forecasts ──────────────────────────────
inflation_forecast <- annual_forecast %>%
  select(c(year, inflation_rate))

write.csv(inflation_forecast, file = "forecasts/inflation.csv", row.names = FALSE)
write.csv(yield_curve_final, file = "forecasts/interest.csv", row.names = FALSE)
