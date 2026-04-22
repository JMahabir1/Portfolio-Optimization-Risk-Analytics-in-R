##Portfolio Optimization & Risk Analytics in R
##Author: Jason Mahabir

install.packages(c(
  "tidyverse",
  "tidyquant",
  "PerformanceAnalytics",
  "xts",
  "lubridate"
))

# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(xts)
library(lubridate)

# ============================================================
# 2. DOWNLOAD DATA
# ============================================================

symbols <- c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL", "SPY")

prices_raw <- tq_get(
  symbols,
  from = "2021-01-01",
  to = Sys.Date(),
  get = "stock.prices"
)

# ============================================================
# 3. CLEAN DATA
# ============================================================

prices <- prices_raw %>%
  select(symbol, date, adjusted) %>%
  arrange(symbol, date)

prices_wide <- prices %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date)

# ============================================================
# 4. CALCULATE RETURNS
# ============================================================

returns <- prices_wide %>%
  arrange(date) %>%
  mutate(across(-date, ~ (.x / lag(.x)) - 1)) %>%
  drop_na()

# ============================================================
# 5. PERFORMANCE METRICS
# ============================================================

metrics <- returns %>%
  summarise(across(-date, list(
    annual_return = ~ mean(.x, na.rm = TRUE) * 252,
    annual_volatility = ~ sd(.x, na.rm = TRUE) * sqrt(252)
  )))

print(metrics)

# ============================================================
# 6. CUMULATIVE RETURNS CHART
# ============================================================

returns_long <- returns %>%
  pivot_longer(-date, names_to = "symbol", values_to = "daily_return")

cumulative_returns <- returns_long %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(cumulative_growth = cumprod(1 + daily_return))

ggplot(cumulative_returns, aes(x = date, y = cumulative_growth, color = symbol)) +
  geom_line() +
  labs(
    title = "Cumulative Growth of $1 Invested",
    x = "Date",
    y = "Portfolio Value"
  ) +
  theme_minimal()

# ============================================================
# 7. SHARPE RATIO
# ============================================================

risk_free_rate <- 0.03  # 3% assumption

sharpe_table <- returns %>%
  summarise(across(-date, ~ {
    ann_return <- mean(.x, na.rm = TRUE) * 252
    ann_vol <- sd(.x, na.rm = TRUE) * sqrt(252)
    (ann_return - risk_free_rate) / ann_vol
  }))

print(sharpe_table)

# ============================================================
# 8. Portfolio vs. Benchmark
# ============================================================

portfolio_returns <- returns %>%
  select(-SPY)

benchmark_returns <- returns %>%
  select(date, SPY)


#Equal Weight Portfolio

portfolio_equal <- portfolio_returns %>%
  mutate(portfolio_return = rowMeans(select(., -date))) %>%
  select(date, portfolio_return)

#Compare Growth

comparison <- portfolio_equal %>%
  left_join(benchmark_returns, by = "date") %>%
  pivot_longer(-date, names_to = "series", values_to = "daily_return") %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(cumulative_growth = cumprod(1 + daily_return))

ggplot(comparison, aes(x = date, y = cumulative_growth, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Portfolio vs SPY Performance",
    x = "Date",
    y = "Growth of $1"
  ) +
  theme_minimal()

comparison_metrics <- comparison %>%
  group_by(series) %>%
  summarise(
    annual_return = mean(daily_return, na.rm = TRUE) * 252,
    annual_volatility = sd(daily_return, na.rm = TRUE) * sqrt(252),
    sharpe_ratio = (annual_return - risk_free_rate) / annual_volatility
  )

print(comparison_metrics)

# ============================================================
# 9. CAGR (GEOMETRIC RETURN)
# ============================================================

cagr_table <- comparison %>%
  group_by(series) %>%
  summarise(
    total_return = last(cumulative_growth) / first(cumulative_growth) - 1,
    years = as.numeric(difftime(max(date), min(date), units = "days")) / 365,
    CAGR = (1 + total_return)^(1 / years) - 1
  )

print(cagr_table)


# ============================================================
# 10. SIMPLE MAX-SHARPE OPTIMIZATION
# ============================================================

# Keep only stock returns for optimization (exclude SPY)
stock_returns <- returns %>%
  select(date, AAPL, MSFT, AMZN, NVDA, GOOGL)

# Convert returns to matrix form
returns_matrix <- stock_returns %>%
  select(-date) %>%
  as.matrix()

# Calculate average daily returns and covariance matrix
mu <- colMeans(returns_matrix, na.rm = TRUE)
Sigma <- cov(returns_matrix, use = "complete.obs")

# Convert annual risk-free rate to daily
rf_daily <- risk_free_rate / 252

# Calculate excess returns
excess_mu <- mu - rf_daily

# Solve for tangency portfolio weights
weights_raw <- solve(Sigma) %*% excess_mu

# Convert to numeric vector and normalize to sum to 1
optimal_weights <- as.numeric(weights_raw / sum(weights_raw))
names(optimal_weights) <- colnames(returns_matrix)

# Optional: remove negative weights to make it long-only
optimal_weights[optimal_weights < 0] <- 0
optimal_weights <- optimal_weights / sum(optimal_weights)

# Print weights nicely
optimal_weights_table <- tibble(
  symbol = names(optimal_weights),
  weight = optimal_weights
) %>%
  arrange(desc(weight))

print(optimal_weights_table)

# ============================================================
# 11. OPTIMIZED PORTFOLIO RETURNS
# ============================================================

# Calculate daily optimized portfolio returns
optimized_returns <- stock_returns %>%
  mutate(
    optimized_portfolio_return =
      AAPL  * optimal_weights["AAPL"] +
      MSFT  * optimal_weights["MSFT"] +
      AMZN  * optimal_weights["AMZN"] +
      NVDA  * optimal_weights["NVDA"] +
      GOOGL * optimal_weights["GOOGL"]
  ) %>%
  select(date, optimized_portfolio_return)

head(optimized_returns)

# ============================================================
# 12. CONSTRAINED MAX-SHARPE VERSION
# ============================================================

# Helper function to apply a max weight cap while keeping weights normalized
apply_max_weight_constraint <- function(weights, max_weight = 0.35) {
  weight_names <- names(weights)
  weights <- as.numeric(weights)
  
  if (any(!is.finite(weights))) {
    stop("Weights contain NA, NaN, or Inf before constraints.")
  }
  
  weights <- pmax(weights, 0)
  
  if (sum(weights) <= 0) {
    stop("All weights are zero after removing negatives.")
  }
  
  weights <- weights / sum(weights)
  
  for (i in 1:100) {
    over_cap <- weights > max_weight
    
    if (!any(over_cap, na.rm = TRUE)) {
      break
    }
    
    excess_weight <- sum(weights[over_cap] - max_weight, na.rm = TRUE)
    weights[over_cap] <- max_weight
    
    under_cap <- weights < max_weight
    
    if (!any(under_cap, na.rm = TRUE)) {
      break
    }
    
    room <- max_weight - weights[under_cap]
    
    if (sum(room, na.rm = TRUE) <= 0) {
      break
    }
    
    weights[under_cap] <- weights[under_cap] + excess_weight * (room / sum(room))
    weights <- weights / sum(weights)
  }
  
  names(weights) <- weight_names
  return(weights)
}

# Start from the raw Sharpe-optimal weights
weights_raw <- solve(Sigma + diag(1e-6, ncol(Sigma))) %*% excess_mu
weights_raw <- as.numeric(weights_raw)
names(weights_raw) <- colnames(returns_matrix)

if (any(!is.finite(weights_raw))) {
  stop("Raw optimized weights contain NA, NaN, or Inf.")
}

# Apply constraint: no stock can exceed 40%
constrained_weights <- apply_max_weight_constraint(weights_raw, max_weight = 0.35)

# Put constrained weights into a table
constrained_weights_table <- tibble(
  symbol = names(constrained_weights),
  weight = constrained_weights
) %>%
  arrange(desc(weight))

print(constrained_weights_table)

# ============================================================
# 13. CONSTRAINED OPTIMIZED PORTFOLIO RETURNS
# ============================================================

constrained_optimized_returns <- stock_returns %>%
  mutate(
    constrained_portfolio_return =
      AAPL  * constrained_weights["AAPL"] +
      MSFT  * constrained_weights["MSFT"] +
      AMZN  * constrained_weights["AMZN"] +
      NVDA  * constrained_weights["NVDA"] +
      GOOGL * constrained_weights["GOOGL"]
  ) %>%
  select(date, constrained_portfolio_return)

head(constrained_optimized_returns)

# ============================================================
# 14. EQUAL-WEIGHT vs UNCONSTRAINED vs CONSTRAINED vs SPY
# ============================================================

comparison_all <- portfolio_equal %>%
  left_join(optimized_returns, by = "date") %>%
  left_join(constrained_optimized_returns, by = "date") %>%
  left_join(benchmark_returns, by = "date") %>%
  rename(
    equal_weight_portfolio_return = portfolio_return,
    spy_return = SPY
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "daily_return"
  ) %>%
  group_by(series) %>%
  arrange(date) %>%
  mutate(cumulative_growth = cumprod(1 + daily_return)) %>%
  ungroup()

ggplot(comparison_all, aes(x = date, y = cumulative_growth, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Equal-Weight vs Optimized vs Constrained vs SPY",
    x = "Date",
    y = "Growth of $1"
  ) +
  theme_minimal()

# Portfolio comparison chart
performance_plot <- ggplot(comparison_all, aes(x = date, y = cumulative_growth, color = series)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Equal-Weight vs Optimized vs Constrained vs SPY",
    x = "Date",
    y = "Growth of $1"
  ) +
  theme_minimal()

print(performance_plot)

# Save chart to outputs folder
ggsave(
  filename = "outputs/performance_plot.png",
  plot = performance_plot,
  width = 10,
  height = 6,
  dpi = 300,
  create.dir = TRUE
)
# ============================================================
# 15. COMPARISON METRICS
# ============================================================

# Main performance metrics
comparison_all_metrics <- comparison_all %>%
  group_by(series) %>%
  summarise(
    annual_return = mean(daily_return, na.rm = TRUE) * 252,
    annual_volatility = sd(daily_return, na.rm = TRUE) * sqrt(252),
    sharpe_ratio = (annual_return - risk_free_rate) / annual_volatility,
    total_return = last(cumulative_growth) / first(cumulative_growth) - 1,
    years = as.numeric(difftime(max(date), min(date), units = "days")) / 365,
    CAGR = (1 + total_return)^(1 / years) - 1,
    .groups = "drop"
  )

# Convert comparison data to wide format for risk metrics
comparison_wide <- comparison_all %>%
  select(date, series, daily_return) %>%
  pivot_wider(names_from = series, values_from = daily_return)

# Convert to xts
comparison_xts <- xts::xts(
  comparison_wide %>% select(-date),
  order.by = as.Date(comparison_wide$date)
)

# Daily risk-free rate
rf_daily <- risk_free_rate / 252

# Sortino Ratio
sortino_vals <- SortinoRatio(
  R = comparison_xts,
  MAR = rf_daily
)

sortino_table <- tibble(
  series = colnames(comparison_xts),
  sortino_ratio = as.numeric(sortino_vals[1, ])
)

# Maximum Drawdown
max_drawdown_vals <- maxDrawdown(comparison_xts)

max_drawdown_table <- tibble(
  series = colnames(comparison_xts),
  max_drawdown = abs(as.numeric(max_drawdown_vals))
)

# Beta vs SPY
beta_vals <- CAPM.beta(
  comparison_xts[, colnames(comparison_xts) != "spy_return"],
  comparison_xts[, "spy_return"]
)

beta_table <- tibble(
  series = rownames(beta_vals),
  beta_vs_spy = as.numeric(beta_vals[, 1])
)

# Join all metrics into one final table
comparison_all_metrics <- comparison_all_metrics %>%
  left_join(sortino_table, by = "series") %>%
  left_join(max_drawdown_table, by = "series") %>%
  left_join(beta_table, by = "series") %>%
  arrange(desc(sharpe_ratio))

print(comparison_all_metrics)







