install.packages(c(
  "tidyverse",
  "tidyquant",
  "PerformanceAnalytics",
  "xts",
  "lubridate",
  "shiny",
  "DT"
))

library(shiny)
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(xts)
library(lubridate)
library(DT)

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
  weights
}

ui <- fluidPage(
  titlePanel("Portfolio Optimization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "symbols",
        "Select Stocks",
        choices = c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL"),
        selected = c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL")
      ),
      dateInput("start_date", "Start Date", value = "2021-01-01"),
      numericInput("risk_free_rate", "Annual Risk-Free Rate", value = 0.03, min = 0, max = 0.20, step = 0.005),
      sliderInput("max_weight", "Max Weight per Stock", min = 0.20, max = 0.50, value = 0.35, step = 0.05),
      actionButton("run_model", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Chart", plotOutput("performancePlot", height = "500px")),
        tabPanel("Metrics", DTOutput("metricsTable")),
        tabPanel("Weights", DTOutput("weightsTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  analysis_data <- eventReactive(input$run_model, {
    
    req(length(input$symbols) >= 2)
    
    symbols <- c(input$symbols, "SPY")
    
    prices_raw <- tq_get(
      symbols,
      from = input$start_date,
      to = Sys.Date(),
      get = "stock.prices"
    )
    
    prices_wide <- prices_raw %>%
      select(symbol, date, adjusted) %>%
      pivot_wider(names_from = symbol, values_from = adjusted) %>%
      arrange(date)
    
    returns <- prices_wide %>%
      arrange(date) %>%
      mutate(across(-date, ~ (.x / lag(.x)) - 1)) %>%
      drop_na()
    
    risk_free_rate <- input$risk_free_rate
    rf_daily <- risk_free_rate / 252
    
    portfolio_returns <- returns %>%
      select(-SPY)
    
    benchmark_returns <- returns %>%
      select(date, SPY)
    
    portfolio_equal <- portfolio_returns %>%
      mutate(portfolio_return = rowMeans(select(., -date))) %>%
      select(date, portfolio_return)
    
    stock_returns <- returns %>%
      select(date, all_of(input$symbols))
    
    returns_matrix <- stock_returns %>%
      select(-date) %>%
      as.matrix()
    
    mu <- colMeans(returns_matrix, na.rm = TRUE)
    Sigma <- cov(returns_matrix, use = "complete.obs")
    excess_mu <- mu - rf_daily
    
    weights_raw <- solve(Sigma + diag(1e-6, ncol(Sigma))) %*% excess_mu
    weights_raw <- as.numeric(weights_raw / sum(weights_raw))
    names(weights_raw) <- colnames(returns_matrix)
    
    optimal_weights <- weights_raw
    optimal_weights[optimal_weights < 0] <- 0
    optimal_weights <- optimal_weights / sum(optimal_weights)
    
    constrained_weights <- apply_max_weight_constraint(weights_raw, max_weight = input$max_weight)
    
    optimized_returns <- stock_returns %>%
      mutate(
        optimized_portfolio_return = as.matrix(select(., -date)) %*% optimal_weights
      ) %>%
      select(date, optimized_portfolio_return)
    
    constrained_optimized_returns <- stock_returns %>%
      mutate(
        constrained_portfolio_return = as.matrix(select(., -date)) %*% constrained_weights
      ) %>%
      select(date, constrained_portfolio_return)
    
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
    
    comparison_wide <- comparison_all %>%
      select(date, series, daily_return) %>%
      pivot_wider(names_from = series, values_from = daily_return)
    
    comparison_xts <- xts::xts(
      comparison_wide %>% select(-date),
      order.by = as.Date(comparison_wide$date)
    )
    
    sortino_vals <- SortinoRatio(comparison_xts, MAR = rf_daily)
    max_drawdown_vals <- maxDrawdown(comparison_xts)
    beta_vals <- CAPM.beta(
      comparison_xts[, colnames(comparison_xts) != "spy_return"],
      comparison_xts[, "spy_return"]
    )
    
    sortino_table <- tibble(
      series = colnames(comparison_xts),
      sortino_ratio = as.numeric(sortino_vals[1, ])
    )
    
    max_drawdown_table <- tibble(
      series = colnames(comparison_xts),
      max_drawdown = abs(as.numeric(max_drawdown_vals))
    )
    
    beta_table <- tibble(
      series = rownames(beta_vals),
      beta_vs_spy = as.numeric(beta_vals[, 1])
    )
    
    comparison_all_metrics <- comparison_all_metrics %>%
      left_join(sortino_table, by = "series") %>%
      left_join(max_drawdown_table, by = "series") %>%
      left_join(beta_table, by = "series") %>%
      arrange(desc(sharpe_ratio))
    
    weights_table <- tibble(
      symbol = names(constrained_weights),
      constrained_weight = constrained_weights,
      unconstrained_weight = optimal_weights[names(constrained_weights)]
    ) %>%
      arrange(desc(constrained_weight))
    
    list(
      comparison_all = comparison_all,
      metrics = comparison_all_metrics,
      weights = weights_table
    )
  })
  
  output$performancePlot <- renderPlot({
    df <- analysis_data()$comparison_all
    
    ggplot(df, aes(x = date, y = cumulative_growth, color = series)) +
      geom_line(linewidth = 1) +
      labs(
        title = "Portfolio Performance Comparison",
        x = "Date",
        y = "Growth of $1"
      ) +
      theme_minimal()
  })
  
  output$metricsTable <- renderDT({
    datatable(
      analysis_data()$metrics,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$weightsTable <- renderDT({
    datatable(
      analysis_data()$weights,
      options = list(pageLength = 10)
    )
  })
}

shinyApp(ui = ui, server = server)