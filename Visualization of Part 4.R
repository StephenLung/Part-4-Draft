#1.0 Load Libraries ----
library(pacman)
pacman::p_load(
  # App
  "flexdashboard",
  "shiny",
  "shinyjs",
  "shinyWidgets",

  # Core
  "tidyquant",
  "tidyverse",
  "timetk",
  "tidyr",
  "tibble",
  "dplyr",
  "furrr",
  "purrr",
  "dplyr",
  "glue",
  "forcats",
  "stringr",

  # Visualizations
  "plotly",
  "highcharter",
  "correlationfunnel",

  # Modeling
  "parsnip"
)

# 1.1 Load Functions ----
# source("../01_Scripts_Flex/wealth_index.R")
source("01_Scripts_Flex/wealth_index.R")
source("00_Scripts/04_forecast.R")
source("00_Scripts/05_basic.R")

# 1.2 Load in initial data ----
p_load(dplyr,
       rlang)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
# tech_symbols <- c("FB", "AMZN", "AAPL", "NFLX", "GOOG")
end     <- today()
start   <- end - years(20) + days(1)
w       <- c(0.3,
             0.4,
             0.15,
             0.075,
             0.075)
wts_tbl <- tibble(symbols, w)
window <- 24

tq_index_options()

benchmark_symbols <- tq_index_options()[[4]] #pull in list of stock indexes
benchmark_w <- 1
benchmark_tbl <- tibble(benchmark_symbols,
                        benchmark_w)
rfr <- .0003 #risk free rate 0.3% - 10 year treasury rate

n_future <- 12
seed <- 123

# test ----
format_table(symbols = symbols,
             w = w) 


# 2.1 Load Data ----
time_plot_tbl <- multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>%
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "monthly") 


# 2.2 Forecast Data ----
# time_predictions_tbl <- 
# monthly view
data <- time_plot_tbl %>% 
  generate_forecast_glmnet(n_future = n_future,
                           seed = seed,
                           penalty = 0,
                           mixture = 0) 

plot_data <- data %>% 
  plot_time_series_portfolio(period = "month")

multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>%
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "daily") %>% 
  generate_forecast_glmnet(n_future = 365,
                           seed = seed,
                           penalty = 0,
                           mixture = 0) %>% 
  plot_time_series_portfolio(period = "day")

multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>%
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "weekly") %>% 
  generate_forecast_glmnet(n_future = 52,
                           seed = seed,
                           penalty = 0,
                           mixture = 0) %>% 
  plot_time_series_portfolio(period = "week")

# 3.1 Integrate forecast returns of stock data ----
data %>% 
  tail(15)


# (port_wealth_index() %>% 
#     mutate(portfolio = as.factor(portfolio) %>% fct_reorder(investment.growth),
#            label_text = str_glue("Portfolio: {str_to_title(portfolio)}
#                                  Investment: {scales::dollar(investment.growth, accuracy = 1)}
#                                  Growth %: {scales::percent(portfolio.wealthindex - 1, accuracy = 0.01)}"))

  # All seasons portfolio ----
multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
    multi_asset_return_portfolio(period = "monthly") %>% 
    wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio")

multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  group_by(symbol) %>% 
  generate_forecast_glmnet(n_future = 12,
                           seed = seed,
                           penalty = 0,
                           mixture = 0)

# 3.1 Visualize Series ----
multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>%
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "monthly") %>%
  plot_time_series(period = "month", ggplotly = TRUE)

# 3.2 Visualize Forecast Series ----
multi_asset_price_portfolio(symbols = symbols, end, start, wts_tbl) %>%
  aggregate_returns_portfolio_2(wts_tbl = wts_tbl, period = "monthly") %>% 
  generate_forecast_glmnet(n_future = 24,
                           seed = seed,
                           penalty = 0,
                           mixture = 0) %>% 
  plot_time_series_portfolio(period = "month", ggplotly = TRUE)

# 4.1 Adjusting Benchmark options
tq_index_options()[[4]]



