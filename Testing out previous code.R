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


benchmark_symbols <-"^GSPC" #pull in list of stock indexes
benchmark_w <- 1
benchmark_tbl <- tibble(benchmark_symbols,
                        benchmark_w)
rfr <- .0003 #risk free rate 0.3% - 10 year treasury rate

n_future <- 12
seed <- 123


# 2.1 wealth index ----
# All seasons portfolio
all_seasons_data <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio")

min_date <- all_seasons_data %>%   
  summarise(min_date = min(date), 
            max_date = max(date)) %>% 
  pull(min_date)

sp500_data <- multi_asset_price_portfolio(symbols = benchmark_symbols, 
                                          end, 
                                          start = min_date 
                                          ) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(name_portfolio = benchmark_symbols)

# Combine data
port_wealth_index <- bind_portfolio(all_seasons_data, sp500_data) 



port_wealth_index %>%
  mutate(portfolio = as.factor(portfolio) %>% fct_reorder(investment.growth),
         label_text = str_glue("Portfolio: {str_to_title(portfolio)}
                                 Investment: {scales::dollar(investment.growth, accuracy = 1)}
                                 Growth %: {scales::percent(portfolio.wealthindex - 1, accuracy = 0.01)}")) %>%
  ggplot(aes(x = date, y = investment.growth, col = portfolio)) +
  geom_point(aes(text = label_text), size = 0.1) + #Must indicate label_text for tooltip to showup
  geom_line() +
  
  # Addition of Global Financial Crisis vertical line in Sept 2008
  geom_vline(xintercept = as.numeric(as.Date("2008-09-01")), linetype = "dotted", color = "red", size = 1.5) +
  annotate("text", x =  as.Date("2008-09-01") + 1200, y = 23000, label = "2008 Financial Crisis", color = "red") +
  
  scale_color_tq() +
  scale_y_continuous(labels = scales::dollar_format()) +
  # scale_x_date(breaks = date_breaks("1 year")) + 
  labs(title = str_glue("New Portfolio vs Benchmark"),
       x = "",
       y = "") 


