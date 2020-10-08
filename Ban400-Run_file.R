library(tidyquant)
library(tidyverse)
library(dplyr)
library(corrplot)
library(nloptr)
library(gtools)
library(skimr)

# Shiny implementation 

# setwd(dir = "C:/Users/magnu/Documents/BAN400/Ban400-stock-optimisation")

source("Ban400-Functions.R")

risk_free_rate <- 0.02

tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")
from_date <- "2005-08-01"
to_date <- "2010-08-01"


input <- stock_input(tickers,from_date ,to_date)

# input 1 = tickers
# input 2 = stock_prices
# input 3 = returns_matrix
# input 4 = stock_correlation
# input 5 = stock_return with date
# input 6 = stock_covariance
# input 7 = portfolio weigths

#finds the portfolio with the higest sharpe ratio
opt_sharpe <- stock_opt_sharpe(input[[1]],input[[7]],input[[3]],input[[6]])
opt_sharpe[[1]]
#finds the portfolio with the lowest yearly volatility
opt_vol <- stock_opt_vol(input[[1]],input[[7]],input[[3]],input[[6]])
opt_vol[[1]]
#creates returns histogram
returns_hist(input[[5]])
#creates correlation matrix
correlation_plot(input[[4]])

# creates stock price history graph
stock_price_history(input[[2]])

#Creates effeciency frontier
efficency_frontier(input[[1]],input[[7]],input[[3]],input[[6]], n = 5000)


compare_SP500(as.matrix(opt_vol[[2]]),input[[3]],from_date,to_date)





