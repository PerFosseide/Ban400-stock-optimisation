
#Input functions 
##############################################################################################################
#calcualates percentage change
per_change <- function(x) {
  x = x/lag(x)-1
  na.fill(x,0)
}


#creates a list of inputs used in the rest of the assignment
stock_input <- function(stocks, from_date, to_date) {
  
  stock_prices <-  tq_get(stocks, from = from_date,
                          to = to_date,
                          get = "stock.prices")
  stock_return <- stock_prices %>%
    group_by(symbol) %>%
    mutate(return = per_change(adjusted))
  
  stock_cor <- stock_return %>%
    group_by(symbol) %>% #symbol = stock/ticker
    select(return,date) %>% # keep date to have uniqe key-value pair
    spread(symbol,return,drop= TRUE) %>%
    select(-(date)) %>%
    cor()
  
  stock_cov <- stock_return %>%
    group_by(symbol) %>% #symbol = stock/ticker
    select(return,date) %>% # keep date to have uniqe key-value pair
    spread(symbol,return,drop= TRUE) %>%
    select(-(date)) %>%
    cov()*251
  
  returns_matrix <- stock_return %>%
    select(date, return) %>%
    spread(symbol, return)%>%
    select(-(date)) %>%
    as.matrix()
  
  weigths <- rep(1/length(stocks),length(stocks))
  weigths <- as.matrix(weigths)
  
  
  
  output <- list(stocks, stock_prices, returns_matrix, stock_cor, stock_return, stock_cov, weigths)
  return(output)
}

##############################################################################################################################
#graphing functions
#############################################################################################################################


returns_hist <- function(stock_return) {
  ret_hist <- stock_return %>%
    ggplot(aes(x = return))+
    geom_histogram(bins = 40) +
    facet_wrap(~symbol, scales = 'free_y') +
    theme_classic() +
    xlab("Daily returns") +
    ylab("Count")
  return(ret_hist)
}
  
correlation_plot <-function(stock_cor) {
  corrplot(stock_cor,
         order = "FPC",
         method = "number",
         tl.pos = "d",
         tl.col = "black",
         cl.ratio = 0.2,
         cl.align = "r",
         type = "upper",
         title = "Stock Correlation",
         mar=c(0,0,2,0)
)
}

stock_price_history <- function(stock_prices) {
  stock_prices %>%
    ggplot(aes(x = date, y = adjusted, color = symbol))+
    geom_line() +
    facet_wrap(~symbol, scales = 'free_y') +
    labs(title = "Adjusted stock returns over time",subtitle = "01.08.2015-01.08.2020") +
    xlab("Years") +
    ylab("Adjusted returns") +
    theme_classic()  
}

neg_sharpe_ef<- function(weigths, stock_returns, stock_cov) {
    x <- (stock_returns %*% weigths)
    avg_return <- mean(x)*251
    std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
    score <- -(avg_return-0.02)/(std)
    return(score)
  } 

min_vol_ef <- function(weigths,stock_returns, stock_cov) {
    std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
    return(std)
  }
  
efficency_frontier <- function(tickers, weigths, returns_matrix, stock_cov, n = 10000) {
  
  aplha <- rep(0.45,length(weigths))
  values = rdirichlet(n,aplha)
  values <- as.data.frame(values)
  
  values <- values %>%
    mutate(avg_return = apply(., 1, mean_return, returns = returns_matrix),
           std = apply(., 1, min_vol_ef, stock_returns = returns_matrix, stock_cov =  stock_cov),
           sharpe = (-(apply(., 1,neg_sharpe_ef, stock_returns= returns_matrix, stock_cov =  stock_cov))))
  
  values_names <- append(tickers,c("Avg_return", "Yearly_std","Sharpe_ratio"))
  colnames(values) <- values_names
  
  plot <- values %>%
    ggplot(aes(x=Yearly_std,y=Avg_return, color = Sharpe_ratio))+
    geom_point()+
    scale_y_continuous(labels = scales::percent) +
    theme_classic() +
    scale_x_continuous() +
    labs(title = "Efficency frontier",
         subtitle = paste(tickers, collapse = ", ")) +
    xlab("Portfolio standard deviation") +
    ylab("Portfolio expected return")
  
  plot <- plot + scale_color_gradient(low="blue",
                              high ="red") +
    theme(text= element_text(size = 14))+
    labs(color = "Sharpe ratio")
  
  return(plot)
}

compare_SP500 <- function(weigths, returns_matrix, from_date, to_date) {
  SP500 <-  tq_get("^GSPC", from = from_date,
                   to = to_date,
                   get = "stock.prices")
  SP500 <- SP500 %>%
    select(date, adjusted, symbol) %>%
    mutate(return = per_change(adjusted))
  
  SP500$SP500_perfomance <- cumprod(SP500$return+1)-1
  

  SP500$opt_port = as.data.frame(returns_matrix %*% weigths)
  SP500$opt_port_performance = cumprod(SP500$opt_port+1)-1
  
  plot <-SP500 %>%
    ggplot(aes(x=date)) +
    geom_line(aes(y = SP500_perfomance, colour = "SP500 perfomance")) +
    geom_line(aes(y = unlist(opt_port_performance), colour = "Optimised portfolio performance")) +
    theme_classic()+
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date",
         y = "Cumulative return") +
    scale_colour_manual("", 
                        breaks = c("SP500 perfomance", "Optimised portfolio performance"),
                        values = c("blue", "red")) +
    labs(title = "Optimise portfolio VS S&P500 index")
  
  
  return(plot)
}








#####################################################################################################
#stats functions
####################################################################################################
#|Standard deviation & average yearly return|
portfolio_stats <- function(weigths, stock_returns, stock_cov) {
  x <- (stock_returns %*% weigths)
  avg_return <- mean(x)*251
  x <- array(x)
  std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
  df <- data.frame(0)
  df$avg_return = avg_return
  df$Std = std
  df <- df %>%
    select(-(X0)) %>%
    rename_at( 1, ~"Avg_yearly_return" ) %>%
    rename_at(2, ~"Yearly_std" )
  
  return(df)
}

#|Returns Sharpe Ratio, Std. deviation and returns|
port_summary <- function(weigths, stock_return, stock_cov) {
  stats = portfolio_stats(weigths, stock_return, stock_cov)
  sharpe_ratio = (stats$Avg_yearly_return-risk_free_rate)/
    stats$Yearly_std
  stats$Sharpe_ratio = sharpe_ratio
  
  return(stats)
}

#portfolio mean_return
mean_return <- function(weigths,returns){
    weigths <- as.numeric(weigths)
    x <- returns %*% weigths
    x <- mean(x)*251
    return(x)
  }

#####################################################################################################
#optimisation functions
######################################################################################################

#|Function for negative Sharpe Ratio|
neg_sharpe<- function(weigths, stock_returns, stock_cov) {
  
  sharpe <- function(weigths) {
    x <- (stock_returns %*% weigths)
    avg_return <- mean(x)*251
    std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
    score <- -(avg_return-0.02)/(std)
    return(score)
  } 
  return(sharpe) 
}

#Function for minimum volatility
min_vol <- function(weigths,stock_returns, stock_cov) {
  vol <- function(weigths){
    std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
    return(std)
  }
  
  return(vol)
}


stock_opt_sharpe <- function(tickers, weigths ,returns,cov_matrix,  upper_bounds = 1, 
                             lower_bounds = 0, port_size = 1) {
  
  weigths = weigths
  bounds <- c(rep(upper_bounds,length(weigths)))
  lower_bounds = c(rep(lower_bounds,length(weigths)))
  
  con <- function(weigths){
    port <-weigths
    return(sum(port)-port_size) }
  nl.opts(list(xtol_rel = 0,ftol_abs = 0.0, maxeval = 10000))
  
  sharpe <- slsqp(weigths, fn = neg_sharpe(weigths,returns,cov_matrix), lower = lower_bounds,
                  upper = bounds, heq = con)
  
  max_sharpe_port <- as.data.frame(t(round(sharpe$par,4)))
  colnames(max_sharpe_port) <- tickers
  Max_sharpe <- port_summary(sharpe$par,returns,cov_matrix)
  max_sharpe_port$Sharpe_ratio = Max_sharpe$Sharpe_ratio
  max_sharpe_port$Yearly_std = Max_sharpe$Yearly_std
  max_sharpe_port$mean_return = Max_sharpe$Avg_yearly_return
  max_sharpe_port$Yearly_std = Max_sharpe$Yearly_std
  
  result <- list(max_sharpe_port, sharpe$par)
  
  return(result)
}
stock_opt_vol <- function(tickers, weigths ,returns,cov_matrix,  upper_bounds = 1, 
                          lower_bounds = 0, port_size = 1) {
  
  weigths = weigths
  bounds <- c(rep(upper_bounds,length(weigths)))
  lower_bounds = c(rep(lower_bounds,length(weigths)))
  
  con <- function(weigths){
    port <-weigths
    return(sum(port)-port_size) }
  
  nl.opts(list(xtol_rel = 0, ftol_abs = 0.0, maxeval = 10000))
  
  min_vol <- slsqp(weigths, fn = min_vol(weigths,returns,cov_matrix), lower = lower_bounds,
                  upper = bounds, heq = con)
  
  min_vol_port <- as.data.frame(t(round(min_vol$par,4)))
  colnames(min_vol_port) <- tickers
  Min_vol <- port_summary(min_vol$par,returns,cov_matrix)
  min_vol_port$Sharpe_ratio =   Min_vol$Sharpe_ratio
  min_vol_port$Yearly_std =   Min_vol$Yearly_std
  min_vol_port$mean_return =   Min_vol$Avg_yearly_return
  min_vol_port$Yearly_std =   Min_vol$Yearly_std
  
  result <- list(min_vol_port, min_vol$par)
  
  return(result)
} 



