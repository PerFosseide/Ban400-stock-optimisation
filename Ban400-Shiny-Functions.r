#Functions for stock optimisation





#import data functions
##############################################################################################################
#data
##############################################################################################################
load("df1.Rdata")
load("stock_info.Rdata")






##############################################################################################################
#Input functions 
##############################################################################################################

# combined inputfunction
input_function <- function(stocks, from_date, to_date) {
  output = 0
  if(from_date < min_date) {
    ouput <- stock_input(stocks, from_date, to_date)
  } else if(to_date>max_date) {
    output <- stock_input(stocks, from_date, to_date)
  } else if (all(stocks %in% stocks_symbols )) {
    output <- input_from_df(stocks, from_date, to_date)
  } else {stock_input(stocks, from_date, to_date)}
  
  return(output)
}


# sample filtred stocks

sample_function <- function(number_of_stocks, selected_industries = c("ALL"), filtered_industries = c("None")) {
  return_stocks <- ""
  if(selected_industries[1] == "ALL" & filtered_industries[1] == "None") {
    return_stocks <- sample(stocks_with_industry$Symbol, number_of_stocks)
  } else if(selected_industries[1] != "ALL") {
    x <- stocks_with_industry %>% 
      filter(Industry %in% selected_industries) %>% 
      filter(!Industry %in% filtered_industries)
    
    return_stocks <- sample(x$Symbol,number_of_stocks)
  } else if (filtered_industries[1] != "None") {
    x <- stocks_with_industry %>% 
      filter(!Industry %in% filtered_industries)
    return_stocks <- sample(x$Symbol,number_of_stocks)
  }
  return(return_stocks)
  
}





#creates a list of inputs used in the rest of the assignment
stock_input <- function(stocks, from_date, to_date) {
  
  stock_prices <-  try(tq_get(stocks, from = from_date,
                          to = to_date,
                          get = "stock.prices"))
  
  stock_return <- stock_prices %>% 
    group_by(symbol) %>% 
    mutate(return = per_change(adjusted),
           rows = n())
  num_rows <- max(stock_return$rows)
  stock_return <- stock_return %>% 
    filter(rows == num_rows)
  
  stock_cor <- stock_return %>% 
    group_by(symbol) %>% 
    mutate(rn = row_number()) %>% 
    pivot_wider(id_cols = c(symbol,return,rn),  values_from = return, names_from = symbol) %>% 
    select(-rn) %>% 
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
  dropped_stocks <- setdiff(unique(stocks), unique(stock_return$symbol))
  
  errors <- if (length(stocks)<length(unique(stock_return$symbol))) {
    errors <- paste("dropped stocks:", dropped_stocks, collapse = " ")
  } else {
    errors <- "none"
  }
  stocks <- unique(stock_return$symbol)
  
  weigths <- rep(1/length(stocks),length(stocks))
  weigths <- as.matrix(weigths)
  
  stock_prices <- stock_prices %>% 
    group_by(symbol) %>% 
    filter(date > from_date) %>% 
    filter(date < to_date) %>% 
    mutate(return = per_change(adjusted),
           rows = n()) %>% 
    select(-c(return, rows))
  
  stock_info <- stock_info %>% 
    filter(Symbol %in% stocks)
 
  
  
  output <- list(stocks, stock_prices, returns_matrix, stock_cor, stock_return, stock_cov, weigths, errors)
  return(output)
}


input_from_df <- function(stocks, from_date, to_date) {
  
  
  stock_prices <- stock_return %>%
    filter(symbol %in% stocks)
  stock_return <- stock_prices %>% 
    group_by(symbol) %>% 
    filter(date > from_date) %>% 
    filter(date < to_date) %>% 
    mutate(return = per_change(adjusted),
           rows = n())
  num_rows <- max(stock_return$rows)
  stock_return <- stock_return %>% 
    filter(rows == num_rows)
  
  stock_cor <- stock_return %>% 
    group_by(symbol) %>% 
    mutate(rn = row_number()) %>% 
    pivot_wider(id_cols = c(symbol,return,rn),  values_from = return, names_from = symbol) %>% 
    select(-rn) %>% 
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
  dropped_stocks <- setdiff(unique(stocks), unique(stock_return$symbol))
  
  errors <- if (length(stocks)<length(unique(stock_return$symbol))) {
    errors <- paste("dropped stocks:", dropped_stocks, collapse = " ")
  } else {
    errors <- "none"
  }
  stocks <- unique(stock_return$symbol)
  
  weigths <- rep(1/length(stocks),length(stocks))
  weigths <- as.matrix(weigths)
  
  stock_prices <- stock_prices %>% 
    group_by(symbol) %>% 
    filter(date > from_date) %>% 
    filter(date < to_date) %>% 
    mutate(return = per_change(adjusted),
           rows = n()) %>% 
    select(-c(return, rows))
  
  stock_info <- stock_info %>% 
    filter(Symbol %in% stocks)
  
  
  output <- list(stocks,stock_prices, returns_matrix, stock_cor, stock_return, stock_cov, weigths, errors,stock_info)
  return(output)
}


##############################################################################################################################
#graphing functions
#############################################################################################################################

#creates a histogram of the returns of each stock
returns_hist <- function(stock_return, chosen_stocks) {
  stock_return <- stock_return %>% 
  group_by(symbol) %>%
  filter(symbol %in% chosen_stocks$Symbol)
  
  ret_hist <- stock_return %>%
    ggplot(aes(x = return)) +
    geom_histogram(bins = 50, color = "black") +
    aes(fill = as.factor(symbol)) +
    facet_wrap(~symbol, scales = 'free_y') +
    theme_classic() +
    xlab("Daily returns") +
    scale_color_brewer(palette = "Set2") +
    ylab("Count") +
    theme(legend.position = "None") +
    theme(text = element_text(size=15))
  return(ret_hist)
}
#creates a plot of all correaltions of the stocks  
correlation_plot <-function(chosen_stocks, from_date, to_date) {
  tickers <- chosen_stocks$Symbol
  stocks <- input_function(tickers, from_date, to_date)
  stock_cor <- stocks[[4]]
  
  
  col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow",
                             "cyan", "#007FFF", "blue", "#00007F"))
  corrplot(stock_cor,
           order = "FPC",
           method = "number",
           tl.pos = "d",
           col = col1(100),
           tl.col = "black",
           cl.ratio = 0.2,
           cl.align = "r",
           type = "upper",
           title = "Portfolio Stock Correlation",
           mar=c(0,0,2,0)
  )
}

#plots the price development of each stock 
stock_price_history <- function(stock_prices,chosen_stocks,from_date,to_date) {
  stock_prices <-  stock_prices %>% 
    group_by(symbol) %>%
    filter(symbol %in% chosen_stocks$Symbol)
  
  stock_prices %>%
    ggplot(aes(x = date, y = adjusted, color = symbol))+
    geom_line() +
    facet_wrap(~symbol, scales = 'free_y') +
    labs(title = "Adjusted stock returns over time",subtitle = paste(from_date, " to ",to_date, sep="")) +
    xlab("Years") +
    ylab("Adjusted returns") +
    theme_classic()+
    theme(text = element_text(size=15))
  
}

#functions to calcualte values for random drawn portfolios(can't be used in optimisation)
neg_sharpe_ef<- function(weigths, stock_returns, stock_cov) {
  x <- (stock_returns %*% weigths)
  avg_return <- mean(x)*251
  std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
  score <- -(avg_return-risk_free_rate())/(std)
  return(score)
} 

#functions to calcualte values for random drawn portfolios(can't be used in optimisation)
min_vol_ef <- function(weigths,stock_returns, stock_cov) {
  std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
  return(std)
}

#drwas the effeciencyfrontier  
efficency_frontier <- function(chosen_stocks, weigths, returns_matrix, stock_cov, n = 10000) {
  tickers <- chosen_stocks$Symbol
  
  if(length(tickers)==1) {
    plot <- ""
  } else {
    stock_cov <- stock_cov[tickers,tickers]
    
    returns_matrix <- returns_matrix[, tickers]
    
    aplha <- rep(0.45,length(tickers))
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
      ylab("Portfolio expected return") +
      theme(text = element_text(size=15))
    
    plot <- plot + scale_color_gradient(low="blue",
                                        high ="red") +
      theme(text= element_text(size = 14))+
      labs(color = "Sharpe ratio")
  }
  
 
  
  return(plot)
}

#compares a given portfolio with the S&P 500 in a line graph
compare_SP500 <- function(weigths, stocks, from_date, to_date) {
  
  SP500 <-  tq_get("^GSPC", from = from_date,
                   to = Sys.Date(),
                   get = "stock.prices")
  SP500 <- SP500 %>%
    select(date, adjusted, symbol) %>%
    mutate(return = per_change(adjusted))
  
  SP500$SP500_perfomance <- cumprod(SP500$return+1)-1
  
  returns <- input_function(stocks,from_date,Sys.Date())
  returns_matrix <- returns[[3]]  
  
  SP500 <- SP500[1:nrow(returns_matrix),]
  
  
  SP500$opt_port = as.data.frame(returns_matrix %*% weigths)
  SP500$opt_port_performance = cumprod(SP500$opt_port+1)-1
  
  plot <-SP500 %>%
    ggplot(aes(x=date)) +
    geom_line(aes(y = SP500_perfomance, colour = "SP500 perfomance")) +
    geom_line(aes(y = unlist(opt_port_performance), colour = "Optimised portfolio performance")) +
    theme_classic()+
    geom_vline(xintercept = as.numeric(as.Date(to_date)), color = "black", linetype=2, size=1) +
    scale_y_continuous(labels = scales::percent)+
    labs(x = "Date",
         y = "Cumulative return") +
    scale_colour_manual("", 
                        breaks = c("SP500 perfomance", "Optimised portfolio performance"),
                        values = c("blue", "red")) +
    labs(title = "Optimise portfolio VS S&P500 index", subtitle = paste("Optimised period(left): ", from_date, "-", to_date, 
                                                                        "\n", "Test Period(rigth):     ", to_date, "-", paste(Sys.Date()) ))+
    theme(text = element_text(size=15))
  
  
  return(plot)
}

#plot portfolio industry composition
plot_industries <- function(stocks) {
  industries <- stocks_with_industry[stocks_with_industry$Symbol %in% stocks,]
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(length(stocks))
  plot <- industries %>% 
    group_by(Industry) %>%
    mutate(count = n()) %>%
    mutate(percent = (count/sum(count))*100) %>% 
    mutate(percent = if_else(abs(percent)<4,NA)) %>% 
    distinct(Industry, count, percent) %>%
    mutate(Industry = factor(x = Industry,
                             levels = Industry)) %>% 
    
    ggplot(aes(x="", y = count, fill = reorder(Industry, -count))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +  
    scale_fill_manual(values = mycolors) +
    
    geom_text(aes(label = paste0(round(percent), "%")), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size=18))
  
  plot + labs(fill = "Industries")
}


# plot portfolio sector composition
plot_sectors <- function(stocks) {
  industries <- stocks_with_industry[stocks_with_industry$Symbol %in% stocks,]
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(length(stocks))
  
  plot <- industries %>% 
    group_by(Sector) %>%
    mutate(count = n()) %>%
    mutate(percent = (count/sum(count))*100) %>% 
    mutate(percent = if_else(abs(percent)<4,NA)) %>% 
    distinct(Sector, count, percent) %>%
    mutate(Sector = factor(x = Sector,
                           levels = Sector)) %>% 
    ggplot(aes(x="", y = count, fill = reorder(Sector, -count))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +  
    scale_fill_manual(values = mycolors) +
    
    geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size=15))
  plot + labs(fill = "Sectors")
  
}

#plot piechart of optimised industry compsisition
#plot piechart of optimised industry compsisition
portfolio_industry <- function(stocks, weigths) {
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(length(stocks))
  port <- as.data.frame(stocks) %>% 
    rename("Symbol" = 1) 
  
  port$weigths <- weigths
  
  plot <- port %>% 
    left_join(stocks_with_industry, by =  c("Symbol"="Symbol")) %>% 
    group_by(Industry) %>%
    mutate(weigths = round(weigths,2)) %>% 
    filter(weigths > 0.005) %>% 
    summarise(Industry, weigths = sum(weigths)) %>%
    distinct(Industry, weigths) %>% 
    rowwise() %>% 
    mutate(labeled = (if_else(abs(weigths)<0.05, "", toString(paste(weigths*100, "%"))))) %>% 
    mutate(Industry = factor(x = Industry,
                             levels = Industry)) %>% 
    ggplot(aes(x="", y = weigths, fill = reorder(Industry, -weigths))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +  
    scale_fill_manual(values = mycolors) +
    
    geom_text(aes(label = paste0(labeled)), position = position_stack(vjust = 0.5)) +
    theme(text = element_text(size=15))  
  plot + labs(fill = "Industries")
  
}

#creates a histogram of the returns of portfolio
returns_final_hist <- function(returns_matrix,weights) {
  
  skew <- round(skewness(returns_matrix%*%weights),3)
  kurtosis <- round(kurtosis(returns_matrix%*%weights),3)
  
  ret_final_hist <- as.data.frame(returns_matrix%*%weights) %>%
    ggplot(aes(x = V1))+
    geom_histogram(bins = 40, col = "black", fill = "steelblue3")+
    theme_classic() +
    xlab("Returns") +
    ylab("Count") +
    theme(plot.title = element_text(color = "black"))+
    labs(title = "Portfolio returns histogram", subtitle = paste("skewness: ", skew, ",  ", "kurtosis: ", kurtosis)) +
    theme(text = element_text(size=15))
  return(ret_final_hist)
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
  sharpe_ratio = (stats$Avg_yearly_return-risk_free_rate())/
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


#calcualates percentage change
per_change <- function(x) {
  x = x/lag(x)-1
  na.fill(x,0)
}




#####################################################################################################
#optimisation functions
######################################################################################################

optimisation_funtion <- function(Method, tickers, weigths,stock_return, cov_matrix,  upper_bounds = 1, 
                                 lower_bounds = 0, port_size = 1) {
  
  input <- 0
  if(Method == 1) {
    input = stock_opt_sharpe(tickers, weigths,stock_return, cov_matrix, upper_bounds, lower_bounds, port_size)
  } else if(Method == 2) {
    input = stock_opt_sortino(tickers, weigths,stock_return, cov_matrix, upper_bounds, lower_bounds, port_size)
  }else if(Method == 3) {
    input = stock_opt_vol(tickers, weigths,stock_return, cov_matrix, upper_bounds, lower_bounds, port_size)
  }
  return(input)                            

}




#|Function for negative Sharpe Ratio|
neg_sharpe<- function(weigths, stock_returns, stock_cov) {
  
   sharpe <- function(weigths) {
    x <- (stock_returns %*% weigths)
    avg_return <- mean(x)*251
    std <- sqrt(t(weigths)%*%(stock_cov%*%weigths))
    score <- -(avg_return-risk_free_rate())/(std)
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




#finds the prtofolio with the optimal sharpe ratio
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
  
  stocks <- as.data.frame(t(max_sharpe_port)) %>% 
    arrange(desc(V1)) %>% 
    filter(abs(V1)>0.001) %>% 
    mutate(Symbol = row.names(.)) %>%  
    left_join(stock_info, by=c("Symbol"="Symbol")) %>% 
    rename("Percentage" = "V1") %>% 
    select(Percentage, Name, Symbol)
  
  
  
  max_sharpe_port$Sharpe_ratio = Max_sharpe$Sharpe_ratio
  max_sharpe_port$Yearly_std = Max_sharpe$Yearly_std
  max_sharpe_port$mean_return = Max_sharpe$Avg_yearly_return
  max_sharpe_port$Yearly_std = Max_sharpe$Yearly_std
  
  stats <- max_sharpe_port %>% 
    select(Sharpe_ratio, Yearly_std, mean_return)
  names <- c("Sharpe Ratio", "Yearly Standard Deviation","Avarge Yearly Return")
  
  colnames(stats) <- names
   
  result <- list(max_sharpe_port, sharpe$par, stocks,stats)
  
  return(result)
}

#finds the prtofolio with the mimimal volatility
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
  stocks <- as.data.frame(t(min_vol_port)) %>% 
    arrange(desc(V1)) %>% 
    filter(abs(V1)>0.001) %>% 
    mutate(Symbol = row.names(.)) %>%  
    left_join(stock_info, by=c("Symbol"="Symbol")) %>% 
    rename("Percentage" = "V1") %>% 
    select(Percentage, Name, Symbol)
  
  Min_vol <- port_summary(min_vol$par,returns,cov_matrix)
  min_vol_port$Sharpe_ratio =   Min_vol$Sharpe_ratio
  min_vol_port$Yearly_std =   Min_vol$Yearly_std
  min_vol_port$mean_return =   Min_vol$Avg_yearly_return
  min_vol_port$Yearly_std =   Min_vol$Yearly_std
  
  stats <- min_vol_port %>% 
    select(Sharpe_ratio, Yearly_std, mean_return)
  names <- c("Sharpe Ratio", "Yearly Standard Deviation","Avarge Yearly Return")
  
  colnames(stats) <- names
  
  
  
  result <- list(min_vol_port, min_vol$par, stocks, stats)
  
  return(result)
} 

#function for sortino ratio
sortino <- function(stock_return, weigths) {
  
  sortino_inner <- function(weigths) {
    
    
    port_return <- (stock_return %*% weigths)
    loss <- which(port_return < 0)
    
    downside_return <- stock_return[loss,]
    
    cov_mat <- cov(downside_return)*251
    
    mean_return <- mean(port_return)*251
    
    downside_std <- sqrt(t(weigths)%*%(cov_mat%*%weigths))
    
    sortino_ratio <- -((mean_return-risk_free_rate())/downside_std)
    return(sortino_ratio)
  }
  return(sortino_inner)
}



#finds the portfolio with the optimal sharpe ratio
stock_opt_sortino <- function(tickers, weigths,stock_return, cov_matrix,  upper_bounds = 1, 
                              lower_bounds = 0, port_size = 1) {
  
  bounds <- c(rep(upper_bounds,length(weigths)))
  lower_bounds = c(rep(lower_bounds,length(weigths)))
  
  con <- function(weigths){
    port <-weigths
    return(sum(port)-port_size) }
  nl.opts(list(xtol_rel = 0,ftol_abs = 0.0, maxeval = 1000))
  
  sortino_opt <- slsqp(weigths, fn = sortino(stock_return, weigths), lower = lower_bounds,
                       upper = bounds, heq = con)
  
  max_sortino_port <- as.data.frame(t(round(sortino_opt$par,4)))
  colnames(max_sortino_port) <- tickers
  stocks <- as.data.frame(t(max_sortino_port)) %>% 
    arrange(desc(V1)) %>% 
    filter(abs(V1)>0.001) %>% 
    mutate(Symbol = row.names(.)) %>%  
    left_join(stock_info, by=c("Symbol"="Symbol")) %>% 
    rename("Percentage" = "V1") %>% 
    select(Percentage, Name, Symbol)
  
  Max_Sortino <- port_summary(sortino_opt$par,stock_return,cov_matrix)
  max_sortino_port$Sortino_ratio = -sortino_opt$value
  max_sortino_port$Sharpe_ratio = Max_Sortino$Sharpe_ratio
  max_sortino_port$Yearly_std = Max_Sortino$Yearly_std
  max_sortino_port$mean_return = Max_Sortino$Avg_yearly_return
  max_sortino_port$Yearly_std = Max_Sortino$Yearly_std
  
  stats <- max_sortino_port %>% 
    select(Sharpe_ratio, Yearly_std, mean_return)
  names <- c("Sharpe Ratio", "Yearly Standard Deviation","Avarge Yearly Return")
  
  colnames(stats) <- names
  
  
  
  result <- list(max_sortino_port, sortino_opt$par, stocks, stats)
  
  return(result)
  
}


