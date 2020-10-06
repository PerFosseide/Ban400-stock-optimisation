library(tidyquant)
library(tidyverse)
library(dplyr)
library(corrplot)
library(nloptr)
library(gtools)
library(RColorBrewer)
library(skimr)
library(pander)
########################################################################################################################################################################
########################################################################################################################################################################

#EXAM BAN 420, fall 2020.
# Group # 17
#Participants:  # 49
                # 149
                # 170


#Objective: Finding optimal portfolio for an arbitrary amount of stocks#

#Content:
  #1. Download stock data from Yahoo Finance
  #2. Create graph on stock development
  #3. Create correlation plot
  #4. Optimization (solver):
      #-Find portfolio with max sharp ratio
      #-Find portfolio with minimum variance
  #5. Draw efficiency frontier & test portfolios against solver result
  #6. Test best portfolio against S&P 500 return in same period


####################################################################################
####################################################################################
####################         1. Download stock data           ######################
####################################################################################
####################################################################################

#Purpose pt.1:  #-Download data from yahoo finance
                #-Inspect data

#|Create variable for risk free rate|
risk_free_rate <- 0.02

                              #Will be used later for calculation of Sharpe Ratio


#|Creating vector|
tickers <- c("AAPL", "XOM", "BAC", "PFE", "NEE", "RTX")

                              #Creating vector with abbreviated stock-names for companies
                              #used in the portfolio. This can be done with any arbitrary set of stocks


#|Download data from Yahoo Finance|
stock_prices <-  tq_get(tickers, from = '2015-08-01',
        to = "2020-08-01",
        get = "stock.prices") #The command tells R to get stock prices of the companies listed in
                              #the previously created "tickers"

#|Inspect Data|
skim(stock_prices)
                              #skim is a variant of summary, used to check for
                              #abnormal data, and data integrity.


####################################################################################
####################################################################################
###################         2. Create graph           ##############################
####################################################################################
####################################################################################

#Purpose pt.2:  #-Create graph

#|Graph|
stock_prices %>%
  ggplot(aes(x = date, y = adjusted, color = symbol))+
  geom_line() +
  facet_wrap(~symbol, scales = 'free_y') +
  labs(title = "Adjusted stock returns over time",subtitle = "01.08.2015-01.08.2020") +
  xlab("Years") +
  ylab("Adjusted returns") +
  theme_classic()

                              #Create graph to check data integrity and see development 
                              #over time.
                              #color=symbol creates separate color for each graph, geom_line
                              #tells us what type of graph we want. facet_wrap tells R that we
                              #want separate graph for each stock and free_y gives each graph
                              #separate different y-scale.
                            


####################################################################################
####################################################################################
################         3. Create correlation plot           ######################
####################################################################################
####################################################################################

#Purpose pt.3:  #-Create histogram to view data
                #-Plot a correlation matrix (returns)

#|Normalize data|
per_change <- function(x) {
  x = x/lag(x)-1
  na.fill(x,0)
  }

                              #This function creates a variable x, which is the
                              #product of x divided by the value of x in the previous period.
                              #because the first value in the time series cant be divided by the previous value, the function will return NA
                              #for the first value. To avoid that we set the first value to 0.


#|Return column|
stock_return <- stock_prices %>%
  group_by(symbol) %>%
  mutate(return = per_change(adjusted))

                              #Creates a colum with stock returns, uses the function per_change
                              #to create return column using the adjusted data.
#Plot returns histogram
stock_return %>%
  ggplot(aes(x = return))+
  geom_histogram() +
  facet_wrap(~symbol, scales = 'free_y') +
  theme_classic() +
  xlab("Daily returns") +
  ylab("Count")

                         # Creates a histogram of daily returns to see if the returns are normaly distribiuted 


#|Correlation Matrix|
stock_cor <- stock_return %>%
  group_by(symbol) %>% #symbol = stock/ticker
  select(return,date) %>% # keep date to have uniqe key-value pair
  spread(symbol,return,drop= TRUE) %>%
  select(-(date)) %>%
  cor()

                              #Groups by stock, selects return column and date.Spread tidys
                              #the information: keeps the grouping but arranges the value.
                              #drops date and then correlates

#|Covarince Matrix|
stock_cov <- stock_return %>%
  group_by(symbol) %>% #symbol = stock/ticker
  select(return,date) %>% # keep date to have uniqe key-value pair
  spread(symbol,return,drop= TRUE) %>%
  select(-(date)) %>%
  cov()
stock_cov <- stock_cov*(251)



#|Correlation Plot|

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

                            #Creates a correlation plot, order FPC puts the highest correlation first.
                            #Method tells the plot to include numbers instead of for example: dots.
                            #tl.pos tells posistion of tickers, d for diagonal
                            #tl.col color on stock tickers
                            #cl.ratio Numbers on level bar (right side)
                            #cl.align displays upper triangular of the correlation matrix
                            #mar adjusts the title placement



####################################################################################
####################################################################################
#################         4. OPTIMAZATION/Solver         ###########################
####################################################################################
####################################################################################

#Purpose pt.4: Find optimal portfolio of the given stocks by using a solver.


#|Starting weights|
weigths <- rep(1/length(tickers),length(tickers))
weigths <- as.matrix(weigths)

                            #Creates weights to be used later, this is an arbitrary starting point.
                            #Gives same weight to each stock. Replicates value of 1/6 for
                            #the length of tickers (6)




#|Return matrix|
returns <- stock_return %>%
  select(date, return) %>%
  spread(symbol, return)%>%
  select(-(date)) %>%
  as.matrix()

                          #Creates returns matrix and removes date.



#|Standard deviation & average yearly return|
portfolio_stats <- function(weigths, stock_returns) {
  x <- (returns %*% weigths)
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

                          #Creates a function that computes average yearly return and standard deviation for a given portfolio
                          #%*% ganget med



                          #shows result of previous function with an equal weigthed portfolio


#|Function for negative Sharpe Ratio|

neg_sharpe<- function(weigths) {
  x <- portfolio_stats(weigths)
  Sharpe_ratio <- -(x$Avg_yearly_return-risk_free_rate)/(x$Yearly_std)
  return(Sharpe_ratio)
}

                        #This function finds the negative Sharpe Ratio, it is negative because the solver finds minimum point
                        #of the objective function.



#|Function to find standard deviation|
min_vol <- function(weigths) {
  x <- portfolio_stats(weigths)
  return(x$Yearly_std)
}

                        #min-vol fetches standard deviation.

#|Returns Sharpe Ratio, Std. deviation and returns|
port_summary <- function(weigths) {
  stats = portfolio_stats(weigths)
  sharpe_ratio = (stats$Avg_yearly_return-risk_free_rate)/
    stats$Yearly_std
  stats$Sharpe_ratio = sharpe_ratio

  return(stats)
}


#|Bound for portfolio|
con <- function(weigths){
  port <-weigths
  return(sum(port)-1) }
                         #constrain for the optimization, will return 0 if the sum of the portfolio = 1


#|Bounds for each stock|
bounds <- c(rep(1,length(tickers)))
lower_bounds = c(rep(0,length(tickers)))

                        #No stock can surpass 100% of the portfolio.
                        #No stock can be negative



#|Solver|Function to find highest Sharpe Ratio|
test_sharpe <- slsqp(weigths, fn = neg_sharpe, lower = lower_bounds,
              upper = bounds, heq = con)

                        #Solver, finds the highest Sharpe Ratio
                        #heq says sum of weights = 0
                       


#|Function to find portfolio with lowest volatility|
test_min_vol <- slsqp(weigths,fn = min_vol, lower = lower_bounds,
              upper = bounds, heq = con)

                        #Measured in std.dev


#|Displays portfolio with max Sharpe Ratio|
-test_sharpe$value
max_sharpe_port <- as.data.frame(t(round(test_sharpe$par,4)))
colnames(max_sharpe_port) <- tickers
pander::pander(max_sharpe_port)

#|Test that weights = 1|
sum(test_sharpe$par)
Max_sharpe <- port_summary(test_sharpe$par) 

                        #Test that weights = 1 and store result.


#|Displays portfolio with minimum volatility|
test_min_vol$value
min_vol_port <- as.data.frame(t(round(test_min_vol$par,4)))
colnames(min_vol_port) <- tickers
pander(min_vol_port)
min_std <- port_summary(test_min_vol$par)  

                        #Checks portifolio with minimum volatility and store result.

#|Test that weights = 1|
sum(test_min_vol$par)


####################################################################################
####################################################################################
##################         5. Draw efficiency frontier          ####################
####################################################################################
####################################################################################

#Purpose pt.5:  #-Plot the efficiency frontier
                #-Compare random-drawn portfolios to the optimal portfolio 

#|Mean return|
mean_return <- function(weigths){
  weigths <- as.numeric(weigths)
  x <- returns %*% weigths
  x <- mean(x)*251
  return(x)}

#|Set alpha for Dirichlet distribution|
aplha <- rep(0.45,length(tickers))

                          #This alpha gives greater control over the drawn portfolios.
                          #Important because optimal portfolio is skewed to only two stocks, makes
                          #it unlikely to draw that portfolio from the normal distribution of
                          #if the n of random portfolio is low. Could set higher but would take
                          #to long to compute the result in the presentation.


#|Draw portfolios|
values = rdirichlet(5000,aplha)
values <- as.data.frame(values)

                          #Draw "n" (1000) number of portfolios, all with the same
                          #amount of columns as alpha. Sum must = 1.

#|Create columns|
values <- values %>%
  mutate(avg_return = apply(., 1, mean_return),
         std = apply(., 1, min_vol),
         sharpe = -(apply(., 1,neg_sharpe)))
                                             # Creates three columns: average return, standard deviation and Sharpe Ratio
                                        
#|Names for columns|
values_names <- append(tickers,c("Avg_return", "Yearly_std","Sharpe_ratio"))
colnames(values) <- values_names
                                # insert ticker names



#|Check if values against solver|
values1 <- values %>%
  arrange(desc(Sharpe_ratio))
pander(round(head(values1,1),3))
values2 <- values %>%
  arrange(Yearly_std)
pander(round(head(values2,1),3))

                            #Checks if the drawn values are similar to the solver results.
                            # if the stats of the random portfolio are better than the optimised portfolios
                            # then there must be something wrong with the optimization


#|Plot efficiency frontier|
plot <- values %>%
  ggplot(aes(x=Yearly_std,y=Avg_return, color = Sharpe_ratio))+
  geom_point()+
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  xlim(0.15,0.35) +
  geom_point(data = Max_sharpe,
             aes(y= Avg_yearly_return, x = Yearly_std),
             size=7, color = "black", shape =3, stroke = 3)+
  labs(title = "Efficency frontier",
       subtitle = paste(tickers, collapse = ", ")) +
  xlab("Portfolio standard deviation") +
  ylab("Portfolio expected return") +
  geom_text(data = Max_sharpe, aes(x = Yearly_std,
                                    y = Avg_yearly_return,
                                    label="Max Sharpe",
                                    vjust = -1,
                                    hjust = 1.2,
                                    size = 10),
            show.legend = FALSE) +
  geom_point(data = min_std,
             aes(y= Avg_yearly_return, x = Yearly_std),
             size=7, color = "black", shape = 3, stroke =3) +
  geom_text(data = min_std, aes(x = Yearly_std,
                                    y = Avg_yearly_return,
                                    label="Min Volatility",
                                    vjust = -1.5,
                                    hjust = 1.1,
                                    size =10),
            show.legend = FALSE)

plot + scale_color_gradient(low="blue",
                            high ="red") +
  theme(text= element_text(size = 14))+
  labs(color = "Sharpe ratio")
                                             # The efficiency frontier displays all efficient portfolios for a given amount of return and risk



#|Download S&P 500 data from Yahoo Finance|
SP500 <-  tq_get("^GSPC", from = '2015-08-01',
                                 to = "2020-08-01",
                                 get = "stock.prices")

#|Create return column|
SP500 <- SP500 %>%
  select(date, adjusted, symbol) %>%
  mutate(return = per_change(adjusted))



#|Data frame for S&P 500 values|
df.500 <- as.data.frame(1)
df.500$Avg_Yearly_return = round((mean(SP500$return)*251),3)
df.500$Avg_Yearly_std = round((STDEV(SP500$return)*sqrt(251)),3)
df.500$Sharpe_ratio = (((mean(SP500$return)*251)-risk_free_rate))/(STDEV(SP500$return)*sqrt(251))
df.500 <- select(df.500, -c(1))

#|Show stats|
pander(df.500, caption = "S&P 500 performance")
pander(Max_sharpe, caption = "Max Sharpe portfolio performance")
pander(min_std, caption = "Min Volatility portfolio performance")

                                # Show stats to compare values to our optimal portfolio

