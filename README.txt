########## README PROJECT ##########

ABOUT

This is the readme file for our project in BAN400

The project idea is to create a shiny app where a user can determine the optimal 
stock portofolio from a larger set of stocks.

Our project is doing the following:

#1. Download stock data from Yahoo Finance
#2. Create graph on stock development
#3. Create correlation plot
#4. Optimization (solver):
-Find portfolio with max sharp ratio
-Find portfolio with minimum variance
-Find portfolio with max sortino ratio
#5. Draw efficiency frontier & test portfolios against solver result
#6. Test best portfolio against S&P 500 return in same period
#7. Classify stocks (by industry, sin stocks or green stocks)
#8. Everything is embedded in a shiny app where
  - the user can give predetermined input   
  - the user can sort out sin stocks
  - the user can easily generate a portofolio only consisting of green stocks 
  - the user can decide whether to 1) select industries to invest in, or 2) sort out industries not desirable to invest in
  - the user can choose optimization method and whether to allow for shorting or not
  - the user get output on which portofolio is the optimal one based upon the 
    chosen optimization method, with optimal portfolio statistics and optimal ratios of eatch stock in this portfolio
  - the user is getting error messages and warnings when something that can affect
    the validity of the output is happening, or input is missing. 

It is recommended to choose a time period between January 2005 and 31st of July 2020, 
as stock values from this time period is stored locally. When using data not stored 
locally, errors might happen if the script fails to collect the data. The set of 
stocks is collected form Yahoo Finance. The lists of sin stocks and green stocks are 
collected manually from different sources, before they are loaded into R as data 
frames. These data frames can be used for including or decluding certain shares in 
the shiny app. 

The app is tested on following R.versions: 4.0.2, 
To check your R.version write R.Version() in R console.

To run the scripts you will need an internet connection. In addition, make 
sure to install all the packages needed. Packages used are:
- htmlTable
- shiny
- tidyquant
- tidyverse
- dplyr
- corrplot
- nloptr
- gtools
- skimr
- svDialogs
- TTR
- markdown
- shinythemes
- shinycssloaders
- RColorBrewer
- pander 

The Shiny app has three different tabs. One for selecting stocks, one for 
selecting the optimalization, and one to see the results represented in 
graphs and charts. For best experience open Shiny app in full screen.

We have chosen to have one Shiny file and several sub files for 
functions and the R-data. If all the files are downloaded, the 
Shiny file will import the information you need from the sub files.

Explanation of the different files:
#Ban400-functions.R: 
- Include all functions needed to calculate. This file also loads
  all the data from the three Rdata-files to get results.

#df1.Rdata:
- Includes the return of a set of stocks at different dates.

#stock_info.Rdata:
- Includes information about different stocks, like symbol, exchange and category.
  It also adds max and min date, and all stock symbols placed in a character vector.

#Ban400-Shiny.r:
- The script to open and run the Shiny app. It is based on results and calculations in 
  the other scripts, and make it possible to select the stocks you wish to include in
  your portfolio. 

