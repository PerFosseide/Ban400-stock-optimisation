  
########## README PROJECT ##########

In this project we create a shiny app where the user has the opportunity to
determine the optimal stock portfolio from a large set of stocks. The user 
can choose to exclude sin stocks, or to only include green stocks.

The set of stocks is collected form Yahoo Finance. The lists of sin stocks 
and green stocks are collected manually from different sources, before they
are loaded into R as data frames. These data frames can be used for including
or decluding certain shares in the shiny app. 

The shiny app has three different tabs. One for selecting stocks, one for 
selecting the optimalization, and one to see the results represented in 
graphs and charts.

In the project, we have chosen to have one master R. file and several sub 
files for functions and the shiny app script. If all the files are downloaded,
the main files will collect the information you need from the sub files. There 
are two important main files. One for the the solution and results in R Markdown, 
and one for the Shiny app.

Explanation of the different files:
#Ban400-functions.R: 
- Include all functions needed to calculate or plot results. This file also loads
  all the data from the three Rdata-files to get results.

#df1.Rdata:
- Includes the return of a set of stocks at different dates.

#stock_info.Rdata:
- Includes information about different stocks, like symbol, exchange and category.
  It also adds max and min date, and all stock symbols placed in a character vector.

#stocks_industry.Rdata:
- Adds a dataframe with information about the industry of each stock.

#Ban400_markdown.Rmd:
- The main file that shows results in numbers and graphs by using the functions from
  the functions-file. 

#Ban400-Shiny.r:
- The script to open and run the Shiny app. It is based on results and calculations in 
  the other scripts, and make it possible to select the stocks you wish to include in
  your portfolio. 
