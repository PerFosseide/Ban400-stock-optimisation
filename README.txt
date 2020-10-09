############ README ##########

ABOUT

app.R is the formerly runfile in the main branch

This is the readme file for our project in BAN 400

The project idea is to create a shiny app where a user can determine the optimal stock portofolio from a larger set of stocks.

We will continue with the project from group 17 in BAN420.
The project from BAN420 is doing the following:

#1. Download stock data from Yahoo Finance
#2. Create graph on stock development
#3. Create correlation plot
#4. Optimization (solver):
-Find portfolio with max sharp ratio
-Find portfolio with minimum variance
#5. Draw efficiency frontier & test portfolios against solver result
#6. Test best portfolio against S&P 500 return in same period

For BAN400 we are planning to do the following:

#1. Make a Shiny app where
- the user can give predetermined input
- the user get output on which portofolio is the optimal one based upon the chosen optimization method
- the user is getting error messages and warnings when something that can affect the validity of the output is happening.

#2. Improve the optmization
- more correlations than just linear correlation so that the user can switch between different optimization methods.
- adjust the existing optimization
- add error messages

#3. Improve the imput
- Broader input of stocks
- Always use the latest available data, and dynamically respond to changes in the data (stocks arrive or dissappear).
- Include classification of stocks (for instance to allow the user to exclude sin stocks or only include "green" stocks in a portofolio)

Generally:
- Make it a streamlined project on git:
- Make a readme file
- Have a master R. file that imports the different parts of the project.
- Focus on readability and structure in the code

