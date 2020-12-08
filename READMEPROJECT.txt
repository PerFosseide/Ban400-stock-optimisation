  
########## README PROJECT ##########

In this project we create a shiny app where the user has the opportunity to
determine the optimal stock portfolio form a large set of stocks. The user 
can choose to exclude sin stocks, or to only include green stocks.

The set of stocks is collected form Yahoo Finance. The lists of sin stocks 
and green stocks are collected manually from different sources, before they
are loaded into R as data frames. These data frames can be used for including
or decluding certain shares in the shiny app. 

The shiny app has three different tabs. One for selecting stocks, one for 
selecting the optimalization, and one to see the results represented in 
graphs and charts.

In the project, we have chosen to have one master R. file and several sub 
files for functions and the shiny app script. 