library(magrittr)
library(rvest)

url.renewable <- "https://en.wikipedia.org/wiki/List_of_renewable_energy_companies_by_stock_exchange"
renewable.stocks <- read_html(url.renewable, as.data.frame = T, stringsAsFactors = T)
renewable.stocks <- renewable.stocks %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill = T)

renewable.stocks <- separate(renewable.stocks, col = Symbol, into = c("Bank","Symbol"), sep = "\\:") 
renewable.stocks <- separate(renewable.stocks, col = Symbol, into = c("Symbol","x3"), sep = "[[:punct:]]") 
renewable.stocks <- subset(renewable.stocks, select = -c(x3,IPO))
renewable.stocks$Symbol[which(is.na(renewable.stocks$Symbol))] <- "EGPW"
                               