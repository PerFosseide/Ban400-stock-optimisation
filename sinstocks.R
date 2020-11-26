

library(rvest)

simple <- read_html("https://sinstocksreport.com/list-of-sin-stocks/")
simple <- simple %>% 
  html_nodes("li") %>% 
  html_text()

sin.stocks <- simple[14:149]
sin.stocks <- as.data.frame(sin.stocks)

sin.stocks <- separate(sin.stocks, col = sin.stocks, into = c("Name","x2"), sep = "\\(") 
sin.stocks <- separate(sin.stocks, col = x2, into = c("x2", "Symbol"), sep = "\\:") 
sin.stocks <- separate(sin.stocks, col = Symbol, into = c("Symbol","x4"), sep = "\\)") 
sin.stocks <- subset(sin.stocks, select = -c(x2, x4))
