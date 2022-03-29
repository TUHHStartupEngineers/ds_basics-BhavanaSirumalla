# install.packages("plotly")

library(tidyverse)
library(tidyquant)
library(broom)
library(umap)

# STOCK PRICES
sp_500_prices_tbl <- read_rds("sp_500_prices_tbl.rds")
sp_500_prices_tbl

# SECTOR INFORMATION
sp_500_index_tbl <- read_rds("sp_500_index_tbl.rds")
sp_500_index_tbl

sp_500_prices_tbl %>% glimpse()


sp_500_daily_returns_tbl <- sp_500_prices_tbl %>%
select(symbol, date, adjusted) %>% filter(year(date) >= 2018)%>% group_by(symbol)%>%
#mutate(pct_return = lag(adjusted)) 
mutate(lag=lag(adjusted))%>% na.omit(.)%>%mutate(difference=adjusted-lag)%>%
select(symbol,date,pct_return)
  
#+remove_rownames(var='NA')
#discard(pct_return='NA')
  