library(rvest)
library(tidyverse)
library(jsonlite)
library(httr)
library(dplyr) 

url_link <- "https://www.radon-bikes.de/en/e-bike/mountainbike/bikegrid/"
ebike_page         <- read_html(url_link)

#price <- ebike_page %>% html_nodes(".m-bikegrid__price--active , :nth-child(3) .m-bikegrid__info .a-heading--small") %>% html_text()

price <- ebike_page %>% html_nodes(".m-bikegrid__price--active") %>% html_text()
model_name <- ebike_page %>% html_nodes(".m-bikegrid__info .a-heading--small")%>% html_text()

details<- data.frame(price,model_name)
view(details)



#testing from below
url_link <- "https://www.radon-bikes.de/en/"
home_page <- read_html(url_link)

