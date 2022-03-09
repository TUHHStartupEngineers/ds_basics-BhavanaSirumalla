library(tidyverse)
library(rvest)
url_home          <- "https://www.canyon.com/en-de"
html_home <- url_home %>% 
  read_html(url_home )
#STEP 1 Retrieving all bike category URLs
bike_category_tbl <- html_home %>% 

  #Get the nodes for the families
  html_nodes(css = ".is-bikeCategory .js-menuItemThirdLevel") %>% 
  html_attr("href") %>% 
  as_tibble() %>% 
  rename("url"=value) %>% 
  mutate(url= str_c("https://www.canyon.com",url))

#2.STEP .retrieve information about single bikes

bike_category_url <- bike_category_tbl$url[2]
html_bike_category <- read_html(bike_category_url)
bike_url_tbl <- html_bike_category %>% 
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

####