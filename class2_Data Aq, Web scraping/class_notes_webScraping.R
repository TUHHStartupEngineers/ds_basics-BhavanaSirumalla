install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp_500 <- url %>% 
  #read from the webpage
  read_html() %>% 
  html_nodes(css = "#constituents") %>% 
  #html_text()
  html_table() %>% 
#Extract first element of the resulting list
.[[1]]


#---------------------4 IMDB example
#4.1 assign URL
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
#4.2read html
html <- url %>% 
  read_html()

result <- html %>% 
  #1.eXtract nodes
  html_nodes(css = ".titleColumn") %>% 
  #2.Extract text
  html_text() %>% 
  #3.clean text
  stringr::str_extract("(?<= )[0-9]*(?=\\.\\n)")%>% 
  #Makes all values numeric
  as.numeric()

#TITLES
result_titles <- 
html %>% 
  html_nodes(css=".titleColumn a") %>% 
  html_text()

#RATINGS
rating <- html %>% 
  html_nodes(css = ".imdbRating > strong") %>% 
  html_text() %>% 
  as.numeric()

##nO of ratings

<strong title= "9.2 bases on 2,476,154 user ratings" class= ""> 9.2 </strong>
  
  result_ranks_number <- url %>% 
  read_html() %>% 
  html_nodes(css=".ratingColumn > strong") %>% 
  html_attr("title") %>% 
  stringr::str_extract("(?<=based on ).*(?=\ user ratings)" ) %>% 
  readr::parse_number()

imdb_tbl <- tibble( result_titles, rating, result_ranks_number)









