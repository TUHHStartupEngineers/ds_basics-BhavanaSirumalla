#2. PART: API
install.packages("httr")
library(httr)

url <- "https://swapi.dev/api/people/1/"

resp <- GET(url)

#to see the content
resp$content

#to convert raw data to char
cont_raw <- resp$content
cont_char <- rawToChar(cont_raw)

#convert Char to JSON

library(jsonlite)
cont_json <- fromJSON(cont_char)

cont_json <- content(resp)


#3.  API Alphavantage
library(glue)
library(httr)
# 3.1 url is from documentation: https://www.alphavantage.co/documentation/

url_demo <- "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=IBM&apikey="
#API token from alphavantage
token <- "DC0V99X2Z90A95BL"

#both are same except we paste the url and token together, "paste" adds space and "Paste0" doesnt add any space

#url_with_token <- paste0(url,token)

#Can also be done like this

#url_with_str <- str_c(url,token)


#3.2 Splitting URL into parts
base_url <- "https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol="
symbol <- "IBM"
api_key <- "&apikey="
#token <- "DC0V99X2Z90A95BL"
token <- Sys.getenv("token")
#example how glue concatenates
#glue("djfsjf{symbol}")
url_merged <- glue("{base_url}{symbol}{api_key}{token}")
resp <- GET(url_merged)
cont <- content(resp)

#Get data from the content

ticker <- cont[["Global Quote"]][["01. symbol"]]
price <- cont[["Global Quote"]][["05. price"]]
date <- cont[["Global Quote"]][["07. latest trading day"]]

get_quote <- function(symbol) {
  
  #1/GET content from the API
  url_merged <- glue("{base_url}{symbol}{api_key}{token}")
  resp <- GET(url_merged)
  cont <- content(resp)
  
  #2.Extract relevant data from the content
  ticker <- cont[["Global Quote"]][["01. symbol"]]
  price <- cont[["Global Quote"]][["05. price"]]
  date <- cont[["Global Quote"]][["07. latest trading day"]]
  
  #3.MERGE relevant data
  merged_data <- glue("STOCK: {ticker},
                      PRICE: {price},
                      DATE: {date}")
  
  #4.PRINT relevant data
  print(merged_data)
}

ibm_data <- get_quote(symbol ="IBM")
















