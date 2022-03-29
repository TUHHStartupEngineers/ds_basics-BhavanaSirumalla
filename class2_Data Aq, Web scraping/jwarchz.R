library(tidyverse)
library(jsonlite)
library(rvest)

url_home          <- "https://www.canyon.com/en-de"
html_home         <- read_html(url_home)

# 01. Get bike categories ----
bike_category_tbl <- html_home %>%
  
  # Get the nodes for the families ...
  html_nodes(css = ".is-bikeCategory .js-menuItemThirdLevel") |> 
  html_attr("href") |> 
  as_tibble() |> 
  rename("url" = value) |> 
  mutate(url = str_c("https://www.canyon.com", url))

# 02a. Get bike urls (single category)
bike_category_url <- bike_category_tbl$url[2]

html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  
  # Get the 'a' nodes, which are hierarchally underneath 
  # the class productTile__contentWrapper
  html_nodes(css = ".productTile__contentWrapper > a") %>%
  html_attr("href") %>%
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# 02b. Get bike description (single category)
bike_desc_tbl <- html_bike_category %>%
  
  # Get the nodes in the meta tag where the attribute itemprop equals description
  html_nodes('.productTile__productSummaryLeft > meta[itemprop="description"]') %>%
  
  # Extract the content of the attribute content
  html_attr("content") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "description")


# 02c. Get JSON data
bike_json_tbl  <- html_bike_category %>%
  
  html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
  html_attr("data-gtm-impression") %>%
  
  # Convert the JSON format to dataframe
  # map runs that function on each element of the list
  map(fromJSON) %>% # need JSON ### need lists
  
  # Extract relevant information of the nested list
  map(purrr::pluck, "ecommerce", "impressions") %>% # Need purrr and expl above
  
  # Set "not defined" and emtpy fields to NA (will be easier to work with)
  map(na_if, "not defined") %>%
  map(na_if, "") %>%
  
  # The class of dimension56 and price varies between numeric and char.
  # This converts this column in each list to numeric
  # across allows to perform the same operation on multiple columns
  map(~mutate(., across(c("dimension56","metric5","price"), as.numeric))) %>%
  
  # Stack all lists together
  bind_rows() %>%
  # Convert to tibble so that we have the same data format
  as_tibble() %>%
  
  # Add consecutive numbers so that we can bind all data together
  # You could have also just use bind_cols()
  rowid_to_column(var='position') %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)



# Function ----------------------------------------------------------------

get_bike_data <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".productTile__contentWrapper > a") %>%
    html_attr("href") %>%
    str_remove(pattern = "\\?.*") %>%
    enframe(name = "position", value = "url")
  
  # Get the descriptions
  bike_desc_tbl <- html_bike_category %>%
    html_nodes(css = '.productTile__productSummaryLeft > 
                      meta[itemprop="description"]') %>%
    html_attr("content") %>%
    enframe(name = "position", value = "description")
  
  # Get JSON data
  bike_json_tbl <- html_bike_category %>%
    html_nodes(css = '.productGrid__listItem.xlt-producttile > div') %>%
    html_attr("data-gtm-impression") %>%
    map(fromJSON) %>% # need JSON ### need lists
    map(purrr::pluck, 2, "impressions") %>% 
    map(na_if, "not defined") %>%
    map(na_if, "") %>%
    map(~mutate(., across(c("dimension56", "metric4","metric5","price"), as.numeric))) %>%
    bind_rows() %>%
    as_tibble() %>%
    rowid_to_column(var='position') %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
  
  
}

bike_category_tbl$url[2]
test <- get_bike_data(bike_category_tbl$url[2])


bike_category_url_vec <- bike_category_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)
bike_data_tbl <- bind_rows(bike_data_lst)


bike_data_tbl %>%
  group_by(id) %>%
  filter(n()>1) %>%
  arrange(id) %>% 
  View()



bike_data_cleaned_tbl <- bike_data_tbl %>%
  
  # Filter for bikes. Only unique ones
  filter(nchar(.$id) == 4) %>%
  filter(!(name %>% str_detect("Frameset"))) %>%
  distinct(id, .keep_all = T) |> 
  
  separate(col = category, into = c("category_1",
                                    "category_2",
                                    "category_3",
                                    "category_4"),
           sep = "(?<!\\s)/(?!\\s)") |> 
  
  
  # Renaming
  rename("year"       = "dimension50") %>%
  rename("model"      = "name") %>%
  rename("gender"     = "dimension63") %>%
  rename("price_euro" = "metric4") %>%
  
  # Fix years manually (have checked the website)
  mutate(year = replace_na(year, 2021)) %>%
  
  # Add frame material
  mutate(frame_material = case_when(
    model %>% str_detect(" CF ") ~ "carbon",
    model %>% str_detect(" CFR ") ~ "carbon",
    TRUE ~ "aluminium"
  )
  ) |> 
  
  # Select and order columns
  select(-c(position, brand, variant, starts_with("dim"), 
            quantity, feedProductId, price, metric5)) %>%
  select(id, model, year, frame_material, price_euro, everything())






url <- bike_data_cleaned_tbl$url[2]
bike_url_vec <- bike_data_cleaned_tbl %>% 
  pull(url)

get_colors <- function(url) {
  
  url %>%
    
    read_html() %>%
    
    # Get all 'script nodes' and convert to char
    html_nodes(css = "script") %>%
    as.character() %>%
    
    # Select the node, that contains 'window.deptsfra'
    str_subset(pattern = "window.deptsfra") %>%
    
    # remove the chars that do not belong to the json
    # 1. replace at the beginning everything until the first "{" with ""
    str_replace("^[^\\{]+", "") %>%
    # 2. replace at the end everything after the last "}" with ""
    str_replace("[^\\}]+$", "") %>%
    
    # Convert from json to an r object and pick the relevant values
    fromJSON() %>%
    purrr::pluck("productDetail", "variationAttributes", "values", 1, "value")
}

# Run the function over all urls and add result to bike_data_cleaned_tbl
# This will take a long time (~ 20-30 minutes) because we have to iterate over many bikes
library(furrr)     # Parallel Processing using purrr (iteration)
plan(multisession, workers = 8)
bike_data_colors_tbl <- bike_data_cleaned_tbl %>% 
  mutate(colors = future_map(bike_url_vec, get_colors))

saveRDS(bike_data_colors_tbl, "bike_data_colors_tbl.rds")



bike_data_colors_tbl %>%
  
  # Create entry for each color variation
  unnest(colors) |> 
  mutate(url_color = glue("{url}?dwvar_{id}_pv_rahmenfarbe={colors}")) %>%
  select(-url) |> 
  View()




library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt  <- fread(url)
covid_data_tbl <- read_csv(url) 

covid_data_dt[year == 2019, sum(cases), by = continentExp]
covid_data_tbl |> 
  filter(year == 2019) |> 
  group_by(continentExp) |> 
  summarise(sum(cases))

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]


covid_data_tbl |> 
  filter(countriesAndTerritories == "Germany", lubridate::month(dateRep, label = T, abbr = F) == "June")


covid_data_dt[countryterritoryCode == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]
covid_data_dt[, deaths_per_capita := deaths / popData2019]


covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]
covid_data_dt[, deaths_per_cases := NULL]


covid_data_dt[,date := lubridate::dmy(date)]

setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")
covid_data_dt[, deaths_per_capita := deaths / popData2019]


covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]



covid_data_dt[, deaths_per_cases := NULL]


covid_data_dt[,date := lubridate::dmy(date)]



covid_data_dt[country == "Germany" & month == 4, 
              .(m_cases = mean(cases), 
                m_death = mean(deaths)
              )
]


covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]


covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              .N
]
covid_data_dt[deaths > 1000, .N, by = country]

covid_data_tbl |> 
  filter(deaths > 1000) |> 
  count(countriesAndTerritories)
covid_data_dt[,.I[deaths > 1000]]


covid_data_dt[continent == "Europe",
              .(asd = mean(cases), asf = mean(deaths)),
              by = .(country, month, year)
]


covid_data_dt[, lapply(.SD, mean), by = year]



covid_data_dt[, lapply(.SD, sum), 
              by = .(year, month), 
              .SDcols = c("cases", "deaths")
]
