library("magrittr")
data <- iris

data <- head(data, n=3)

data <- iris %>% head(data, n=3)

str(data)

typeof(data)
?filter


library("readxl")

bikes <- read_excel("00_data/bikes.xlsx")
vehicles <- as_tibble(cars[1:5,])

