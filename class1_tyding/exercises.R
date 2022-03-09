
library("readxl")
library("readr")
library(tidyverse)
bikes <- read_excel("00_data/bikes.xlsx")
write_rds(bikes,file="00_data/bikesAgain.rds")

#next case
#never have numbers as column names

diamonds2 <- readRDS("00_data/diamonds2.rds")
view(diamonds2)

data2 <- diamonds2 %>%
  pivot_longer(cols = c("2008", "2009"),
               names_to = 'year',
               values_to = 'price')


model <- lm(price ~ . , data = data2)
summary(model)

data3 <- diamonds3 %>% 
  pivot_wider(names_from  = "dimension",
              values_from = "measurement")

diamonds4 <- readRDS("00_data/diamonds4.rds")
diamonds4 %>% 
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = T)
view(diamonds4)
typeof("cut")

diamonds5 <- readRDS("00_data/diamonds5.rds")

data5 <- diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')
#no separator




