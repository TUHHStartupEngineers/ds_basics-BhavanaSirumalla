#1.Loading libraries
library(tidyverse)
library(readxl)

#2.Reading excel files
#bikes <- read_excel("00_data/bikes.xlsx")


#3installation and loading of the package/databases
install.packages("RSQLite")
library(RSQLite)

#bikes <- read_excel("00_data/bikes.xlsx")

#3.1 Connection to the database
con <- dbConnect(drv = SQLite(),
                 dbname= "00_data/Chinook_Sqlite.sqlite")

#3.2to list the tables
dbListTables(con)

#3.3Get first 10 rows of a table
tbl(con, "Album")

#3.4 Get entire content of a table
album_tb <- tbl(con, "Album") %>% collect()

artist_tb <- tbl(con, "Artist") %>% collect()

#3.5 joing 2 tables and arrange"-n" is descending order
artist_tb %>% 
  left_join(album_tb) %>% 
  count(Name) %>% 
  arrange(-n)






