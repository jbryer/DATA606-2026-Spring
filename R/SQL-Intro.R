library(RSQLite)
library(RMySQL)
library(DBI)

# my_data <- read.csv("")


# db <- DBI::dbConnect(RMySQL::)
db <- DBI::dbConnect(RSQLite::SQLite(), 'mydatabase.sqlite')

DBI::dbListTables(db)

data(mtcars)

DBI::dbWriteTable(db, 'mtcars', mtcars)
DBI::dbReadTable(db, 'mtcars')

DBI::dbSendQuery(db, 'SELECT * FROM mtcars')
