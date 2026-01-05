library(googlesheets4)
library(tidyverse)
source('config.R')

omp <- read_sheet(one_minute_paper_results)

omp_by_student <- table(omp$`Your name:`) %>% as.data.frame()
write.csv(omp_by_student, file = 'private/One_Minute_Papers.csv', row.names = FALSE)
