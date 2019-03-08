library(tidyverse)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster') %>% select(pawsId, fullname, sectionName) %>% collect()
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect()
assignments <- tbl(conDatasource,'exchunk') %>% collect()

# Inputs
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Outputs
## Submissions
levels <- distinct(assignments, subTopic)
levels <- as.vector(levels$subTopic)
assignments$st2 <- factor(assignments$subTopic, levels = levels)

## Attendance (NA -> 0)
big[is.na(big)] <- 0
