library(tidyverse)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster') %>% select(pawsId, fullname, sectionName) %>% collect()
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect()
assignments <- tbl(conDatasource,'exchunk') %>% filter(mainTopic == 'excel') %>% collect()
attendance <- tbl(conDatasource, 'vwStudentDate') %>% collect()
dates <- tbl(conDatasource, 'attevent') %>% select(eventId, eventDate) %>% collect() %>%
  mutate(section = substr(eventId, 1, 7))

# Inputs
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Outputs
## Submissions
colnames(assignments)[1] <- 'label'

## Attendance (NA -> 0)
big[is.na(big)] <- 0

