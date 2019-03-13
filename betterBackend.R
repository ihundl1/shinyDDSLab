library(tidyverse)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster') %>% select(pawsId, fullname, sectionName) %>% collect()
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect() %>% mutate(eNum = substr(label, 2, 2))
assignments <- tbl(conDatasource,'exchunk') %>% filter(mainTopic == 'excel') %>% collect()
attendance <- tbl(conDatasource, 'vwStudentDate') %>% filter(!is.na(eventDate)) %>% collect()
dates <- tbl(conDatasource, 'attevent') %>% select(eventId, eventDate) %>% collect() %>%
  mutate(section = paste0("Section ", substr(eventId, 7, 7)))

# Inputs
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Outputs
## Submissions
colnames(assignments)[1] <- 'label'

## Attendance (NA -> 0)
big[is.na(big)] <- 0

