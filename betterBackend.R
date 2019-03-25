library(tidyverse)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster')
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect() %>% mutate(eNum = substr(label, 2, 2))
assignments <- tbl(conDatasource,'exchunk') %>% filter(mainTopic == 'excel') %>% collect()
attendance <- tbl(conDatasource, 'vwStudentDate') %>% filter(!is.na(eventDate)) %>% collect()
dates <- tbl(conDatasource, 'attevent') %>% select(eventId, eventDate) %>% collect() %>%
  mutate(section = paste0("Section ", substr(eventId, 7, 7)))

# Fix for current semester
nameTable <- nameTable %>% filter(substr(section, 1, 6) == "2019SP") %>% 
  select(pawsId, fullname, sectionName) %>% collect()
big <- big %>% filter(substr(section, 1, 6) == "2019SP")
dates <- dates %>% filter(substr(eventId, 1, 6) == "2019SP")

# Inputs
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Outputs
## Submissions
colnames(assignments)[1] <- 'label'

## Attendance (NA -> 0)
big[is.na(big)] <- 0

