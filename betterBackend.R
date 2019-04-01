library(tidyverse)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster') %>% 
  select(section, pawsId, fullname, sectionName) %>% collect()
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect() %>% mutate(eNum = substr(label, 2, 2))
assignments <- tbl(conDatasource,'exchunk') %>% filter(mainTopic == 'excel') %>% collect()
attendance <- tbl(conDatasource, 'vwStudentDate') %>% filter(!is.na(eventDate)) %>% collect()
dates <- tbl(conDatasource, 'attevent') %>% select(eventId, eventDate) %>% 
  collect() %>% filter(eventDate < Sys.Date()) %>%
  mutate(section = paste0("Section ", substr(eventId, 7, 7)))

# limit selection to Spring 2019
nameTable <- nameTable %>% filter(substr(section, 1, 6) == "2019SP")
dates <- dates %>% filter(substr(eventId, 1, 6) == "2019SP")
big <- big %>% filter(substr(section, 1, 6) == "2019SP")

# Inputs
## Students by Semester
semesterVector <- as.list(distinct(nameTable, substr(section, 1, 6)))
semesterVector <- semesterVector$`substr(section, 1, 6)`
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Outputs
## Submissions
colnames(assignments)[1] <- 'label'
## Attendance (NA -> 0)
big[is.na(big)] <- 0

