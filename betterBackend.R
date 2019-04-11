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
avgBestScore <- tbl(conDatasource, 'vwAvgBestScore') %>% collect()

# limit selection to Spring 2019
nameTable <- nameTable %>% filter(substr(section, 1, 6) == "2019SP")
dates <- dates %>% filter(substr(eventId, 1, 6) == "2019SP")
big <- big %>% filter(substr(section, 1, 6) == "2019SP")
avgBestScore <- avgBestScore %>% filter(substr(section, 1, 6) == "2019SP")
subs <- subs %>% filter(mainTopic == "excel")

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
subs$bestScore <- as.double(subs$bestScore)

subNames <- subs %>% left_join(nameTable, by= "pawsId")
secSubs <- subNames %>% group_by(section, label) %>% summarise(classSubs = sum(submissions))
enrollment <- nameTable %>% group_by(section) %>% summarise(enrolled = n())
avgSubs <- left_join(secSubs, enrollment, by="section") %>% mutate(classAvgSubs = classSubs/enrolled) %>%
  filter(!is.na(section))
classAvg <- left_join(avgSubs, avgBestScore, by=c("section", "label")) %>% 
  select(section, label, classAvgSubs, classAvgBestScore) %>% mutate(eNum = substr(label, 2, 2)) %>%
  select(label, classAvgSubs, classAvgBestScore, eNum, section)
classAvg <- as.data.frame(classAvg) %>% mutate(classAvgSubs = round(classAvgSubs, 2), classAvgBestScore = round(classAvgBestScore, 2))
colnames(classAvg) <- c("label", "submissions", "bestScore", "eNum", "type")
## Attendance (NA -> 0)
big[is.na(big)] <- 0

