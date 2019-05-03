library(tidyverse)
library(lubridate)

source('connection.R')

#import data
nameTable <- tbl(conDatasource, 'vwRoster') %>% 
  select(section, pawsId, fullname, sectionName) %>% collect()
big <- tbl(conDatasource, 'vwBig') %>% collect()
subs <- tbl(conDatasource, 'vwSubs') %>% collect() %>% mutate(eNum = substr(label, 2, 2))
assignments <- tbl(conDatasource,'exchunk') %>% filter(mainTopic == 'excel') %>% collect() %>%
  mutate(eNum = substr(chunkId, 2, 2))
attendance <- tbl(conDatasource, 'vwStudentDate') %>% filter(!is.na(eventDate)) %>% collect()
avgBestScore <- tbl(conDatasource, 'vwAvgBestScore') %>% collect()

# limit selection to current semester
## Variable for current semester (yyyyss where Spring = SP and Fall = FA)
currentSemester <- "2019SP"
## Filter tables
nameTable <- nameTable %>% filter(substr(section, 1, 6) == currentSemester)
big <- big %>% filter(substr(section, 1, 6) == currentSemester)
avgBestScore <- avgBestScore %>% filter(substr(section, 1, 6) == currentSemester)
## Filter out any word exercises from subs, change this if there are exercises other than excel
subs <- subs %>% filter(mainTopic == "excel")

# Inputs
## Students by Section
sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName
## Exercise Selection
exVector <- as.list(distinct(assignments, eNum))
exVector <- exVector$eNum

# Outputs
## Submissions
colnames(assignments)[1] <- 'label'
subs$bestScore <- as.double(subs$bestScore)

subNames <- subs %>% left_join(nameTable, by= "pawsId")
secSubs <- subNames %>% group_by(section, label) %>% summarise(classSubs = sum(submissions))
enrollment <- nameTable %>% group_by(section) %>% summarise(enrolled = n())
avgSubs <- left_join(secSubs, enrollment, by="section") %>% mutate(classAvgSubs = round(classSubs/enrolled, 1)) %>%
  filter(!is.na(section))
classAvg <- left_join(avgSubs, avgBestScore, by=c("section", "label")) %>% 
  select(section, label, classAvgSubs, classAvgBestScore) %>% mutate(eNum = substr(label, 2, 2)) %>%
  select(label, classAvgSubs, classAvgBestScore, eNum, section)
classAvg <- as.data.frame(classAvg) %>% mutate(classAvgSubs = round(classAvgSubs, 2), classAvgBestScore = round(classAvgBestScore, 2))
colnames(classAvg) <- c("label", "submissions", "bestScore", "eNum", "type")
## Attendance (NA -> 0)
big[is.na(big)] <- 0
## Attendance by Class
classAtt <- left_join(nameTable, attendance, by="pawsId") %>% group_by(section, sectionName, eventDate) %>%
  summarise(classAttendance = n()) %>% filter(year(eventDate) == substr(currentSemester, 1, 4))

# Dashboard Title
sem <- distinct(nameTable, substr(section, 1, 6)) %>% as.character()
semYear <- substr(sem, 1, 4)
semSeason <- substr(sem, 5, 6)
semSeason <- ifelse(semSeason == "SP", "Spring", "Fall")
dashTitle <- paste("ISDS 1102 Instructor Dashboard", semSeason, semYear)
