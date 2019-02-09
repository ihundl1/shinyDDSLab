library(tidyverse)

source('connection.R')

# import data
roster <- tbl(conDatasource, 'student_user')
attendance <- tbl(conDatasource, 'attendance')
classes <- tbl(conDatasource, 'attevent')
sections <- tbl(conDatasource, 'section')
chunk <- tbl(conDatasource, 'exchunk')
submission <- tbl(conDatasource, 'exsubmission')

# Inputs
## Students by Section
nameTable <- left_join(roster, sections, by = c('section' = 'sectionId')) %>% 
  distinct(pawsId, lastname, firstname, section) %>% 
  arrange(lastname) %>% collect() %>%
  mutate(sectionName = paste("Section", substr(section, 7, 7))) %>% 
  mutate(fullname = paste0(lastname, ", ", firstname)) %>% select(pawsId, fullname, sectionName) %>%
  arrange(sectionName, fullname)

sectionVector <- as.list(distinct(nameTable, sectionName))
sectionVector <- sectionVector$sectionName

# Attendance
course <- classes %>% collect() %>% mutate(sectionId = substr(eventId, 1, 7)) %>%
  group_by(sectionId) %>% summarise(classTotal = n())

# Double check the attendance table
# section & att_event duplicated, half empty
attend <- attendance %>% filter(att_event == "") %>%
  left_join(classes, by = c('section' = 'eventId')) %>% collect() %>%
  group_by(pawsId) %>% summarise(att = n())

big <- sections %>% filter(delivery == "inclass") %>% select(sectionId, instructor) %>% 
  left_join(roster, by = c('sectionId' = 'section')) %>% collect() %>% 
  left_join(attend, by = 'pawsId') %>% left_join(course, by = 'sectionId') %>%
  mutate(attPerc = att / classTotal) %>% mutate(missed = classTotal - att)

# Submissions
subs <- submission %>% filter(pawsId != "") %>% left_join(chunk, by = c('label' = 'chunkId')) %>% 
  group_by(pawsId, label, mainTopic, subTopic) %>% 
  summarise(submissions = n(), bestScore = max(totalscore, na.rm = TRUE)) %>% collect() %>% 
  as.data.frame()

assignments <- chunk %>% collect()

levels <- distinct(assignments, subTopic)
levels <- as.vector(levels$subTopic)
assignments$st2 <- factor(assignments$subTopic, levels = levels)

