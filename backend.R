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
splitStudents <- split(nameTable, nameTable$sectionName)

sec1 <- setNames(splitStudents$`Section 1`$pawsId, splitStudents$`Section 1`$fullname) %>% as.list()
sec1["Select a Student"] <- "NA"
sec2 <- setNames(splitStudents$`Section 2`$pawsId, splitStudents$`Section 2`$fullname) %>% as.list()
sec2["Select a Student"] <- "NA"
sec3 <- setNames(splitStudents$`Section 3`$pawsId, splitStudents$`Section 3`$fullname) %>% as.list()
sec3["Select a Student"] <- "NA"
sec4 <- setNames(splitStudents$`Section 4`$pawsId, splitStudents$`Section 4`$fullname) %>% as.list()
sec4["Select a Student"] <- "NA"
sec5 <- setNames(splitStudents$`Section 5`$pawsId, splitStudents$`Section 5`$fullname) %>% as.list()
sec5["Select a Student"] <- "NA"
sec6 <- setNames(splitStudents$`Section 6`$pawsId, splitStudents$`Section 6`$fullname) %>% as.list()
sec6["Select a Student"] <- "NA"
sec8 <- setNames(splitStudents$`Section 8`$pawsId, splitStudents$`Section 8`$fullname) %>% as.list()
sec8["Select a Student"] <- "NA"

# Attandance
course <- classes %>% collect() %>% mutate(sectionId = substr(eventId, 1, 7)) %>%
  group_by(sectionId, eventTopic) %>% summarise(classTotal = n())

attend <- attendance %>% left_join(classes, by = c('att_event' = 'eventId')) %>% collect() %>%
  group_by(pawsId, eventTopic) %>% summarise(att = n())

big <- sections %>% filter(delivery == "inclass") %>% select(sectionId, instructor) %>% 
  left_join(roster, by = c('sectionId' = 'section')) %>% collect() %>% 
  left_join(attend, by = 'pawsId') %>% left_join(course, by = c('sectionId', 'eventTopic')) %>%
  mutate(attPerc = att / classTotal) %>% mutate(missed = classTotal - att)

lecTotal <- big %>% filter(eventTopic == "lecture")
pracTotal <- big %>% filter(eventTopic == "practice")

# Submissions
subs <- submission %>% filter(pawsId != "") %>% left_join(chunk, by = c('label' = 'chunkId')) %>% 
  group_by(pawsId, label, mainTopic, subTopic) %>% 
  summarise(submissions = n(), bestScore = max(totalscore, na.rm = TRUE)) %>% collect() %>% 
  as.data.frame()

assignments <- chunk %>% collect()

levels <- distinct(assignments, subTopic)
levels <- as.vector(levels$subTopic)
assignments$st2 <- factor(assignments$subTopic, levels = levels)
