library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(scales)

source('betterBackend.R')

sectionSelect <- setNames(sectionVector, sectionVector)
studentSelect <- setNames(nameTable$pawsId, nameTable$fullname)
exSelect <- setNames(exVector, exVector)

ui <- dashboardPage(
  # App Title
  dashboardHeader(title = dashTitle, titleWidth = 450),

  # Sidebar for Inputs
  dashboardSidebar(
    disable = TRUE
  ),
  
  # Panel for outputs
  dashboardBody(
    # create a row
    fluidRow(
      box(selectInput("section", "Select a Section", c(" " = "", sectionSelect)), width = 3),
      box(selectInput("student", "Select a Student", c(" " = "", studentSelect)), width = 3),
      box(selectInput("ex", "Select an Exercise", c(" " = "", exSelect)), width = 3),
      valueBoxOutput("attValue", width = 3)
    ),
    fluidRow(
      box(plotOutput("submissions", height = 200), width = 12)
    ),
    fluidRow(
      box(plotOutput("attChart", height = 175), width = 12)
    )
  )
)

server <- function(session, input, output) {
  # limit student selection
  limitStudents <- reactive({
    lessStudents <- filter(nameTable, sectionName == input$section) %>% select(-sectionName)
    newSelect <- setNames(lessStudents$pawsId, lessStudents$fullname)
    return(c(" " = "", newSelect))
  })
  
  observeEvent(input$section,
    updateSelectInput(session, "student", "Select a Student", c(" " = "", limitStudents())))
  
  # Pull Attendance Value Box info
  attPerc <- reactive(ifelse(input$student %in% big$pawsId,
                            filter(big, pawsId == input$student) %>% select(attPerc) %>% as.double(),
                            0))
  attWarning <- reactive(ifelse(
    input$student %in% big$pawsId, ifelse(
      attPerc() >= .75, "blue", ifelse(
        attPerc() >= .5, "orange", "red"
      )
    ),
    "red"
  ))
  attTotal <- reactive(ifelse(input$student %in% big$pawsId,
                       filter(big, pawsId == input$student) %>% select(classTotal) %>% as.character(),
                       "0"))
  
  # Update & Generate Submissions Tables
  ## Student submissions table
  studentSubs <- reactive(filter(subs, pawsId == input$student) %>% 
                            select(label, submissions, bestScore, eNum) %>% mutate(type = "student"))
  ## Assignments that the student didn't submit
  emptyAssignments <- reactive(filter(assignments, !(label %in% studentSubs()$label)) %>%
                                 mutate(submissions = 0, bestScore = as.integer(0)) %>% 
                                 mutate(type = "student") %>%
                                 select(label, submissions, bestScore, eNum, type))
  ## Filter class averages for specific section
  sectionAverages <- reactive(filter(classAvg, type == as.character(
    filter(nameTable, pawsId == input$student) %>% select(section)
  )))
  ## Reactive function to determine x-axis limit of chart
  maxEx <- reactive(ifelse(input$ex == "", max(studentSubs()$eNum), input$ex))
  ## Bind student, missing, & section tables & filter by maxEx
  fullSubs <- reactive(rbind(studentSubs(), emptyAssignments(), sectionAverages()) %>% 
    filter(eNum <= maxEx()) %>% mutate(labelY = case_when(
      type == "student" ~ bestScore/1.25,
      TRUE ~ bestScore/2
    )))
  
  # Generate attendance table for chart
  studentSectionAtt <- reactive(filter(classAtt, sectionName == input$section))
  studentAttendance <- reactive(filter(attendance, pawsId == input$student))
  dateValues <- reactive(studentSectionAtt() %>% 
                mutate(attended = ifelse(eventDate %in% studentAttendance()$eventDate, "Yes", "No")))
    
  # generate value box
  output$attValue <- renderValueBox({
    valueBox(percent(attPerc(), accuracy=1), paste0("of ", attTotal(), " classes attended"), 
             color = attWarning())
  })
  
  # generate plots
  output$submissions <- renderPlot({
    ggplot(fullSubs(), aes(x = label, y = bestScore, fill = type)) +
      geom_col(position = "dodge") + 
      geom_text(aes(y = labelY, label = submissions), position = position_dodge(width = 0.9), 
                size = 6) + 
      theme_minimal() + ylab("Best Score") + xlab("Chunk") +
      theme(panel.grid.major.x = element_blank()) +
      scale_fill_brewer(palette = "Pastel1")
  })
  output$attChart <- renderPlot({
    ggplot(dateValues(), aes(x = eventDate, y = classAttendance, fill = attended)) + 
      geom_col() + theme_minimal() + 
      ylab("Attendance") + xlab("Class Date") + labs(fill = "Student Attended") +
      theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Paired")
  })
}

shinyApp(ui = ui, server = server)

