library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(scales)

source('betterBackend.R')

sectionSelect <- setNames(sectionVector, sectionVector)
studentSelect <- setNames(nameTable$pawsId, nameTable$fullname)

ui <- dashboardPage(
  # App Title
  dashboardHeader(title = "ISDS 1102 Instructor Dashboard"),

  # Sidebar for Inputs
  dashboardSidebar(
    disable = TRUE
  ),
  
  # Panel for outputs
  dashboardBody(
    # create a row
    fluidRow(
      box(selectInput("section", "Select a Section", c(" " = "", sectionSelect)), width = 4),
      box(selectInput("student", "Select a Student", c(" " = "", studentSelect)), width = 4),
      valueBoxOutput("attValue", width = 4)
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
  studentSubs <- reactive(filter(subs, pawsId == input$student) %>% 
                            select(label, submissions, bestScore, eNum))
  emptyAssignments <- reactive(filter(assignments, !(label %in% studentSubs()$label)) %>%
                                 mutate(submissions = 0, bestScore = as.integer(0)) %>%
                                 mutate(eNum = substr(label, 2, 2)) %>%
                                 select(label, submissions, bestScore, eNum))
  fullSubs <- reactive(rbind(studentSubs(), emptyAssignments()) %>% 
    filter(eNum <= max(studentSubs()$eNum)))
  
  # Generate attendance table for chart
  studentAttendance <- reactive(filter(attendance, pawsId == input$student))
  dateValues <- reactive(filter(dates, section == input$section) %>% 
                           mutate(attended = ifelse(eventDate %in% studentAttendance()$eventDate, 1, 0)))
  
  # generate value box
  output$attValue <- renderValueBox({
    valueBox(percent(attPerc(), accuracy=1), paste0("of ", attTotal(), " classes attended"), 
             color = attWarning())
  })
  
  # generate plots
  output$submissions <- renderPlot({
    ggplot(fullSubs(), aes(x = label, y = bestScore)) +
      geom_col(fill = "#AED6F1") + 
      geom_text(data = filter(fullSubs(), submissions > 0),
                aes(label = submissions), position = position_stack(vjust = .5), size = 10) + 
      theme_minimal() + ylab("Best Score") + xlab("Chunk") +
      theme(panel.grid.major.x = element_blank())
  })
  output$attChart <- renderPlot({
    ggplot(dateValues(), aes(x = eventDate, y = attended)) + 
      geom_col(fill = attWarning()) + theme_minimal() + 
      ylab("Attended") + xlab("Class Date") +
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui = ui, server = server)

