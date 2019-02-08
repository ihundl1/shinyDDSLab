library(shiny)
library(shinydashboard)
library(RColorBrewer)

source('backend.R')

sectionSelect <- setNames(sectionVector, sectionVector)
studentSelect <- setNames(nameTable$pawsId, nameTable$fullname)

ui <- dashboardPage(
  # App Title
  dashboardHeader(title = "Shiny Dashboard"),

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
      valueBox(12, "Attendace", icon = icon("chalkboard-teacher"), width = 6)
    ),
    fluidRow(
      box(plotOutput("submissions", height = 200), width = 12)
    ),
    fluidRow(
      box(plotOutput("noSubs", height = 200), width = 12)
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
  
  # Update data
  studentSubs <- reactive(filter(subs, pawsId == input$student) %>% select(label, submissions, bestScore))
  
  # generate plots
  output$submissions <- renderPlot({
    ggplot(studentSubs(), aes(x = label, y = bestScore)) +
      geom_col(fill = "#FF9999") + 
      geom_text(aes(label = submissions), position = position_stack(vjust = .5), size = 10) + 
      theme_minimal() + ylab("Best Score") + xlab("Chunk") +
      theme(panel.grid.major.x = element_blank())
  })
  output$noSubs <- renderPlot({
    filter(assignments, !(chunkId %in% studentSubs()$label)) %>% arrange(chunkId) %>%
      ggplot(aes(x = st2, fill = mainTopic)) + geom_bar() + xlab("Topic") + 
      ylab("Assignments Left") + theme_minimal() + 
      scale_fill_manual(values = brewer.pal(3, "Accent")) + 
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui = ui, server = server)