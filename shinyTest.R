library(shiny)
library(shinydashboard)
library(RColorBrewer)

source('betterBackend.R')

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
      box(selectInput("student", "Select a Student", c(" " = "", studentSelect)), width = 8),
      valueBoxOutput("attendance", width = 4)
    ),
    fluidRow(
      box(plotOutput("submissions", height = 200), width = 12)
    ),
    fluidRow(
      box(plotOutput("noSubs", height = 200), width = 12)
    )
  )
)

server <- function(input, output) {
  # Update data
  studentSubs <- reactive(filter(subs, pawsId == input$student) %>% select(label, submissions, bestScore))
  
  # Pull Attendance Info
  attValue <- reactive(ifelse(input$student %in% big$pawsId, 
                              filter(big, pawsId == input$student) %>% select(missed) %>% as.integer(),
                              "All"))
  attPerc <- reactive(filter(big, pawsId == input$student) %>% select(attPerc) %>% as.double())
  attWarning <- reactive(ifelse(
    input$student %in% big$pawsId, ifelse(
      attPerc() >= .75, "blue", ifelse(
        attPerc() >= .5, "orange", "red"
      )
    ),
    "red"
  ))
  
  # generate value box
  output$attendance <- renderValueBox({
    valueBox(attValue(), "Classes Missed", color = attWarning())
  })
  
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