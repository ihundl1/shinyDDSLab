library(shiny)

source('backend.R')

ui <- fluidPage(
  # App Title
  titlePanel("Shiny Dashboard"),
  
  # Sidebar for Inputs
  sidebarLayout(
    
    # Panel for inputs
    sidebarPanel(
      # Inputs
      selectInput("student1", "Section 1", sec1, selected = "NA"),
      selectInput("student2", "Section 2", sec2, selected = "NA"),
      selectInput("student3", "Section 3", sec3, selected = "NA"),
      selectInput("student4", "Section 4", sec4, selected = "NA"),
      selectInput("student5", "Section 5", sec5, selected = "NA"),
      selectInput("student6", "Section 6", sec6, selected = "NA"),
      selectInput("student8", "Section 8", sec8, selected = "NA"),
      width = 3
    ),
    
    # Panel for outputs
    mainPanel(
      # create a row
      fluidRow(
        column(4, "Name & Section"),
        column(4, "Value Box 1"),
        column(4, "Value Box 2")
      ),
      fluidRow(
        plotOutput("submissions")
      ),
      fluidRow(
        "Assignments not Submitted"
      )
    )
  )
)

server <- function(input, output) {
  # create a reactive value to bridge inputs & outputs
  rv <- reactiveValues(key = 'NA')
  
  # observeEvent will trigger any time the value changes, update key
  observeEvent(input$student1, rv$key <- input$student1)
  observeEvent(input$student2, rv$key <- input$student2)
  observeEvent(input$student3, rv$key <- input$student3)
  observeEvent(input$student4, rv$key <- input$student4)
  observeEvent(input$student5, rv$key <- input$student5)
  observeEvent(input$student6, rv$key <- input$student6)
  observeEvent(input$student8, rv$key <- input$student8)
  
  # Update data
  studentSubs <- reactive(filter(subs, pawsId == rv$key) %>% select(label, submissions, bestScore))
  
  # generate plot
  output$submissions <- renderPlot({
    ggplot(studentSubs(), aes(x = label, y = bestScore)) +
      geom_col(fill = "#FF9999") + 
      geom_text(aes(label = submissions), position = position_stack(vjust = .5), size = 10) + 
      theme_minimal() + ylab("Best Score") + xlab("Chunk") +
      theme(panel.grid.major.x = element_blank())
  })
}

shinyApp(ui = ui, server = server)