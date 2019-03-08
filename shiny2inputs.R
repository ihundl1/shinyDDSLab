library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(scales)

source('betterBackend.R')

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
      box(selectInput("section", "Select a Section", c(" " = "", sectionSelect)), width = 4),
      box(selectInput("student", "Select a Student", c(" " = "", studentSelect)), width = 4),
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

server <- function(session, input, output) {
  # limit student selection
  limitStudents <- reactive({
    lessStudents <- filter(nameTable, sectionName == input$section) %>% select(-sectionName)
    newSelect <- setNames(lessStudents$pawsId, lessStudents$fullname)
    return(c(" " = "", newSelect))
  })
  
  observeEvent(input$section,
    updateSelectInput(session, "student", "Select a Student", c(" " = "", limitStudents())))
  
  # Pull Attendance info
  attValue <- reactive(ifelse(input$student %in% big$pawsId, 
                              filter(big, pawsId == input$student) %>% select(missed) %>% as.integer(),
                              "All"))
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
  
  # Update data table for charts
  studentSubs <- reactive(filter(subs, pawsId == input$student) %>% select(label, submissions, bestScore))
  
  # generate value box
  output$attendance <- renderValueBox({
    valueBox(percent(attPerc(), accuracy=1), paste0("of ", attTotal(), " classes attended"), color = attWarning())
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