#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
MyData <- read.csv("mxmh_survey_results.csv")
# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Sidebar
    dHead <- dashboardHeader(title = "My Dashboard"),
    sidebar <- dashboardSidebar(
      sidebarMenu(
      menuItem("Dashboard", tabname = "dashboard", icon = icon("dashboard")),
      menuItem("Second Thing", tabname = "Number 2", icon = icon("dashboard"))
    )),
    #Body
    body <- dashboardBody(
      fluidRow(
        tabBox(
          title = "Mental Health Distribution",
          id = "tabs1",
          width = "200px",
          tabPanel("Tab1",
            fluidRow(
              box(plotOutput("distPlot")),
              box(sliderInput("bins","Number of bins:", min = 1, max = 10, value = 5)),
              box(selectInput("Condition", "Choose a condition: ",
                        list("Anxiety", "Insomnia", "Depression", "OCD"), "Anxiety"))))))
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- MyData[, input$Condition]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = paste('Severity of ', input$Condition),
             main = paste(input$Condition, ' Distribution'))
    })
}

# Run the application 
shinyApp(ui = dashboardPage(dHead,
                            sidebar,
                            body), server = server)
