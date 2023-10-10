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
library(tidyr)
library(ggstatsplot)
MyData <- read.csv("mxmh_survey_results.csv")


# UI for app
ui <- dashboardPage(
    
    # Title
    dHead <- dashboardHeader(title = "Mental Health and Music Analysis", titleWidth = 500),
    sidebar <- dashboardSidebar(disable = TRUE),
    #Body
    body <- dashboardBody(
      fluidRow(
        # Sections of visualization are seperated via tab boxes
        tabBox(
          title = "",
          id = "tabs1",
          width = "200px",
          # Shows distribution of the severity of the chosen mental health issue based on favorite genre of music
          # Users can choose the type of genre and type of mental health issue distribution
          tabPanel("Condition Based on Fav Genre",
                   fluidRow(
                     box(plotOutput("distPlot2")),
                     box(selectInput("Genre", "Choose a Genre: ",
                                     list('Classical', 'Country', 'EDM', 'Folk', 'Gospel', 'Hip hop', 'Jazz', 
                                          'K pop', 'Latin', 'Lofi', 'Metal', 'Pop', 'R&B', 'Rap', 'Rock',
                                          'Video game music'), 'Rap')),
                     box(selectInput("Condition2", "Choose a condition: ",
                                     list("Anxiety", "Insomnia", "Depression", "OCD"), "Anxiety")))),
          # Scatterplot for Mental health condition to age
          # Users can choose the Mental health issue
          tabPanel("Condition Based on Age",
                   fluidRow(
                     box(plotOutput("distPlot3")),
                     box(selectInput("Condition3", "Choose a condition: ",
                                     list("Anxiety", "Insomnia", "Depression", "OCD"), "Anxiety")))), 
          # Based on frequency of listening to music a correlation is mapped to another type of music and mental health issue
          # User can choose which types of music and mental health issues they wanna see the correlation for
          tabPanel("Correlation",
                   fluidRow(
                     box(plotOutput("distPlot4")),
                     box(checkboxGroupInput("corr_v", "Variables For Correlation: ", 
                                            c("Classical" = "Frequency..Classical.", "Country" = "Frequency..Country.",
                                                "EDM" = "Frequency..EDM.", "Folk" = "Frequency..Folk.",
                                                "Gospel" = "Frequency..Gospel.", "Hip hop" = "Frequency..Hip.hop.",
                                                "Jazz" = "Frequency..Jazz.", "K pop" = "Frequency..K.pop.",
                                                "Latin" = "Frequency..Latin.", "Lofi" = "Frequency..Lofi.",
                                                "Metal" = "Frequency..Metal.", "Pop" = "Frequency..Pop.",
                                                "R&B" = "Frequency..R.B.", "Rap" = "Frequency..Rap.",
                                                "Rock" = "Frequency..Rock.", "Video Game Music" = "Frequency..Video.game.music.",
                                                "OCD" = "OCD", "Anxiety" = "Anxiety", "Insomnia" = "Insomnia",
                                                "Depression" = "Depression"),
                                            selected = c("Frequency..EDM.", "OCD", "Anxiety", "Insomnia", "Depression"))))),
          # Creates 3 Bar graphs to show the distribution of mental health severity based on whether the user played an instrument
          # or is a composer
          # Able to choose which mental health affliction they'd like to compare
          tabPanel("Musician/Composer",
                   fluidRow(
                     box(plotOutput("distPlot5")),
                     box(plotOutput("distPlot6")),
                     box(plotOutput("distPlot7")),
                     box(selectInput("Condition0", "Choose a condition: ",
                                     list("Anxiety", "Insomnia", "Depression", "OCD"), "Anxiety")))),
          ))
    
  )
)

# Server logic for data vis construction
server <- function(input, output) {

    output$distPlot2 <- renderPlot({
      # Create a new Data Frame that contains only the rows where the favorite genre is what the app user selects
      # Then only select the column that correlates to the Mental health issue they want to examine
      mD <- MyData
      x1 <- mD[mD$Fav.genre == input$Genre, ]
      x1 <- x1[, input$Condition2]
      #Histogram construction, 10 bins because the scale of severity is 1-10 only integers
      hist(x1, breaks = 10, col = 'darkgray', border = 'white',
           xlab = paste('Severity of ', input$Condition2), 
           main = paste(input$Condition, ' Distribution'))
    })
    output$distPlot3 <- renderPlot({
      # New Data Frame with all rows where the age was not entered are omitted
      x3 <- na.omit(data.frame(Age = MyData$Age, C = MyData[, input$Condition3]))
      # Construct scatterplot of age to severity of condition
      plot(as.numeric(x3$Age),as.numeric(x3$C), main = "Age vs. Severity", xlab = "Age", ylab = input$Condition3)
    })
    output$distPlot4 <- renderPlot({
      # Clean a new data frame where the answers(Never, Rarely, Sometimes, and Very Frequently) are mapped to an integer value
      x4 <- read.csv("mxmh_survey_results.csv")
      x4[x4 == "Never"] <- 0
      x4[x4 == "Rarely"] <- 1
      x4[x4 == "Sometimes"] <- 2
      x4[x4 == "Very frequently"] <- 3
      # Segment data to get the columns containing listening frequency by genre and convert the values into numeric type variables
      x4[, 12:31] <- sapply(x4[,12:31], as.numeric)
      # Construction of Correlation Matrix, type pearson
      ggcorrmat(x4, 
                method = "pearson",
                label = TRUE,
                cor.vars = input$corr_v,
                size = 10)
    })
    # The following three outputs are all histograms mapping the Severity of a Condition to whether or not
    # the person entering the data played an instrument, composed music or did neither
    output$distPlot5 <-renderPlot({
      df <- read.csv("mxmh_survey_results.csv")
      df <- df[df$Instrumentalist == 'Yes',]
      df <- df[, input$Condition0]
      hist(df, breaks = 10, col = 'red', border = 'white',
           xlab = paste('Severity of ', input$Condition0),
           main = paste('Musician ', input$Condition0, ' Distribution'))

    })
    output$distPlot6 <- renderPlot({
      df <- read.csv("mxmh_survey_results.csv")
      df <- df[df$Composer == 'Yes',]
      df <- df[, input$Condition0]
      hist(df, breaks = 10, col = 'blue', border = 'white',
           xlab = paste('Severity of ', input$Condition0), 
           main = paste('Composer ',input$Condition0, ' Distribution'))
    })
    output$distPlot7 <- renderPlot({
      df <- read.csv("mxmh_survey_results.csv")
      df <- df[df$Composer == 'No' & df$Instrumentalist == 'No',]
      df <- df[, input$Condition0]
      hist(df, breaks = 10, col = 'darkgreen', border = 'white',
           xlab = paste('Severity of ', input$Condition0), 
           main = paste('Non Composer or Musician ',input$Condition0, ' Distribution'))
    })
    
}

# Run the application 
shinyApp(ui = dashboardPage(dHead,
                            sidebar,
                            body), server = server)
