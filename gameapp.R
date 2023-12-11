library(shiny)
library(ggplot2)

game_data <- read.csv(file = "BGG_Data_Set.csv")


ui <- fluidPage(tabsetPanel(
  tabPanel("Histogram",
           sidebarLayout(
             sidebarPanel(
               radioButtons(
                 inputId = "a", 
                 label = "Variable", 
                 choices = c(
                   "Year.Published", 
                   "Min.Players",
                   "Max.Players",
                   "Play.Time",
                   "Min.Age",
                   "Users.Rated",
                   "Rating.Average",
                   "Complexity.Average",
                   "Owned.Users"
                   )
                 ),
               sliderInput(inputId = "b", label = "Bins", min = 1, max = 40, value = 5)
             ),
             mainPanel(plotOutput(outputId = "hist"))
           )
           ),
  tabPanel("tab2", dataTableOutput(outputId = "data")),
  tabPanel("tab3", "contents")
)
)


server <- function(input, output, session) {
  output$data <- renderDataTable(game_data)
  
  
  
  output$hist <- renderPlot({
    ggplot(game_data, aes_string(x = input$a)) + geom_histogram(binwidth = input$b)
  })
}


shinyApp(ui = ui, server = server)