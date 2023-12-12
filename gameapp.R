library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

game_data <- read.csv(file = "BGG_Data_Set.csv")

domain_data <- game_data %>% separate_rows(Domains, sep = "\\, ")

mechanic_data <- game_data %>% separate_rows(Mechanics, sep = "\\, ")


ui <- fluidPage(tabsetPanel(
  tabPanel("Table", dataTableOutput(outputId = "data")),
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
               sliderInput(inputId = "b", label = "Bins", min = 1, max = 40, value = 5),
               numericInput(inputId = "c", label = 'X min', value = 0),
               numericInput(inputId = "d", label = "X max", value = 100000)
             ),
             mainPanel(plotOutput(outputId = "hist"))
           )
           ),
  tabPanel("Boxplots", 
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "e", label = "X var", choices = c(
                 "Mechanic",
                 "Domain"
               )),
               radioButtons(inputId = "f", label = "Y var", choices = c(
                 "Year.Published", 
                 "Min.Players",
                 "Max.Players",
                 "Play.Time",
                 "Min.Age",
                 "Users.Rated",
                 "Rating.Average",
                 "Complexity.Average",
                 "Owned.Users"
               )),
               textInput(inputId = "g", label = "1st domain or mechanic to plot"),
               textInput(inputId = "h", label = "2st domain or mechanic to plot"),
               textInput(inputId = "i", label = "3st domain or mechanic to plot")
             ),
             mainPanel(
               plotOutput(outputId = "boxplot")
             )
           )
           )
)
)


server <- function(input, output, session) {
  output$data <- renderDataTable(game_data)
  
  
  
  output$hist <- renderPlot({
    ggplot(game_data, aes_string(x = input$a)) + geom_histogram(binwidth = input$b) + xlim(input$c, input$d)
  })
  
  re <- reactive({
    val <- input$e
    if (val == "Mechanic") {
      
      return(ggplot(mechanic_data, aes(x = mechanic_data$Mechanics)) + geom_boxplot(aes_string(y = input$f))) 
      
    } else {
      
      return(ggplot(domain_data, aes(x = domain_data$Domains)) + geom_boxplot(aes_string(y = input$f)))
    
    }
  })
  
  
  output$boxplot <- renderPlot({
    re()
  })
}


shinyApp(ui = ui, server = server)