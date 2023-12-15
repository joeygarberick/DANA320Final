library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

game_data <- read.csv(file = "BGG_Data_Set.csv")

domain_data <- game_data %>% separate_rows(Domains, sep = "\\, ")

mechanic_data <- game_data %>% separate_rows(Mechanics, sep = "\\, ")


ui <- fluidPage(tabsetPanel(
    tabPanel("Table", 
             sidebarLayout(
               sidebarPanel(
                 numericInput(inputId = "min_ratings_table", label = "Minimum Ratings", value = 0)
               ),
               mainPanel(dataTableOutput(outputId = "data_filtered"))
             )
    ),
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
               sliderInput(inputId = "b", label = "Bins", min = 5, max = 100, value = 30),
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
                 "Mechanics",
                 "Domains"
               )),
               radioButtons(inputId = "f", label = "Y var", choices = c(
                 "Year.Published",
                 "Max.Players",
                 "Play.Time",
                 "Min.Age",
                 "Users.Rated",
                 "Rating.Average",
                 "Complexity.Average",
                 "Owned.Users"
               )),
               textInput(inputId = "g", label = "1st domain or mechanic to plot", value = "Insert filter here"),
               textInput(inputId = "h", label = "2st domain or mechanic to plot", value = "Insert filter here"),
               textInput(inputId = "i", label = "3st domain or mechanic to plot", value = "Insert filter here"),
               checkboxInput(inputId = "j", label = "Filter")
             ),
             mainPanel(
               plotOutput(outputId = "boxplot")
             )
           )
  ),
  tabPanel("Scatterplot",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 inputId = "x_var",
                 label = "X Variable",
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
               selectInput(
                 inputId = "y_var",
                 label = "Y Variable",
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
               numericInput(inputId = "min_ratings", label = "Minimum Ratings", value = 0),
               actionButton(inputId = "plot_scatter", label = "Plot Scatterplot")
             ),
             mainPanel(plotOutput(outputId = "scatterplot"))
           )
  )
)
)


server <- function(input, output, session) {
  output$data_filtered <- renderDataTable({
    min_ratings_table <- input$min_ratings_table
    filtered_table <- game_data[game_data$Users.Rated >= min_ratings_table, ]
    filtered_table
  })
  
  output$hist <- renderPlot({
    ggplot(game_data, aes_string(x = input$a)) + geom_histogram(bins = input$b) + xlim(input$c, input$d) + theme_bw()
  })
  
  re <- reactive({
    val <- input$e
    yval <- input$f
    filter1 <- input$g
    filter2 <- input$h
    filter3 <- input$i
    val2 <- input$j
    if (val == "Mechanics") {
      if (val2) {
        data <- filter(mechanic_data, Mechanics == filter1 | Mechanics == filter2 | Mechanics == filter3)
      } else {
        data <- mechanic_data
      }
      ylim1 = boxplot.stats(data[[yval]])$stats[c(1,5)]
      return(ggplot(data, aes(x = Mechanics)) + geom_boxplot(outlier.shape = NA, aes_string(y = yval), na.rm = TRUE) + coord_cartesian(ylim = ylim1) + theme_bw() + labs(x = "Mechanic")) 
      
    } else {
      if (val2) {
        data <- filter(domain_data, Domains == filter1 | Domains == filter2 | Domains == filter3)
      } else {
        data <- domain_data
      }
      ylim1 = boxplot.stats(data[[yval]])$stats[c(1,5)]
      return(ggplot(data, aes(x = Domains)) + geom_boxplot(outlier.shape = NA, aes_string(y = input$f), na.rm = TRUE) + coord_cartesian(ylim = ylim1) + theme_bw() + labs(y = "Domain"))
      
    }
  })
  
  output$boxplot <- renderPlot({
    re()
  })
  
  output$scatterplot <- renderPlot({
    x_var <- input$x_var
    y_var <- input$y_var
    min_ratings <- input$min_ratings
    
    filtered_data <- game_data[game_data$Users.Rated >= min_ratings, ]
    
    ggplot(filtered_data, aes_string(x = x_var, y = y_var)) +
      geom_point(size = 1) +
      labs(x = x_var, y = y_var, title = "Scatterplot") +
      theme_bw()
  })
}


shinyApp(ui = ui, server = server)
