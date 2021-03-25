library(shiny)

# Define UI ----
ui <- fluidPage(
    titlePanel("The Monty Hall Problem"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("choice", 
                        label = "Choice",
                        choices = list("Stay", 
                                       "Switch"),
                        selected = "Percent White"),
            sliderInput("rounds", 
                        label = "Rounds:",
                        min = 100, max = 3000, value = 1000)
        ),
        
        mainPanel(
            strong("If you stay"),
            p("Monty's opening of a door to reveal a goat is completely useless since you stay on your original door. Thus the probability of winning is 1/3"),
            strong("If you switch"),
            p("If your original choice is a car, you lose. If your original choice is a goat, you win. Thus the probability of winning is 2/3 "),
            tableOutput("table"),
            plotOutput("plot1")
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    library(tidyverse)
    
    monty_hall <- function(choice, samples){
        win = 0
        lose = 0
        for (i in seq(1, samples)){
            prize <- sample( c("1", "2", "3"), 1, replace = TRUE )
            my_door <- "1"
            if (choice == "Switch"){
                if(my_door == prize){lose = lose + 1}
                else{win = win + 1}
            }
            else if (choice == "Stay"){
                if(my_door == prize){win = win + 1}
                else {lose = lose + 1}
            }
        }
        tibble_table <- tibble(
            Number_of_win = win, 
            Number_of_lose = lose, 
            probabiliy_to_win = win/(win+lose)
        )
        return(tibble_table)
    }
    
    output$table <- renderTable({ monty_hall(input$choice, input$rounds) })
    
    output$plot1 <- renderPlot({
        plot(tibble_table$win, tibble_table$lose)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)


