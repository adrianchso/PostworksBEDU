#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Postwork 8",
                 
                 tabPanel("Bar plot",   
                          verticalLayout(
                              
                              selectInput("team", "Selecciona el equipo", 
                                          choices = c("Home", "Visitor")),
                              
                              plotOutput("barplot", height = 300, width = 460) 
                            
                         )
                 ),
                 
                 tabPanel("ImagesPostwork 3",  
                          verticalLayout(
                              
                            img(src="postwork3plot1.png", height = 450, width = 450),
                            
                            img(src="postwork3plot2.png", height = 450, width = 450),
                            
                            img(src="postwork3plot3.png", height = 450, width = 450)
                            
                          )
                 ), 
                 
                 tabPanel("Table", 
                          tableOutput("table")
                 ),
                 
                 tabPanel("Factores de ganancia", 
                          verticalLayout(
                              
                            img(src="plot1.png", height = 450, width = 450),
                            
                            img(src="plot2.png", height = 450, width = 450)
                            
                          )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(ggplot2)
    
    data <- read.csv("match.data.csv")
    
    output$result <- renderText({
        paste("You chose", input$team)
    })
    
    output$barplot <- renderPlot({
        goals <- data$home.score
        if(input$team == "Home"){
            goals <- data$home.score
        }
        else{
            goals <- data$away.score
        }
        goals_freq <- table(goals)/sum(table(goals))
        ggplot(as.data.frame(goals_freq), aes(x= goals, y= Freq)) + 
            geom_bar(stat = "identity")
    })
    
    output$table <- renderTable(data)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
