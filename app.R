#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(glue)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel("Gotcha"),
    
        uiOutput('tst1')
    

    
)


server <- function(input, output, session) {

    
    
    timer <- reactiveVal(10000000000)
    active <- reactiveVal(TRUE)
    
    observe({
        invalidateLater(400, session)
        isolate({
            if(active())
            {
                timer(timer()-1)
                if(timer()<1)
                {
                    active(FALSE)
                    showModal(modalDialog(
                        title = "Important message",
                        "Congratulations on the persistence"
                    ))
                }
            }
        })
    })
    
    observeEvent(input$press1, {
        showModal(modalDialog(
            title = "Important message",
            "Thanks for playing!"
        ))
    })
    
    output$tst1 <- renderUI({
        
        tst1 <- timer()
        # generate bins based on input$bins from ui.R
        floor1 <- round(runif(1, min = 1, max = 12),0)
        floor2 <- round(runif(1, min = 0, max = 12),0)
        floor3 <- round(runif(1, min = 1, max = 4),0)
        floor4 <- round(runif(1, min = 1, max = 10),0)
        floor5 <- round(runif(1, min = 1, max = 6),0)
        floor6 <- round(floor1*1.5, 0)
        
        cols1 <- c('default', 'primary', 'warning', 'danger', 'success', 'royal')
        
        text1 <- c('Hi!', 'Almost', 'Closer', 'Thought you had me', 
                   'Boo', 'Na Na Na', 'Eeek', 'Whoopsie Daisy', 
                   'Going Left', 'Going Right')
        
        style1 <- c('simple', 'bordered', 'minimal', 'stretch', 'jelly', 'gradient',
                    'fill', 'material-circle', 'material-flat', 'pill', 'float', 'unite')
        
        buttons1 <- c('xs', 'sm', 'md', 'lg')
        
        if(floor1 + floor2 > 12){
            floor2 <- 0
        }
        fluidRow(
            column(
                width = floor1, offset = floor2,
                div(style = as.character(glue::glue("height:{floor6}00px; display: flex; align-items:center; justify-content: center;")), 
                    shinyWidgets::actionBttn('press1', text1[floor4], color = cols1[floor5], style = style1[floor1], size = buttons1[floor3]))
            )
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
