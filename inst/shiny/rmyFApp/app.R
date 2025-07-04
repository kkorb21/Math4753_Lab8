#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(Lab8KORBMath4753)

ui <- fluidPage(
  titlePanel("Simulate B = X + Y using Inverse Optimization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("iter", "Number of Simulations:",
                  min = 100, max = 20000, value = 1000, step = 100)
    ),
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    result <- rmyF(iter = input$iter)
    # Just triggering plot inside rmyF
  })

  output$summary <- renderPrint({
    result <- rmyF(iter = input$iter)
    result
  })
}

shinyApp(ui = ui, server = server)
