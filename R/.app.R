library(shiny)
ui <- fluidPage(
  div(
    p("Hello World"),
    sliderInput("priceInput", "Price", min = 0, max = 100,
                value = c(25, 40)
                )
    )
)

server <- function(input, output) {

  priceDiff <- reactive({
    # print(input$priceInput)
    diff(input$priceInput)
  })

  # observe({
  #   print(priceDiff())
  # })

}

shinyApp(ui = ui, server = server)
