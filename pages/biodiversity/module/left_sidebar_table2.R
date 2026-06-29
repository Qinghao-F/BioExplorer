left_sidebar_table2_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "bio-left-sidebar",
    h6("This is left sidebar 2."),
    plotOutput(ns("plot"))
  )
}

left_sidebar_table2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      hist(mtcars$mpg, col = "#80cbc4", border = "#004d40")
    })
  })
}
