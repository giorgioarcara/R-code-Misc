ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  actionButton("updateplot", "Update Plot:"),
  actionButton("refreshlinie", "Rlinie"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  
  var1 <- c(3, 10, 15, 3, 4, 7, 1, 12, 8, 18, 20, 4, 4, 5, 10)
  var2 <- c(4, 10, 12, 17, 15, 20, 14, 3, 4, 15, 12, 5, 5, 6, 2)
  n <- length(var1)
  
  # initialize reactive values with existing data
  val <- reactiveValues( clickx = NULL, clicky = NULL, data = cbind (var1, var2))
  
  observe({
    input$plot_click
    isolate({
      # save new points added
      val$clickx = c(val$clickx, input$plot_click$x)
      val$clicky = c(val$clicky, input$plot_click$y)
      # add new points to data
      val$data <- rbind(val$data, cbind(input$plot_click$x, input$plot_click$y))
    })
  })
  
  
  output$plot1 <- renderPlot({
    input$updateplot
    plot(val$data[,1], val$data[,2])
    if(input$refreshlinie)
      # I changed the order of the variables to create the model.
      abline(lm(val$data[,2]~ val$data[,1]))
  })
  
  output$info <- renderText({
    input$plot_click
    paste0("x = ", val$clickx, ", y = ",val$clicky, "\n")
  })
  
}

shinyApp(ui, server)