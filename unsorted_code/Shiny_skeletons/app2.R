#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(erpR)




library(shiny)


# Define UI for application that draws a histogram
# the ShinyUI does not seem necessary, but useful for better arrangement
ui <- shinyUI(fluidPage(
  fluidRow(
    column(12,
           # Application title
           titlePanel("erpR Shiny Example"),
           
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               
               # sliderInput("Window_Start",
               #              "Window Start in ms:",
               #              min = -200,
               #              max = 1500,
               #              value = 400, 
               #              step=50),
               # 
               #    sliderInput("Window_End",
               #                "Window End in ms",
               #                min = -200,
               #                max = 1500,
               #                value = 600, 
               #                step=50),
               
               
               sliderInput("Window_edges", label = h3("Topoplot window range"), min = -200, 
                           max = 1500, value = c(400, 600), step=50),
               
               
               hr(), # to add separator
               
               selectInput("electrode", "Electrode:", 
                           choices=c("ciao", "bau")),
               helpText("Select Electrode to plot as ERP")
             ),
             
             
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("Shiny_Topoplot",  click="plot_click") 
             )
           )
    ),
    fluidRow(
      column(12, offset=2,
             mainPanel(
               plotOutput("Shiny_scalp") 
             )
      )
    )
    
  )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  

    
    # basic plot   
    output$Shiny_Topoplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x0    <- input$Window_edges[1]
      x1    <- input$Window_edges[2]
      plot(x0, x1)
      
    })
    
    #############################
    # TO GET VALUES FROM CLICK
    #############################
    
    val <- reactiveValues( clickx = NULL, clicky = NULL, data = cbind (400, 400))
    
    observe({
      input$plot_click
      isolate({
        # save new points added
        val$clickx = c(val$clickx, input$plot_click$x)
        val$clicky = c(val$clicky, input$plot_click$y)
        val$data <- rbind(val$data, cbind(input$plot_click$x, input$plot_click$y))
      })
    })
    

    
    
    
    #############################
  
      output$Shiny_Topoplot <- renderPlot({
        # generate bins based on input$bins from ui.R


        x0    <- input$Window_edges[1]
        x1    <- input$Window_edges[2]
        #make a topoplot excluding not found electrode
        
        plot(x0, x1)
        
        #text(x0, x1,labels = val$data)
        #if(input$refreshlinie)         
        abline(v = val$clickx)

          
        

      }) # close renderPlot
      
      
    
    
    output$Shiny_scalp= renderPlot({
      
      plot(rnorm(100))
      
    })


} # end server




# Run the application 
shinyApp(ui = ui, server = server)

