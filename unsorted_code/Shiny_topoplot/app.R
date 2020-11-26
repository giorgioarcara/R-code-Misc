#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(erpR)

# source functions
source("functions/erp.R")
source("functions/erp.xaxis.R")
source("functions/erp.yaxis.R")
source("functions/msectopoints.R")
source("functions/topoplot.R")
source("functions/grandaverage.R")# load data

load("data/ERPsets.RData")



library(shiny)
library(gridExtra)
data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
my_startmsec=-200
my_endmsec=1500

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
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
                     choices=names(word)),
         helpText("Select Electrode to plot as ERP")
         ),
      
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Shiny_Topoplot", hover="plot_hoever") 
      )
   )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$Shiny_Topoplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x0    <- input$Window_edges[1]
      x1    <- input$Window_edges[2]
      #make a topoplot excluding not found electrode
      par(mai=c(0,0,0,0), mar=c(1.5,1.5,1.5,1.5), pty="s", mfrow=c(1,2))
      topoplot(word, startmsec=-200, endmsec=1500, win.ini=x0, win.end=x1,
                draw.nose=T, draw.ears=T, cex=1,
               lwd=3, head.lwd=5, elec.lab.cex=1.5, draw.elec.lab = F)   
      
      mtext(3, text =paste( x0 , "-", x1 , " ms", sep=""), cex=1.5, font=2)
      
      par(mar=c(0.7, 0.7, 0.7, 0.7))
      erp(word[, input$electrode], main=input$electrode, startmsec=my_startmsec, endmsec=my_endmsec, lwd=2, frame.plot=F, 
           x.lwd=2, x.lwd.ticks=2, y.lwd=2, y.lwd.ticks=2, 
          y.cex=1.2, y.las=2, 
          x.tick=c(-200, 0, 200, 400, 600, 800, 1000, 1200, 1400))
      
      x_start_point = msectopoints( x0 , dim(word)[1], my_startmsec, my_endmsec)
      x_end_point = msectopoints( x1 , dim(word)[1], my_startmsec, my_endmsec)
      
      
      rect(xleft=x_start_point, xright=x_end_point, ytop=4, ybottom=-4, col=rgb(0.5, 0.5, 0.5, alpha=0.2))
      
      
   })
   
   
   
   
   
   
}




# Run the application 
shinyApp(ui = ui, server = server)

