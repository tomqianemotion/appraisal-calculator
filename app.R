#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(neuralnet)
load('calculator.RData')
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Neural Network Appraisal Calculator Demo"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(3,
           sliderInput("ang",
                       "Anger",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("anx",
                       "Anxiety",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("bor",
                       "Boredom",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("fea",
                       "Fear",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("fru",
                       "Frustration",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("gui",
                       "Guilt",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("reg",
                       "Regret",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("res",
                       "Resignation",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("sad",
                       "Sadness",
                       min = 0,
                       max = 9,
                       value = 0)
    ),
    column(3,
           sliderInput("cal",
                       "Calm",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("cha",
                       "Challenge",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("gra",
                       "Gratitude",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("hap",
                       "Happy",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("hop",
                       "Hope",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("int",
                       "Interest",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("rel",
                       "Relief",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("pri",
                       "Pride",
                       min = 0,
                       max = 9,
                       value = 0),
           sliderInput("sur",
                       "Surprise",
                       min = 0,
                       max = 9,
                       value = 0)
    ),
    
    
    # Show a plot of the generated distribution
    column(5,
      plotOutput("appra")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    as.data.frame(input$reg,input$sur,input$gui,input$res,input$cal,input$fru,input$ang,
                  input$cha,input$sad,input$int,input$rel,input$bor,input$anx,input$hop,
                  input$pri,input$fea,input$hap,input$gra)
  })
  output$appra <- renderPlot({
    
    new <- c(input$reg,input$sur,input$gui,input$res,input$cal,input$fru,input$ang,
             input$cha,input$sad,input$int,input$rel,input$bor,input$anx,input$hop,
             input$pri,input$fea,input$hap,input$gra)
    maxs=c(9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9)
    mins=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1) 
    new <- as.data.frame(scale(t(new), center = mins, scale = maxs - mins))
    #predicted from neural network
    pr.nn1 <- compute(nn,new)
    #rele
    rele <- pr.nn1$net.result[,1]*8+1
    #cong
    cong <- pr.nn1$net.result[,2]*9
    #guilt
    self <- pr.nn1$net.result[,3]*8+1
    #resignation
    other <- pr.nn1$net.result[,4]*8+1
    #calm
    futu <- pr.nn1$net.result[,5]*8+1
    #frustration
    pfcp <- pr.nn1$net.result[,6]*8+1
    #anger
    afcp <- pr.nn1$net.result[,7]*8+1
    
    
    neuralnetwork=as.array(round(c(rele,cong,self,other,futu,pfcp,afcp),2))
    
    par(mar=c(15,4,1,1))
    barplot(t(neuralnetwork),
            names=c("Relevance","Congruence","Self-accountability","Other-accountability","Future expectancy",
                   "Problem coping","Accomodative coping"),main='Appraisal Predictions',col='gold',
            border='black',cex.axis=1.3,cex.names=1.3,ylim=c(0,9),ylab="Intensity",las=3)
    
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

