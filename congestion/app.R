#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(latex2exp)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Congestion Externality"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "proportion of cars on route 1:",
                        min = 0,
                        max = 1,
                        value = .5,
                        step=.01)
             ),

        # Show a plot of the generated distribution
        mainPanel(
            print("Two route congestion problem where both routes suffer from congestion (where marginal and average travel times are increasing in usage).  Equilibrium occurs when average travel time is equalized across routes: a quicker route attracts more drivers, until the time advantage is gone. In contrast, society's total travel time is minimized when marginal travel time is equalized across routes.  Social planner would limit access to route 1."),
            plotOutput("costs")
        )
    )
)

# Define server logic required to draw graph
server <- function(input, output) {

    output$costs <- renderPlot({
        par(mfrow=c(1,2))
        n <- 1
        gx <- seq(0,n,.01)
        tc <- function(x) x^3+(n-x)^2
        mc1 <- function(x) 3*x^2
        mc2 <- function(x) 2*(n-x)
        ac1 <- function(x) x^2
        ac2 <- function(x) n-x
        plot(gx,mc1(gx),type="l",col="blue",xlab="proportion on route 1",ylab="$",lty=2,main=TeX("$TC_1=x^3$,  $TC_2=(1-x)^2$"))
        points(input$x,mc1(input$x),col="blue")
        lines(gx,mc2(gx),col="red",lty=2)
        points(input$x,mc2(input$x),col="red")
        lines(gx,ac1(gx),col="blue")
        points(input$x,ac1(input$x),col="blue")
        lines(gx,ac2(gx),col="red")
        points(input$x,ac2(input$x),col="red")
        legend("top",legend=c(TeX("$MC_1=3x^2$"),TeX("$MC_2=2(1-x)$"),TeX("$AC_1=x^2$"),TeX("$AC_2=1-x$")),col=c("blue","red","blue","red"),lty=c(2,2,1,1))
        plot(gx,tc(gx),type="l",xlab="proportion on route 1",ylab="$",main=paste("total travel time ",round(100*(tc(input$x)/tc(gx[which.min(tc(gx))])-1)),"% higher than minimum"))
        points(input$x,tc(input$x))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
