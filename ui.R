library(shiny)



shinyUI(fluidPage(
  # Application title
  h4("Optimal designs for group randomized trials and group administered treatments with outcomes at individual and group level.
     "),
  h4("Scenario 2: group sizes not fixed a priori."),
  hr(),
  fluidRow(
    column(4,
           h4("Variances for group level outcome"),
           numericInput("phi2.T", label = h6("Variance in intervention"), value = 100,min=0,step=0.05),
           numericInput("phi2.C", label = h6("Variance in control"), value = 100,min=0,step=0.05)
    ),
    column(4, 
           h4("Variances and ICC for individual level outcome"),
           numericInput("var.Y.T", label = h6("Total variance in intervention"), value = 144,min=0),
           numericInput("var.Y.C", label = h6("Total variance in control"), value = 144,min=0),
           numericInput("ICC.T", label = h6("ICC in intervention"), value = 0.025,min=0,max=1,step=0.05),
           numericInput("ICC.C", label = h6("ICC in control"), value = 0.025,min=0,max=1,step=0.05)
    ),
    column(4, 
           h4("Cost specification"),
           numericInput("c.T", label = h6("Group-level costs in intervention"), value = 20000,min=0),
           numericInput("c.C", label = h6("Group-level costs in control"), value = 500,min=0),
           numericInput("s.T", label = h6("Subject-level costs in intervention"), value = 15,min=0),
           numericInput("s.C", label = h6("Subject-level costs in control "), value = 15,min=0),
           numericInput("B", label = h6("Budget"), value = 100000,min=0)
    ),


    hr(),
    h4("Single-objective optimal designs"),
    fluidRow(
    column(3,
    plotOutput("plot1")),
    column(3,
    plotOutput("plot2")),
    column(3,
    plotOutput("plot3"))
      ),
    
    hr(),
    h4("Multiple-objective optimal design"),
    fluidRow(

      column(3,
             plotOutput("plot4")),
      column(3,
             plotOutput("plot5")),
      column(3,
             plotOutput("plot6"))
    )
    
    
  )
))