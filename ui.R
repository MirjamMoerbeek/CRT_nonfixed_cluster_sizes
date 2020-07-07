library(shiny)



shinyUI(fluidPage(
  # Application title
  h4("Optimal designs for group randomized trials and group administered treatments with outcomes at individual and group level."),
  h4("Scenario 1: group sizes fixed a priori."),
  hr(),
  fluidRow(
    column(3,
           h4("Fixed group sizes"),
           numericInput("n.T", label = h6("Group size in intervention"), value = 25,min=0),
           numericInput("n.C", label = h6("Group size in control"), value = 25,min=0)
    ),    
    column(3,
           h4("Variances for group level outcome"),
           numericInput("phi2.T", label = h6("Variance in intervention"), value = 2,min=0,step=0.05),
           numericInput("phi2.C", label = h6("Variance in control"), value = 1,min=0,step=0.05)
    ),
    column(3, 
           h4("Variances and ICC for individual level outcome"),
           numericInput("var.Y.T", label = h6("Total variance in intervention"), value = 44.8,min=0),
           numericInput("var.Y.C", label = h6("Total variance in control"), value = 51.13,min=0),
           numericInput("ICC.T", label = h6("ICC in intervention"), value = 0.065,min=0,max=1,step=0.05),
           numericInput("ICC.C", label = h6("ICC in control"), value = 0.127,min=0,max=1,step=0.05)
    ),
    column(3, 
           h4("Cost specification"),
           numericInput("c.T", label = h6("Group-level costs in intervention"), value = 214,min=0),
           numericInput("c.C", label = h6("Group-level costs in control"), value = 47,min=0),
           numericInput("s.T", label = h6("Subject-level costs in intervention"), value = 2.12,min=0),
           numericInput("s.C", label = h6("Subject-level costs in control "), value = 2.12,min=0),
           numericInput("B", label = h6("Budget"), value = 1000,min=0)
          
    ),



    hr(),
    h4("Single-objective optimal designs"),
    fluidRow(
    column(3,
    plotOutput("plot1")),
    column(3,
    plotOutput("plot2"))
      ),
    
    
    hr(),
    h4("Multiple-objective optimal design"),
    fluidRow(
      column(3,
             plotOutput("plot3")),
      column(3,
             plotOutput("plot4"))
    )
  )
))