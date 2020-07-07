library(shiny)
source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ################################################################################################################################
  ### efficiency plot group level outcome
  ################################################################################################################################  
  output$plot1 <- renderPlot({
    prop.vec=seq(0.01,0.99,0.01)
    ratio.vec=prop.vec/(1-prop.vec)
    theta.2.vec=theta.2(ratio=ratio.vec,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C,n.T=input$n.T,n.C=input$n.C)
    plot(prop.vec,min(theta.2.vec)/theta.2.vec,type="l",ylim=c(0,1),xlab="Proportion groups in intervention",ylab="Efficiency")
    title(main="Group level outcome")
  })

  ################################################################################################################################
  ### efficiency plot individual level outcome
  ################################################################################################################################  
  output$plot2<-  renderPlot({
    prop.vec=seq(0.01,0.99,0.01)
    ratio.vec=prop.vec/(1-prop.vec)
    theta.1.vec=theta.1(ratio=ratio.vec,n.T=input$n.T,n.C=input$n.C,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    plot(prop.vec,min(theta.1.vec)/theta.1.vec,type="l",ylim=c(0,1),xlab="Proportion groups in intervention",ylab="Efficiency")
    title(main="Individual level outcome")
  })

  ################################################################################################################################
  ### compund optimal design: optimal design as a function of lambda
  ################################################################################################################################    
  output$plot3 <- renderPlot({
    # calculate minimal value theta.1
    opt.ratio.1=sqrt( (input$var.Y.T/input$var.Y.C) * (input$n.C/input$n.T) * (((input$n.T-1)*input$ICC.T+1)/ ((input$n.C-1)*input$ICC.C+1)) * ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)) )
    min.theta.1=theta.1(ratio=opt.ratio.1,n.T=input$n.T,n.C=input$n.C,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)

    # calculate minimal value theta.2
    opt.ratio.2=sqrt( (input$phi2.T/input$phi2.C) * ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)) )
    min.theta.2=theta.2(ratio=opt.ratio.2,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C,n.T=input$n.T,n.C=input$n.C)

    # calculate compound optimal design for each lambda
    lambda=seq(0,1,by=0.01)
    w.1=(1-lambda)/min.theta.1
    w.2=(lambda)/min.theta.2
    
    opt.ratio.comp=sqrt((input$n.C/input$n.T)*(w.1*(1-input$ICC.T)*input$var.Y.T+input$n.T*(w.1*input$ICC.T*input$var.Y.T+w.2*input$phi2.T))/(w.1*(1-input$ICC.C)*input$var.Y.C+input$n.C*(w.1*input$ICC.C*input$var.Y.C+w.2*input$phi2.C))* ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)))
    opt.prop.comp=opt.ratio.comp/(1+opt.ratio.comp)

    
    # plot compound optimal design
    plot(lambda,opt.prop.comp,type="l",ylim=c(0,1),xlab="Lambda",ylab="Optimal proportion groups in intervention")
    title(main="Compound optimal design")
  })

  ################################################################################################################################
  ### compund optimal design: efficiencies as a function of lambda
  ################################################################################################################################    
  
  output$plot4 <- renderPlot({
    # calculate minimal value theta.1
    opt.ratio.1=sqrt( (input$var.Y.T/input$var.Y.C) * (input$n.C/input$n.T) * (((input$n.T-1)*input$ICC.T+1)/ ((input$n.C-1)*input$ICC.C+1)) * ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)) )
    min.theta.1=theta.1(ratio=opt.ratio.1,n.T=input$n.T,n.C=input$n.C,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    
    # calculate minimal value theta.2
    opt.ratio.2=sqrt( (input$phi2.T/input$phi2.C) * ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)) )
    min.theta.2=theta.2(ratio=opt.ratio.2,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C,n.T=input$n.T,n.C=input$n.C)
    
    # calculate compound optimal design for each lambda
    lambda=seq(0,1,by=0.01)
    w.1=(1-lambda)/min.theta.1
    w.2=(lambda)/min.theta.2
    
    opt.ratio.comp= sqrt((input$n.C/input$n.T)*(w.1*(1-input$ICC.T)*input$var.Y.T+input$n.T*(w.1*input$ICC.T*input$var.Y.T+w.2*input$phi2.T))/(w.1*(1-input$ICC.C)*input$var.Y.C+input$n.C*(w.1*input$ICC.C*input$var.Y.C+w.2*input$phi2.C))* ((input$c.C+input$n.C*input$s.C)/(input$c.T+input$n.T*input$s.T)))
    
    # calculate RE for group level outcome
    theta.2.comp=theta.2(ratio=opt.ratio.comp,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C,n.T=input$n.T,n.C=input$n.C)  
    RE.c=min.theta.2/theta.2.comp
    
    # calculate RE for individual level outcome
    theta.1.comp=theta.1(ratio=opt.ratio.comp,n.T=input$n.T,n.C=input$n.C,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    RE.s=min.theta.1/theta.1.comp
    
    # plot compound optimal design
    plot(lambda,RE.c,type="l",ylim=c(0,1),xlab="Lambda",ylab="Efficiency")
    lines(lambda,RE.s,lty=2)
    title(main="Compound optimal design")
    legend(0.2, 0.25, legend=c("Group level outcome", "Individual level outcome"),lty=1:2, cex=0.8,box.lty = 0)
  })
  
  })




