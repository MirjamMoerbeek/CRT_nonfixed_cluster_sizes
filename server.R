library(shiny)
source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ################################################################################################################################
  ### efficiency plot group level outcome: efficiency of proportion budget allocated to intervention
  ################################################################################################################################  
  output$plot1 <- renderPlot({
    prop.vec=seq(0.01,0.99,0.01)
    ratio.vec=prop.vec/(1-prop.vec)
    theta.2.vec=theta.2(ratio=ratio.vec,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C)
    plot(prop.vec,min(theta.2.vec)/theta.2.vec,type="l",ylim=c(0,1),xlab="Proportion of budget to intervention",ylab="Efficiency")
    title(main="Group level outcome")
  })

  ################################################################################################################################
  ### efficiency plot individual level outcome: efficiency of proportion budget allocated to intervention
  ################################################################################################################################  
  output$plot2<-  renderPlot({
    prop.vec=seq(0.01,0.99,0.01)
    ratio.vec=prop.vec/(1-prop.vec)
    theta.1.vec=theta.1(ratio=ratio.vec,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    plot(prop.vec,min(theta.1.vec)/theta.1.vec,type="l",ylim=c(0,1),xlab="Proportion of budget to intervention",ylab="Efficiency")
    title(main="Individual level outcome")
  })
  
  ################################################################################################################################
  ### efficiency plot individual level outcome: efficiency of cluster size for both treatments
  ################################################################################################################################  
  output$plot3<-  renderPlot({
    n1.vec=seq(1,250,1)
    n2.vec.T=input$B/(input$c.T+input$s.T*n1.vec)
    var.mean.T=input$var.Y.T*((n1.vec-1)*input$ICC.T+1)/(n1.vec*n2.vec.T)
    n2.vec.C=input$B/(input$c.C+input$s.C*n1.vec)
    var.mean.C=input$var.Y.C*((n1.vec-1)*input$ICC.C+1)/(n1.vec*n2.vec.C)
    plot(n1.vec,min(var.mean.T)/var.mean.T,type="l",ylim=c(0,1),xlab="Cluster size",ylab="Efficiency")
    lines(n1.vec,min(var.mean.C)/var.mean.C,lty=2)
    legend(20, 0.25, legend=c("Intervention condition", "Control condition"),lty=1:2, cex=0.8,box.lty = 0)
    title(main="Individual level outcome")
  })
  
  
  

  ################################################################################################################################
  ### compund optimal design: optimal design for budget ratio as a function of lambda
  ################################################################################################################################    
  output$plot4 <- renderPlot({
    # calculate minimal value theta.1
    opt.ratio.1=(sqrt(input$var.Y.T)*(sqrt(input$ICC.T*input$c.T)+sqrt((1-input$ICC.T)*input$s.T))) / (sqrt(input$var.Y.C)*(sqrt(input$ICC.C*input$c.C)+sqrt((1-input$ICC.C)*input$s.C)))
    min.theta.1=theta.1(ratio=opt.ratio.1,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    
    # calculate minimal value theta.2
    opt.ratio.2=sqrt( (input$phi2.T/input$phi2.C) * ((input$c.T)/(input$c.C)) )
    min.theta.2=theta.2(opt.ratio.2,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C)
    
    # calculate compound optimal design for each lambda
    lambda=seq(0,1,by=0.01)
    w.1=(1-lambda)/min.theta.1
    w.2=(lambda)/min.theta.2
    
    opt.ratio.comp=(sqrt((w.1*input$var.Y.T*input$ICC.T+w.2*input$phi2.T)*input$c.T) + sqrt(w.1*input$var.Y.T*(1-input$ICC.T)*input$s.T)) /(sqrt((w.1*input$var.Y.C*input$ICC.C+w.2*input$phi2.C)*input$c.C) + sqrt(w.1*input$var.Y.C*(1-input$ICC.C)*input$s.C))
    opt.prop.comp=opt.ratio.comp/(1+opt.ratio.comp)

    # plot compound optimal design
    plot(lambda,opt.prop.comp,type="l",ylim=c(0,1),xlab="Lambda",ylab="Optimal proportion of budget to intervention")
    title(main="Compound optimal design")
  })
  
  
  ################################################################################################################################
  ### compund optimal design: optimal design for cluster size in both condition as a function of lambda
  ################################################################################################################################    
  output$plot5 <- renderPlot({
    # calculate minimal value theta.1
    opt.ratio.1=(sqrt(input$var.Y.T)*(sqrt(input$ICC.T*input$c.T)+sqrt((1-input$ICC.T)*input$s.T))) / (sqrt(input$var.Y.C)*(sqrt(input$ICC.C*input$c.C)+sqrt((1-input$ICC.C)*input$s.C)))
    min.theta.1=theta.1(ratio=opt.ratio.1,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    
    # calculate minimal value theta.2
    opt.ratio.2=sqrt( (input$phi2.T/input$phi2.C) * ((input$c.T)/(input$c.C)) )
    min.theta.2=theta.2(opt.ratio.2,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C)
    
    # calculate compound optimal design for each lambda
    lambda=seq(0,1,by=0.01)
    w.1=(1-lambda)/min.theta.1
    w.2=(lambda)/min.theta.2
    
    opt.n1T.comp=sqrt((w.1*input$var.Y.T*(1-input$ICC.T))/(w.1*input$var.Y.T*input$ICC.T+w.2*input$phi2.T)   * (input$c.T)/(input$s.T))
    opt.n1C.comp=sqrt((w.1*input$var.Y.C*(1-input$ICC.C))/(w.1*input$var.Y.C*input$ICC.C+w.2*input$phi2.C)   * (input$c.C)/(input$s.C))
    
    # plot compound optimal design
    plot(lambda,opt.n1T.comp,type="l",ylim=c(0,250),xlab="Lambda",ylab="Optimal cluster size")
    lines(lambda,opt.n1C.comp,lty=2)
    legend(0.2, 240, legend=c("Intervention condition", "Control condition"),lty=1:2, cex=0.8,box.lty = 0)
    title(main="Compound optimal design")
  })
  
  
  
  
  

  ################################################################################################################################
  ### compund optimal design: efficiencies as a function of lambda
  ################################################################################################################################    
  
  output$plot6 <- renderPlot({
    # calculate minimal value theta.1
    opt.ratio.1=(sqrt(input$var.Y.T)*(sqrt(input$ICC.T*input$c.T)+sqrt((1-input$ICC.T)*input$s.T))) / (sqrt(input$var.Y.C)*(sqrt(input$ICC.C*input$c.C)+sqrt((1-input$ICC.C)*input$s.C)))
    min.theta.1=theta.1(ratio=opt.ratio.1,var.Y.T=input$var.Y.T,var.Y.C=input$var.Y.C,ICC.T=input$ICC.T,ICC.C=input$ICC.C,B=input$B,c.T=input$c.T,c.C=input$c.C,s.T=input$s.T,s.C=input$s.C)
    
    # calculate minimal value theta.2
    opt.ratio.2=sqrt( (input$phi2.T/input$phi2.C) * ((input$c.T)/(input$c.C)) )
    min.theta.2=theta.2(ratio=opt.ratio.2,phi2.T=input$phi2.T,phi2.C=input$phi2.C,B=input$B,c.T=input$c.T,c.C=input$c.C)
    
    # calculate compound optimal design for each lambda
    lambda=seq(0,1,by=0.01)
    w.1=(1-lambda)/min.theta.1
    w.2=(lambda)/min.theta.2
    
    opt.ratio.comp=(sqrt((w.1*input$var.Y.T*input$ICC.T+w.2*input$phi2.T)*input$c.T) + sqrt(w.1*input$var.Y.T*(1-input$ICC.T)*input$s.T)) /(sqrt((w.1*input$var.Y.C*input$ICC.C+w.2*input$phi2.C)*input$c.C) + sqrt(w.1*input$var.Y.C*(1-input$ICC.C)*input$s.C))
    opt.prop.comp=opt.ratio.comp/(1+opt.ratio.comp) 
    
    B.T=input$B*opt.prop.comp
    B.C=input$B*(1-opt.prop.comp)
    
    opt.n1T.comp=sqrt((w.1*input$var.Y.T*(1-input$ICC.T))/(w.1*input$var.Y.T*input$ICC.T+w.2*input$phi2.T)   * (input$c.T)/(input$s.T))
    opt.n1C.comp=sqrt((w.1*input$var.Y.C*(1-input$ICC.C))/(w.1*input$var.Y.C*input$ICC.C+w.2*input$phi2.C)   * (input$c.C)/(input$s.C))
    
    opt.KT.comp=B.T/(opt.n1T.comp*input$s.T+input$c.T)
    opt.KC.comp=B.C/(opt.n1C.comp*input$s.C+input$c.C)    
       
    # calculate RE for cluster level outcome
    theta.2.comp=input$phi2.T/opt.KT.comp+input$phi2.C/opt.KC.comp  
    RE.c=min.theta.2/theta.2.comp
    
    # calculate RE for individual level outcome
    theta.1.comp=((opt.n1T.comp-1)*input$ICC.T+1)*input$var.Y.T/(opt.n1T.comp*opt.KT.comp)+((opt.n1C.comp-1)*input$ICC.C+1)*input$var.Y.C/(opt.n1C.comp*opt.KC.comp)
    RE.s=min.theta.1/theta.1.comp
    
    # plot compound optimal design
    plot(lambda,RE.c,type="l",ylim=c(0,1),xlab="Lambda",ylab="Efficiency")
    lines(lambda,RE.s,lty=2)
    title(main="Compound optimal design")
    legend(0.2, 0.25, legend=c("Cluster level outcome", "Individual level outcome"),lty=1:2, cex=0.8,box.lty = 0)
  })
  
  })




