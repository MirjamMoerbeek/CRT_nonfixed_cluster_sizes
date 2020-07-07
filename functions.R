theta.2=function(ratio,phi2.T,phi2.C,B,c.T,c.C,s.T,s.C,n.T,n.C)
{
  K.C=B / ( ratio*(c.T+s.T*n.T)+(c.C+s.C*n.C))
  K.T=ratio*K.C
  theta.2=phi2.T/K.T+phi2.C/K.C  
  return(theta.2)
}  


theta.1=function(ratio,n.T,n.C,var.Y.T,var.Y.C,ICC.T,ICC.C,B,c.T,c.C,s.T,s.C)
{
  K.C=B / ( ratio*(c.T+s.T*n.T)+(c.C+s.C*n.C))
  K.T=ratio*K.C
  theta.1=((n.T-1)*ICC.T+1)*var.Y.T/(n.T*K.T)+((n.C-1)*ICC.C+1)*var.Y.C/(n.C*K.C)
  return(theta.1)
}

theta.comp=function(ratio)
{
  K.C=B / ( ratio*(c.T+s.T*n.T)+(c.C+s.C*n.C))
  K.T=ratio*K.C
  theta.comp=lambda*( phi2.T/K.T+phi2.C/K.C ) /min.theta.c+ (1-lambda) * (((n.T-1)*ICC.T+1)*var.Y.T/(n.T*K.T)+((n.C-1)*ICC.C+1)*var.Y.C/(n.C*K.C))/min.theta.s
  return(theta.comp)
}
