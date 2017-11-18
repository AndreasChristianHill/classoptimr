
confint_oaa<- function(n.refs, n.correct.classified, method="exact", conf.level=0.95){

  #------------------------------------------------------------------------------#
  #
  # Function calculates the lower and upper confident intervals of a given
  # the overall accuracy (INPUT) and a defined Confidence-Level (INPUT)
  #
  # further INPUT-parameters: - number of reference objects (n.ref)
  #                           - number of correctly classified objects / pixels
  #
  # requires package Hmisc for computing the exact confidence intervals of the binomial distribution
  #
  # 13.03.2014
  # A. Hill
  #------------------------------------------------------------------------------#


  # determine quantil to compute lower and upper conf.interval-limits:
    twosided.conf<- conf.level + (1-conf.level)/2
    p.est<- n.correct.classified / n.refs

  # compute lower and upper Confidence Limits with normal approximation (method="norm"):
  if (method=="norm"){
    qn<- qnorm(p=twosided.conf,mean=0,sd=1)
    lower.ci<- p.est - qn * sqrt((1/n.refs) * (n.correct.classified / n.refs) * (1-(n.correct.classified / n.refs)))
    upper.ci<- p.est + qn * sqrt((1/n.refs) * (n.correct.classified / n.refs) * (1-(n.correct.classified / n.refs)))
  }

  if (method=="exact"){
  # compute correct lower and upper Confidence Limits (method = "exact"):
    exact.cis<- binconf(x=n.correct.classified, n=n.refs, alpha= 1-conf.level, method="exact",return.df=TRUE)
    lower.ci<- exact.cis$Lower
    upper.ci<- exact.cis$Upper
  }

    # Create Output:
      conf.ints<- data.frame(matrix(data=c(p.est*100, lower.ci*100, upper.ci*100),ncol=3))
      colnames(conf.ints)<- c("OverallAccuracy", paste("Lower",conf.level*100,"CI",sep=""),
                              paste("Upper",conf.level*100,"CI",sep=""))
      return(conf.ints)
}

