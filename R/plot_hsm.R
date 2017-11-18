#' Plotting objects of class \code{'hsmclass'}
#'
#' @description
#' Function \code{plot.hsmclass} provides four plots that describe the
#' behaviour of the Simulated Annealing Heuristic applied by \code{\link{HSMclass}}.
#' The plots can be used to optimize the heuristics' behaviour by altering the optimization
#' parameters \code{coolfactor} and \code{InitTemp} in \code{\link{HSMclass}}.
#'
#' @param x object of class \code{'hsmclass'} containing results of a
#'          optimized classification scheme
#' @param ... additional arguments, so far ignored.
#'
#' @examples
#'
#' hsm<- HSMclass(refdata.gr, predictions.gr, nclasses = 6,
#'                  iterations = 1000, coolfactor=0.99, InitTemp = 80,
#'                  weight.norefs = 2, weight.classwidth = 2)
#' plot(hsm)
#'
#' @export
#' @import zoo
#' @importFrom graphics abline legend par plot points

plot.hsmclass<- function(x, ...){



  # closure for calculating moving avarage:
  # ma <- function(x,windsize){na.omit(as.numeric(filter(x,rep(1/windsize,windsize), sides=2)))}

  acc.dat<- data.frame(p.acc=x$p.acc, p=x$p, sol.worse = x$deltaF > 0, sol.worse.acc = 1*(x$p.acc<= x$p))
  acc.dat[acc.dat$sol.worse == FALSE, "sol.worse.acc"]<- NA


  # dataset of pacc and p for cases of new solution = worse than best solution:
  #acc.dat.solworse<- acc.dat[acc.dat$sol.worse == TRUE, ]

  # compute moving average of p.acc.worse (prob. of accepting a worse solution):
  #p.acc.worse<- ma(x = acc.dat.solworse$sol.worse.acc, windsize = round(nrow(acc.dat.solworse)*0.05))
  p.acc.worse<- rollapply(acc.dat$sol.worse.acc, round(nrow(acc.dat)*0.2), mean, align = "center", na.rm = TRUE)



par(mfrow=c(2,2))
plot(x$Solutions~seq(1,x$settings$iterations,1),type="l",xlab="Iterations", ylab="Solution", ...)
plot(x$Temperature~seq(1,x$settings$iterations,1),type="l",xlab="Iterations", ylab="Temperature", ...)
legend("topright", legend = paste("coolingfactor:",x$settings$coolfactor,sep=""), bty="n", ...)
plot(x$deltaF[-1]~seq(1,x$settings$iterations-1,1),type="l",xlab="Iterations", ylab="deltaF", ...)
abline(h = 0, col="red")
plot(p.acc.worse~seq(1,x$settings$iterations,length.out=length(p.acc.worse))
     ,type="l",xlab="Iterations", ylab="P[acc.worse]", ...)
points(p.acc.worse~seq(1,x$settings$iterations,length.out=length(p.acc.worse)), pch=20, cex=0.7, ...)



}
