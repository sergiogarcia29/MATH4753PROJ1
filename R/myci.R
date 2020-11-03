#' myci function (Confidence interval for the mean of a single sample)
#'
#' @param x sample
#' @param ci desired confidence interval
#' @param ...
#'
#' @return returns a range of the desired confidence interval
#' @export
#'
#' @examples
myci=function(x,ci=0.95,...){
  n=length(x)
  con=1-((1-ci)/2)
  t=qt(con,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci

}
