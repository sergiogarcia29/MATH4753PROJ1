#' pareto
#'
#' @param x
#'
#' @return Creates a pareto barplot using a replicated vector form by frequencies with their respective names
#' @export
#'
#' @examples
#' freq=c(15,8,63,20) # Vector of frequencies of different types of robot limbs
#' names=c("None","Both","LegsO","WheelsO") # Respective names of the robot libmbs according to their frequencies
#' l=rep(names,freq) # Replicated vector
#' pareto(l) #Pareto diagram output
#'
pareto<-function(x,mn="Pareto barplot",...){  # x is a vector
  x.tab=table(x)
  xx.tab=sort(x.tab, decreasing=TRUE,index.return=FALSE)
  cumsum(as.vector(xx.tab))->cs
  length(x.tab)->lenx
  bp<-barplot(xx.tab,ylim=c(0,max(cs)),las=2)
  lb<-seq(0,cs[lenx],l=11)
  axis(side=4,at=lb,labels=paste(seq(0,100,length=11),"%",sep=""),las=1,line=-1,col="Blue",col.axis="Red")
  for(i in 1:(lenx-1)){
    segments(bp[i],cs[i],bp[i+1],cs[i+1],col=i,lwd=2)
  }
  title(main=mn,...)
}
