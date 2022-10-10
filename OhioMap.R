OhioMap <-function(data, ncol=5, figmain="", digits=5, type="e",
                   lower=NULL, upper=NULL) {
  if (is.null(lower)) lower <- min(data)
  if (is.null(upper)) upper <- max(data) 

  if (type=="q"){p <- seq(0,1,length=ncol+1)
                 br <- round(quantile(data,probs=p),2)}
  if (type=="e"){br <- round(seq(lower,upper,length=ncol+1),2)}
  shading <- gray((ncol-1):0/(ncol-1))
  data.grp <- findInterval(data,vec=br,rightmost.closed=T,all.inside=T)
  data.shad <- shading[data.grp]
  map("county", "ohio", fill=TRUE, col=data.shad)
  leg.txt<-paste("[",br[ncol],",",br[ncol+1],"]",sep="")
  for(i in (ncol-1):1){
    leg.txt<-append(leg.txt,paste("[",br[i],",",br[i+1],")",sep=""),)
  }
  leg.txt<-rev(leg.txt)
  legend(-81.7,39.4,legend=leg.txt,fill=shading,bty="n",ncol=1,cex=.8,xpd = TRUE)
  title(main=figmain,cex=1.5)
  invisible()
}

