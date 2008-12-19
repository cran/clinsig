clinsig<-function(pre.x,post.x,dys.mct=NA,func.mct=NA,dys.disp=NA,
 func.disp=NA,mct="mean",disp="sd",disp.mult=c(2,2),dys.qnts=NA,
 dir.effect=NA,xlim=range(c(pre.x,post.x),na.rm=TRUE),
 ylim=range(c(pre.x,post.x),na.rm=TRUE),pch=par("pch"),
 do.plot=TRUE,point.id=NA,...) {

 # if the dysfunctional score MCT is not supplied, use the pre-MCT
 if(is.na(dys.mct)) dys.mct<-do.call(mct,list(pre.x,na.rm=TRUE))
 if(is.na(dys.disp)) dys.disp<-do.call(disp,list(pre.x,na.rm=TRUE))
 # calculate the MCT of the post-intervention scores
 post.mct<-do.call(mct,list(post.x,na.rm=TRUE))
 crit<-rep(NA,3)
 sigsums<-rep(0,3)
 # The difference between func.mct and dys.mct takes precedence over dir.effect
 if(is.na(func.mct)) {
  if(is.na(dir.effect)) dir.effect<-sign(post.mct-dys.mct)
 }
 else {
  dir.effect<-sign(func.mct-dys.mct)
  # if the functional dispersion measure is there, weight the MCTs
  if(is.na(func.disp)) crit[3]<-(func.mct+dys.mct)/2
  else {
   crit[3]<-(dys.disp*func.mct+dys.mct*func.disp)/(func.disp+dys.disp)
   crit[2]<-func.mct-disp.mult[length(disp.mult)]*func.disp*dir.effect
  }
 }
 # calculate the "a" criterion, the point in the dysfunctional distribution beyond
 # which the subject has moved to the tail of the distribution nearest the
 # functional distribution
 if(is.na(dys.qnts[1])) crit[1]<-dys.mct+disp.mult[1]*dys.disp*dir.effect
 else crit[1]<-quantile(pre.x,dys.qnts)[(dir.effect+3)/2]
 if(dir.effect > 0) sigsums[1]<-sum(post.x > crit[1],na.rm=TRUE)
 else sigsums[1]<-sum(post.x < crit[1],na.rm=TRUE)
 a.pass<-ifelse(dir.effect < 0,post.mct < crit[1],post.mct > crit[1])
 if(!is.na(crit[2])) {
  if(dir.effect > 0) sigsums[2]<-sum(post.x > crit[2],na.rm=TRUE)
  else sigsums[2]<-sum(post.x < crit[2],na.rm=TRUE)
  b.pass<-ifelse(dir.effect < 0,post.mct < crit[2],post.mct > crit[2])
 }
 else b.pass<-FALSE
 if(!is.na(crit[3])) {
  if(dir.effect > 0) sigsums[3]<-sum(post.x > crit[3],na.rm=TRUE)
  else sigsums[3]<-sum(post.x < crit[3],na.rm=TRUE)
  c.pass<-ifelse(dir.effect < 0,post.mct < crit[3],post.mct > crit[3])
 }
 else c.pass<-FALSE
 clinsignif<-list(pre.x=pre.x,post.x=post.x,crit=crit,sigsums=sigsums,
  pre.mct=dys.mct,post.mct=post.mct,func.mct=func.mct,mct=mct,disp=disp,
  post.n=sum(!is.na(post.x)),passed=c(a.pass,b.pass,c.pass))
 class(clinsignif)<-"clinsig"
 if(do.plot) plot(clinsignif,xlim=xlim,ylim=ylim,pch=pch,point.id=point.id,...)
 return(clinsignif)
}

plot.clinsig<-function(x,main="Clinical significance plot",
 xlab="Pre-intervention score",ylab="Post-intervention score",
 xlim=NA,ylim=NA,pch=par("pch"),point.id=NA,...) {

 oldmar<-par("mar")
 par(mar=c(5,4,4,4))
 if(is.na(xlim[1])) xlim<-range(c(x$pre.x,x$post.x))
 if(is.na(ylim[1])) ylim<-range(c(x$pre.x,x$post.x))
 if(is.na(point.id[1]))
  plot(x$pre.x,x$post.x,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,pch=pch,...)
 else {
  plot(x$pre.x,x$post.x,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,type="n",...)
  text(x$pre.x,x$post.x,point.id)
  # set pch to 1 to get pch=10 below
  pch<-1
 }
 cutofflaby<-x$crit
 cutoffrange<-range(x$crit)
 minlabspace<-diff(par("usr")[3:4]/10)
 if(length(x$crit[!is.na(x$crit)]) > 1) {
  # if any crit are so close that the labels would overlap
  if(any(diff(x$crit[!is.na(x$crit)]) < minlabspace)) {
   minindex<-which(x$crit == cutoffrange[1])[1]
   cutofflaby[minindex]<-x$crit[minindex]-minlabspace/4
   maxindex<-which(x$crit == cutoffrange[2])[1]
   cutofflaby[maxindex]<-x$crit[maxindex]+minlabspace/4
  }
 }
 abline(h=x$crit[1],lty=3)
 mtext(paste(" a",signif(x$crit[1],2),sep="="),4,at=cutofflaby[1],las=1)
 if(!is.na(x$crit[2])) {
  abline(h=x$crit[2],lty=3)
  mtext(paste(" b",signif(x$crit[2],2),sep="="),4,at=cutofflaby[2],las=1)
 }
 if(!is.na(x$crit[3])) {
  abline(h=x$crit[3],lty=3)
  mtext(paste(" c",signif(x$crit[3],2),sep="="),4,at=cutofflaby[3],las=1)
 }
 if(!is.na(x$func.mct)) {
  abline(h=x$func.mct,lty=1)
  mtext(paste(" norm",signif(x$func.mct,2),sep="="),4,at=x$func.mct,las=1)
 }
 points(x$pre.mct,x$post.mct,pch=pch+9,cex=1.5)
 par(mar=oldmar)
}

hist.clinsig<-function(x,breaks=NA,main="",xlab="Score",ylab="Frequency",
 xlim=NA,ylim=NA,col=2:3,border=par("fg"),...) {
 xrange<-range(c(x$pre.x,x$post.x),na.rm=TRUE)
 if(is.na(breaks)) breaks<-floor(xrange[1]):ceiling(xrange[2])
 prexcounts<-table(cut(x$pre.x,breaks))
 postxcounts<-table(cut(x$post.x,breaks))
 if(is.na(ylim[1])) ylim<-c(0,max(c(prexcounts,postxcounts)))
 breakint<-breaks[2]-breaks[1]
 barwidth<-0.33*breakint
 xaxpos<-(breaks[1]+0.5*breakint):(breaks[length(breaks)]-0.5*breakint)
 if(is.na(xlim[1])) xlim<-c(xaxpos[1]-barwidth,xaxpos[length(xaxpos)]+barwidth) 
 plot(0,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim,type="n",axes=FALSE,...)
 axis(1,labels=names(prexcounts),at=xaxpos)
 axis(2)
 rect(xaxpos-barwidth,rep(0,length(breaks)),xaxpos,prexcounts,col=col[1],border=border)
 rect(xaxpos,rep(0,length(breaks)),xaxpos+barwidth,postxcounts,col=col[2],border=border)
 par(xpd=TRUE)
 Mht<-strheight("M")
 segments(x$pre.mct,-0.1,x$pre.mct,ylim[2],lty=2,lwd=2)
 text(x$pre.mct,ylim[2]+Mht,"pre",adj=c(0.5,0))
 segments(x$post.mct,-0.1,x$post.mct,ylim[2],lty=2,lwd=2)
 shiftpost<-abs(x$pre.mct-x$post.mct < strwidth("post"))
 text(x$post.mct,ylim[2]+(1+shiftpost)*Mht,"post",adj=c(0.5,0))
 if(!is.na(x$func.mct)) {
  segments(x$func.mct,-0.1,x$func.mct,ylim[2],lty=1,lwd=2)
  text(x$func.mct,ylim[2]+Mht,"norm",adj=c(0.5,0))
 }
 par(xpd=FALSE)
 shiftb<-abs(x$crit[1]-x$crit[2] < strwidth("m")) ||
  abs(x$crit[2]-x$crit[3] < strwidth("m"))
 if(!is.na(x$crit[1])) {
  segments(x$crit[1],-0.1,x$crit[1],ylim[2],lty=3,lwd=2)
  text(x$crit[1],ylim[2],"a",adj=c(0.5,0))
 }
 if(!is.na(x$crit[2])) {
  segments(x$crit[2],-0.1,x$crit[2],ylim[2],lty=3,lwd=2)
  text(x$crit[2],ylim[2]+shiftb*Mht,"b",adj=c(0.5,0))
 }
 if(!is.na(x$crit[3])) {
  segments(x$crit[3],-0.1,x$crit[3],ylim[2],lty=3,lwd=2)
  text(x$crit[3],ylim[2],"c",adj=c(0.5,0))
 }
}

print.clinsig<-function(x,...) {
 cat("\nClinical significance test\n",x$post.n,"post-assessments\n")
 cat(x$sigsums[1],"clients passed the \"a\" criterion of",signif(x$crit[1],2),"\n")
 if(!is.na(x$crit[2]))
  cat(x$sigsums[2],"clients passed the \"b\" criterion of",signif(x$crit[2],2),"\n")
 if(!is.na(x$crit[3]))
  cat(x$sigsums[3],"clients passed the \"c\" criterion of",signif(x$crit[3],2),"\n")
 cat("The",x$mct,"of the post-intervention scores met:\n")
 if(any(x$passed)) {
  if(x$passed[1]) cat("the a criterion\n",sep="")
  if(x$passed[2]) cat("the b criterion\n",sep="")
  if(x$passed[3]) cat("the c criterion\n",sep="")
 }
 else cat("no criteria\n")
}