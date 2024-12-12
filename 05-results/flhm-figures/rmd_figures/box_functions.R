

bx90<-function(v,first,last,nt,ylim1,ylim2,beg=1,end=length(seq(from=first,to=last,by=nt)),las=1,...){

  print(beg)
  print(end)
  
  c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
  z<-boxplot(c,plot=FALSE)
  
  for(i in beg:end){  
    z$stats[,i-beg+1]<-quantile(v[i,],p=c(0.05,0.25,0.5,0.75,0.95))  
  }

  bxp(z,ylim=c(ylim1,ylim2),axes=FALSE,...)
  axis(2)
  axis(1,at=1:length(beg:end),lab=seq(from=first,to=last,by=nt))
  box()
  points(z$stats[3,],type="l")
  
}

bx90nl<-function(v,first,last,nt,ylim1,ylim2,bwd,beg=1,end=length(seq(from=first,to=last,by=nt)),las=1,...){

  print(beg)
  print(end)
  c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
  z<-boxplot(c,plot=FALSE)
  for(i in beg:end){
  z$stats[,i-beg+1]<-quantile(v[i,],p=c(0.05,0.25,0.5,0.75,0.95))
    }
  
  bxp(z,ylim=c(ylim1,ylim2),axes=FALSE,boxwex=bwd,...)
  axis(2)
  #axis(1,at=1:length(beg:end),lab=seq(from=first,to=last,by=nt))
  box() 
}

#as bx3g but no lines between boxes
bx3gnl<-function(v,first,last,nt,name1,name2,bwd,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),...){

  beg<-1

  end<-length(seq(from=first,to=last,by=nt)) 

  print(beg)

  print(end)

  c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)

  z<-boxplot(c,plot=FALSE)



  for(i in beg:end){

    var<-paste(name1,i,name2,sep="")

    z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))

  }

  bxp(z,ylim=ylim,axes=FALSE,at=seq(from=first,to=last,by=nt),boxwex=bwd,...)
  axis(2)

  #axis(1,at=beg:end,lab=rep("",times=length(beg:end)))

  box()

}


