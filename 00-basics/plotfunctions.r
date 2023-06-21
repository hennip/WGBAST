# bx: function for creating times series bocplots from bugs/jags output

bx<-function(v,first,last,nt,name1,name2,...){
beg<-1
end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
print(beg)
print(end)
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end
bxp(z,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),axes=FALSE,...)
axis(2)
axis(1,at=beg:end,lab=seq(from=first,to=last,by=nt))
box()
points(z$stats[3,],type="l")
}



#and with user specified x-axis limits
bx1<-function(v,first,last,nt,name1,name2,...){
beg<-1
end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
print(beg)
print(end)
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end
bxp(z,axes=FALSE,...)
axis(2)
axis(1,at=beg:end,lab=seq(from=first,to=last,by=nt))
box()
points(z$stats[3,],type="l")
}
##############################################

#and with no x-axis

bx2<-function(v,first,last,nt,name1,name2,...){
beg<-1
end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
print(beg)
print(end)
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end
bxp(z,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),axes=FALSE,...)
axis(2)
box()
points(z$stats[3,],type="l")
}

#as bx2 but no line between boxes

bx2nl<-function(v,first,last,nt,name1,name2,bwd,...){
beg<-1
end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
print(beg)
print(end)
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end
bxp(z,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),axes=FALSE,at=seq(from=first,to=last,by=nt),boxwex=bwd,...)

axis(2)
box()

}

################################
#and with no x-axis and user ylims...

bx3<-function(v,first,last,nt,name1,name2,ylim1,ylim2,...){
beg<-1
end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
print(beg)
print(end)
c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
z<-boxplot(c,plot=FALSE)

for(i in beg:end){
var<-paste(name1,i,name2,sep="")
z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
}
#z$names=beg:end

bxp(z,ylim=c(ylim1,ylim2),cex.lab=1.25,axes=FALSE,...)
axis(2)
box()
points(z$stats[3,],type="l")
}

########################################################################
####################################################################################
##############grey median lines
bxg<-function(v,first,last,nt,name1,name2,...){
  
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
  

  bxp(z,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),axes=FALSE,...)
  
  axis(2)
  
  axis(1,at=1:end,lab=seq(from=first,to=last,by=nt))
  
  box()
  
  points(z$stats[3,],type="l",col="gray60")
  
}


bx2g<-function(v,first,last,nt,name1,name2,beg=1,end=length(seq(from=first,to=last,by=nt)),ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),las=1,...){
  
  #beg<-1
  #end<-length(seq(from=first,to=last,by=nt)) #(last-first+1)*nt
  
  print(beg)
  
  print(end)
  
  c<-matrix(c(beg:end),nrow=1,ncol=end-beg+1)
  
  z<-boxplot(c,plot=FALSE)
  
  
  
  for(i in beg:end){
    
    var<-paste(name1,i,name2,sep="")
    
    z$stats[,i-beg+1]<-quantile(v[,var],p=c(0.025,0.25,0.5,0.75,0.975))
    
  }
  
  #z$names=beg:end
  
  bxp(z,ylim=ylim,axes=FALSE,...)
  
  axis(2)
  
  axis(1,at=1:end,lab=seq(from=first,to=last,by=nt))
  
  box()
  
  points(z$stats[3,],type="l",col="gray60")
  
}

#no x axis
bx3g<-function(v,first,last,nt,name1,name2,ylim=c(min(0,1.2*min(z$stats)),1.2*max(z$stats)),...){

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

    
  bxp(z,ylim=ylim,axes=FALSE,...)
  axis(2)

  axis(1,at=beg:end,lab=rep("",times=length(beg:end)))

  box()

  points(z$stats[3,],type="l",col="gray60")

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

  axis(1,at=beg:end,lab=rep("",times=length(beg:end)))

  box()

}


