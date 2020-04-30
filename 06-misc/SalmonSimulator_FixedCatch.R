#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



simulate=function(input){
  
  
  ## "input" is a list of parameters, controlled by the Shiny interface
  
  go=input$go
  K=input$K
  K_ratio=input$K_ratio
  k=c()
  alpha=input$alpha
  alpha_ratio=input$alpha_ratio
  RCV=input$RCV
  phiR=input$Rphi
  SS=-log(input$SS)
  SCV=input$SCV
  phiS=input$Sphi
  Sratio=input$Sratio
  M=input$M
  MCV=input$MCV
  phiM=input$Mphi
  fec=input$fec
  HR1=input$HR1
  #HR2=input$HR2
  catch2=input$catch2
  HRylim=input$HRylim
  
  hr=c()
  smolt_lag=c(0,0,0.3,0.4,0.3)
  spawn_lag=c(0,0.2,0.6,0.2)
  smolts=c()
  Smolts=array(NA,dim=c(input$rep,input$Y))
  HR=array(NA,dim=c(input$rep,input$Y))
  Effort=array(NA,dim=c(input$rep,input$Y))
  pEffort=array(NA,dim=c(input$rep,input$Y))
  Catch=array(NA,dim=c(input$rep,input$Y))
  catch=c()
  recruits=c()
  spawners=c()
  Spawners=array(NA,dim=c(input$rep,input$Y))
  catch1=c()###
  eggs=c()
  Eggs=c()
  Adults=c()
  egg_surv=c()
  R_err=c()
  Mps=c()
  ss=c()
  m=c()
  Y=input$Y
  CP=Y*input$cp
  epr=c()
  eprH=c()
  Epr=c()
  R0=c()
  SP0=c()
  SP0_y=c()
  SP0H=c()
  R0H=c()
  SP0_y_rep=array(NA,dim=c(input$rep,input$Y))
  R0_y=c()
  R0_y_rep=array(NA,dim=c(input$rep,input$Y))
  SP75=c()
  M74_1=input$M74_1
  M74_2=input$M74_2
  M74sd=input$M74sd
  M74phi=input$M74phi
  M74=c()
  M74_rep=array(NA,dim=c(input$rep,input$Y))
  SS_rep=array(NA,dim=c(input$rep,input$Y))
  
  log_AR<-function(length,initial,mean,CV,autoc){   # generic function for log AR(1) process
    x=c()
    M=log(mean)-0.5*log(CV^2+1)
    sd=sqrt(log(CV^2+1))
    v_e=(1-autoc^2)*sd^2
    if(is.na(initial)) x[1]=exp(rnorm(1,M,sd))
    else x[1]=initial
    for(i in 2:length){
      x[i]=exp(rnorm(1,log(autoc*(x[i-1])+(1-autoc)*mean)-0.5*v_e,sqrt(v_e)))
    }
    return(x)
  }
  logit_AR<-function(length,initial,mean,sd,autoc){  # generic function for logit AR(1) process
    ilogit<-function(theta){
      return(exp(theta)/(1+exp(theta)))
    }
    logit<-function(p){return(log(p/(1-p)))}
    x=c()
    M=logit(mean)
    v_e=(1-autoc^2)*sd^2
    if(is.na(initial)) x[1]=ilogit(rnorm(1,M,sd))
    else x[1]=initial
    for(i in 2:length){
      x[i]=ilogit(rnorm(1,autoc*logit(x[i-1])+(1-autoc)*logit(mean)-0.5*v_e,sqrt(v_e)))
    }
    return(x)
  }
 
  
for(j in 1:input$rep){  # for each simulation
#j<-1  
  # predict vital rates using AR-models
  
  M74[1:round(input$Y*input$cp)]=logit_AR(length(1:round(input$Y*input$cp)),NA,M74_1,M74sd,M74phi)
  M74[(round(input$Y*input$cp)+1):input$Y]=logit_AR(length(M74[(round(input$Y*input$cp)+1):input$Y]),M74[round(input$Y*input$cp)],M74_2,M74sd,M74phi)
 
  Mps=log_AR(Y,NA,SS,SCV,phiS)
  
  m=log_AR(Y,NA,M,MCV,phiM)
  R_err=log_AR(Y,NA,1,RCV,phiR)
  
  
  EprH=c()
  
  # first four years of "empty river"
  
  for(y in 1:4){
    smolts[y]=0
    recruits[y]=0
    catch1[y]=0
    Adults[y]=0
    spawners[y]=0
    eggs[y]=0
    Eggs[y]=0
    egg_surv[y]=0
  }
  
  # year 5, a set amount of spawners enters the river
  
  for(y in 5:5){
    smolts[y]=0
    recruits[y]=0
    catch1[y]=0
    Adults[y]=0
    spawners[y]=input$ini_spawn 
    eggs[y]=spawners[y]*fec
    Eggs[y]=sum(eggs[(y-4):y]*rev(smolt_lag))
    egg_surv[y]=K/(K/alpha+Eggs[y])
  }
  
  for(y in 6:Y){  # rest of the years
    
    
    smolts[y]=Eggs[y-1]*egg_surv[y-1]*R_err[y]    
    if(y>CP){
      ss[y]=exp(-Mps[y]*Sratio)
    }else{
      ss[y]=exp(-Mps[y])
    }
    recruits[y]=smolts[y]*ss[y]

    if(y>CP){     # years after the Change point, "ratio" parameters control the change relative to intial period
      hr[y]<-catch2/recruits[y]
      Epr[y]=exp(-SS*Sratio)*exp(-M)*fec
      #hr[y]=HR2
      EprH[y]=(1-hr[y])*Epr[y]*(1-M74_2)
      k[y]=K*K_ratio
      R0[y]=k[y]*(Epr[y]*(1-M74_2)-1/(alpha*alpha_ratio))/(Epr[y]*(1-M74_2))
      epr[y]=ss[y]*exp(-m[y])*fec*(1-M74[y])
      R0_y[y]=k[y]*(epr[y]-1/(alpha*alpha_ratio))/epr[y]
      R0H[y]=k[y]*(EprH[y]-1/(alpha*alpha_ratio))/EprH[y]
      SP75[y]=R0[y]*0.75/(fec*(1-M74_2)*alpha*alpha_ratio*(1-0.75*(1-1/(alpha*alpha_ratio*Epr[y]*(1-M74_2)))))
      SP0[y]=R0[y]*Epr[y]/fec
      SP0H[y]=R0H[y]*EprH[y]/(fec*(1-M74_2))
    }
    else{         # years before the change point
      Epr[y]=exp(-SS)*exp(-M)*fec                  
      hr[y]=HR1
      catch1[y]=recruits[y]*hr[y]
      EprH[y]=(1-hr[y])*Epr[y]*(1-M74_1)
      k[y]=K
      R0[y]=k[y]*(Epr[y]*(1-M74_1)-1/(alpha))/(Epr[y]*(1-M74_1))
      epr[y]=ss[y]*exp(-m[y])*fec*(1-M74[y])
      R0_y[y]=k[y]*(epr[y]-1/(alpha))/epr[y]
      R0H[y]=k[y]*(EprH[y]-1/(alpha))/EprH[y]
      SP75[y]=R0[y]*0.75/(fec*(1-M74_1)*alpha*(1-0.75*(1-1/(alpha*Epr[y]*(1-M74_1)))))
      SP0[y]=R0[y]*Epr[y]/(fec)
      SP0H[y]=R0H[y]*EprH[y]/(fec*(1-M74_1))
    }
    
    
    #SP0H[y]=R0H[y]*EprH[y]/(fec*(1-M74[y]))
    #SP0[y]=R0[y]*Epr/(fec*(1-M74[y]))
    SP0_y[y]=R0_y[y]*epr[y]/(fec*(1-M74[y]))
    Adults[y]=recruits[y]*(1-hr[y])*exp(-m[y])
    spawners[y]=sum(Adults[(y-3):y]*rev(spawn_lag))     # spawners are a weighted average of adults from previous years
    #if(spawners[y]<2){
    #  eggs[y]<-0
    #}else{
      eggs[y]=spawners[y]*fec*(1-M74[y])
    #}
      Eggs[y]=sum(eggs[(y-4):y]*rev(smolt_lag))   # Eggs that produce smolts for a given year are weighted average of eggs laid in previous years
    
    if(y>CP){
      egg_surv[y]=k[y]/(k[y]/(alpha*alpha_ratio)+Eggs[y])
      catch[y]=catch2
      }else{
        egg_surv[y]=k[y]/(k[y]/(alpha)+Eggs[y])
      catch[y]=catch1[y]
      }
  }
  
  Smolts[j,]=smolts
  Spawners[j,]=spawners
  SP0_y_rep[j,]=SP0_y
  R0_y_rep[j,]=R0_y
  M74_rep[j,]=M74
  SS_rep[j,]=ss
  HR[j,]=hr
  Catch[j,]=catch
  
  q=0.1
  Effort[j,]=log(1-HR[j,])/(-q)
  for(y in 1:Y){
    if(y==1)pEffort[j,y]=1
    else    pEffort[j,y]=Effort[j,y]/Effort[j,input$cp*input$Y+1]
  }
}
  
  return(list(smolts=Smolts,spawners=Spawners,R0=R0,R0_y=R0_y_rep,
              SP0=SP0,SP0_y=SP0_y_rep,R_err=R_err,m=m,HR=HR,Catch=Catch,
              Effort=Effort,pEffort=pEffort,
              R0H=R0H,SP0H=SP0H,SP75=SP75,M74=M74_rep,SS=SS_rep))
  
}


### Ends the population dynamics

### Rest of the code implements the shiny GUI

smooth_lag<-function(x,lag){
  m=c()
  for(i in 1:length(x)){
    d=max(1,i-(lag-1))
    m[i]=mean(x[d:i])
  }
  return(m)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Salmon simulator"),
   hr(),
 
  
   fluidRow(column(6,
   
   titlePanel("Population parameters"),
   fluidRow(column(2,"Parameter"),column(2,"Mean"),column(2,"CV"),column(2,"AutoC"),column(2,"Ratio"),column(2,"Notes")),
   
   #### K ##############
   
   fluidRow(
   
      column(2,"K: carrying capacity"),
      column(2,
         sliderInput("K",
                     NA,
                     min = 5000,
                     max = 15000,
                     value = 10000)
      ),
      column(2,"NA"),
      column(2,"NA"),
      column(2,sliderInput("K_ratio",
                           NA,
                           min = 0.1,
                           max = 2,
                           value = 1)),
      column(2,"")),
   
   ### Alpha
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Alpha: maximum survival of eggs"),
     column(2,
            sliderInput("alpha",
                        NA,
                        min = 0.001,
                        max = 0.05,
                        value = 0.01)
     ),
     column(2,"NA"),
     column(2,"NA"),
     column(2,sliderInput("alpha_ratio",
                          NA,
                          min = 0.1,
                          max = 2,
                          value = 1)),
     column(2,"")),
   
   ### recruitment error
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Recruitment residual"),
     column(2,"1"),
     column(2,sliderInput("RCV",
                        NA,
            min = 0.001,
            max = 1,
            value = 0.25)),
     column(2, sliderInput("Rphi",
                        NA,
            min = 0,
            max = 0.99,
            value = 0)),
     column(2,"1"),
     column(2,"Multiplies expected recruitment")),
   
   ### Post smolt survival
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Post-smolt survival"),
     column(2,sliderInput("SS",
                          NA,
                          min = 0.001,
                          max = 1,
                          value = 0.2)),
     column(2,sliderInput("SCV",
                          NA,
                          min = 0.001,
                          max = 1,
                          value = 0.001)),
     column(2, sliderInput("Sphi",
                           NA,
                           min = 0,
                           max = 0.99,
                           value = 0)),
     column(2,sliderInput("Sratio",
                          NA,
                          min = 0.1,
                          max = 2,
                          value = 1)),
     column(2,"Ratio increases MORTALITY")),
   
   ### Adult natural mortality
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Adult natural mortality"),
     column(2,sliderInput("M",
                          NA,
                          min = 0.001,
                          max = 1,
                          value = 0.1)),
     column(2,sliderInput("MCV",
                          NA,
                          min = 0.001,
                          max = 1,
                          value = 0.001)),
     column(2, sliderInput("Mphi",
                           NA,
                           min = 0,
                           max = 0.99,
                           value = 0)),
     column(2,"1"),
     column(2,"Instantaneous rate")),
   
   ### fecundity
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Fecundity"),
     column(2,sliderInput("fec",
                          NA,
                          min = 500,
                          max = 5000,
                          value = 2000)),
     column(2,"NA"),
     column(2, "NA"),
     column(2,"1"),
     column(2,"Eggs per spawner")),
   
   
   
   #### Initial spawners
   
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"Initial spawners"),
     column(2,sliderInput("ini_spawn",
                          NA,
                          min = 10,
                          max = 5000,
                          value = 100)),
     column(2,"NA"),
     column(2, "NA"),
     column(2,"1"),
     column(2,"These establish the population")),
   fluidRow(
     # Sidebar with a slider input for number of bins 
     column(2,"M74 mortality"),
     column(2,sliderInput("M74_1",
                          "Mean, period 1",
                          min = 0.001,
                          max = 1,
                          value = 0.001)),
     column(2,sliderInput("M74_2",
                          "Mean, period 2",
                          min = 0.001,
                          max = 1,
                          value = 0.001)),
     column(2, sliderInput("M74sd",
                           "M74 SD",
                           min = 0.001,
                           max = 0.99,
                           value = 0.001)),
     column(2, sliderInput("M74phi",
                           "M74 AutoC",
                           min = 0,
                           max = 0.99,
                           value = 0)),
     column(2,"Mortality as % of eggs")),
   hr(),
   titlePanel("Fishing settings"),
   
   fluidRow(            column(4,sliderInput("HR1",
                                 "Harvest rate before change point",
                                 min=0,max=1,value=0)),
                        # column(4,sliderInput("HR2",
                        #                      "Harvest rate after change point",
                        #                      min=0,max=1,value=0)),
                        column(4,sliderInput("catch2",
                                             "TAC after change point",
                                             min=0,max=1000,value=10)),
                        column(4,sliderInput("HRylim",
                                             "Upper limit for HR graph",
                                             min=0.001,max=1,value=0.8)),
                        column(4,"")
   ),
   
   
      hr(),   
   titlePanel("Simulation settings"),
   fluidRow(column(2,sliderInput("Y",
                  "Number of years",
                  min=50,
                  max=500,
                  value=100)),
            column(2,sliderInput("rep",
                                 "Number of simulations",
                                 min=1,max=100,value=10)),
            column(2,sliderInput("cp",
                                 "Change point location",
                                 min=0.001,
                                 max=1,
                                 value=0.5)),
            
            
            column(6,"")
        ),
   hr(),
   titlePanel("Vital rates"),
   plotOutput("Mps"),
   plotOutput("M74")
   
      
   ),
   column(6,
      # Show a plot of the generated distribution
      actionButton("go", "Update!"),
        
         plotOutput("SmoltPlot"),
      plotOutput("SpPlot"),
      plotOutput("HRPlot"),
      plotOutput("catchPlot"),
      plotOutput("effortPlot"),
      checkboxInput("average", label = "Show average", value = T),
      sliderInput("lag",
                  "Years to average",
                  min=1,max=30,value=5),
      plotOutput("BHplot1"),
      plotOutput("BHplot2")
      
   )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    sim=reactive({
    s=simulate(input)
    s
    })
   
   output$SmoltPlot <- renderPlot({
     sim=sim()
     par(mfrow=c(1,1))
     plot(sim$smolts[1,],type="l",ylab="Smolts",xlab="Years",ylim=c(0,1.1*max(sim$smolts,sim$R0,sim$R0_y,na.rm = 1)),lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$smolts[i,],type="l",lwd=3, col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)))
     }
     if(input$average==TRUE){
     for(i in 1:input$rep){
       points(smooth_lag(x=sim$smolts[i,],lag=input$lag),type="l",col=rgb(1,0,1,alpha=1/(1+0.1*input$rep)),lwd=3)  
     }
     }
     for(i in 1:input$rep){
       points(sim$R0_y[i,],type="l",col=rgb(0,0,1,alpha=1/(1+0.1*input$rep)))  
     }
     
     points(sim$R0,type="l",col="red")
     points(sim$R0H,type="l",col="pink")
     
     points(sim$R0*0.75,type="l",col="grey")
     legend("topleft",legend=c("Smolts","R0","Snapshot R0","0.75R0","R_Harvest"),lwd=c(1,1,1,1,1),col=c("black","red","blue","grey","pink"))
    })
   
   output$SpPlot <- renderPlot({
     sim=sim()
     plot(sim$spawners[1,],type="l",ylab="Spawners",xlab="Years",
          ylim=c(0,1.1*max(sim$spawners,sim$SP0,sim$SP0_y,na.rm=1)),lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$spawners[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
     if(input$average==TRUE){
     for(i in 1:input$rep){
       points(smooth_lag(sim$spawners[i,],input$lag),type="l",col=rgb(1,0,1,alpha=1/(1+0.1*input$rep)),lwd=3)  
     }
     }
     points(sim$SP0,type="l",col="red")
     points(sim$SP0H,type="l",col="pink")
     points(sim$SP75,type="l",col="grey")
     
     for(i in 1:input$rep){
     points(sim$SP0_y[i,],type="l",col=rgb(0,0,1,alpha=1/(1+0.1*input$rep)))
     }
     legend("topleft",legend=c("Spawners","Sp0","Snapshot Sp0","Spawners@0.75R0","Sp_Harvest"),lwd=c(1,1,1,1,1),col=c("black","red","blue","grey","pink"))
   })

   output$HRPlot <- renderPlot({
     sim=sim()
     plot(sim$HR[1,],type="l",ylab="Harvest rate",xlab="Years",
          ylim=c(0,input$HRylim),
          #c(0,1.1*max(sim$HR, na.rm=1)),
          lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$HR[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
     if(input$average==TRUE){
       for(i in 1:input$rep){
         points(smooth_lag(x=sim$HR[i,],lag=input$lag),type="l",
                col=rgb(1,0,1,alpha=1/(1+0.1*input$rep)),lwd=3)  
       }
     }
     
   })

   output$catchPlot <- renderPlot({
     sim=sim()
     plot(sim$Catch[1,],type="l",ylab="Catch",xlab="Years",
          ylim=#c(0,input$HRylim),
          c(0,1.1*max(sim$Catch, na.rm=1)),
          lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$Catch[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
     if(input$average==TRUE){
       for(i in 1:input$rep){
         points(smooth_lag(x=sim$Catch[i,],lag=input$lag),type="l",
                col=rgb(1,0,1,alpha=1/(1+0.1*input$rep)),lwd=3)  
       }
     }
     
   })
   
   output$effortPlot <- renderPlot({
     sim=sim()
     plot(sim$Effort[1,],type="l",ylab="Effort",xlab="Years",
          ylim=#c(0,input$HRylim),
            c(0,1.1*max(sim$Effort, na.rm=1)),
          lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$Effort[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
     if(input$average==TRUE){
       for(i in 1:input$rep){
         points(smooth_lag(x=sim$Effort[i,],lag=input$lag),type="l",
                col=rgb(1,0,1,alpha=1/(1+0.1*input$rep)),lwd=3)  
       }
     }
     
   })
   
 output$BHplot1<-renderPlot({
     sim=sim()
     k=round(input$Y*input$cp)-2
     Spawners=seq(0,max(sim$spawners,na.rm = 1),by=max(sim$spawners,na.rm = 1)/100)
     Eggs=Spawners*input$fec*(1-input$M74_1)
     Smolts=Eggs*input$K/(input$K/input$alpha+Eggs)
     plot(Spawners,Smolts,type="l",lwd=3,ylim=c(0,input$K*1.1),main="Stock-recruitment, period 1")
     points(sim$SP0[k],sim$R0[k],pch=19,col="red",cex=2)
     abline(h=input$K)
     text(x=0,y=input$K*1.03,"K")
     abline(h=sim$R0[k])
     text(x=0,y=sim$R0[k]*1.04,"R0")
     abline(h=sim$R0[k]*0.75)
     text(x=0,y=sim$R0[k]*0.75*1.05,"0.75R0")
     abline(v=sim$SP0[k])
   })
   
   output$BHplot2<-renderPlot({
     sim=sim()
     k=round(input$Y*input$cp)+5
     Spawners=seq(0,max(sim$spawners,na.rm = 1),by=max(sim$spawners,na.rm = 1)/100)
     Eggs=Spawners*input$fec*(1-input$M74_2)
     Smolts=Eggs*input$K*input$K_ratio/(input$K*input$K_ratio/(input$alpha*input$alpha_ratio)+Eggs)
     plot(Spawners,Smolts,type="l",lwd=3,ylim=c(0,input$K*1.1),main="Stock-recruitment, period 2")
     points(sim$SP0[k],sim$R0[k],pch=19,col="red",cex=2)
     abline(h=input$K*input$K_ratio)
     text(x=0,y=input$K*input$K_ratio*1.03,"K")
     abline(h=sim$R0[k])
     text(x=0,y=sim$R0[k]*1.04,"R0")
     abline(h=sim$R0[k]*0.75)
     text(x=0,y=sim$R0[k]*0.75*1.05,"0.75R0")
     abline(v=sim$SP0[k])
   })
   
   output$Mps<-renderPlot({
     sim=sim()
     plot(sim$SS[1,],type="l",ylab="Post-smolt survival",xlab="Years",ylim=c(0,1.1*max(sim$SS,na.rm=1)),lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$SS[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
   })
   
   output$M74<-renderPlot({
     sim=sim()
     plot(sim$M74[1,],type="l",ylab="M74 Mortality",xlab="Years",ylim=c(0,1.1*max(sim$M74,na.rm=1)),lwd=3, col=rgb(0,0,0,alpha=1/input$rep))
     abline(v=input$Y*input$cp)
     for(i in 1:input$rep){
       points(sim$M74[i,],type="l",col=rgb(0,0,0,alpha=1/(1+0.1*input$rep)),lwd=3)
     }
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

