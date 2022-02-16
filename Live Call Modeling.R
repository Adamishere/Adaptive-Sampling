#Demonstration of Adapative Call Allocation
#AKA, 
#Dynamic Call Optimation, 
#Reinforcement Learning Response Rate Maximization
#Multi-Armed Adaptive Sampling
#Live Optimization


setwd("C:/Users/21509/Desktop/")
#Read In
mydat<-read.csv("balancedLL_1.csv")

#Collapsing cells for smaller table
mydat$hr_1[mydat$hour_1==9] <-9
mydat$hr_1[mydat$hour_1==10]<-9
mydat$hr_1[mydat$hour_1==11]<-11
mydat$hr_1[mydat$hour_1==12]<-11
mydat$hr_1[mydat$hour_1==13]<-13
mydat$hr_1[mydat$hour_1==14]<-13
mydat$hr_1[mydat$hour_1==15]<-15
mydat$hr_1[mydat$hour_1==16]<-15
mydat$hr_1[mydat$hour_1==17]<-17
mydat$hr_1[mydat$hour_1==18]<-17
mydat$hr_1[mydat$hour_1==19]<-19
mydat$hr_1[mydat$hour_1==20]<-19
mydat$hr_1[mydat$hour_1==21]<-19

#outcome of event
table(mydat$contact_1)

#Update Function - outcomes by dimensions into the function to update probabilities:
# i - ith row
# dim1 - String of Dimension 1 i.e., "Hour"
# dim2 - String of Dimension 2 i.e., "Day"
# outcome - Binary outcome variable [1/2]
#     1- Event
#     2- Non Event
# mydat - Dataframe with dim1, dim2, and outcome.

update<-function(i,dim1,dim2,outcome,mydat){
  #Document Dimensions for outcome for probability table
  d1<-mydat[i,dim1]
  d2<-mydat[i,dim2]

    if (mydat[i,outcome]==1){
      #Update Probabilities table
      prob[prob$dim1==d1 & prob$dim2==d2, "event"]<<-prob[prob$dim1==d1 & prob$dim2==d2, "event"]+1
      prob[prob$dim1==d1 & prob$dim2==d2, "attempts"]<<-prob[prob$dim1==d1 & prob$dim2==d2, "attempts"]+1
    }
     else if (mydat[i,outcome]==2){
      #Update Probabilities table
       prob[prob$dim1==d1 & prob$dim2==d2, "attempts"]<<-prob[prob$dim1==d1 & prob$dim2==d2, "attempts"]+1
     }
}

###################################
#Intializing Function Values      #
###################################

#Event probabilities design matrix
d1<-c(1:7)
d2<-c(9,11,13,15,17,19)
prob<-as.data.frame(expand.grid(d2,d1))
names(prob)<-(c("dim2","dim1"))

#Intialize Values in probability table
prob$event<-1
prob$attempts<-1
prob$prob<-1
arm<-"Explore!"


##########################
# Run to start simulation#
##########################

for(z in 1:1000){
  #Random Start
  #Select first case at random
  #if(z==1){samplist<-sample(1:nrow(mydat),1)}
  
  #Setup for the each iteration
  #Force Exploration:
  if(runif(1)<.75){
    arm<<-"Exploit!"
    
    #best time to call
    bdim12<-as.data.frame(prob[prob$prob==max(prob$prob),c('dim1','dim2')])
    
    #If multiple outcome select just 1
    bdimrow<<-sample(1:nrow(bdim12),1)
    bdim1x<<-bdim12[bdimrow,'dim1']
    bdim2x<<-bdim12[bdimrow,'dim2']
  }
  else {
    arm<<-"Explore!"
    bdim1<<-as.list(prob[,'dim1'])
    bdim2<<-as.list(prob[,'dim2'])
    
    #If multiple outcome select just 1
    bdim1x<<-sample(bdim1,1)
    bdim2x<<-sample(bdim2,1)
  }
  
  #select new record based on criteria selected
  subsamp<-(which((mydat[,dim1]==bdim1x & mydat[,dim2]==bdim2x)))
  samplist<<-(sample(subsamp,1))
  x<-samplist
  
  #run update function
  update(x,"day_1","hr_1","contact_1",mydat)
  #output results
  prob[prob$attempts>0,"prob"]<-round(prob[prob$attempts>0,"event"]/prob[prob$attempts>0,"attempts"],2)
  prob[is.na(prob$prob),"prob"]<-0
  prob$Indicator<-''
  prob[prob$prob==max(prob$prob),'Indicator']<-"***Best Time to Call!!***"
  cat("\014")
  print(arm)
  print(prob)
  
  
  #pause for dramatic effect
  #Sys.sleep(.15)
}
