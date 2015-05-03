rankhospital <- function(state, outcome,num) {
  ## Read outcome data
  outcomes<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  possibleoutcomes<-c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  states<-outcomes[,"State"]
  if (!state%in%outcomes[,"State"]){
    stop("invalid state")  
  }
  
  if (!outcome %in% possibleoutcomes) {
    stop("invalid outcome")
    
  }
  if (outcome=="heart attack"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
  if (outcome=="heart failure"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
  if (outcome=="pneumonia"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
  bystate<-subset(outcomes,State==state,select=c(2,11,17,23))
 
  ##Sort by name
  bystate<-bystate[order(bystate$Hospital.Name),]
  
  #Change all rates to numerics
  suppressWarnings(bystate[,2]<-as.numeric(bystate[,2]))
  suppressWarnings(bystate[,3]<-as.numeric(bystate[,3]))
  suppressWarnings(bystate[,4]<-as.numeric(bystate[,4]))
  
  #Sort by rate (Ascendind)
  bystate<-bystate[order(bystate[outcome],na.last=TRUE),]
  
  ##Create subset for completed cases
  relist<-subset(bystate,select=c("Hospital.Name",outcome))
  relist<-relist[complete.cases(relist),]
  
  ## Return hospital name in that state with the given rank
  ## 30-dat death rate
  if (num=="worst"){return(relist[[nrow(relist),1]])}
  if (num=="best"){return(relist[[1,1]])}
  if (num>nrow(relist)){return(NA)}
    b<-relist[[num,1]]
  return(as.character(b))
  
  
  
  
  
}
