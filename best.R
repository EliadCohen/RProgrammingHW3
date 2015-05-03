best <- function(state, outcome) {
  ## Read outcome data
  outcomes<-read.csv("outcome-of-care-measures.csv")
  possibleoutcomes<-c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  states<-outcomes[,"State"]
  if (!state%in%outcomes[,"State"]){
    return(paste("Error in best(",dQuote(state),", ",dQuote(outcome),") : invalid state"))
  
  }
    
   if (!outcome %in% possibleoutcomes) {
     return(paste("Error in best(",dQuote(state),", ",dQuote(outcome),") : invalid outcome"))
     
   }
    if (outcome=="heart attack"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"}
    if (outcome=="heart failure"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"}
    if (outcome=="pneumonia"){outcome<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"}
    bystate<-subset(outcomes,State==state,select=c(2,11,17,23))
    bystate<-bystate[order(bystate$Hospital.Name),]
    bystate[,2]<-as.numeric(bystate[,2])
    bystate[,3]<-as.numeric(bystate[,3])
    bystate[,4]<-as.numeric(bystate[,4])
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    b<-bystate[which.min(bystate[,outcome]),]
    as.character(b[[1]])
    
   
  
  
  
}
