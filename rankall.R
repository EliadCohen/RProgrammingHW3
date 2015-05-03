rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  ## Check that state and outcome are valid
  states<-outcomes[,"State"]
  if (!state%in%outcomes[,"State"]){
    stop("invalid state")  
  }
  possibleoutcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!outcome %in% possibleoutcomes) {
    stop("invalid outcome")
    
  }
  
  ## For each state, find the hospital of the given rank
  states<-unique(states)
  states<-sort(states)
  hnames<-c()
  source("rankhospital.R")
  for (st in states){
    hnames<-c(hnames,rankhospital(st,outcome,num))
  }
  
 
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  res<-data.frame(hnames,states)
  rownames(res)<-states
  colnames(res)<-c("hospital","state")
  return(res)
}
