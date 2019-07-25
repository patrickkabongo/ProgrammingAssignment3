best <- function(state, outcome){
        dataRead <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        j <- 1
        id <- 1:4706
        cont <- c()
        for(i in id){
                if(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i] == "Not Available"){
                        cont[j] <- i
                        j <- j + 1
                }
        }
        
        dataRead <- dataRead[-cont,]
        
        if(!(state %in% dataRead$State)){
                message("invalid state")
        }
        
        
        else if(outcome == "heart attack"){
                low <- 999
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                data <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i])
                                if(data < low){
                                        low <- data
                                        hospname <- dataRead$Hospital.Name[i]
                                }
                        }
                 
                }
        }
        
        else if(outcome == "heart failure"){
                low <- 999
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                data <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i])
                                if(data < low){
                                        low <- data
                                        hospname <- dataRead$Hospital.Name[i]
                                }
                        }
                        
                }
        }
        
        else if(outcome == "pneumonia"){
                low <- 999
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                data <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i])
                                if(data < low){
                                        low <- data
                                        hospname <- dataRead$Hospital.Name[i]
                                }
                        }
                        
                }
        }
        
        else{
                message(" invalid outcome")
        }
        hospname
}