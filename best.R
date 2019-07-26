##The function called "best" that take two arguments: the 2-character abbreviated name of a state and an
##outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
##with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
##in that state.

best <- function(state, outcome){
        dataRead <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        dataRead <- dataRead[complete.cases(dataRead), ] 
        
        if(!(state %in% dataRead$State)){
                stop("invalid state")
        }
        
        else if(outcome == "heart attack"){
                j <- 1
                id <- 1:4706
                cont <- c()
                for(i in id){
                        if(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i] == "Not Available"){
                                cont[j] <- i
                                j <- j + 1
                        }
                }
                
                dataRead <- dataRead[-cont,]
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
                j <- 1
                id <- 1:4706
                cont <- c()
                for(i in id){
                        if(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i] == "Not Available"){
                                cont[j] <- i
                                j <- j + 1
                        }
                }
                
                dataRead <- dataRead[-cont,]
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
                j <- 1
                id <- 1:4706
                cont <- c()
                for(i in id){
                        if(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i] == "Not Available"){
                                cont[j] <- i
                                j <- j + 1
                        }
                }
                
                dataRead <- dataRead[-cont,]
                
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
                stop(" invalid outcome")
        }
        hospname
}