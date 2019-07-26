## The function "rankhospital" that takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best"){
        dataRead <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        dataRead <- dataRead[complete.cases(dataRead), ] 
        
        if(!(state %in% dataRead$State)){
                stop("invalid state")
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
                
                j <- 1
                p <- c()
                hp <- c()
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                p[j] <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i])
                                hp[j] <- dataRead$Hospital.Name[i]
                                j <- j + 1
                        }
                
                }
                x <- cbind(hp, p)
                colnames(x) <- c("Hospital.Name","Rate")
                x <- as.data.frame(x)
                #x$Rate <- as.numeric(as.character(x$Rate))
                #x$Hospital.Name <- as.character(x$Hospital.Name)
                x <- x[order(x[,2], x[,1]),]
                
                if(num == "best"){
                        return(x$Hospital.Name[1])
                }
                else if(num == "worst"){
                        return(x$Hospital.Name[nrow(x)])
                }
                else if(num > nrow(x)){
                        return(NA)
                }
                else{
                        return(x$Hospital.Name[num])
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
                j <- 1
                p <- c()
                hp <- c()
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                p[j] <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i])
                                hp[j] <- dataRead$Hospital.Name[i]
                                j <- j + 1
                        }
                        
                }
                x <- cbind(hp, p)
                colnames(x) <- c("Hospital.Name","Rate")
                x <- as.data.frame(x)
                x$Rate <- as.numeric(as.character(x$Rate))
                x$Hospital.Name <- as.character(x$Hospital.Name)
                x <- x[order(x[,2], x[,1]),]
                
                if(num == "best"){
                        return(head(x$Hospital.Name,1))
                }
                else if(num == "worst"){
                        return(tail(x$Hospital.Name,1))
                }
                else if(num > nrow(x)){
                        return(NA)
                }
                else{
                        return(x$Hospital.Name[num])
                }
                
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
                
                j <- 1
                p <- c()
                hp <- c()
                for(i in 1:nrow(dataRead)) {
                        if(dataRead$State[i] == state){
                                p[j] <- as.numeric(dataRead$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[i])
                                hp[j] <- dataRead$Hospital.Name[i]
                                j <- j + 1
                        }
                        
                }
                x <- cbind(hp, p)
                colnames(x) <- c("Hospital.Name","Rate")
                x <- as.data.frame(x)
                x$Rate <- as.numeric(as.character(x$Rate))
                x$Hospital.Name <- as.character(x$Hospital.Name)
                x <- x[order(x[,2], x[,1]),]
                
                if(num == "best"){
                        return(head(x$Hospital.Name,1))
                }
                else if(num == "worst"){
                        return(tail(x$Hospital.Name,1))
                }
                else if(num > nrow(x)){
                        return(NA)
                }
                else{
                        return(x$Hospital.Name[num])
                }
        }
        
        else{
                stop(" invalid outcome")
        }
        
}