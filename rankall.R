## The function "rankall" takes two arguments: an outcome name (outcome) and a hospital ranking
## (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num. 

rankall <- function(outcome, num = "best"){
        dataRead <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if(outcome == "pneumonia"){
                j <- 1
                id <- 1:4706
                cont <- c()
                for(i in id){
                        if(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[i] == "Not Available"){
                                cont[j] <- i
                                j <- j + 1
                        }
                }
                dataRead <- dataRead[-cont,]
                
                j <- 1
                state <- c()
                for(i in 1:nrow(dataRead)) {
                        state[j] <- dataRead$State[i]
                        j <- j + 1
                }
                
                state <- sort(unique(state))
               
                k <- 1
                p <- c()
                hp <- c()
                st <- c()
                for(i in 1:length(state)) {
                        for (j in 1:nrow(dataRead)) {
                                if(state[i] == dataRead$State[j]){
                                        p[j] <-as.numeric(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[j])
                                        hp[j] <- dataRead$Hospital.Name[j]
                                        st[j] <- dataRead$State[j]
                                }
                        }
                }
                x <- cbind(st,hp, p)
                colnames(x) <- c("State","Hospital.Name","Rate")
                x <- as.data.frame(x)
                x$Rate <- as.numeric(as.character(x$Rate))
                x$Hospital.Name <- as.character(x$Hospital.Name)
                x$State <- as.character(x$State)
                x <- x[order(x[,1], x[,3],x[,2]),]
                
                new_data <- split(x, x$State)
                if(num == "best"){
                        new_data <- lapply(new_data, function(x) head(x, 1)) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num == "worst"){
                        new_data <- lapply(new_data, function(x) tail(x, 1))
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num > nrow(x)){
                        return(NA)
                }
                
                else{
                        new_data <- lapply(new_data, function(x) x[num,]) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                colnames(df) <- c("State","Hospital","Rate")
                df <- subset(df, select = c("Hospital", "State"))
                df
                
        }
        
        else if(outcome == "heart attack"){
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
                
                j <- 1
                state <- c()
                for(i in 1:nrow(dataRead)) {
                        state[j] <- dataRead$State[i]
                        j <- j + 1
                }
                
                state <- sort(unique(state))
                
                k <- 1
                p <- c()
                hp <- c()
                st <- c()
                for(i in 1:length(state)) {
                        for (j in 1:nrow(dataRead)) {
                                if(state[i] == dataRead$State[j]){
                                        p[j] <-as.numeric(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[j])
                                        hp[j] <- dataRead$Hospital.Name[j]
                                        st[j] <- dataRead$State[j]
                                }
                        }
                }
                x <- cbind(st,hp, p)
                colnames(x) <- c("State","Hospital.Name","Rate")
                x <- as.data.frame(x)
                x$Rate <- as.numeric(as.character(x$Rate))
                x$Hospital.Name <- as.character(x$Hospital.Name)
                x$State <- as.character(x$State)
                x <- x[order(x[,1], x[,3],x[,2]),]
                
                new_data <- split(x, x$State)
                if(num == "best"){
                        new_data <- lapply(new_data, function(x) head(x, 1)) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num == "worst"){
                        new_data <- lapply(new_data, function(x) tail(x, 1)) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num > nrow(x)){
                        return(NA)
                }
                
                else{
                        new_data <- lapply(new_data, function(x) x[num,]) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                colnames(df) <- c("State","Hospital","Rate")
                df <- subset(df, select = c("Hospital", "State"))
                df
        }
        
        
        else if(outcome == "heart failure"){
                j <- 1
                id <- 1:4706
                cont <- c()
                for(i in id){
                        if(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[i] == "Not Available"){
                                cont[j] <- i
                                j <- j + 1
                        }
                }
                dataRead <- dataRead[-cont,]
                
                j <- 1
                state <- c()
                for(i in 1:nrow(dataRead)) {
                        state[j] <- dataRead$State[i]
                        j <- j + 1
                }
                
                state <- sort(unique(state))
                
                k <- 1
                p <- c()
                hp <- c()
                st <- c()
                for(i in 1:length(state)) {
                        for (j in 1:nrow(dataRead)) {
                                if(state[i] == dataRead$State[j]){
                                        p[j] <-as.numeric(dataRead$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[j])
                                        hp[j] <- dataRead$Hospital.Name[j]
                                        st[j] <- dataRead$State[j]
                                }
                        }
                }
                x <- cbind(st,hp, p)
                colnames(x) <- c("State","Hospital.Name","Rate")
                x <- as.data.frame(x)
                x$Rate <- as.numeric(as.character(x$Rate))
                x$Hospital.Name <- as.character(x$Hospital.Name)
                x$State <- as.character(x$State)
                x <- x[order(x[,1], x[,3],x[,2]),]
                
                new_data <- split(x, x$State)
                if(num == "best"){
                        new_data <- lapply(new_data, function(x) head(x, 1)) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num == "worst"){
                        new_data <- lapply(new_data, function(x) tail(x, 1)) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                else if(num > nrow(x)){
                        return(NA)
                }
                
                else{
                        new_data <- lapply(new_data, function(x) x[num,]) 
                        df <- data.frame(matrix(unlist(new_data), nrow=length(new_data), byrow=T))
                }
                
                colnames(df) <- c("State","Hospital","Rate")
                df <- subset(df, select = c("Hospital", "State"))
                df
        }
        
        else{
                stop(" invalid outcome")
        }
        
}