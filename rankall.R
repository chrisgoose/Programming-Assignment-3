## Ranking hospitals in all states

## The rankall function takes two arguments: an outcome name (outcome) and a
## hospital ranking (num).  The function reads the outcome-of-care-measures.csv
## file and returns a 2-column data frame containing the hospital in each state
## that has the ranking specified in num. For example the function call
## rankall("heart attack", "best") would return a data frame containing the names
## of the hospitals that are the best in their respective states for 30-day heart
## attack death rates. The function should return a value for every state
## (some may be NA). The first column in the data frame is named hospital,
## which contains the hospital name, and the second column is named state,
## which contains the 2-character abbreviation for the state name. Hospitals that
## do not have data on a particular outcome should be excluded from the set of
## hospitals when deciding the rankings.

## Handling ties. The rankall function should handle ties in the 30-day mortality
## rates in the same way that the rankhospital function handles ties.

rankall <- function(outcome, num = "best") {
## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                         colClasses = "character")
        outcomesData <- cbind.data.frame(data[,2],                #Hospital
                                         data[,7],                #State
                                         as.numeric(data[,11]),   #Heart attack
                                         as.numeric(data[,17]),   #Heart failure
                                         as.numeric(data[,23]),   #Pneumonia
                                         stringsAsFactors = FALSE)
        colnames(outcomesData) <- c("hospital", "state", "heart attack",
                                    "heart failure", "pneumonia")
        
## Check that outcome is valid
        if((outcome %in% c("heart attack", "heart failure", "pneumonia"))
           == FALSE) {
                stop(print("invalid outcome"))
        }
          
## When num argument is best or worst)
        if(num == "best") {
                num <- 1
        }
        else if(is.numeric(num)) {
                num <- num
        }
               
## For each state, find the hospital of the given rank Return a data frame with
## the hospital names and the (abbreviated) state name
        by_state <- with(outcomesData, split(outcomesData, state))
        ranked <- list()
        if (is.numeric(num)) {
                for (i in seq_along(by_state)){
                        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                             by_state[[i]][, "hospital"]), ]
                        ranked[[i]]  <-  c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
                }
        }
        else if (num == "worst") {
                for (i in seq_along(by_state)){
                        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                             by_state[[i]][, "hospital"], 
                                                             decreasing = TRUE), ]
                        ranked[[i]]  <-  c(by_state[[i]][1, c("hospital", "state")])
                }
        }
        else {
                stop('invalid num')
        }
        result <- do.call(rbind, ranked)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
return(output)
}
