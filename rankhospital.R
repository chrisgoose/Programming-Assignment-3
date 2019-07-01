## Ranking hospitals by outcome in a state

## The rankhospital() function takes three arguments: the 2-character
## abbreviated name of a state (state), an outcome (outcome), and the ranking of
## a hospital in that state for that outcome (num). The function reads the
## outcome-of-care-measures.csv file and returns a character vector with the
## name of the hospital that has the ranking specified by the num argument. For
## example, the call rankhospital("MD", "heart failure", 5) would return a
## character vector containing the name of the hospital with the 5th lowest
## 30-day death rate for heart failure. The num argument can take values "best",
## "worst", or an integer indicating the ranking (smaller numbers are better).
## If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on
## a particular outcome should be excluded from the set of hospitals when
## deciding the rankings.

## Handling ties. The rankall function should handle ties in the 30-day mortality
## rates in the same way that the rankhospital function handles ties.

rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
        outcomesData <- cbind.data.frame(data[,2],                #Hospital
                                         data[,7],                #State
                                         as.numeric(data[,11]),   #Heart attack
                                         as.numeric(data[,17]),   #Heart failure
                                         as.numeric(data[,23]),   #Pneumonia
                                         stringsAsFactors = FALSE)
        colnames(outcomesData) <- c("Hospital", "State", "Heart Attack",
                                    "Heart Failure", "Pneumonia")

## Check that state and outcome is valid
        if(!any(state == outcomesData$State)) {
                stop("invalid state")
        }
        else if((outcome %in% c("heart attack", "heart failure", "pneumonia"))
                == FALSE) {
                stop(print("invalid outcome"))
        }

        
## Filter and sort by outcome rate
                stateData <- subset(outcomesData, State == state)
        if(outcome == "heart attack") {
                stateOutcome <- cbind.data.frame(stateData[,1],stateData[,3])
                stateRanked  <- na.omit(stateOutcome[order(stateOutcome[,2],
                                                   stateOutcome[,1]),])
        }
        else if(outcome == "heart failure") {
                stateOutcome <- cbind.data.frame(stateData[,1],stateData[,4])
                stateRanked  <- na.omit(stateOutcome[order(stateOutcome[,2],
                                                   stateOutcome[,1]),])
        }
        else if (outcome == "pneumonia") {
                stateOutcome <- cbind.data.frame(stateData[,1],stateData[,5])
                stateRanked  <- na.omit(stateOutcome[order(stateOutcome[,2],
                                                   stateOutcome[,1]),])
        }
        

## if num argument is best or worst)
        if(num == "best") {
                num <- 1
        }
        else if(num == "worst") {
                num <- nrow(stateRanked)
        }
        else {
                num <- num
        }

                        
## Return hospital name in that state with the given rank 30-day death rate
        return(stateRanked[num,])
                
}
