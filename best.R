## Finding the best hospital in a state

## The best() function takes two arguments: the 2-character abbreviated name of
## a state and an outcome name. The function reads the
## outcome-of-care-measures.csv file and returns a character vector with the
## name of the hospital that has the best (i.e. lowest) 30-day mortality for the
## specified outcome in that state.

## Handling ties. The rankall function should handle ties in the 30-day mortality
## rates in the same way that the rankhospital function handles ties.

best <- function(state, outcome ) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv",
                        colClasses = "character")
        outcomes <- as.data.frame(cbind(data[, 2],   # hospital
                                        data[, 7],   # state
                                        data[, 11],  # heart attack
                                        data[, 17],  # heart failure
                                        data[, 23]), # pneumonia
                                  stringsAsFactors = FALSE)
        colnames(outcomes) <- c("hospital", "state", "heart attack",
                                "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        ## States must be 2-character abbreviation, i.e. for Virginia best("VA",)
        if(!state %in% outcomes$state){
                stop('invalid state')
        }
        else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        }
        ## Return hospital name in that state with lowest 30-day death rate
        else {
        state1 <- which(outcomes[,"state"] == state)
        state2 <- outcomes[state1,]
        outcomeRates <- as.numeric(state2[, eval(outcome)])
        min_val <- min(outcomeRates, na.rm = TRUE)
        result  <- state2[, "hospital"][which(outcomeRates == min_val)]
        output  <- result[order(result)]
        }
        
        return(output[1])
}

## The hospital name is the name provided in the Hospital.Name variable.
## The outcomes can be one of \heart attack", \heart failure", or \pneumonia".
## Hospitals that do not have data on a particular outcome should be excluded
## from the set of hospitals when deciding the rankings.

## The function checks the validity of its arguments.
## If an invalid state value is passed to best, the function should throw an
## error via the stop function with the exact message "\invalid state".
## If an invalid outcome value is passed to best, the function should throw an
## error via the stop function with the exact message "\invalid outcome".

## Handling ties.
## If there is a tie for the best hospital for a given outcome,
## then the hospital names should be sorted in alphabetical order
## and the first hospital in that set should be chosen
## (i.e. if hospitals \b", \c", and \f" are tied for best,
##  then hospital \b" should be returned).