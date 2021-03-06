
rankhospital <- function(state, outcome, num = "best") {
        stateColNumber <- 7
        hospitalNameColNumber <- 2
        outcomeColNumber <- NULL
        if ( outcome == "heart attack" ) {
                outcomeColNumber <- 11
        } else if ( outcome == "heart failure" ) {
                outcomeColNumber <- 17
        } else if ( outcome == "pneumonia" ) {
                outcomeColNumber <- 23
        } else {
                stop("invalid outcome")
        }
        
        #Read outcome data
        outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        states <- unique(outcomeData[[stateColNumber]])
        if ( !(state %in% states) ) {
                stop("invalid state")
        }
        
        
        #ELSE:
        outcomeWoNa <- subset(outcomeData, outcomeData[outcomeColNumber] != "NA")
        
        outcomeWoNa <- subset(outcomeData, grepl("\\-?[0-9]+(\\.[0-9]+)?", outcomeData[[outcomeColNumber]]) & outcomeData[[stateColNumber]] == state )
        
        outcomeWoNa[, outcomeColNumber] <- as.numeric(outcomeWoNa[, outcomeColNumber])
        
        orderedOutcomeData <- outcomeWoNa[order(outcomeWoNa[[outcomeColNumber]], outcomeWoNa[[hospitalNameColNumber]]),]
        
        stdNum = num
        if( stdNum == "best" ){
                stdNum = 1
        } else if( stdNum == "worst" ){
                stdNum = nrow(orderedOutcomeData)
        }
        
        #Return
        orderedOutcomeData[stdNum, hospitalNameColNumber]
        #head(orderedOutcomeData[c(stateColNumber,hospitalNameColNumber,outcomeColNumber)])
}
