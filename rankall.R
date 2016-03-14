
rankall <- function(outcome, num = "best") {
        
        retVal <- matrix(nrow=0, ncol=2)
        
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
        
        states <- sort(unique(outcomeData[[stateColNumber]]))
        if ( !(state %in% states) ) {
                stop("invalid state")
        }        
        
        #ELSE:
        outcomeWoNa <- subset(outcomeData, outcomeData[outcomeColNumber] != "NA")
        
        outcomeWoNa <- subset(outcomeData, grepl("\\-?[0-9]+(\\.[0-9]+)?", outcomeData[[outcomeColNumber]]))
        
        outcomeWoNa[, outcomeColNumber] <- as.numeric(outcomeWoNa[, outcomeColNumber])
        
        
        for( state in states ){
                outcomeByState <- subset(outcomeWoNa, outcomeWoNa[[stateColNumber]] == state)
                orderedOutcomeByState <- outcomeByState[order(outcomeByState[[outcomeColNumber]], outcomeByState[[hospitalNameColNumber]]), ]
                
                #Get by rank
                stdNum = num
                if( stdNum == "best" ){
                        stdNum = 1
                } else if( stdNum == "worst" ){
                        stdNum = nrow(orderedOutcomeByState)
                }
                hospitalName <- orderedOutcomeByState[stdNum, hospitalNameColNumber]                
                retVal <- rbind(retVal, list(hospitalName, state))
        }
        
        retVal <- data.frame(retVal)
        colnames(retVal) <- c("hospital","state")
        
        #Order by state
        #retVal <- retVal[order(retVal[["state"]]),]
        
        retVal
}