pollutantmean <- function(directory, pollutant, id = 1:332){
    recordsCount <- 0
    totalPollutantValue <- 0
    for(monitorId in id){
        fileName <- paste(1000 + monitorId, sep='')
        fileName <- substr(fileName,2,4)
        fileNameWithExtension <- paste(fileName,"csv",sep = ".")
        csvPath <- paste(directory,fileNameWithExtension,sep='/')
        csvData <- read.csv(csvPath)
        
        pollutantData <- csvData[,c(pollutant)]
        notNAPollutantData <- pollutantData[!is.na(pollutantData)]
        
        recordsCount <- recordsCount + length(notNAPollutantData)
        totalPollutantValue <- totalPollutantValue + sum(notNAPollutantData)
    }
    totalPollutantValue / recordsCount
}

complete <- function(directory, id = 1:332){
    retVal <- matrix(nrow=0, ncol=2)
    for(monitorId in id){
        fileName <- paste(1000 + monitorId, sep='')
        fileName <- substr(fileName,2,4)
        fileNameWithExtension <- paste(fileName,"csv",sep = ".")
        csvPath <- paste(directory,fileNameWithExtension,sep='/')
        csvData <- read.csv(csvPath)
        
        completeData <- subset(csvData, !is.na(sulfate) & !is.na(nitrate))
        retVal <- rbind(retVal, list(monitorId,nrow(completeData)))
    }
    retVal <- data.frame(retVal)
    colnames(retVal) <- c("id","nobs")
    retVal
}

corr <- function(directory,threshold = 0){
    retVal = numeric(0)
    
    cpl <- complete(directory)
    cplWithThresHold <- subset(cpl, nobs > threshold)
    
    monitorIds <- cplWithThresHold[,c("id")]
    
    for(monitorId in monitorIds){
        fileName <- paste(1000 + monitorId, sep='')
        fileName <- substr(fileName,2,4)
        fileNameWithExtension <- paste(fileName,"csv",sep = ".")
        csvPath <- paste(directory,fileNameWithExtension,sep='/')
        csvData <- read.csv(csvPath)
        
        completeData <- subset(csvData, !is.na(sulfate) & !is.na(nitrate))
        retVal <- c(retVal, cor(completeData[,c("sulfate")],completeData[,c("nitrate")]))
    }
    retVal
}









