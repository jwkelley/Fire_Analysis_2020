#This is a start of the Analysis project
#Rx Fire data from Emma\Outputs\RxFires Files\Fires_AllCalcs.csv
library(lubridate)

fireData <- read.csv("Fires_AllCalcs.csv", stringsAsFactors = FALSE)

#Examine data
head(fireData)
range(fireData$Year)
class(fireData$Date)
fireData$Date <- as_date(fireData$Date)
class(fireData$Date)
fireData$Month <- month(fireData$Date)
fireData$Day <- day(fireData$Date)
fireData$YDay <- yday(fireData$Date)

#Calculate State Specific Date range
dateRanges <- data.frame(State = character(), Start = as.Date(character()), End = as.Date(character()))
states <- unique(fireData$State)

for (i in 1:length(states)){
  i = 1
  state <- states[i]
  
  subset <- fireData[fireData$State == state,]
  minDay <- min(subset$YDay)
  maxDay <- max(subset$YDay)
  
  temp <- data.frame(State = state, Start = minDay, End = maxDay)
  dateRanges <- rbind(dateRanges, temp)
}

dOY <- seq.Date()
