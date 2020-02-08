library(lubridate)

#Import Weather data
dir <- "H:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/"
setwd(dir)

#Since date range is almost the full year in most cases the sample time will be a full year
fullRange <- data.frame(Date = seq(ymd('2007-01-01'), ymd('2014-12-31'), by = 'days'))

aqData <- c("PM2.5_AllYears.csv", "PM10_AllYears.csv", "Lead_AllYears.csv", 
            "CO_AllYears.csv", "NO2_AllYears.csv", "O3_AllYears.csv", "SO2_AllYears.csv")

yearStart <- seq(ymd('2007-01-01'), ymd('2014-12-31'), by = 'years')
yearEnd <- seq(ymd('2007-12-31'), ymd('2014-12-31'), by = 'years')
state.abbr <- c("AZ", "CA", "CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
state.num <- c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56)
years <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)



for (i in 1:8){ #year
  fullSet <- subset(fullRange, fullRange$Date >= yearStart[i] & fullRange$Date <= yearEnd[i])
  for (j in 1:11){
    state = state.num[j]
    for (k in 1:4){ #datA
      table <- read.csv(aqData[k], stringsAsFactors = FALSE)
      table$Date <- as_date(table$Date)
      table <- subset(table, table$State == state)
      tableSet <- subset(table, table$Date >= yearStart[i] & table$Date <= yearEnd[i])
      rm(table)
      if(k == 1){
        airQuality <- merge(fullSet, tableSet, by.x = "Date", by.y = "Date", all.x = TRUE)
        rm(tableSet)
      }else{
        airQuality <- merge(airQuality, tableSet, by.x = "Date", by.y = "Date", all.x = TRUE)
        rm(tableSet)
      }
      print(paste("Done ", aqData[k], " in ", years[i], " for ", state.abbr[j], "...", sep = ""))
    }
    write.csv(airQuality, paste("./", state.abbr[j], "_Daily_", years[i], ".csv", sep =""), row.names = FALSE)
    print("CSV Written...")
    
    for (k in 5:7){ #datA
      table <- read.csv(aqData[k], stringsAsFactors = FALSE)
      table$Date <- as_date(table$Date)
      table <- subset(table, table$State == state)
      tableSet <- subset(table, table$Date >= yearStart[i] & table$Date <= yearEnd[i])
      rm(table)
      if(k == 5){
        airQuality <- merge(fullSet, tableSet, by.x = "Date", by.y = "Date", all.x = TRUE)
        rm(tableSet)
      }else{
        airQuality <- merge(airQuality, tableSet, by.x = "Date", by.y = "Date", all.x = TRUE)
        rm(tableSet)
      }
      print(paste("Done ", aqData[k], " in ", years[i], " for ", state.abbr[j], "...", sep = ""))
    }
    write.csv(airQuality, paste("./", state.abbr[j], "_Hr2Day_", years[i], ".csv", sep =""), row.names = FALSE)
  }
}