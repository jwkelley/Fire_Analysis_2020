#Import Weather data
PM25 <- read.csv("PM2.5_AllYears.csv", stringsAsFactors = FALSE)
PM10 <- read.csv("PM10_AllYears.csv", stringsAsFactors = FALSE)
Lead <- read.csv("Lead_AllYears.csv", stringsAsFactors = FALSE)
CO <- read.csv("CO_AllYears.csv", stringsAsFactors = FALSE)
NO2 <- read.csv("NO2_AllYears.csv", stringsAsFactors = FALSE)
O3 <- read.csv("O3_AllYears.csv", stringsAsFactors = FALSE)
So2 <- read.csv("SO2_AllYears.csv", stringsAsFactors = FALSE)

PM25$Date <- as_date(PM25$Date)
PM10$Date <- as_date(PM10$Date)
Lead$Date <- as_date(Lead$Date)
CO$Date <- as_date(CO$Date)
NO2$Date <- as_date(NO2$Date)
O3$Date <- as_date(O3$Date)
So2$Date <- as_date(So2$Date)

airQuality <- merge(fullRange, PM25, by.x = "Date", by.y = "Date", all.x = TRUE)
airQuality <- merge(airQuality, PM10, by.x = "Date", by.y = "Date", all.x = TRUE)
