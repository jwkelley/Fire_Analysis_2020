# 
# kruskal.test(weight ~ group, data = my_data)
# DunnTest(weight ~ group, data = my_data, method = "holm")


#SETUP
library(dplyr)
library(devtools)
library(ggpubr)
library(car)
library(DescTools)
library(nortest)

state.abbr <- c("AZ", "CA", "CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
state.num <- c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56)

data <- c("PM2.5_JoinedData_USFS.csv", "PM10_JoinedData_USFS.csv", "Lead_JoinedData_USFS.csv", "CO_JoinedData_USFS.csv", "NO2_JoinedData_USFS.csv","O3_JoinedData_USFS.csv","SO2_JoinedData_USFS.csv",
          "PM2.5_JoinedData_DOI.csv", "PM10_JoinedData_DOI.csv", "Lead_JoinedData_DOI.csv", "CO_JoinedData_DOI.csv", "NO2_JoinedData_DOI.csv","O3_JoinedData_DOI.csv","SO2_JoinedData_DOI.csv")

#Import Data
for(i in 1:length(data)){
  writeLines(paste("\nStarting ", data[i], "...", sep = ""))
  file <- read.csv(data[i], stringsAsFactors = FALSE) 
  varName <- colnames(file)[3]
  
  if(i < 8){
    agent = "USFS"
  }else{
    agent = "DOI"
  }
  if(varName == "PM2.5"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(PM2.5 ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
  }else if(varName == "PM10"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(PM10 ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
    
  }else if(varName == "Lead"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(Lead ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
    
  }else if(varName == "CO"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(CO ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
    
  }else if(varName == "NO2"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(NO2 ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
    
  }else if(varName == "O3"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(O3 ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
    
  }else if(varName == "SO2"){
    writeLines(paste("Agent is ", agent, "...", sep = ""))
    file$Type <- as.factor(file$Type)
    fireObs <- file[which(file$Type == "Fire Observations (USFS)" | file$Type == "Fire Observations (DOI)"),]    
    fireObs$State <- as.factor(fireObs$State)    
    writeLines("Fire Obs Subset...")
    dTest <- DunnTest(SO2 ~ State, data = fireObs, method = "holm")
    table <- as.data.frame(dTest[1][1])
    
    write.csv(table , paste("./ANOVA/", varName, "/DunnTest_",varName, "_", agent, ".csv", sep = ""), row.names = TRUE)
    writeLines("Dunn Results Written...\n")
  }
  writeLines(paste("Finished ", data[i], "...\n\n", sep = ""))
}
