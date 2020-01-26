setwd("H:/R_Package_Cbone_FIRES/AirQuality/")
poll.code <- c("88101","81102","LEAD","42101","42602","44201","42401")
poll.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2")
t1 <- Sys.time()
for(i in 2007:2014){
  t2 <- Sys.time()
  print(paste("Starting ", i, " at ", t2, "...", sep = ""))
  for(j in 1:3){
    t3 <- Sys.time()
    print(paste("Starting ", poll.name[j], " at ", t3, "...", sep = ""))
    temp <- read.csv(paste0("Daily/",poll.code[j], "/daily_",poll.code[j],"_",i,"/daily_",poll.code[j],"_",i,".csv"))
    write.csv(temp, paste0("Daily/", poll.name[j],"_",i,".csv"))
    t4 <- Sys.time()
    print(paste("Writing finished for ", poll.name[j], " in...", sep = ""))
    print(t4 - t3)
  }
  for(j in 4:7){
    t5 <- Sys.time()
    print(paste("Starting ", poll.name[j], " at ", t5, "...", sep = ""))
    temp <- read.csv(paste0("hourly/",poll.code[j], "/hourly_",poll.code[j],"_",i,"/hourly_",poll.code[j],"_",i,".csv"))
    write.csv(temp, paste0("hourly/", poll.name[j],"_",i,".csv"))
    t6 <- Sys.time()
    print(paste("Writing finished for ", poll.name[j], " in...", sep = ""))
    print(t6 - t5)
  }
  t7 <- Sys.time()
  print(paste("Finished ", i, " in...", sep = ""))
  print(t7-t2)
}
t8 <- Sys.time()
print(paste("Finished All in...", sep = ""))
print(t8 - t1)

#### data in only the 11 states##############################
### write new pollutant file with only the necessary columns
setwd("E:/Rx Fire/pollutant_data/")
poll.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2")
states <- c(4,6,8,16,30,32,35,41,49,53,56)
start.time <- proc.time()
for(i in 7:7){
  for(j in 2007:2014){
    file <- read.csv(paste0(poll.name[i],"_",j,".csv"))
    file.states <- unique(file$State.Code)
    common.states <- intersect(file.states, states)
    file <- subset(file, State.Code %in% common.states, select=c("State.Code","County.Code","Site.Num", "Date.Local", "Sample.Measurement"))
    write.csv(file, file = paste0("in_11_states/",poll.name[i],"_",j,".csv"))
  }
}
for(i in 1:3){
  for(j in 2007:2014){
    file <- read.csv(paste0(poll.name[i],"_",j,".csv"))
    file.states <- unique(file$State.Code)
    common.states <- intersect(file.states, states)
    file <- subset(file, State.Code %in% common.states, select=c("State.Code", "Date.Local", "Arithmetic.Mean"))
    write.csv(file, file = paste0("in_11_states/",poll.name[i],"_",j,".csv"))
  }
}
end.time <- proc.time()
print(end.time-start.time)

########
setwd("E:/Rx Fire/pollutant_data/in_11_states/")
for(i in 4:7){
  for(j in 2007:2014){
    file <- read.csv(paste0(poll.name[i],"_",j,".csv"))
    
    file <- ddply(file,c("State.Code","County.Code","Site.Num", "Date.Local"),numcolwise(mean))
    
    write.csv(file, file = paste0("hourly_to_daily/",poll.name[i],"_",j,".csv"))
  }
}
old <- read.csv("E:/Rx Fire/pollutant_data/in_11_states/CO_2007.csv")
new <- read.csv("E:/Rx Fire/pollutant_data/in_11_states/hourly_to_daily/CO_2007.csv")
dim(old)
dim(new)
head(old, n=30)
head(new)
###########################################################################
### write new pollutant file with only the necessary columns
setwd("E:/Rx Fire/pollutant_data/")
poll.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2")
states <- c(4,6,8,16,30,32,35,41,49,53,56,38,46,31,20,40,48,80)
start.time <- proc.time()
for(i in 4:4){
  for(j in 2007:2014){
    file <- read.csv(paste0(poll.name[i],"_",j,".csv"))
    file.states <- unique(file$State.Code)
    common.states <- intersect(file.states, states)
    file <- subset(file, State.Code %in% common.states, select=c("State.Code", "County.Code","Site.Num", "Date.Local", "Sample.Measurement"))
    write.csv(file, file = paste0("new_pollutant_data/",poll.name[i],"_",j,".csv"))
  }
}
end.time <- proc.time()
print(end.time-start.time)