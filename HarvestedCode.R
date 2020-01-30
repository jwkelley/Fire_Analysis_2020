setwd("E:/R_Package_Cbone_FIRES/AirQuality/")
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
setwd("E:/R_Package_Cbone_FIRES/AirQuality/")
poll.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2")
states <- c(4,6,8,16,30,32,35,41,49,53,56)
start.time <- proc.time()
for(i in 4:7){
  t2 <- Sys.time()
  print(paste("Starting ", poll.name[i], " at ", t2, "...", sep = ""))
  for(j in 2007:2014){
    t3 <- Sys.time()
    print(paste("Starting ", j, " at ", t3, "...", sep = ""))
    file <- read.csv(paste0("./Hourly/", poll.name[i],"_",j,".csv"))
    file.states <- unique(file$State.Code)
    common.states <- intersect(file.states, states)
    file <- subset(file, State.Code %in% common.states, select=c("State.Code","County.Code","Site.Num", "Date.Local", "Sample.Measurement"))
    print("Files subset...")
    write.csv(file, file = paste0("./Hourly/", poll.name[i],"_",j,"_clean.csv"))
    t4 <- Sys.time()
    print(paste("Writing finished for ", j, " in...", sep = ""))
    print(t4 - t3)
  }
  t7 <- Sys.time()
  print(paste("Finished ", poll.name[i], " in...", sep = ""))
  print(t7-t2)
}
for(i in 1:3){
  t2 <- Sys.time()
  print(paste("Starting ", poll.name[i], " at ", t2, "...", sep = ""))
  for(j in 2007:2014){
    t3 <- Sys.time()
    print(paste("Starting ", j, " at ", t3, "...", sep = ""))
    file <- read.csv(paste0("./Daily/", poll.name[i],"_",j,".csv"))
    file.states <- unique(file$State.Code)
    common.states <- intersect(file.states, states)
    file <- subset(file, State.Code %in% common.states, select=c("State.Code", "Date.Local", "Arithmetic.Mean"))
    print("Files subset...")
    write.csv(file, file = paste0("./Daily/", poll.name[i],"_",j,"_clean.csv"))
    t4 <- Sys.time()
    print(paste("Writing finished for ", j, " in...", sep = ""))
    print(t4 - t3)
  }
  t7 <- Sys.time()
  print(paste("Finished ", poll.name[i], " in...", sep = ""))
  print(t7-t2)
}
end.time <- proc.time()
print(end.time-start.time)

########
setwd("E:/R_Package_Cbone_FIRES/AirQuality/")
t1 <- Sys.time()
for(i in 4:7){
  t2 <- Sys.time()
  print(paste("Starting ", poll.name[i], " at ", t2, "...", sep = ""))
  for(j in 2007:2014){
    t3 <- Sys.time()
    print(paste("Starting ", j, " at ", t3, "...", sep = ""))
    file <- read.csv(paste0("./Hourly/", poll.name[i],"_",j,"_clean.csv"))
    file <- ddply(file,c("State.Code","County.Code","Site.Num", "Date.Local"),numcolwise(mean))
    print("Daily mean calculated...")
    write.csv(file, file = paste0("./Hourly/", poll.name[i],"_",j,"_Daily.csv"))
    t4 <- Sys.time()
    print(paste("Writing finished for ", j, " in...", sep = ""))
    print(t4 - t3)
  }
  t7 <- Sys.time()
  print(paste("Finished ", poll.name[i], " in...", sep = ""))
  print(t7-t2)
}
t8 <- Sys.time()
print(paste("Finished All in...", sep = ""))
print(t8 - t1)
# old <- read.csv("E:/Rx Fire/pollutant_data/in_11_states/CO_2007.csv")
# new <- read.csv("E:/Rx Fire/pollutant_data/in_11_states/hourly_to_daily/CO_2007.csv")
# dim(old)
# dim(new)
# head(old, n=30)
# head(new)
###########################################################################
### write new pollutant file with only the necessary columns
# setwd("E:/Rx Fire/pollutant_data/")
# poll.name <- c("PM2.5","PM10","Lead","CO","NO2","O3","SO2")
# states <- c(4,6,8,16,30,32,35,41,49,53,56,38,46,31,20,40,48,80)
# start.time <- proc.time()
# for(i in 4:4){
#   for(j in 2007:2014){
#     file <- read.csv(paste0(poll.name[i],"_",j,".csv"))
#     file.states <- unique(file$State.Code)
#     common.states <- intersect(file.states, states)
#     file <- subset(file, State.Code %in% common.states, select=c("State.Code", "County.Code","Site.Num", "Date.Local", "Sample.Measurement"))
#     write.csv(file, file = paste0("new_pollutant_data/",poll.name[i],"_",j,".csv"))
#   }
# }
# end.time <- proc.time()
# print(end.time-start.time)


####Make plots
library(ggplot2)
library(grid)
library(gridExtra)
library(Rmisc)
library(ggpubr)
library(cowplot)
library("devtools")
install_github("https://github.com/kassambara/easyGgplot2")
library(easyGgplot2)

var.name <- c("PM2.5", "PM10", "Lead", "CO", "NO2", "O3", "SO2") #,"wind", "tavg", "tmax", "tmin", "precip","PDSI")
state.abbr <- c("AZ", "CA", "CO", "ID", "MT", "NV", "NM", "OR", "UT", "WA", "WY")
state.num <- c(4, 6, 8, 16, 30, 32, 35, 41, 49, 53, 56)
state.name <- c("Arizona", "California", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Oregon", "Utah", "Washington", "Wyoming")
x.labs <- c(expression(paste("PM2.5 ",mu,'g/m'^3)),
            expression(paste("PM10 ",mu,'g/m'^{3})),
            expression(paste("Lead ",mu,'g/m'^{3})),
            "CO (ppm)",
            expression(paste("NO"[2]," (ppb)")),
            expression(paste("O"[3]," (ppb)")),
            expression(paste("SO"[2]," (ppb)")),
            "Wind Speed (m/s)", 
            expression(paste("Temperature (",degree,"C)")), 
            expression(paste("Temperature (",degree,"C)")), 
            expression(paste("Temperature (",degree,"C)")), 
            "Precipitation (mm)",
            "Palmer Drought Severity Index")

var.full.name <- c("PM2.5 Measurements","PM10 Measurements","Lead Measurements","CO Measurements","NO2 Measurements",
                   "O3 Measurements","SO2 Measurements","Daily Average Wind Speed", 
                   "Daily Average Temperature", "Daily Maximum Temperature", "Daily Minimum Temperature",
                   "Daily Precipitation","Palmer Drought Severity Index")
x.mins <-c(0,0,0,0,0,0,0) 
#x.maxs <- c(40,120,.15,2,60,.08,10) #less tail
x.maxs <- c(65, 250, .25, 2, 60, .08, 20) #w tail

threshold <- c(35, 150, .15, 35, 100, .07, 75)
#############################################################

for(j in 1:7){ #var
  #make file
  if(j==1|j==2|j==3){
    setwd("E:/R_Package_Cbone_FIRES/AirQuality/")
    for(i in 2007:2014){
     assign(paste0(var.name[j],"_",i), read.csv(paste0("Daily/",var.name[j],"_",i,"_clean.csv")))
    }
    Var <- rbind(get(paste0(var.name[j],"_2007")),get(paste0(var.name[j],"_2008")),get(paste0(var.name[j],"_2009")),get(paste0(var.name[j],"_2010")),
                 get(paste0(var.name[j],"_2011")),get(paste0(var.name[j],"_2012")),get(paste0(var.name[j],"_2013")),get(paste0(var.name[j],"_2014")))
    colnames(Var) <- c("X", "State", "Date", var.name[j])
    Var$X <- NULL
    # Var$Date <- NULL
  } else if(j==4|j==5|j==6|j==7){
    setwd("E:/R_Package_Cbone_FIRES/AirQuality/")
    for(i in 2007:2014){
      assign(paste0(var.name[j],"_",i), read.csv(paste0("Hourly/",var.name[j],"_",i,"_Daily.csv")))
    }
    Var <- rbind(get(paste0(var.name[j],"_2007")),get(paste0(var.name[j],"_2008")),get(paste0(var.name[j],"_2009")),get(paste0(var.name[j],"_2010")),
                 get(paste0(var.name[j],"_2011")),get(paste0(var.name[j],"_2012")),get(paste0(var.name[j],"_2013")),get(paste0(var.name[j],"_2014")))
    colnames(Var) <- c("X", "State", "C","S","Date", "XX",var.name[j])
    Var$X <- NULL
    # Var$Date <- NULL
    Var$C <- NULL
    Var$S <- NULL
    Var$XX <- NULL
  }
  
  
  write.csv(Var, paste("E:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/", var.name[j], "_AllYears.csv", sep = ""), row.names = FALSE)
  Var$Type <- "All Observations"
  head(Var)
  
  Fires <- read.csv("E:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/Fires_AllCalcs.csv")
  Fires <- subset(Fires, select = c("State", "Date", var.name[j]))
  colnames(Fires) <- c("State", "Date", var.name[j])
  Fires$Type <- "Fire Observations"
  
  file <- rbind(Var, Fires)
  unique(file$State)
  plots <- list()
  
  for(i in 1:11){ #state
    state <- subset(file, State==state.abbr[i] | State==state.num[i])
    state <- state[!(is.na(state[,3])),]
    #make plot, save to list
    title <-x.labs[j]
    a <- ggplot2.histogram(data=state, xName=var.name[j], xtitle=title, ytitle="Density",
                           xtitleFont=c(9,"plain", "black"), ytitleFont=c(9,"plain", "black"),
                           xTickLabelFont=c(7,"plain", "black"), yTickLabelFont=c(7,"plain", "black"),
                           xlim=c(x.mins[j],x.maxs[j]), 
                           showLegend=FALSE,
                           groupName = "Type", groupColors = c( "grey26","#FF6666"),legendPosition=NULL,
                           alpha=0.5, position="dodge",
                           addDensity=TRUE,
                           binwidth=((x.maxs[j]-x.mins[j])/20), mainTitle=paste0(state.name[i]),
                           addMeanLine=TRUE, meanLineColor=c("black","red"),
                           meanLineType="dashed", meanLineSize=.4) 
    #geom_vline(aes(xintercept=mean.PM10.WA), colour="black", linetype="dashed")
    
    plots[[i]] <- a
  }
  state <- subset(file, State==state.abbr[1] | State==state.num[1])
  b <- ggplot2.histogram(data=state, xName=var.name[j], 
                         groupName = "Type", groupColors = c( "grey26","#FF6666"),
                         legendTitle="",
                         binwidth=((x.maxs[j]-x.mins[j])/20)) 
  
  b
  extractLegend <- function(gg) {
    grobs <- ggplot_gtable(ggplot_build(gg))
    foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
    grobs$grobs[[foo]]
  }
  legend <- extractLegend(b)
  plot(legend)
  
  
  if(j==3){
    plots[[5]] <- plots[[6]]
    plots[[6]] <- plots[[7]]
    plots[[7]] <- plots[[8]]
    plots[[8]] <- plots[[9]]
    plots[[9]] <- plots[[10]]
    plots[[10]] <- NULL
    plots[[10]] <- NULL
    
    plots[[10]] <- ggdraw(legend)
    lay <- rbind(c(1,2,3,NA),
                 c(4,5,6,10),
                 c(7,8,9,NA))
    
    
    
    setwd("E:/R_Package_Cbone_FIRES/Outputs/Frequency Plots/Test/")
    pdf(paste0(var.name[j],"_FrequencyAnalysis.pdf"), width=10, height=7, onefile=FALSE)
    figure <- grid.arrange(grobs=plots,layout_matrix=lay)
    print(figure)
    dev.off()
    
  } else {
    plots[[12]] <- ggdraw(legend)
    lay <- rbind(c(1,1,1,2,2,2,3,3,3,4,4,4),
                 c(5,5,5,6,6,6,7,7,7,8,8,8),
                 c(9,9,9,10,10,10,11,11,11,NA,12,NA))
    
    setwd("E:/R_Package_Cbone_FIRES/Outputs/Frequency Plots/Test/")
    pdf(paste0(var.name[j],"_FrequencyAnalysis.pdf"), width=10, height=7, onefile=FALSE)
    figure <- grid.arrange(grobs=plots,layout_matrix=lay)
    print(figure)
    dev.off()
  }
  
}


graphics.off()