#This is a start of the Analysis project
#Rx Fire data from Emma\Outputs\RxFires Files\Fires_AllCalcs.csv
library(lubridate)
memory.limit(160000)
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
  state <- states[i]
  
  subset <- fireData[fireData$State == state,]
  minDay <- min(subset$YDay)
  maxDay <- max(subset$YDay)
  
  temp <- data.frame(State = state, Start = minDay, End = maxDay)
  dateRanges <- rbind(dateRanges, temp)
}


#Since date range is almost the full year in most cases the sample time will be a full year
fullRange <- data.frame(Date = seq(ymd('2007-01-01'), ymd('2014-12-31'), by = 'days'))

#Split by Source
doiFires <- fireData[which(fireData$Source == "DOI"),]
USFSFires <- fireData[which(fireData$Source == "USFS"),]

#Join to full date range and remove the nulls to be unburned dates
doiFireDays <- merge(fullRange, doiFires, by.x = "Date", by.y = "Date", all.x = TRUE)
doiNoFire <- data.frame(Date = doiFireDays[is.na(doiFireDays$unique_ID),c(1)])
doiFireDays <- doiFireDays[!is.na(doiFireDays$unique_ID),]

USFSFireDays <- merge(fullRange, USFSFires, by.x = "Date", by.y = "Date", all.x = TRUE)
USFSNoFire <- data.frame(Date = USFSFireDays[is.na(USFSFireDays$unique_ID),c(1)])
USFSFireDays <- USFSFireDays[!is.na(USFSFireDays$unique_ID),]

aqs <- c("PM2.5_AllYears.csv", "PM10_AllYears.csv", "Lead_AllYears.csv", 
            "CO_AllYears.csv", "NO2_AllYears.csv", "O3_AllYears.csv", "SO2_AllYears.csv")

####Make plots
library(ggplot2)
library(grid)
library(gridExtra)
library(Rmisc)
library(ggpubr)
library(cowplot)
library("devtools")
# install_github("https://github.com/kassambara/easyGgplot2")
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
x.maxs <- c(65, 250, .25, 2, 60, .08, 20) #w tail
y.max <- c(0.2, 0.07, 450, 5, 0.45, 70, 2.5)
texX <- c(0.95)
texy <- c(0.90)
threshold <- c(35, 150, .15, 35, 100, .07, 75)
#############################################################
for(agent in 1:2){
  if(agent == 1){
    fireDays <- USFSFireDays
    nonFireDays <- USFSNoFire
    agnt <- "USFS"
  }else if(agent == 2){
    fireDays <- doiFireDays
    nonFireDays <- doiNoFire
    agnt <- "DOI"
  }
  
  for(j in 1:7){ #var
    if(j == 6){
      #make file
      csv <- aqs[j]
      aqData <- read.csv(csv)
      aqData$Date <- as_date(aqData$Date)
      aqNonFire <- merge(nonFireDays, aqData, by.x = "Date", by.y = "Date")
      aqNonFire$Type <- "Non-Fire Observations"
      
      
      
      Fires <- subset(fireDays, select = c("State", "Date", var.name[j]))
      colnames(Fires) <- c("State", "Date", var.name[j])
      Fires$Type <- paste("Fire Observations (", agnt, ")", sep = "") 
      
      file <- rbind(aqNonFire, Fires)
      unique(file$State)
      plots <- list()
      
      write.csv(file, paste0(var.name[j],"_JoinedData_", agnt, ".csv"), row.names = FALSE)
      for(i in 1:11){ #state
        if(i ==10){
          state <- subset(file, State==state.abbr[i] | State==state.num[i])
          state <- state[!(is.na(state[,3])),]
          nNon <- nrow(state[which(state$Type == "Non-Fire Observations"),])
          nFire <- nrow(state[which(state$Type == paste("Fire Observations (", agnt, ")", sep = "")),])
          #make plot, save to list
          title <-x.labs[j]
          a <- ggplot2.histogram(data=state, xName=var.name[j], xtitle=title, ytitle="Density",
                                 xtitleFont=c(9,"plain", "black"), ytitleFont=c(9,"plain", "black"),
                                 xTickLabelFont=c(7,"plain", "black"), yTickLabelFont=c(7,"plain", "black"),
                                 xlim=c(x.mins[j],x.maxs[j]),
                                 ylim=c(0, y.max[j]),
                                 showLegend=FALSE,
                                 groupName = "Type", groupColors = c("#FF6666", "grey26"),legendPosition=NULL,
                                 alpha=0.5, position="dodge",
                                 addDensity=TRUE,
                                 binwidth=((x.maxs[j]-x.mins[j])/20), mainTitle=paste0(state.name[i]),
                                 addMeanLine=TRUE, meanLineColor=c("black","red"),
                                 meanLineType="dashed", meanLineSize=.4) 
          a <- a + geom_vline(aes(xintercept=threshold[j]), colour="steelblue", linetype="dashed")
          a <- a + theme_classic() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
          
          grob1 <- grobTree(textGrob(paste("N = ", format(nNon, big.mark = ","), sep = ""),
                                   x=(texX-0.45),  y=texy, hjust=0,
                                   gp=gpar(col="grey26", fontsize=09, fontface="italic")))
          grob2 <- grobTree(textGrob(paste("N = ", format(nFire, big.mark = ","), sep = ""), 
                                     x=(texX-0.93),  y=(texy), hjust=0,
                                     gp=gpar(col="#FF6666", fontsize=09, fontface="italic")))
          a <- a + annotation_custom(grob1) + annotation_custom(grob2) 
         
          a
          
          plots[[i]] <- a
        }else{
          state <- subset(file, State==state.abbr[i] | State==state.num[i])
          state <- state[!(is.na(state[,3])),]
          nNon <- nrow(state[which(state$Type == "Non-Fire Observations"),])
          nFire <- nrow(state[which(state$Type == paste("Fire Observations (", agnt, ")", sep = "")),])
          #make plot, save to list
          title <-x.labs[j]
          a <- ggplot2.histogram(data=state, xName=var.name[j], xtitle=title, ytitle="Density",
                                 xtitleFont=c(9,"plain", "black"), ytitleFont=c(9,"plain", "black"),
                                 xTickLabelFont=c(7,"plain", "black"), yTickLabelFont=c(7,"plain", "black"),
                                 xlim=c(x.mins[j],x.maxs[j]),
                                 ylim=c(0, y.max[j]),
                                 showLegend=FALSE,
                                 groupName = "Type", groupColors = c("#FF6666", "grey26"),legendPosition=NULL,
                                 alpha=0.5, position="dodge",
                                 addDensity=TRUE,
                                 binwidth=((x.maxs[j]-x.mins[j])/20), mainTitle=paste0(state.name[i]),
                                 addMeanLine=TRUE, meanLineColor=c("black","red"),
                                 meanLineType="dashed", meanLineSize=.4) 
          a <- a + geom_vline(aes(xintercept=threshold[j]), colour="steelblue", linetype="dashed")
          a <- a + theme_classic() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
          
          grob1 <- grobTree(textGrob(paste("N = ", format(nNon, big.mark = ","), sep = ""),
                                     x=(texX-0.93),  y=texy, hjust=0,
                                     gp=gpar(col="grey26", fontsize=09, fontface="italic")))
          grob2 <- grobTree(textGrob(paste("N = ", format(nFire, big.mark = ","), sep = ""), 
                                     x=(texX-0.93),  y=(texy-0.1), hjust=0,
                                     gp=gpar(col="#FF6666", fontsize=09, fontface="italic")))
          a <- a + annotation_custom(grob1) + annotation_custom(grob2) 
          
          a
          
          plots[[i]] <- a
        }
      }
      state <- subset(file, State==state.abbr[1] | State==state.num[1])
      b <- ggplot2.histogram(data=state, xName=var.name[j], 
                              groupName = "Type", groupColors = c("#FF6666", "grey26"),
                              legendTitle="",
                              binwidth=((x.maxs[j]-x.mins[j])/20)) 
        
      b
      extractLegend <- function(gg) {
          grobs <- ggplot_gtable(ggplot_build(gg))
          foo <- which(sapply(grobs$grobs, function(x) x$name) == "guide-box")
          grobs$grobs[[foo]]
        }
      legendnd <- extractLegend(b)
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
        
        setwd("H:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/")
        pdf(paste0(var.name[j],"_FrequencyAnalysis_", agnt,".pdf"), width=10, height=7, onefile=FALSE)
        figure <- grid.arrange(grobs=plots,layout_matrix=lay)
        print(figure)
        dev.off()
      } else {
        plots[[12]] <- ggdraw(legend)
        lay <- rbind(c(1,1,1,2,2,2,3,3,3,4,4,4),
                     c(5,5,5,6,6,6,7,7,7,8,8,8),
                     c(9,9,9,10,10,10,11,11,11,NA,12,NA))
        
        setwd("H:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/")
        pdf(paste0(var.name[j],"_FrequencyAnalysis_", agnt,".pdf"), width=10, height=7, onefile=FALSE)
        figure <- grid.arrange(grobs=plots,layout_matrix=lay)
        print(figure)
        dev.off()
      }
    }else{
      #make file
      csv <- aqs[j]
      aqData <- read.csv(csv)
      aqData$Date <- as_date(aqData$Date)
      aqNonFire <- merge(nonFireDays, aqData, by.x = "Date", by.y = "Date")
      aqNonFire$Type <- "Non-Fire Observations"
      
      
      
      Fires <- subset(fireDays, select = c("State", "Date", var.name[j]))
      colnames(Fires) <- c("State", "Date", var.name[j])
      Fires$Type <- paste("Fire Observations (", agnt, ")", sep = "") 
      
      file <- rbind(aqNonFire, Fires)
      unique(file$State)
      plots <- list()
      
      write.csv(file, paste0(var.name[j],"_JoinedData_", agnt,".csv"), row.names = FALSE)
      for(i in 1:11){ #state
        state <- subset(file, State==state.abbr[i] | State==state.num[i])
        state <- state[!(is.na(state[,3])),]
        nNon <- nrow(state[which(state$Type == "Non-Fire Observations"),])
        nFire <- nrow(state[which(state$Type == paste("Fire Observations (", agnt, ")", sep = "")),])
        #make plot, save to list
        title <-x.labs[j]
        a <- ggplot2.histogram(data=state, xName=var.name[j], xtitle=title, ytitle="Density",
                               xtitleFont=c(9,"plain", "black"), ytitleFont=c(9,"plain", "black"),
                               xTickLabelFont=c(7,"plain", "black"), yTickLabelFont=c(7,"plain", "black"),
                               xlim=c(x.mins[j],x.maxs[j]),
                               ylim=c(0, y.max[j]),
                               showLegend=FALSE,
                               groupName = "Type", groupColors = c("#FF6666", "grey26"),legendPosition=NULL,
                               alpha=0.5, position="dodge",
                               addDensity=TRUE,
                               binwidth=((x.maxs[j]-x.mins[j])/20), mainTitle=paste0(state.name[i]),
                               addMeanLine=TRUE, meanLineColor=c("black","red"),
                               meanLineType="dashed", meanLineSize=.4) 
        a <- a + geom_vline(aes(xintercept=threshold[j]), colour="steelblue", linetype="dashed")
        a <- a + theme_classic() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        
        grob1 <- grobTree(textGrob(paste("N = ", format(nNon, big.mark = ","), sep = ""),
                                   x=texX,  y=texy, hjust=1,
                                   gp=gpar(col="grey26", fontsize=09, fontface="italic")))
        grob2 <- grobTree(textGrob(paste("N = ", format(nFire, big.mark = ","), sep = ""), 
                                   x=texX,  y=(texy-0.1), hjust=1,
                                   gp=gpar(col="#FF6666", fontsize=09, fontface="italic")))
        a <- a + annotation_custom(grob1) + annotation_custom(grob2) 
        
        a
        
        plots[[i]] <- a
      }
      state <- subset(file, State==state.abbr[1] | State==state.num[1])
      b <- ggplot2.histogram(data=state, xName=var.name[j], 
                             groupName = "Type", groupColors = c("#FF6666", "grey26"),
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
        
        setwd("H:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/")
        pdf(paste0(var.name[j],"_FrequencyAnalysis_", agnt,".pdf"), width=10, height=7, onefile=FALSE)
        figure <- grid.arrange(grobs=plots,layout_matrix=lay)
        print(figure)
        dev.off()
      } else {
        plots[[12]] <- ggdraw(legend)
        lay <- rbind(c(1,1,1,2,2,2,3,3,3,4,4,4),
                     c(5,5,5,6,6,6,7,7,7,8,8,8),
                     c(9,9,9,10,10,10,11,11,11,NA,12,NA))
        
        setwd("H:/R_Package_Cbone_FIRES/Code/Fire_Analysis_2020/")
        pdf(paste0(var.name[j],"_FrequencyAnalysis_", agnt,".pdf"), width=10, height=7, onefile=FALSE)
        figure <- grid.arrange(grobs=plots,layout_matrix=lay)
        print(figure)
        dev.off()
      }
    }
  }
  graphics.off()
}
