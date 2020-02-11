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
  
  writeLines(paste("Agent is ", agent, "...", sep = ""))
  if(i == 3 | i == 10){
    for(j in c(1,2,3,4,6,7,8,9,10)){ 
      writeLines(paste("\nStarting ", state.abbr[j], " state...", sep = ""))
      state <- subset(file, State==state.abbr[j] | State==state.num[j])
      state$Type <- as.factor(state$Type)
      
      #Check your data
      if(i < 8){
        head(state)
        levels(state$Type)
        state$Type <- ordered(state$Type,
                              levels = c("Fire Observations (USFS)", "Non-Fire Observations"))
      }else{
        head(state)
        levels(state$Type)
        state$Type <- ordered(state$Type,
                              levels = c("Fire Observations (DOI)", "Non-Fire Observations"))
      }
      
      #Visualize you data
      if(varName == "PM2.5"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(PM2.5, na.rm = TRUE),
                    sd = sd(PM2.5, na.rm = TRUE),
                    median = median(PM2.5, na.rm = TRUE),
                    IQR = IQR(PM2.5, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "PM2.5", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "PM 2.5", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "PM2.5", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "PM 2.5", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(PM2.5 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(PM2.5 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(PM2.5 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(PM2.5 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(PM2.5 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(PM2.5 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "PM10"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(PM10, na.rm = TRUE),
                    sd = sd(PM10, na.rm = TRUE),
                    median = median(PM10, na.rm = TRUE),
                    IQR = IQR(PM10, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "PM10", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "PM 10", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "PM10", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "PM 10", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(PM10 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(PM10 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(PM10 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(PM10 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(PM10 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(PM10 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "Lead"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(Lead, na.rm = TRUE),
                    sd = sd(Lead, na.rm = TRUE),
                    median = median(Lead, na.rm = TRUE),
                    IQR = IQR(Lead, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "Lead", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "Lead", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "Lead", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "Lead", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(Lead ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(Lead ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(Lead ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(Lead ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(Lead ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(Lead ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "CO"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(CO, na.rm = TRUE),
                    sd = sd(CO, na.rm = TRUE),
                    median = median(CO, na.rm = TRUE),
                    IQR = IQR(CO, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "CO", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "CO", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "CO", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "CO", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(CO ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(CO ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(CO ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(CO ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(CO ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(CO ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "NO2"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(NO2, na.rm = TRUE),
                    sd = sd(NO2, na.rm = TRUE),
                    median = median(NO2, na.rm = TRUE),
                    IQR = IQR(NO2, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "NO2", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "NO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "NO2", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "NO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(NO2 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(NO2 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(NO2 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(NO2 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(NO2 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(NO2 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "O3"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(O3, na.rm = TRUE),
                    sd = sd(O3, na.rm = TRUE),
                    median = median(O3, na.rm = TRUE),
                    IQR = IQR(O3, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "O3", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "O3", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "O3", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "O3", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(O3 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(O3 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(O3 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(O3 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(O3 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(O3 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "SO2"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(SO2, na.rm = TRUE),
                    sd = sd(SO2, na.rm = TRUE),
                    median = median(SO2, na.rm = TRUE),
                    IQR = IQR(SO2, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "SO2", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "SO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "SO2", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "SO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(SO2 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(SO2 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(SO2 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(SO2 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(SO2 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(SO2 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }
    }
    
    write.csv(KWRESULTS, paste("./ANOVA/", varName, "/KWTest_", varName, "_", agent, ".csv", sep = ""), row.names = FALSE)
    writeLines("KW Results Written...")
    writeLines(paste("Finished ", data[i], "...", sep = ""))
    rm(KWRESULTS)
  }else{
    for(j in 1:11){ 
      writeLines(paste("\nStarting ", state.abbr[j], " state...", sep = ""))
      state <- subset(file, State==state.abbr[j] | State==state.num[j])
      state$Type <- as.factor(state$Type)
      
      
      #Check your data
      if(i < 8){
        head(state)
        levels(state$Type)
        state$Type <- ordered(state$Type,
                              levels = c("Fire Observations (USFS)", "Non-Fire Observations"))
      }else{
        head(state)
        levels(state$Type)
        state$Type <- ordered(state$Type,
                              levels = c("Fire Observations (DOI)", "Non-Fire Observations"))
      }
      
      #Visualize you data
      if(varName == "PM2.5"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
                    summarise(count = n(),
                              mean = mean(PM2.5, na.rm = TRUE),
                              sd = sd(PM2.5, na.rm = TRUE),
                              median = median(PM2.5, na.rm = TRUE),
                              IQR = IQR(PM2.5, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "PM2.5", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "PM 2.5", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "PM2.5", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "PM 2.5", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(PM2.5 ~ Type, data = state)
            
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(PM2.5 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(PM2.5 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(PM2.5 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(PM2.5 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(PM2.5 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "PM10"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
                    summarise(count = n(),
                              mean = mean(PM10, na.rm = TRUE),
                              sd = sd(PM10, na.rm = TRUE),
                              median = median(PM10, na.rm = TRUE),
                              IQR = IQR(PM10, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "PM10", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "PM 10", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "PM10", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "PM 10", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
          
        #Compute ANOVA
        res.aov <- aov(PM10 ~ Type, data = state)
          
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
          
        write.csv(leveneTest(PM10 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(PM10 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
          
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
          
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(PM10 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(PM10 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(PM10 ~ Type, data = state)$p.value[[1]]
            
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "Lead"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(Lead, na.rm = TRUE),
                    sd = sd(Lead, na.rm = TRUE),
                    median = median(Lead, na.rm = TRUE),
                    IQR = IQR(Lead, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "Lead", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "Lead", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "Lead", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "Lead", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(Lead ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(Lead ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(Lead ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(Lead ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(Lead ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(Lead ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "CO"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(CO, na.rm = TRUE),
                    sd = sd(CO, na.rm = TRUE),
                    median = median(CO, na.rm = TRUE),
                    IQR = IQR(CO, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "CO", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "CO", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "CO", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "CO", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(CO ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(CO ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(CO ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(CO ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(CO ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(CO ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "NO2"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(NO2, na.rm = TRUE),
                    sd = sd(NO2, na.rm = TRUE),
                    median = median(NO2, na.rm = TRUE),
                    IQR = IQR(NO2, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "NO2", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "NO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "NO2", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "NO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(NO2 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(NO2 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(NO2 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(NO2 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(NO2 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(NO2 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "O3"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(O3, na.rm = TRUE),
                    sd = sd(O3, na.rm = TRUE),
                    median = median(O3, na.rm = TRUE),
                    IQR = IQR(O3, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "O3", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "O3", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "O3", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "O3", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(O3 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(O3 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(O3 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(O3 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(O3 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(O3 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }else if(varName == "SO2"){
        writeLines("Data Vis...")
        dataDesc <- group_by(state, Type) %>%
          summarise(count = n(),
                    mean = mean(SO2, na.rm = TRUE),
                    sd = sd(SO2, na.rm = TRUE),
                    median = median(SO2, na.rm = TRUE),
                    IQR = IQR(SO2, na.rm = TRUE))
        dataDesc$Var <- varName
        dataDesc <- dataDesc[,c(1,7,2:6)]
        write.csv(dataDesc, paste("./ANOVA/", varName, "/", state.abbr[j], "/DataDesc_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE)    
        writeLines("Data Desc Written...")
        ggboxplot(state, x = "Type", y = "SO2", 
                  color = "Type", palette = c("#00AFBB", "#E7B800"),
                  order = c("Fire Observations (USFS)", "Non-Fire Observations"),
                  ylab = "SO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Box_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Box Plot Written...")
        ggline(state, x = "Type", y = "SO2", 
               add = c("mean_se", "jitter"), 
               order = c("Fire Observations (USFS)", "Non-Fire Observations"),
               ylab = "SO2", xlab = "Observation")
        ggsave(paste("./ANOVA/", varName, "/", state.abbr[j], "/data_Means_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""), plot = last_plot(), device = "png")    
        writeLines("Means Plot Written...")
        
        #Compute ANOVA
        res.aov <- aov(SO2 ~ Type, data = state)
        
        #Check the homogeneity of variance
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 1)
        dev.off()
        
        write.csv(leveneTest(SO2 ~ Type, data = state), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_VarianceTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        levP <- leveneTest(SO2 ~ Type, data = state)$`Pr(>F)`[[1]]  
        writeLines("ANOVA Var Check Written...")
        
        #Check the normality assumption
        png(paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".png", sep = ""))
        plot(res.aov, 2)
        dev.off()
        
        aov_residuals <- residuals(object = res.aov )
        normTest <- ad.test(aov_residuals)
        write.csv(data.frame(Stat = normTest$statistic, PVal = normTest$p.value), paste("./ANOVA/", varName, "/", state.abbr[j], "/ANOVA_NormTest_", varName, "_", state.abbr[j], "_", agent, ".csv", sep = ""), row.names = FALSE) 
        normP <- normTest$p.value
        writeLines("ANOVA Norm Check Written...")
        writeLines("Perform KW Test...")
        if(levP < 0.05 | normP < 0.05){
          #perform KW
          stat <- kruskal.test(SO2 ~ Type, data = state)$statistic[[1]]
          df <- kruskal.test(SO2 ~ Type, data = state)$parameter[[1]]
          pval <- kruskal.test(SO2 ~ Type, data = state)$p.value[[1]]
          
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = stat, DF = df, PVAL = pval, note = "")
        }else{
          KWRes <- data.frame(State = state.abbr[j], Var = varName, Stat = "", DF = "", PVAL = "", note = "ANOVA ASSUMP PASSED")
        }
        if(j == 1){
          KWRESULTS <- KWRes
        }else{
          KWRESULTS <- rbind(KWRESULTS, KWRes)
          writeLines("KW Results Stored...")
        }
      }
    }

    write.csv(KWRESULTS, paste("./ANOVA/", varName, "/KWTest_", varName, "_", agent, ".csv", sep = ""), row.names = FALSE)
    writeLines("KW Results Written...")
    writeLines(paste("Finished ", data[i], "...\n\n", sep = ""))
    rm(KWRESULTS)
  }
}