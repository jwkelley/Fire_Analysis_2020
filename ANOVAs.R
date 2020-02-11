library(dplyr)
library(devtools)
library(ggpubr)
library(car)
library(DescTools)
#Import Data
my_data <- PlantGrowth

#Check your data
set.seed(1234)
dplyr::sample_n(my_data, 10)
levels(my_data$group)
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )


#Visualize you data
ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")
ggline(my_data, x = "group", y = "weight", 
      add = c("mean_se", "jitter"), 
      order = c("ctrl", "trt1", "trt2"),
      ylab = "Weight", xlab = "Treatment")


#Compute ANOVA
res.aov <- aov(weight ~ group, data = my_data)
summary(res.aov)
TukeyHSD(res.aov)


#Check the homogeneity of variance
plot(res.aov, 1)
leveneTest(weight ~ group, data = my_data)

#Check the normality assumption
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )


####################################################################
##KW

#Import Data
my_data <- PlantGrowth

#Check your data
head(my_data)
levels(my_data$group)
my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))

#Visualize you data
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE),
    median = median(weight, na.rm = TRUE),
    IQR = IQR(weight, na.rm = TRUE)
  )

ggboxplot(my_data, x = "group", y = "weight", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Weight", xlab = "Treatment")

ggline(my_data, x = "group", y = "weight", 
       add = c("mean_se", "jitter"), 
       order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")

kruskal.test(weight ~ group, data = my_data)
DunnTest(weight ~ group, data = my_data, method = "holm")
