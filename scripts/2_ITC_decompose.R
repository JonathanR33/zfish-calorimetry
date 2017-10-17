#R-script for ITC data anaylsis described in Rodenfels et al. 
#Script name: 2_ITC_decompose
#purpose: decompose time normalized ITC data into its trend, oscillatory and noise component
#example files provided: 1_example_timeNorm.csv 
#output files: 
#2_example_trend.csv                (trend component)
#2_example_oscillation_Wnoise.csv   (noisy oscillatory component)
#2_example_oscillation_WOnoise.csv  (noise substracted oscillatory component)
#2_example_noise.csv                (noise compoment)
#2_example_all.csv                  (all components combine in one .csv file)

#load required libraries
library(ggplot2)
#set working directory
setwd("~/Documents/1_PostDoc/1_EnergyWorkHeat/7_publication/scripts/")

#load data for each replicate
df<- read.csv("1_example_timeNorm.csv")


#define observed data
total <- data.frame(df$time,df$dp)
total$cond <- rep("1_observed", length(df$time))
colnames(total) <- c("time", "Q","cond")


#Trend determination by using a Lowess filter (f, emperically determined)
#For details see: http://stat.ethz.ch/R-manual/R-devel/library/stats/html/lowess.html
df$osc_lowess <- lowess(df$dp, f=0.3, iter = 100)[[2]]

#plot trend
ggplot( df , aes(time, osc_lowess)) + geom_line()

#write trend into new data frame
dftrend <- data.frame (df$time, df$osc_lowess)
dftrend$cond <- rep("2_trend",length(df$time))

#Define column names & write.csv for trend
colnames(dftrend) <- c("time", "Q","cond")
write.csv(dftrend, "2_example_trend.csv")

#filter data for the trend component (Oscillatory + noise component)
df$oscn <- df$dp-df$osc_lowess

#Define column names & write.csv for oscn
dfoscn <- data.frame (df$time, df$oscn)
dfoscn$cond <- rep("3_oscillatory_noise",length(df$time))

#plot noisey oscillatory component
ggplot( df , aes( time, oscn)) + geom_line()+ylim(-3,3)

#Define column names & write.csv for oscn
colnames(dfoscn) <- c("time", "Q","cond")
write.csv(dfoscn, "2_example_oscillation_Wnoise.csv")

#Lowess filter to determine oscillatory component 
#f= variable determined emperically (care to not overfit)
#For details see: http://stat.ethz.ch/R-manual/R-devel/library/stats/html/lowess.html
df$osc <- lowess(df$oscn, f=0.01,iter = 100)[[2]]
#plot oscillatory component
ggplot( df , aes( time, osc)) + geom_line()+ylim(-2,2)

#write oscillatory component into new data frame
dfosc <- data.frame (df$time, df$osc)
dfosc$cond <- rep("4_oscillatory",length(df$time))

#Define column names & write.csv for osc
colnames(dfosc) <- c("time", "Q","cond")
write.csv(dfosc, "2_example_oscillation_WOnois.csv")

#determine noise component
df$noise <- df$oscn-df$osc

#plot noise
ggplot( df , aes( time, noise)) + geom_line()+ylim(-2,2)

#write noise component into new data frame
dfnoise <- data.frame (df$time, df$noise)
dfnoise$cond <- rep("5_noise",length(df$time))

#Define column names & write.csv for noise
colnames(dfnoise) <- c("time", "Q","cond")
write.csv(dfnoise, "2_example_noise.csv")

#dataframe of all compoments
total1 <- rbind(total,dftrend,dfoscn,dfosc,dfnoise)
total1$cond <- as.factor(total1$cond)

#Define column names & write.csv for osc
write.csv(total1, "2_example_all.csv")

#plot decomposed components
ggplot(total1, aes(time, Q)) + geom_line() + 
  xlab("Time (sec)") + ylab("Heat dissipation (nJ/sec)")+
  facet_grid(cond ~ ., scales = "free_y", space="fixed")

#end script

