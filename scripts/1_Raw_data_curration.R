#R-script for ITC data anaylsis described in Rodenfels et al. 
#Script name: Raw_data_curration
#purpose: indentification of pre-injection peak data and initial time normalization of raw ITC data
#the raw .itc file has been converted to .csv prior to this script using a text editor of your choice,
#columns have been renamed as time, dp, temp and the @ signs have been manually removed in the data
#example files provided: example.itc & example.csv
#output file: exampleNorm.csv


#load neccesary librarys
library(ggplot2)

#set working directory
setwd("~/Documents/1_PostDoc/1_EnergyWorkHeat/7_publication/scripts/")

#read ITC raw data & create data frame
df <- read.csv("1_example.csv" , check.names = FALSE) 

#plot data raw data 
ggplot( df , aes(time , dp)) + geom_line()

#define time range of the raw data
#idenfication of pre- &post- injection peak time range
#pre-injection peak data
dfpre =df[1:4130,]
#plot pre-injection peak data & identification of last y value pre-injection
ggplot( dfpre , aes(time , dp)) + geom_line()
#y value=10.88

#post-injection peak data
dfpost=df[4350:18000,]
#plot pre-injection peak data & identification of last y value pre-injection
ggplot( dfpost , aes(time , dp)) + geom_line()
#first time value post-injection =4350 sec

#write post-injection peak data into new dataframe 
dfnew <- data.frame(dfpost$time,dfpost$dp)
#define cole names)
colnames(dfnew) <- c("time", "dp")

#substract initial signal
#dfnorm <-(dfpost$dp-x)*-1
#with x=last y (here x=10.885) value of the pre-inejection data rage before embryo injection
dfnorm <-(dfpost$dp-10.88)*-1

#transform heat data from ucal/sec to nj/sec normalized per embryo
dfnorm1 <-(dfnorm*4.184)/X*1000
#X=number of embryos here 30
dfnorm1 <-(dfnorm*4.184)/30*1000

#time normalization of post-injection peak data to t=1
#timerom <-(dfpost$time -x)
# with x= time value -1 (here x=4350-1=4349) of the first data in the post-injection peak data
timerom <-(dfpost$time -4349)
#combine new time and heat values into a new data.frame
dffinal <- data.frame (timerom, dfnorm1)
colnames(dffinal) <- c("time", "dp")

#plot of time normalized raw data in a csv file
ggplot( dffinal, aes(time , dp)) + geom_line()+
  xlab("Time (s)") + ylab("Heat dissipation (nJ/sec)")

#write time normalized raw data in a csv file
write.csv(dffinal, "1_example_Norm.csv")

#plot data to manually identify the first oscilliatory minima on the time axis
ggplot( dffinal, aes(time , dp)) + geom_line()+
  xlab("Time (s)") + ylab("Heat dissipation (nJ/sec)")

#plot data to manually identify the first oscilliatory minima on the time axis
#adjust xlim and ylim
#adjust geom_vline(xintercept=x), x=first minima
ggplot( dffinal, aes(time , dp)) + geom_line()+geom_vline(xintercept=300)+xlim(0,5000)+ylim(60,80)+
  xlab("Time (s)") + ylab("Heat dissipation (nJ/sec)")

#minima here x=300

#Time normalization of the data
datasub <- subset(dffinal, time>x,)
#x=300 (first minima)
datasub <- subset(dffinal, time>300,)

#Normalize time time
time_norm <- datasub$time-300

#new time normalized data frame
data_TN <- data.frame(time_norm,datasub$dp)
#change col names
colnames(data_TN) <- c("time", "dp")

#plot time normalized data
ggplot(data_TN, aes(time,dp))+geom_line()

#write time normalized raw data in a csv file
write.csv(data_TN, "1_example_timeNorm.csv")

#end script
#next script: 2_ITC_decompose.R
