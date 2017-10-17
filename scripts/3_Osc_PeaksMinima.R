#R-script for ITC data anaylsis described in Rodenfels et al. 
#Script name: 3_Osc_PeaksMinima.R
#purpose: indentification oscillation peaks and minima
#the raw .itc file has been converted to .csv prior to this script using a text editor of your choice,
#columns have been renamed as time, dp, temp and the @ signs have been manually removed in the data
#example files provided: 2_example_oscillation_WOnoise.csv
#outputfile: 
#3_osc_peaks.csv
#3_osc_min.csv

#load neccesary librarys
library(ggplot2)

#set working directory
setwd("~/Documents/1_PostDoc/1_EnergyWorkHeat/7_publication/scripts/")

#read ITC raw data & create data frame
df <- read.csv("2_example_oscillation_WOnoise.csv" , check.names = FALSE) 

#function to find mins and max

#http://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks <- function (x, m = 50){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
#find min and max on filtered osscilation data
peaks <- find_peaks(df$Q)
peaksmin <- find_peaks(-df$Q)

#plot mins and max as lines on oscillations
ggplot( df , aes( time , Q)) + geom_line() +geom_vline(xintercept=peaks)
ggplot( df , aes( time , Q)) + geom_line() +geom_vline(xintercept=peaksmin)


#write mins and max in dataframe & csv files
dfpeaks <- data.frame(peaks)
dfmin <-data.frame(peaksmin)

write.csv(dfmin, "3_osc_peaks.csv")
write.csv(dfmin, "3_osc_min.csv")


