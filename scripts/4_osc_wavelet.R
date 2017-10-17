#R-script for ITC data anaylsis described in Rodenfels et al. 
#Script name: 4_Osc_wavelet.R
#purpose: Wavelet transform of the oscillatory component
#example files provided: 2_example_oscillation_WOnoise.csv


#load libraries
library(ggplot2)
library(WaveletComp)

#set working directory
setwd("")

#load data
df<- read.csv("")

#plot data (check)
ggplot(df, aes(time,Q))+geom_line(size=1)+ylim(-2,2)

#wavelet transform (for details see:
#http://www.hs-stat.com/projects/WaveletComp/WaveletComp_guided_tour.pdf
#https://cran.r-project.org/web/packages/WaveletComp/index.html)

my.w = analyze.wavelet(df, "Q",
                       loess.span = 0,
                       dt = 1, dj = 1/200,
                       lowerPeriod = 64,
                       upperPeriod = 2048,
                       make.pval = T, n.sim = 10)

#plot wavelet transform power in period/time domain
wt.image(my.w, my.series = 1,
         plot.coi = T,
         plot.contour = T, siglvl = 0.05, col.contour = "white",
         plot.ridge = T, lvl = 0.1, col.ridge = "black",
         color.key = "i",
         n.levels = 250, color.palette="rainbow(n.levels, start=0, end=.7)",
         useRaster = T, max.contour.segments = 250000,
         plot.legend = T,
         legend.params = list(width = 1.2, shrink = 0.9, mar = 5.1,
                              n.ticks = 4, label.digits = 2, label.format = "f",
                              lab = "wavelet power", lab.line = 2.5),
         label.time.axis = T, show.date = F, date.format = NULL, timelab = NULL,
         label.period.axis = T, periodlab = NULL,
         main = NULL,
         lwd = 2,
         graphics.reset = T,
         verbose = T)

# plot period vs averave wavelet power 
wt.avg(my.w, my.series = 1,
       show.siglvl = T, siglvl = c(0.05), sigcol = c("black", "blue"), sigpch = 20,
       label.avg.axis = T, averagelab = NULL,
       label.period.axis = T, periodlab = NULL,
       show.legend = T, legend.coords = "topright",
       main = NULL, lwd = 0.5,
       verbose = T)

#extract peroid vs average wavelet power
period <- my.w$Period
power <- my.w$Power.avg
#write new data frame for period/sigificant avg. power plot
cv <- data.frame(period,power)
colnames(cv) <- c("period", "power")

#determine peak in the period/power plot =  average period
per <- cv$period[cv$power==max(cv$power)]


#plot period/power plot
ggplot(cv, aes(power,period))+geom_point()+geom_hline(yintercept=per)

#end


