#R-script for ITC data anaylsis described in Rodenfels et al. 
#Script name: 5_CellcyleModel.R
#purpose: Mathematical modeling of the cell cycle and its heat dissipation


#load neccesary librarys
library(deSolve)
library(scatterplot3d)
library(ggplot2)


#define odinary differential equiations.
Lorenz <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dCDKa <- ksynth - kdest*APCa*CDKa + 1/sqrt(r)*kcdkon*(1+p*(CDKa^ncdc25/(CDKa^ncdc25+ec50cdc25^ncdc25)))*CDKi - sqrt(r)*kcdkoff*(1+p*(ec50wee1^nwee1/(CDKa^nwee1+ec50wee1^nwee1)))*CDKa
    dCDKi <- -kdest*APCa*CDKi - 1/sqrt(r)*kcdkon*(1+p*(CDKa^ncdc25/(CDKa^ncdc25+ec50cdc25^ncdc25)))*CDKi + sqrt(r)*kcdkoff*(1+p*(ec50wee1^nwee1/(CDKa^nwee1+ec50wee1^nwee1)))*CDKa
    dPLXa <- kplxon*(CDKa^nplx/(CDKa^nplx+ec50plx^nplx))*(1-PLXa)-kplxoff*PLXa
    dAPCa <- kapcon*(PLXa^napc/(PLXa^napc+ec50apc^napc))*(1-APCa)-kapcoff*APCa
    dS  <- kcdkoff1*SCDK - kcdkon1*S*CDKa + kcatpp2*SPPP2
    dSCDK <- kcdkon1*S*CDKa - (kcatcdk1+kcdkoff1)*SCDK
    dSP <- kcatcdk1*SCDK - kpp2on*SP*PP2 + kpp2off*SPPP2
    dPP2 <-kpp2off*SPPP2 + kcatpp2*SPPP2 - kpp2on*SP*PP2
    dSPPP2 <- -(kcatpp2+kpp2off)*SPPP2 + kpp2on*SP*PP2
    dST <- dS + dSP + dSCDK + dSPPP2
    
    
    list(c(dCDKa,dCDKi,dPLXa,dAPCa,dS, dSCDK,dSP, dPP2,dSPPP2,dST))
  })
}
#define parameters of the model
parameters <- c(ksynth = 1.5*1.9, 
                kdest = 0.4*1.9 , 
                kcdkon = 0.0354*1.9 ,
                kcdkoff = 0.0354*1.9, 
                kplxon = 1.5*1.9  ,
                kplxoff = 0.125*1.9, 
                kapcon= 1.5*1.9, 
                kapcoff=0.15*1.9, 
                r=1/32, 
                ec50cdc25=30,
                ec50wee1=35,
                ncdc25=11,
                nwee1=3.5,
                p=5,
                ec50plx=60,
                ec50apc=0.5,
                napc=4,
                nplx=5,
                kcdkon1=10,
                kcdkoff1=0.1,
                kcatcdk1=180,
                kpp2on=100,
                kpp2off=0.01,
                kcatpp2=1800)
#start values
state <- c(CDKa = 60, CDKi = 0, PLXa = 0, APCa=0,S = 60, SCDK = 0, SP=0, PP2=60, SPPP2=0,ST=60)
times <- seq(0, 200, by = 0.002)
out <- ode(y = state, times = times, func = Lorenz, parms = parameters)

#plot output data
plot(out)

#write output in a dataframe
dfa <- data.frame(out)

#subdataframes for specific
cdka <- data.frame(dfa$time,dfa$CDKa)
cdki <- data.frame(dfa$time,dfa$CDKi)
apc <- data.frame(dfa$time,dfa$APCa)
S <- data.frame(dfa$time,dfa$S)
SCDK <-data.frame(dfa$time,dfa$SCDK)
SP <- data.frame(dfa$time,dfa$SP)
PP2<- data.frame(dfa$time,dfa$PP2)
SPPP2<- data.frame(dfa$time,dfa$SPPP2)

#rename dataframes
cdka$cond <- rep("1_Active CDK1",length(dfa$time))
colnames(cdka) <- c("time", "mean","cond")

S$cond <- rep("2_Substrate",length(dfa$time))
colnames(S) <- c("time", "mean","cond")

SCDK$cond <- rep("3_SCDK complex",length(dfa$time))
colnames(SCDK) <- c("time", "mean","cond")

SP$cond <- rep("4_Phosphorylated S",length(dfa$time))
colnames(SP) <- c("time", "mean","cond")

PP2$cond <- rep("5_free PP2A",length(dfa$time))
colnames(PP2) <- c("time", "mean","cond")

SPPP2$cond <- rep("5_SPPP2 complex",length(dfa$time))
colnames(SPPP2) <- c("time", "mean","cond")

#combine dataframes
model12 <- rbind(cdka, S,SCDK)
model13 <- rbind(SP,PP2,SPPP2)
model14 <- rbind(cdka,SCDK,SP,SPPP2)

#plot subset of the model results
ggplot(model13, aes(time, mean)) + geom_line(size=0.8)+   facet_grid(cond ~ ., scales = "free_y", space="fixed")+xlim(0,150)+
  scale_y_continuous(breaks = c(-1.5,-1,-0.5,0,0.5,1,1.5), limits = c(-1.5,1.5))+geom_vline(xintercept=20,linetype="dashed")
model13 <- rbind(cdka,PP2)

#Calulcation of theoretical heat dissipation by cell cycle dephopshorylation events
dfa$heat <- dfa$SPPP2*(0.6*10^-7)*42*10^3*30

#filter heat dissipation
dfa$heat_lowess <- lowess(dfa$heat, f=0.5)[[2]]
dfa$heatosc <- dfa$heat - dfa$heat_lowess

#write data into data.frame
heat <- data.frame(dfa$time,dfa$heatosc)
heat$cond <- rep("heat model",length(dfa$time))
colnames(heat) <- c("time", "mean","cond")

#plot theoretical heat data
ggplot(heat, aes(time*60,mean))+ geom_line(size=0.8) + xlab("Time (sec)") + ylab(" (nM/sec)")+
  scale_x_continuous(breaks = c(0,1500,3000,4500,6000,7500,9000), limits = c(0,9000))

