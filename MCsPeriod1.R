
###################################################################################################################
############################################ Library Load#####################################
###################################################################################################################

library(matrixStats)
library(gridExtra)

###################################I. DEFORESTATION#############################

rm(list=ls(all=TRUE))

# Address to read inputs
setwd("C:/Users/ASUS/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR/Period1/Inputs")

### Reading of inputs
BaseDef<-0
BaseDef<-read.csv("1_U_AD_EF_Def_csv.csv")
dim(BaseDef)

###Estimation of emissions per Forest Typeand Period
#Number of years in the period
ny<-1
#Estimation of emissions per Forest Type and Period
BaseDef$Emission<-round(BaseDef$AD_ha*BaseDef$EF_tCO2_ha/1000/ny,digits = 3)
#Table of Emissions per Period
BaseEmiDefPeriod <- aggregate(data=BaseDef, Emission~Period, sum)

### Simulation of random numbers
# cumputing of Sd: DA y FE
BaseDef$DesEstDA<-abs((BaseDef$U_AD_per*BaseDef$AD_ha)/(1.96*100))
BaseDef$DesEstFE<-abs((BaseDef$U_EF_per*BaseDef$EF_tCO2_ha)/(1.96*100))

# number of simulations
n<-100000
MatrizDef<-seq(1:n)

# Simulations of total emissions
for (i in 1:length(BaseDef$Code))
{
  DAsim<-rnorm(n,mean=BaseDef$AD_ha[i], sd=BaseDef$DesEstDA[i])
  FEsim<-rnorm(n,mean=BaseDef$EF_tCO2_ha[i], sd=BaseDef$DesEstFE[i])
  EmiSim<-(DAsim*FEsim)/1000/ny
  MatrizDef<-cbind(MatrizDef,EmiSim)
}

# Convertion of Metrix to DataFrame
MatrizDFdef<-as.data.frame(MatrizDef)
MatrizDFdef$EmiTot<-rowSums(MatrizDFdef[,c(1:length(BaseDef$Code)+1)])

# Estimation of uncertainties per Period-Forest Type
for (i in 1:(length(BaseDef$Code)))
{
  BaseDef$Uinf_MMC[i]<-abs((quantile(MatrizDFdef[,i+1],0.025)-mean(MatrizDFdef[,i+1]))/mean(MatrizDFdef[,i+1]))*100
  BaseDef$Usup_MMC[i]<-abs((quantile(MatrizDFdef[,i+1],0.975)-mean(MatrizDFdef[,i+1]))/mean(MatrizDFdef[,i+1]))*100
}

# Estimaton of uncertainties per Period
MatrizDFdef$Period1<-rowSums(MatrizDFdef[,c(2:7)])
MatrizDFdef$Period2<-rowSums(MatrizDFdef[,c(8:13)])
MatrizDFdef$Period3<-rowSums(MatrizDFdef[,c(14:19)])
MatrizDFdef$Period4<-rowSums(MatrizDFdef[,c(20:25)])
MatrizDFdef$Period5<-rowSums(MatrizDFdef[,c(26:31)])
MatrizDFdef$Period6<-rowSums(MatrizDFdef[,c(32:37)])

Q_25_Period1<-quantile(MatrizDFdef$Period1,0.025)[[1]]
Q_975_Period1<-quantile(MatrizDFdef$Period1,0.975)[[1]]
Q_25_Period2<-quantile(MatrizDFdef$Period2,0.025)[[1]]
Q_975_Period2<-quantile(MatrizDFdef$Period2,0.975)[[1]]
Q_25_Period3<-quantile(MatrizDFdef$Period3,0.025)[[1]]
Q_975_Period3<-quantile(MatrizDFdef$Period3,0.975)[[1]]
Q_25_Period4<-quantile(MatrizDFdef$Period4,0.025)[[1]]
Q_975_Period4<-quantile(MatrizDFdef$Period4,0.975)[[1]]
Q_25_Period5<-quantile(MatrizDFdef$Period5,0.025)[[1]]
Q_975_Period5<-quantile(MatrizDFdef$Period5,0.975)[[1]]
Q_25_Period6<-quantile(MatrizDFdef$Period6,0.025)[[1]]
Q_975_Period6<-quantile(MatrizDFdef$Period6,0.975)[[1]]

U_Def_Period1_inf<-abs((quantile(MatrizDFdef$Period1,0.025)-mean(MatrizDFdef$Period1))/mean(MatrizDFdef$Period1))*100
U_Def_Period1_sup<-abs((quantile(MatrizDFdef$Period1,0.975)-mean(MatrizDFdef$Period1))/mean(MatrizDFdef$Period1))*100
U_Def_Period2_inf<-abs((quantile(MatrizDFdef$Period2,0.025)-mean(MatrizDFdef$Period2))/mean(MatrizDFdef$Period2))*100
U_Def_Period2_sup<-abs((quantile(MatrizDFdef$Period2,0.975)-mean(MatrizDFdef$Period2))/mean(MatrizDFdef$Period2))*100
U_Def_Period3_inf<-abs((quantile(MatrizDFdef$Period3,0.025)-mean(MatrizDFdef$Period3))/mean(MatrizDFdef$Period3))*100
U_Def_Period3_sup<-abs((quantile(MatrizDFdef$Period3,0.975)-mean(MatrizDFdef$Period3))/mean(MatrizDFdef$Period3))*100
U_Def_Period4_inf<-abs((quantile(MatrizDFdef$Period4,0.025)-mean(MatrizDFdef$Period4))/mean(MatrizDFdef$Period4))*100
U_Def_Period4_sup<-abs((quantile(MatrizDFdef$Period4,0.975)-mean(MatrizDFdef$Period4))/mean(MatrizDFdef$Period4))*100
U_Def_Period5_inf<-abs((quantile(MatrizDFdef$Period5,0.025)-mean(MatrizDFdef$Period5))/mean(MatrizDFdef$Period5))*100
U_Def_Period5_sup<-abs((quantile(MatrizDFdef$Period5,0.975)-mean(MatrizDFdef$Period5))/mean(MatrizDFdef$Period5))*100
U_Def_Period6_inf<-abs((quantile(MatrizDFdef$Period6,0.025)-mean(MatrizDFdef$Period6))/mean(MatrizDFdef$Period6))*100
U_Def_Period6_sup<-abs((quantile(MatrizDFdef$Period6,0.975)-mean(MatrizDFdef$Period6))/mean(MatrizDFdef$Period6))*100

# Quantiles of avegarage emissions
Q_25_AveEmissions<-quantile(MatrizDFdef[,i+2]/6,0.025)[[1]]
Q_975_AveEmissions<-quantile(MatrizDFdef[,i+2]/6,0.975)[[1]]

# Estimation of uncertainties for average emissions
U_AveEmi_inf_MMC_Def<-abs((quantile(MatrizDFdef[,i+2],0.025)-mean(MatrizDFdef[,i+2]))/mean(MatrizDFdef[,i+2]))*100
U_AveEmi_sup_MMC_Def<-abs((quantile(MatrizDFdef[,i+2],0.975)-mean(MatrizDFdef[,i+2]))/mean(MatrizDFdef[,i+2]))*100

#Saving of quantiles at Period level
BaseEmiDefPeriod$Q2_5<-0
BaseEmiDefPeriod$Q2_5[1]<-round(Q_25_Period1, digits = 3)
BaseEmiDefPeriod$Q2_5[2]<-round(Q_25_Period2, digits = 3)
BaseEmiDefPeriod$Q2_5[3]<-round(Q_25_Period3, digits = 3)
BaseEmiDefPeriod$Q2_5[4]<-round(Q_25_Period4, digits = 3)
BaseEmiDefPeriod$Q2_5[5]<-round(Q_25_Period5, digits = 3)
BaseEmiDefPeriod$Q2_5[6]<-round(Q_25_Period6, digits = 3)
BaseEmiDefPeriod$Q97_5<-0
BaseEmiDefPeriod$Q97_5[1]<-round(Q_975_Period1, digits = 3)
BaseEmiDefPeriod$Q97_5[2]<-round(Q_975_Period2, digits = 3)
BaseEmiDefPeriod$Q97_5[3]<-round(Q_975_Period3, digits = 3)
BaseEmiDefPeriod$Q97_5[4]<-round(Q_975_Period4, digits = 3)
BaseEmiDefPeriod$Q97_5[5]<-round(Q_975_Period5, digits = 3)
BaseEmiDefPeriod$Q97_5[6]<-round(Q_975_Period6, digits = 3)

# Saving of uncertainties at Period level
BaseEmiDefPeriod$U_Def_inf<-0
BaseEmiDefPeriod$U_Def_inf[1]<-round(U_Def_Period1_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_inf[2]<-round(U_Def_Period2_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_inf[3]<-round(U_Def_Period3_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_inf[4]<-round(U_Def_Period4_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_inf[5]<-round(U_Def_Period5_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_inf[6]<-round(U_Def_Period6_inf[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup<-0
BaseEmiDefPeriod$U_Def_sup[1]<-round(U_Def_Period1_sup[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup[2]<-round(U_Def_Period2_sup[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup[3]<-round(U_Def_Period3_sup[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup[4]<-round(U_Def_Period4_sup[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup[5]<-round(U_Def_Period5_sup[[1]], digits = 1)
BaseEmiDefPeriod$U_Def_sup[6]<-round(U_Def_Period6_sup[[1]], digits = 1)

# Saving of Totals
BaseEmiDefTot<-data.frame(Period="FREL",
                          Emission=sum(BaseEmiDefPeriod$Emission,na.rm = TRUE)/6,
                          Q2_5=round(Q_25_AveEmissions, digits = 3),
                          Q97_5=round(Q_975_AveEmissions, digits = 3),
                          U_Def_inf=round(U_AveEmi_inf_MMC_Def[[1]], digits = 1),
                          U_Def_sup=round(U_AveEmi_sup_MMC_Def[[1]], digits = 1))

BaseEmiDefPeriodFREL<-rbind(BaseEmiDefPeriod, BaseEmiDefTot)


### Density Functions of Emissions from Deforestation

# Address to save outputs
setwd("C:/Users/ASUS/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR/Period1/Outputs")


# Density functions from deforestation per Island

pdf("1_DensityFun_DEFORESTATION.pdf")

par(mfrow=c(3,2))

# hist Def: Period 1
hist(MatrizDFdef$Period1, 
     main="PDF Emissions from Deforestation Period 1", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period1/1000))
# hist Def: Period 2
hist(MatrizDFdef$Period2/1000, 
     main="PDF Emissions from Deforestation Period 2", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period2/1000))
# hist Def: Period 3
hist(MatrizDFdef$Period3/1000, 
     main="PDF Emissions from Deforestation Period 3", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period3/1000))
# hist Def: Period 4
hist(MatrizDFdef$Period4/1000, 
     main="PDF Emissions from Deforestation Period 4", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period4/1000))
# hist Def: Period 5
hist(MatrizDFdef$Period5/1000, 
     main="PDF Emissions from Deforestation Period 5", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period5/1000))
# hist Def: Period 6
hist(MatrizDFdef$Period6/1000, 
     main="PDF Emissions from Deforestation Period 6", 
     xlab="Annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="gray",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef$Period6/1000))

#cierra el archivo de gráficos
dev.off()


### Table of emissions and uncertanties from Deforestation

pdf("2_EstEmiUncer_FREL.pdf")

par(mfrow=c(3,1))

hist(MatrizDFdef[,i+2]/6/1000, 
     main="PDF of FREL", 
     xlab="Average annual emissions from deforestation (Gg of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="red",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(MatrizDFdef[,i+2]/6/1000))

colnames(BaseEmiDefPeriodFREL) <- c("Period","Emissions GgCO2e","Quantile 2.5","Quantile 97.5","Lower Uncertainties (%)","Upper Uncertainties (%)")
tt <- ttheme_default(base_size = 7)
grid.table(BaseEmiDefPeriodFREL,theme=tt)

dev.off()


### Saving of Outputs
write.csv(MatrizDFdef, file = "1_MatrixSimDef.csv")
write.csv(BaseDef, file = "2_TableUncerDef_PeriodLevel.csv")
write.csv(BaseEmiDefPeriodFREL, file = "3_TableUncerDef_NationalLevel.csv")


################################### END ##############################





