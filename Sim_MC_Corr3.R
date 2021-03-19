
###############################################################################
################# Library Loading and reading of inputs     ###################
###############################################################################

library(matrixStats)
library(gridExtra)
library(reshape)

###############################################################################

rm(list=ls(all=TRUE))

### Address to read inputs
setwd("C:/Users/ochoa/Documents/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Inputs")

### Reading of inputs
# AD inputs
BaseDef<-read.csv("1_U_AD_EF_Def_csv.csv")
BaseDef <- BaseDef[,c(1:5)]
dim(BaseDef)

# EF inputs
BaseEF<-read.csv("2_U_EF_ABG_BGB.csv")
dim(BaseEF)


###############################################################################
####################  1. Simulations of Activity Data       ###################
###############################################################################


#### Computing of SE of AD
BaseDef$DesEstDA<-abs((BaseDef$U_AD_per*BaseDef$AD_ha)/(1.96*100))

### Number of simulations
n<-100000
MatrizDef<-seq(1:n)

### Simulations of AD per period and convertions
for (i in 1:length(BaseDef$Code))
{
  DAsim<-rnorm(n,mean=BaseDef$AD_ha[i], sd=BaseDef$DesEstDA[i])
  MatrizDef<-cbind(MatrizDef,DAsim)
}

### Convertion of AD Matrix to DataFrame 
MatrizDFdef<-as.data.frame(MatrizDef)

### Correct colnames of AD-Dataframe per "Period" and "Transition"
colnames(MatrizDFdef) = c("Id_sim_AD",
                          "AD_P1_FT1_nFT","AD_P1_FT2_nFT",
                          "AD_P2_FT1_nFT","AD_P2_FT2_nFT",
                          "AD_P3_FT1_nFT","AD_P3_FT2_nFT",
                          "AD_P4_FT1_nFT","AD_P4_FT2_nFT",
                          "AD_P5_FT1_nFT","AD_P5_FT2_nFT",
                          "AD_P6_FT1_nFT","AD_P6_FT2_nFT",
                          "AD_P7_FT1_nFT","AD_P7_FT2_nFT",
                          "AD_P8_FT1_nFT","AD_P8_FT2_nFT",
                          "AD_P9_FT1_nFT","AD_P9_FT2_nFT",
                          "AD_P10_FT1_nFT","AD_P10_FT2_nFT")


###############################################################################
##################  2. Simulations of Emissions Factors    ####################
###############################################################################

###############################################################################
### Simulations and CF, R:S and AGB per Forest Type

### Matrix for saving simulated CF, R:S and AGB 
MatrizEF_Def <-seq(1:n)

### Simulations of CF, R:S and AGB
for (i in 1:length(BaseEF$Value))
{
  EF_Sim<-rnorm(n,mean=BaseEF$Value[i], sd=BaseEF$SE[i])
  MatrizEF_Def<-cbind(MatrizEF_Def,EF_Sim)
}

### Converting the "CF, R:S and AGB" Matrix to DataFrame
MatrizEF_Def<-as.data.frame(MatrizEF_Def)

### Correct colnames of "CF, R:S and AGB" Dataframe
colnames(MatrizEF_Def) = c("Id_sim_EF","CF","Root_S","AGB_FT1","AGB_FT2","AGB_nFT")

###############################################################################
### Simulation of BGB, Carbon Densities and EF per Transition

### Simulation of BGB per stratum
MatrizEF_Def$BGB_FT1<- MatrizEF_Def$AGB_FT1 * MatrizEF_Def$Root_S
MatrizEF_Def$BGB_FT2<- MatrizEF_Def$AGB_FT2 * MatrizEF_Def$Root_S
MatrizEF_Def$BGB_nFT<- MatrizEF_Def$AGB_nFT * MatrizEF_Def$Root_S

### Simulation of Carbon Densities per stratum
MatrizEF_Def$C_FT1  <- (MatrizEF_Def$AGB_FT1+MatrizEF_Def$BGB_FT1 ) * MatrizEF_Def$CF 
MatrizEF_Def$C_FT2  <- (MatrizEF_Def$AGB_FT2+MatrizEF_Def$BGB_FT2 ) * MatrizEF_Def$CF
MatrizEF_Def$C_nFT  <- (MatrizEF_Def$AGB_nFT+MatrizEF_Def$BGB_nFT ) * MatrizEF_Def$CF

### Simulation of EF per Transition
MatrizEF_Def$EF_FT1_nFT<- MatrizEF_Def$C_FT1 - MatrizEF_Def$C_nFT
MatrizEF_Def$EF_FT2_nFT<- MatrizEF_Def$C_FT2 - MatrizEF_Def$C_nFT

### Filtering of simulated EF per Transition
EF_DEFOREST<-data.frame(Id_sim_EF=MatrizEF_Def$Id_sim_EF,EF_FT1_nFT=MatrizEF_Def$EF_FT1_nFT, 
                        EF_FT2_nFT=MatrizEF_Def$EF_FT2_nFT)
length(EF_DEFOREST$Id_sim_EF)


###############################################################################
###########################  3. Emission Estimation  ##########################
###############################################################################

### Merge of "AD-Dataframe" and "EF-Dataframe"
Table_Emi<- merge(MatrizDFdef, EF_DEFOREST, by.x = "Id_sim_AD", by.y = "Id_sim_EF",all=T)
dim(Table_Emi)

### Estimation of Emission per Period and Transition
Table_Emi$Emi_P1_TF1_nNF <- Table_Emi$AD_P1_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P1_TF2_nNF <- Table_Emi$AD_P1_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P2_TF1_nNF <- Table_Emi$AD_P2_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P2_TF2_nNF <- Table_Emi$AD_P2_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P3_TF1_nNF <- Table_Emi$AD_P3_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P3_TF2_nNF <- Table_Emi$AD_P3_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P4_TF1_nNF <- Table_Emi$AD_P4_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P4_TF2_nNF <- Table_Emi$AD_P4_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P5_TF1_nNF <- Table_Emi$AD_P5_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P5_TF2_nNF <- Table_Emi$AD_P5_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P6_TF1_nNF <- Table_Emi$AD_P6_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P6_TF2_nNF <- Table_Emi$AD_P6_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P7_TF1_nNF <- Table_Emi$AD_P7_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P7_TF2_nNF <- Table_Emi$AD_P7_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P8_TF1_nNF <- Table_Emi$AD_P8_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P8_TF2_nNF <- Table_Emi$AD_P8_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P9_TF1_nNF <- Table_Emi$AD_P9_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P9_TF2_nNF <- Table_Emi$AD_P9_FT2_nFT* Table_Emi$EF_FT2_nFT

Table_Emi$Emi_P10_TF1_nNF <- Table_Emi$AD_P10_FT1_nFT* Table_Emi$EF_FT1_nFT
Table_Emi$Emi_P10_TF2_nNF <- Table_Emi$AD_P10_FT2_nFT* Table_Emi$EF_FT2_nFT


###############################################################################
########### 3.1 Simulation of Emissions of Base Line  (7 periods)##############

### Selectiting of emissions for Base Line period
Table_Emi_FREL<- Table_Emi[, c(1,24:37)]
Table_Emi_FREL$FREL<- ( rowSums(Table_Emi_FREL[,c(2:15)]) ) / 7

### Quantiles  and Uncertainties estimations of average annual emissions for Base Line
Q_25_FREL  <-quantile(Table_Emi_FREL$FREL,0.025)[[1]]
Q_975_FREL <-quantile(Table_Emi_FREL$FREL,0.975)[[1]]
U_FREL_inf<-abs((quantile(Table_Emi_FREL$FREL,0.025)-mean(Table_Emi_FREL$FREL))/
                         mean(Table_Emi_FREL$FREL))*100
U_FREL_sup<-abs((quantile(Table_Emi_FREL$FREL,0.975)-mean(Table_Emi_FREL$FREL))/
                         mean(Table_Emi_FREL$FREL))*100

### Saving of Base Line emissions and associated quantiles and Uncertainties
Table_FREL<-data.frame(Period="FREL",
                          Emission=mean(Table_Emi_FREL$FREL),
                          Q2_5=round(Q_25_FREL, digits = 3),
                          Q97_5=round(Q_975_FREL, digits = 3),
                          U_inf=round(U_FREL_inf, digits = 1),
                          U_sup=round(U_FREL_sup, digits = 1))

### Saving of PDF of Base Line emissions
setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")

pdf("1_Emissions_Uncertainty_FREL.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_FREL$FREL, 
     main="PDF of FREL", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="green",
     las=1, 
     breaks=100,
     prob = TRUE)
lines(density(Table_Emi_FREL$FREL ))
dev.off()

###############################################################################
########## 3.2 Simulation of Emissions in Credit Period  (3 Periods) ##########

### Selectiting of emissions for Credit Period
Table_Emi_CP<- Table_Emi[, c(1,38:43)]
Table_Emi_CP$CP<- ( rowSums(Table_Emi_CP[,c(2:7)]) ) / 3

### Quantiles and Uncertanties estimations of average annual emissions for Credit Period
Q_25_CP  <-quantile(Table_Emi_CP$CP,0.025)[[1]]
Q_975_CP <-quantile(Table_Emi_CP$CP,0.975)[[1]]
U_CP_inf<-abs((quantile(Table_Emi_CP$CP,0.025)-mean(Table_Emi_CP$CP))/
                      mean(Table_Emi_CP$CP))*100
U_CP_sup<-abs((quantile(Table_Emi_CP$CP,0.975)-mean(Table_Emi_CP$CP))/
                      mean(Table_Emi_CP$CP))*100

### Saving of Credit Period emissions and associated quantiles and uncertanties
Table_CP<-data.frame(Period="CP",
                       Emission=mean(Table_Emi_CP$CP),
                       Q2_5=round(Q_25_CP, digits = 3),
                       Q97_5=round(Q_975_CP, digits = 3),
                       U_inf=round(U_CP_inf, digits = 1),
                       U_sup=round(U_CP_sup, digits = 1))

### Saving of PDF of Credit Period emissions
setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")

pdf("2_Emissions_Uncertainty_CP.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_CP$CP, 
     main="PDF of CP", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="blue",
     las=1, 
     breaks=100,
     prob = TRUE)
lines(density(Table_Emi_CP$CP ))
dev.off()


###############################################################################
###################  3.3 Simulation of Emission Reduction    ##################

### Simulated Emissions Redutions
Tab_Reduc <-0
Tab_Reduc<- Table_Emi_FREL$FREL-Table_Emi_CP$CP

### Quantiles and Uncertanties estimations of average annual emissions reductions
Q_25_ER  <-quantile(Tab_Reduc,0.025,na.rm=T)[[1]]
Q_975_ER <-quantile(Tab_Reduc,0.975,na.rm=T)[[1]]
U_ER_inf<-abs((quantile(Tab_Reduc,0.025,na.rm=T)-mean(Tab_Reduc))/ mean(Tab_Reduc))*100
U_ER_sup<-abs((quantile(Tab_Reduc,0.975,na.rm=T)-mean(Tab_Reduc))/ mean(Tab_Reduc))*100

# Saving of Emission Reductions
Table_ER<-data.frame(Period="ER",
                            Emission=mean(Tab_Reduc,na.rm = T),
                            Q2_5=round(Q_25_ER, digits = 3),
                            Q97_5=round(Q_975_ER, digits = 3),
                            U_inf=round(U_ER_inf, digits = 1),
                            U_sup=round(U_ER_sup, digits = 1))


###############################################################################
###############  4. Summary of Monte Carlo Simulation Analysis  ###############
###############################################################################

#### Table of simulated Base Line emissions, Credit Period emissions and Emissions Reductions
BaseEmi_A <-rbind(Table_FREL, Table_CP)
BaseEmi_Al <-rbind(BaseEmi_A, Table_ER)
BaseEmi_All <- data.frame(BaseEmi_Al)
dim(BaseEmi_All)

#### Saving of PDF of Base Line emissions, Credit Period emissions and Emissions Reduction

setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")

pdf("3_Estimate_Emission_Uncer_MCM.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_FREL$FREL, 
     main="PDF of FREL", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="green",
     las=1, 
     breaks=100,
     prob = TRUE)
lines(density(Table_Emi_FREL$FREL ))
hist(Table_Emi_CP$CP, 
     main="PDF of CP", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="blue",
     las=1, 
     breaks=100,
     prob = TRUE)
lines(density(Table_Emi_CP$CP ))
par(mfrow=c(3,1))
hist(Tab_Reduc, 
     main="PDF of ER", 
     xlab="Emissions reductions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="red",
     las=1, 
     breaks=100,
     prob = TRUE)
lines(density(Tab_Reduc ))

tt <- ttheme_default(base_size = 7)
grid.table(BaseEmi_All,theme=tt)

dev.off()


### Saving of Outputs
write.csv(MatrizDFdef, file = "1_Matrix_DA.csv")
write.csv(EF_DEFOREST, file = "1_Matrix_FE.csv")
write.csv(Table_Emi_FREL, file = "2_simulated_emissions_FREL.csv")
write.csv(Table_Emi_CP,   file = "2_simulated_emissions_CP.csv")
write.csv(BaseEmi_All,    file = "3_summary_ER.csv")

###############################################################################
####################################  END  ####################################
###############################################################################









