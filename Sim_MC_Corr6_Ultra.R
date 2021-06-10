
###############################################################################
################# Library Loading and reading of inputs     ###################
###############################################################################

library(matrixStats)
library(gridExtra)
library(reshape)

###############################################################################

rm(list=ls(all=TRUE))

### Address to read inputs
#setwd("C:/Users/ochoa/Documents/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Inputs")
setwd("C:/Users/marco/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Inputs")

### Reading of inputs
# AD inputs
BaseAD <- read.csv("1_Activity_Data.csv")
dim(BaseAD)

BaseAD_4yr <- read.csv("1_Activity_Data_4yr.csv")
dim(BaseAD_4yr)

# EF inputs
BaseEF <- read.csv("2_Emission_Factors.csv")
dim(BaseEF)

###############################################################################
####################  1. Simulations of Activity Data       ###################
###############################################################################


#### Computing of SD of AD
BaseAD$DesEstDA<-abs((BaseAD$U_AD_per*BaseAD$AD_ha)/(1.96*100))

### Number of simulations
n<-1000000
MatrizDef<-seq(1:n)
### Switch for AD if it is 1 incorporates MC and if it is 0 then use simple value
SWITCH_AD =1

### Simulations of AD per period and conversions
for (i in 1:length(BaseAD$Code))
{
  DAsim<-rnorm(n,mean=BaseAD$AD_ha[i], sd=BaseAD$DesEstDA[i]*SWITCH_AD)
  MatrizDef<-cbind(MatrizDef,DAsim)
}

### Convertion of AD Matrix to DataFrame 
Matrix_AD<-as.data.frame(MatrizDef)

### Correct colnames of AD-Dataframe per "Period" and "Transition"
colnames(Matrix_AD) = c("Id_sim_AD",
                        "AD_P1_FT1_NF","AD_P1_FT2_NF",
                        "AD_P2_FT1_NF","AD_P2_FT2_NF",
                        "AD_P3_FT1_NF","AD_P3_FT2_NF")
dim(Matrix_AD)


###############################################################################
##################  2. Simulations of Emissions Factors    ####################
###############################################################################

###############################################################################
### Simulations and CF, R:S and AGB  per Forest Type and Transition

### Matrix for saving simulated CF, R:S and AGB 
MatrizEF_Def <-seq(1:n)
### Switches for EF
SWITCH_CF_RT =1
SWITCH_EF =1

### Simulations of CF, R:S and AGB
for (i in 1:length(BaseEF$Value))
{
  EF_Sim<-rnorm(n,mean=BaseEF$Value[i], sd=BaseEF$SE[i]*SWITCH_EF)
  MatrizEF_Def<-cbind(MatrizEF_Def,EF_Sim)
}

### Converting the "CF, R:S and AGB" Matrix to DataFrame
Matrix_EF<-as.data.frame(MatrizEF_Def)

### Correct colnames of "CF, R:S and AGB" Dataframe
colnames(Matrix_EF) = c("Id_sim_EF","CF","Root_S","AGB_FT1","AGB_FT2","AGB_NF")

###############################################################################
### Simulation of BGB, Carbon Densities and EF per Transition

### Simulation of BGB per stratum
Matrix_EF$BGB_FT1 <-   Matrix_EF$AGB_FT1  * Matrix_EF$Root_S
Matrix_EF$BGB_FT2 <-   Matrix_EF$AGB_FT2  * Matrix_EF$Root_S
Matrix_EF$BGB_NF  <-   Matrix_EF$AGB_NF   * Matrix_EF$Root_S

### Simulation of Carbon Densities per stratum
Matrix_EF$C_FT1  <- (Matrix_EF$AGB_FT1 + Matrix_EF$BGB_FT1 ) * Matrix_EF$CF 
Matrix_EF$C_FT2  <- (Matrix_EF$AGB_FT2 + Matrix_EF$BGB_FT2 ) * Matrix_EF$CF
Matrix_EF$C_NF   <- (Matrix_EF$AGB_NF  + Matrix_EF$BGB_NF  ) * Matrix_EF$CF

### Simulation of EF per Transition
Matrix_EF$EF_FT1_NF <- Matrix_EF$C_FT1 - Matrix_EF$C_NF
Matrix_EF$EF_FT2_NF <- Matrix_EF$C_FT2 - Matrix_EF$C_NF

### Filtering of simulated EF per Transition
Matrix_EF1<-data.frame(Id_sim_EF =Matrix_EF$Id_sim_EF,
                       EF_FT1_NF =Matrix_EF$EF_FT1_NF, 
                       EF_FT2_NF =Matrix_EF$EF_FT2_NF)
length(Matrix_EF1$Id_sim_EF)
dim(Matrix_EF1)


###############################################################################
###########################  3. Emission Estimation  ##########################
###############################################################################

### Merge of "AD-Dataframe" and "EF-Dataframe"
Table_Emi<- merge(Matrix_AD, Matrix_EF1, by.x = "Id_sim_AD", by.y = "Id_sim_EF",all=T)

### Estimation of Emission per Period and Transition annualized
yearP1=7
yearP2=3
yearP3=2

Table_Emi$Emi_P1_FT1_NF   <- Table_Emi$AD_P1_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P1_FT2_NF   <- Table_Emi$AD_P1_FT2_NF * Table_Emi$EF_FT2_NF
Table_Emi$Emi_P2_FT1_NF   <- Table_Emi$AD_P2_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P2_FT2_NF   <- Table_Emi$AD_P2_FT2_NF * Table_Emi$EF_FT2_NF
Table_Emi$Emi_P3_FT1_NF   <- Table_Emi$AD_P3_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P3_FT2_NF   <- Table_Emi$AD_P3_FT2_NF * Table_Emi$EF_FT2_NF
dim(Table_Emi)

###############################################################################
########### 3.1 Simulation of Emissions of Base Line  (2 Periods,7 and 3 years)  ####

### Selecting of emissions for Base Line period
Table_Emi_FREL<- Table_Emi[, c(1,10:13)]
Table_Emi_FREL$FREL<- ( rowSums(Table_Emi_FREL[,c(2:5)]) ) / (yearP1+yearP2)
### Quantiles  and Uncertainties estimations of average annual emissions for Base Line
Q_05_FREL <-quantile(Table_Emi_FREL$FREL,0.05)[[1]]
Q_95_FREL <-quantile(Table_Emi_FREL$FREL,0.95)[[1]]
half_CI <- (Q_95_FREL - Q_05_FREL)/2
U_FREL<-abs( half_CI / median(Table_Emi_FREL$FREL))*100
### Saving of Base Line emissions and associated quantiles and Uncertainties
Table_FREL<-data.frame(Period = "FREL",
                       Emission = median(Table_Emi_FREL$FREL),
                       Q_05 = round(Q_05_FREL , digits = 3),
                       Q_95 = round(Q_95_FREL , digits = 3),
                       Uncertainty = round(U_FREL, digits = 2))
Table_FREL

### Saving of PDF of Base Line emissions
#setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")
setwd("C:/Users/marco/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Outputs")

pdf("1_Emissions_Uncertainty_FREL.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_FREL$FREL, 
     main="Histogram of FREL", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
      #border="blue", 
     col="green",
     las=1, 
     breaks = 200,
     prob = TRUE)
lines(density(Table_Emi_FREL$FREL ))
dev.off()

###############################################################################
########## 3.2 Simulation of Emissions in Credit Period  (3 years)   ##########

### Selecting of emissions for Credit Period
Table_Emi_CP<- Table_Emi[, c(1,14:15)]
Table_Emi_CP$CP<- ( rowSums(Table_Emi_CP[,c(2:3)]) ) / yearP3
### Quantiles and Uncertanties estimations of average annual emissions for Credit Period
Q_05_CP  <-quantile(Table_Emi_CP$CP,0.05)[[1]]
Q_95_CP  <-quantile(Table_Emi_CP$CP,0.95)[[1]]
half_CI <- (Q_95_CP - Q_05_CP)/2
U_CP<-abs( half_CI / median(Table_Emi_CP$CP))*100
### Saving of Credit Period emissions and associated quantiles and uncertainties
Table_CP<-data.frame(Period="CP",
                     Emission = median(Table_Emi_CP$CP),
                     Q_05 = round(Q_05_CP, digits = 3),
                     Q_95 = round(Q_95_CP, digits = 3),
                     Uncertainty = round(U_CP, digits = 2))
Table_CP

### Saving of PDF of Credit Period emissions
#setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")
setwd("C:/Users/marco/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Outputs")

pdf("2_Emissions_Uncertainty_CP.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_CP$CP, 
     main="Histogram of CP", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="blue",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(Table_Emi_CP$CP ))
dev.off()

###############################################################################
###################  3.3 Simulation of Emission Reduction    ##################

### Simulated Emissions Reductions
Tab_Reduc <-0
Tab_Reduc<- Table_Emi_FREL$FREL-Table_Emi_CP$CP
### Quantiles and Uncertainties estimations of average annual emissions reductions
Q_05_ER <-quantile(Tab_Reduc,0.05,na.rm=T)[[1]]
Q_95_ER <-quantile(Tab_Reduc,0.95,na.rm=T)[[1]]
half_CI <- (Q_95_ER - Q_05_ER)/2
U_ER<-abs( half_CI / median(Tab_Reduc))*100
# Saving of Emission Reductions
Table_ER <- data.frame(Period = "ER",
                       Emission = median(Tab_Reduc,na.rm = T),
                       Q_05 = round(Q_05_ER, digits = 3),
                       Q_95 = round(Q_95_ER, digits = 3),
                       Uncertainty = round(U_ER, digits = 2))
Table_ER


###############################################################################
###############  4. Summary of Monte Carlo Simulation Analysis  ###############
###############################################################################

#### Table of simulated Base Line emissions, Credit Period emissions and Emissions Reductions
BaseEmi_A <-rbind(Table_FREL, Table_CP)
BaseEmi_Al <-rbind(BaseEmi_A, Table_ER)
BaseEmi_All <- data.frame(BaseEmi_Al)
dim(BaseEmi_All)

### Printing a summary table
grid.table(BaseEmi_All,theme = ttheme_default(base_size = 12))

#### Saving of PDF of Base Line emissions, Credit Period emissions and Emissions Reduction

setwd("C:/Users/marco/MEGA/SILVACARBON/PRODUCTO4/MCScodeR_Correlation/Period1/Outputs")
#setwd("C:/Users/oswal/Desktop/UTEMRV/15 Propuestas de Financiamiento/42 SilvaCarbon/Uncertanty/MCScodeR4")

pdf("3_Estimate_Emission_Uncer_MCM.pdf")
par(mfrow=c(2,1))
hist(Table_Emi_FREL$FREL, 
     main="Histogram of FREL", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="green",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(Table_Emi_FREL$FREL ))

hist(Table_Emi_CP$CP, 
     main="Histogram of CP", 
     xlab="Average annual emissions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="blue",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(Table_Emi_CP$CP ))

par(mfrow=c(3,1))
hist(Tab_Reduc, 
     main="Histogram of ER", 
     xlab="Emissions reductions from deforestation (Ton of CO2e)",
     cex.lab=1, cex.axis=0.8, cex.main=1, 
     #border="blue", 
     col="red",
     las=1, 
     breaks=200,
     prob = TRUE)
lines(density(Tab_Reduc ))

tt <- ttheme_default(base_size = 7)
grid.table(BaseEmi_All,theme=tt)

dev.off()


### Saving of Outputs
write.csv(Matrix_AD,     file = "1_Matrix_DA.csv")
write.csv(Matrix_EF1,    file = "1_Matrix_FE.csv")
write.csv(Table_Emi_FREL,file = "2_simulated_emissions_FREL.csv")
write.csv(Table_Emi_CP,  file = "2_simulated_emissions_CP.csv")
write.csv(Tab_Reduc,     file = "2_simulated_emissions_Reduc.csv")
write.csv(BaseEmi_All,   file = "3_Summary_ER.csv")


###############################################################################
###############  5. Adjustment to the 4-year scenario as a Credit period ######
###############################################################################

## Adjust sections 1 and 2 with the letter "B" and
## continue as normal in section 3.1 

###############################################################################
####################  1.B Simulations of Activity Data       ##################
###############################################################################


#### Computing of SD of AD
BaseAD_4yr$DesEstDA<-abs((BaseAD_4yr$U_AD_per*BaseAD_4yr$AD_ha)/(1.96*100))

### Number of simulations
n<-1000000
MatrizDef<-seq(1:n)
### Switch for AD if it is 1 incorporates MC and if it is 0 then use simple value
SWITCH_AD =1

### Simulations of AD per period and conversions
for (i in 1:length(BaseAD_4yr$Code))
{
  DAsim<-rnorm(n,mean=BaseAD_4yr$AD_ha[i], sd=BaseAD_4yr$DesEstDA[i]*SWITCH_AD)
  MatrizDef<-cbind(MatrizDef,DAsim)
}

### Convertion of AD Matrix to DataFrame 
Matrix_AD_4yr<-as.data.frame(MatrizDef)

### Correct colnames of AD-Dataframe per "Period" and "Transition"
colnames(Matrix_AD_4yr) = c("Id_sim_AD",
                        "AD_P1_FT1_NF","AD_P1_FT2_NF",
                        "AD_P2_FT1_NF","AD_P2_FT2_NF",
                        "AD_P3_FT1_NF","AD_P3_FT2_NF")
dim(Matrix_AD_4yr)


###############################################################################
##################  2.B. Simulations of Emissions Factors    ##################
###############################################################################

###############################################################################
### Simulations and CF, R:S and AGB  per Forest Type and Transition

### Matrix for saving simulated CF, R:S and AGB 
MatrizEF_Def <-seq(1:n)
### Switches for EF
SWITCH_CF_RT =1
SWITCH_EF =1

### Simulations of CF, R:S and AGB
for (i in 1:length(BaseEF$Value))
{
  EF_Sim<-rnorm(n,mean=BaseEF$Value[i], sd=BaseEF$SE[i]*SWITCH_EF)
  MatrizEF_Def<-cbind(MatrizEF_Def,EF_Sim)
}

### Converting the "CF, R:S and AGB" Matrix to DataFrame
Matrix_EF<-as.data.frame(MatrizEF_Def)

### Correct colnames of "CF, R:S and AGB" Dataframe
colnames(Matrix_EF) = c("Id_sim_EF","CF","Root_S","AGB_FT1","AGB_FT2","AGB_NF")

###############################################################################
### Simulation of BGB, Carbon Densities and EF per Transition

### Simulation of BGB per stratum
Matrix_EF$BGB_FT1 <-   Matrix_EF$AGB_FT1  * Matrix_EF$Root_S
Matrix_EF$BGB_FT2 <-   Matrix_EF$AGB_FT2  * Matrix_EF$Root_S
Matrix_EF$BGB_NF  <-   Matrix_EF$AGB_NF   * Matrix_EF$Root_S

### Simulation of Carbon Densities per stratum
Matrix_EF$C_FT1  <- (Matrix_EF$AGB_FT1 + Matrix_EF$BGB_FT1 ) * Matrix_EF$CF 
Matrix_EF$C_FT2  <- (Matrix_EF$AGB_FT2 + Matrix_EF$BGB_FT2 ) * Matrix_EF$CF
Matrix_EF$C_NF   <- (Matrix_EF$AGB_NF  + Matrix_EF$BGB_NF  ) * Matrix_EF$CF

### Simulation of EF per Transition
Matrix_EF$EF_FT1_NF <- Matrix_EF$C_FT1 - Matrix_EF$C_NF
Matrix_EF$EF_FT2_NF <- Matrix_EF$C_FT2 - Matrix_EF$C_NF

### Filtering of simulated EF per Transition
Matrix_EF1<-data.frame(Id_sim_EF =Matrix_EF$Id_sim_EF,
                       EF_FT1_NF =Matrix_EF$EF_FT1_NF, 
                       EF_FT2_NF =Matrix_EF$EF_FT2_NF)
length(Matrix_EF1$Id_sim_EF)
dim(Matrix_EF1)


###############################################################################
###########################  3. Emission Estimation  ##########################
###############################################################################

### Merge of "AD-Dataframe" and "EF-Dataframe"
Table_Emi<- merge(Matrix_AD_4yr, Matrix_EF1, by.x = "Id_sim_AD", by.y = "Id_sim_EF",all=T)

### Estimation of Emission per Period and Transition annualized
yearP1=7
yearP2=3
yearP3=4

Table_Emi$Emi_P1_FT1_NF   <- Table_Emi$AD_P1_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P1_FT2_NF   <- Table_Emi$AD_P1_FT2_NF * Table_Emi$EF_FT2_NF
Table_Emi$Emi_P2_FT1_NF   <- Table_Emi$AD_P2_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P2_FT2_NF   <- Table_Emi$AD_P2_FT2_NF * Table_Emi$EF_FT2_NF
Table_Emi$Emi_P3_FT1_NF   <- Table_Emi$AD_P3_FT1_NF * Table_Emi$EF_FT1_NF
Table_Emi$Emi_P3_FT2_NF   <- Table_Emi$AD_P3_FT2_NF * Table_Emi$EF_FT2_NF
dim(Table_Emi)

################ here begins subsection 3.1            ########################

###############################################################################
####################################  END  ####################################
###############################################################################








###############################################################################
### Correlacion entre variables en las emisiones

Matrix_Corr<-data.frame(var_CF     = Matrix_EF$CF,
                        var_Root_S = Matrix_EF$Root_S, 
                        var_AGB1   = Matrix_EF$AGB_FT1,
                        var_AGB2   = Matrix_EF$AGB_FT2,
                        var_AGB_NF = Matrix_EF$AGB_NF,
                        var_FREL   = Table_Emi_FREL$FREL,
                        var_CP     = Table_Emi_CP$CP)
length(Matrix_Corr$var_CF)
dim(Matrix_Corr)
head(Matrix_Corr)

correlacion <- round(cor(Matrix_Corr),digits = 4)
correlacion1 <- data.frame(correlacion)

### ver tabla impresa
tabla_corr_1 <- grid.table(correlacion1,theme = ttheme_default(base_size = 15))
tabla_corr_1

### Correlacion entre variables de FE en las emisiones

Matrix_Corr_A<-data.frame(var_AGB1_EF = Matrix_EF1$EF_FT1_NF,
                        var_AGB2_EF = Matrix_EF1$EF_FT2_NF,
                        var_FREL   = Table_Emi_FREL$FREL,
                        var_CP     = Table_Emi_CP$CP)
length(Matrix_Corr_A$var_AGB1_EF)
dim(Matrix_Corr_A)
head(Matrix_Corr_A)

correlacionA <- round(cor(Matrix_Corr_A),digits = 4)
correlacion2 <- data.frame(correlacionA)

### ver tabla impresa
tabla_corr_2 <- grid.table(correlacion2,theme = ttheme_default(base_size = 15))
tabla_corr_2


write.csv(correlacion1, file = "correlation_1.csv")
write.csv(correlacion2, file = "correlation_2.csv")



