
# Read in the data
CC_Train = read.csv("ClimateChangeTrain.csv")
CC_Test = read.csv("ClimateChangeTest.csv")

# Linear regression
CCReg = lm(Temp ~ MEI	+ CO2 +	CH4 +	N2O +	CFC.11 +	CFC.12 +	TSI	+ Aerosols,data =CC_Train)
summary(CCReg)

# Correlations
cor(CC_Train[,3:10])
library(corrplot)
corrplot(cor(CC_Train[,3:10]), order="hclust")

#New linear regression
CCReg1 = lm(Temp ~ MEI + N2O +	TSI	+ Aerosols,data =CC_Train)
summary(CCReg1)

#Test predict
CCprediction = predict(CCReg1,newdata=CC_Test)
CCprediction

SSE = sum((CC_Test$Temp-CCprediction)^2)
SSE

SST = sum((CC_Test$Temp-mean(CC_Train$Temp))^2) 
SST

# Out-of-Sample R^2
R2 = 1 - SSE/SST 
R2
