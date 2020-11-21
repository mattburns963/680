library(readxl)
library(caret)
library(Rmisc)
library(ggplot2)
library(forecastML)
library(effects)
library(DataCombine)
require("forecast")
require("expsmooth") # required for the data
library(smooth)

# pull in monthly economic data from Jan. 2000 thru Dec. 2019
econ <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 680/Project 2/AutoData.xlsx")

# clean dataframe
econ[!complete.cases(econ),]

# Break apart the componets of auto sales
LtWgtSales <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 680/Project 2/LtWgtSales2020.xlsx")
salestimeseries <- ts(LtWgtSales$LtWgtSales, frequency=12, start=c(2000,1))
salescomponents <- decompose(salestimeseries)
plot(salescomponents)

# Test 3 candidate models
multi.fit1 = lm(LtWgtSales~Employment+Inv2Sales+VehclMiTravelSA+Brent+DomProdSA+MortDebt, data=econ)
summary(multi.fit1)

multi.fit2 = lm(LtWgtSales~Employment+Inv2Sales+VehclMiTravelSA+Brent+MortDebt, data=econ)
summary(multi.fit2)

multi.fit3 = lm(LtWgtSales~Employment+VehclMiTravelSA+Brent+DomProdSA+MortDebt, data=econ)
summary(multi.fit3)

# Investigate lagging Inventory
df_lead2 <- as.data.frame(econ)
df_lead2 <- slide(df_lead2, "Inv2Sales", NewVar = "I2SLead2", slideBy = 2)  # create lag1 variable
multi.fit1.lead2 = lm(LtWgtSales~Employment+I2SLead2+VehclMiTravelSA+Brent+DomProdSA+MortDebt, data=df_lead2)
summary(multi.fit1.lead2)

df_lag2 <- as.data.frame(econ)
df_lag2 <- slide(df_lag2, "Inv2Sales", NewVar = "I2SLag2", slideBy = -2)  # create lag1 variable
multi.fit1.lag2 = lm(LtWgtSales~Employment+I2SLag2+VehclMiTravelSA+Brent+DomProdSA+MortDebt, data=df_lag2)
summary(multi.fit1.lag2)


# Investigate Nonlinear adjustements to Inventory to Sales and Vehilcle Miles Traveled
multi.fit4 = lm(LtWgtSales~Employment+log(Inv2Sales)+VehclMiTravelSA+Brent+DomProdSA+MortDebt, data=econ)
summary(multi.fit4)

multi.fit5 = lm(LtWgtSales~Employment+log(Inv2Sales)+log(VehclMiTravelSA)+Brent+DomProdSA+MortDebt, data=econ)
summary(multi.fit5)


eff.fit5 <- allEffects(multi.fit5, xlevels=50)
for(i in 1:6) {plot(eff.fit5[i])}


fit5 <- predict(multi.fit5)
econ <- cbind(econ, fit5)
ggplot(econ, aes(DATE, y = value, color = variable)) + 
  geom_point(aes(y = LtWgtSales, col = "Lt Auto Sales Actual")) +
  geom_line(aes(y = fit5, col = "Lt Auto Sales Fitted")) +
  theme(legend.position="bottom")+ ggtitle("Fitted vs Actual")


saleses <- es(econ$LtWgtSales, "ZZZ", h=12)
plot(saleses)

empes <- es(econ$Employment, "ZZZ", h=12)
plot(empes)

mileses <- es(econ$VehclMiTravelSA, "ZZZ", h=12)
plot(mileses)

oiles <- es(econ$Brent, "ZZZ", h=12)
plot(oiles)

prodes <- es(econ$DomProdSA, "ZZZ", h=12)
plot(prodes)
