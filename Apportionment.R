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
df <- read_excel("C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 680/Project 3/State Data.xlsx")
names(df)[1] <- "year"

# clean dataframe
df[!complete.cases(df),]

# Test candidate models
multi.fit1 = lm(AZ_pop~AZ_pie+AZ_elct+AZ_tax, data=df)
summary(multi.fit1)

multi.fit2 = lm(AZ_pop~AZ_pie+AZ_elct+log(AZ_tax), data=df)
summary(multi.fit2)

multi.fit3 = lm(AZ_pop~AZ_pie+AZ_elct, data=df)
summary(multi.fit3)

multi.fit4 = lm(AZ_pop~AZ_pie+AZ_elct+year+log(AZ_tax)-1, data=df)
summary(multi.fit4)

multi.fit5 = lm(AZ_pop~AZ_pie+AZ_elct+year, data=df)
summary(multi.fit5)

multi.fit6 = lm(IL_pop~IL_pie+IL_elct, data=df)
summary(multi.fit6)


#explore impacts from two variables
eff.fit3 <- allEffects(multi.fit3, xlevels=50)
for(i in 1:2) {plot(eff.fit3[i])}


# reference only
coefficients(multi.fit5) # model coefficients
confint(multi.fit5, level=0.95) # CIs for model parameters
fitted(multi.fit5) # predicted values
residuals(multi.fit5) # residuals
anova(multi.fit5) # anova table
vcov(multi.fit5) # covariance matrix for model parameters
influence(multi.fit5) # regression diagnostics

# make new data frame
fitted <- data.frame("Year" = df$year)


multi.AK = lm(AK_pop~AK_pie+AK_elct, data=df)
fitted$AK <- fitted(multi.AK)

multi.AL = lm(AL_pop~AL_pie+AL_elct, data=df)
fitted$AL <- fitted(multi.AL)

multi.AR = lm(AR_pop~AR_pie+AR_elct, data=df)
fitted$AR <- fitted(multi.AR)

multi.AZ = lm(AZ_pop~AZ_pie+AZ_elct, data=df)
fitted$AZ <- fitted(multi.AZ)

multi.CA = lm(CA_pop~CA_pie+CA_elct, data=df)
fitted$CA <- fitted(multi.CA)

multi.CO = lm(CO_pop~CO_pie+CO_elct, data=df)
fitted$CO <- fitted(multi.CO)

multi.CT = lm(CT_pop~CT_pie+CT_elct, data=df)
fitted$CT <- fitted(multi.CT)

multi.DC = lm(DC_pop~DC_pie+DC_elct, data=df)
fitted$DC <- fitted(multi.DC)

multi.DE = lm(DE_pop~DE_pie+DE_elct, data=df)
fitted$DE <- fitted(multi.DE)

multi.FL = lm(FL_pop~FL_pie+FL_elct, data=df)
fitted$FL <- fitted(multi.FL)

multi.GA = lm(GA_pop~GA_pie+GA_elct, data=df)
fitted$GA <- fitted(multi.GA)

multi.HI = lm(HI_pop~HI_pie+HI_elct, data=df)
fitted$HI <- fitted(multi.HI)

multi.IA = lm(IA_pop~IA_pie+IA_elct, data=df)
fitted$IA <- fitted(multi.IA)

multi.ID = lm(ID_pop~ID_pie+ID_elct, data=df)
fitted$ID <- fitted(multi.ID)

multi.IL = lm(IL_pop~IL_pie+IL_elct, data=df)
fitted$IL <- fitted(multi.IL)

multi.IN = lm(IN_pop~IN_pie+IN_elct, data=df)
fitted$IN <- fitted(multi.IN)

multi.KS = lm(KS_pop~KS_pie+KS_elct, data=df)
fitted$KS <- fitted(multi.KS)

multi.KY = lm(KY_pop~KY_pie+KY_elct, data=df)
fitted$KY <- fitted(multi.KY)

multi.LA = lm(LA_pop~LA_pie+LA_elct, data=df)
fitted$LA <- fitted(multi.LA)

multi.MA = lm(MA_pop~MA_pie+MA_elct, data=df)
fitted$MA <- fitted(multi.MA)

multi.MD = lm(MD_pop~MD_pie+MD_elct, data=df)
fitted$MD <- fitted(multi.MD)

multi.ME = lm(ME_pop~ME_pie+ME_elct, data=df)
fitted$ME <- fitted(multi.ME)

multi.MI = lm(MI_pop~MI_pie+MI_elct, data=df)
fitted$MI <- fitted(multi.MI)

multi.MN = lm(MN_pop~MN_pie+MN_elct, data=df)
fitted$MN <- fitted(multi.MN)

multi.MO = lm(MO_pop~MO_pie+MO_elct, data=df)
fitted$MO <- fitted(multi.MO)

multi.MS = lm(MS_pop~MS_pie+MS_elct, data=df)
fitted$MS <- fitted(multi.MS)

multi.MT = lm(MT_pop~MT_pie+MT_elct, data=df)
fitted$MT <- fitted(multi.MT)

multi.NC = lm(NC_pop~NC_pie+NC_elct, data=df)
fitted$NC <- fitted(multi.NC)

multi.ND = lm(ND_pop~ND_pie+ND_elct, data=df)
fitted$ND <- fitted(multi.ND)

multi.NE = lm(NE_pop~NE_pie+NE_elct, data=df)
fitted$NE <- fitted(multi.NE)

multi.NH = lm(NH_pop~NH_pie+NH_elct, data=df)
fitted$NH <- fitted(multi.NH)

multi.NJ = lm(NJ_pop~NJ_pie+NJ_elct, data=df)
fitted$NJ <- fitted(multi.NJ)

multi.NM = lm(NM_pop~NM_pie+NM_elct, data=df)
fitted$NM <- fitted(multi.NM)

multi.NV = lm(NV_pop~NV_pie+NV_elct, data=df)
fitted$NV <- fitted(multi.NV)

multi.NY = lm(NY_pop~NY_pie+NY_elct, data=df)
fitted$NY <- fitted(multi.NY)

multi.OH = lm(OH_pop~OH_pie+OH_elct, data=df)
fitted$OH <- fitted(multi.OH)

multi.OK = lm(OK_pop~OK_pie+OK_elct, data=df)
fitted$OK <- fitted(multi.OK)

multi.OR = lm(OR_pop~OR_pie+OR_elct, data=df)
fitted$OR <- fitted(multi.OR)

multi.PA = lm(PA_pop~PA_pie+PA_elct, data=df)
fitted$PA <- fitted(multi.PA)

multi.RI = lm(RI_pop~RI_pie+RI_elct, data=df)
fitted$RI <- fitted(multi.RI)

multi.SC = lm(SC_pop~SC_pie+SC_elct, data=df)
fitted$SC <- fitted(multi.SC)

multi.SD = lm(SD_pop~SD_pie+SD_elct, data=df)
fitted$SD <- fitted(multi.SD)

multi.TN = lm(TN_pop~TN_pie+TN_elct, data=df)
fitted$TN <- fitted(multi.TN)

multi.TX = lm(TX_pop~TX_pie+TX_elct, data=df)
fitted$TX <- fitted(multi.TX)

multi.UT = lm(UT_pop~UT_pie+UT_elct, data=df)
fitted$UT <- fitted(multi.UT)

multi.VA = lm(VA_pop~VA_pie+VA_elct, data=df)
fitted$VA <- fitted(multi.VA)

multi.VT = lm(VT_pop~VT_pie+VT_elct, data=df)
fitted$VT <- fitted(multi.VT)

multi.WA = lm(WA_pop~WA_pie+WA_elct, data=df)
fitted$WA <- fitted(multi.WA)

multi.WI = lm(WI_pop~WI_pie+WI_elct, data=df)
fitted$WI <- fitted(multi.WI)

multi.WV = lm(WV_pop~WV_pie+WV_elct, data=df)
fitted$WV <- fitted(multi.WV)

multi.WY = lm(WY_pop~WY_pie+WY_elct, data=df)
fitted$WY <- fitted(multi.WY)

#Export fitted population
write.csv(fitted, "C:/Users/burns/OneDrive/Desktop/Matt/Grad School/DSC 680/Project 3//blurg.csv", row.names = FALSE)

