

# Load packages
library(mosaic)  
library(tidyverse)
library(usmap)
library(readxl)


# Import Opioid Data
opioidsCDC <- read_xlsx("~/Documents/CDCChallenge/OPIOID FINAL DATA.xlsx")
str(opioidsCDC)

## Map of United States

opioidsCDC <- rename(opioidsCDC, state=State)

opioidsCDC <- opioidsCDC %>% 
  mutate(sqrtDeathRate=sqrt(DeathRate)) %>% 
  mutate(VeteranPercent=(VeteranPopu/StatePopu)*100)

plot_usmap(data = opioidsCDC, values = "sqrtDeathRate", lines = "black") + 
  scale_fill_continuous(name = "Sqrt of OD Death Rates \nper 100,000 (2016)", label = scales::comma, high = "#3182bd", low = "#deebf7") + 
  theme(legend.position = "right")

plot_usmap(data = opioidsCDC, values = "VeteranPercent", lines = "black") + 
  scale_fill_continuous(name = "Percentage of Veterans \nby State (2016)", label = scales::comma, high = "#3182bd", low = "#deebf7") + 
  theme(legend.position = "right")


## Linear Plot
cor(RxRate ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(RxRate.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.09874413
# 0.0989
cor(RxNo ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.01618705
cor(HeroinRate ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.1331886
cor(SyntheticRate ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.09112659
cor(OpioidRxRate ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(OpioidRxRate.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.06358407
# 0.06079155
cor(MeanElevation ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(MeanElevation.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.2637968
# -0.3174252
cor(MinWage ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(MinWage.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.08391214
# -0.07884835
cor(DemParty/(DemParty+RepParty) ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.1792301
cor(RepParty/(DemParty+RepParty) ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.1792301
cor(VetsNo ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(VetsNo.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(VetsNo.log ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.08040018
# -0.0716207
# -0.07940177
cor(VeteranPopu ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.07493056
cor(VeteranUnemp ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(VeteranUnemp.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.1696929
# 0.1713344
cor(VeteranPoverty ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(VeteranPoverty.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# -0.0451603
# -0.05005902
cor(VeteranDisability ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
cor(VeteranDisability.sqrt ~ DeathRate, data = opioids2, use = "pairwise.complete.obs")
# 0.09417756
# 0.0859443
cor (DeathRate ~ RxRate, data = opioids2, use = "pairwise.complete.obs")
# 0.09874413

ggplot(opioidsCDC, aes(x=DeathRate, y=DemParty/(DemParty+RepParty), col=factor(Region))) +
  xlab("Overdose Death Rates per 100,000") +
  ylab("Proportion of Democratic Representatives") + 
  geom_point() +
  scale_color_discrete(name="Democratic Rep Ratio \nby Region", 
                       breaks=c(1, 2, 3, 4), 
                       labels=c("Northeast", "Midwest", "South", "West"))

ggplot(opioidsCDC, aes(x=DeathRate, y=RepParty/(DemParty+RepParty), col=factor(Region))) +
  ggtitle("") +
  xlab("Overdose Death Rates per 100,000") +
  ylab("Proportion of Republican Representatives") + 
  geom_point() +
  scale_color_discrete(name="Republican Rep Ratio \nby Region", 
                       breaks=c(1, 2, 3, 4), 
                       labels=c("Northeast", "Midwest", "South", "West"))

ggplot(opioidsCDC, aes(x=HighElevation, y=DeathRate)) +
  ggtitle("") +
  ylab("Overdose Death Rates per 100,000") +
  xlab("Highest Elevation by State (feet)") + 
  geom_point()

ggplot(opioids2, aes(x=MeanElevation.sqrt, y=DeathRate)) +
  ggtitle("") +
  ylab("Overdose Death Rates per 100,000") +
  xlab("Mean Elevation by State (feet)") + 
  geom_point()

ggplot(opioids2, aes(x=MinWage.sqrt, y=DeathRate)) +
  ggtitle("") +
  ylab("Overdose Death Rates per 100,000") +
  xlab("Minimum Wage by State ($)") + 
  geom_point()

ggplot(opioids2, aes(x=VeteranDisability, y=DeathRate)) +
  ylab("Overdose Death Rates per 100,000") +
  xlab("Disability Rate of Veterans by State") + 
  geom_point()

ggplot()

opioids2 <- mutate(opioidsCDC, VetsPercent=(VetsNo/StatePopu)*100)
opioids2 <- select(opioidsCDC, -c(state,StateAbr,FentanylRate))

#mutated graphs: 

#DeathRate, HeroinRate, SyntheticRate -- MeanElevation
opioids2 <- mutate (opioids2, MeanElevation.sqrt = sqrt(MeanElevation))
opioids2 <- mutate (opioids2, MeanElevation.sq = MeanElevation^2)
opioids2 <- mutate (opioids2, MeanElevation.log = log(MeanElevation))
opioids2 <- mutate (opioids2, MeanElevation.recipsqrt = MeanElevation^(-1/2))
opioids2 <- mutate (opioids2, MeanElevation.recip = MeanElevation^(-1))
pairs(opioids2[c("DeathRate", "MeanElevation", "MeanElevation.sqrt", "MeanElevation.log", "MeanElevation.recipsqrt", "MeanElevation.recip")])

#NOTHING
#DeathRate, HeroinRate, SyntheticRate -- MinWage
opioids2 <- mutate (opioids2, MinWage.sqrt = sqrt(MinWage))
opioids2 <- mutate (opioids2, MinWage.sq = MinWage^2)
opioids2 <- mutate (opioids2, MinWage.log = log(MinWage))
opioids2 <- mutate (opioids2, MinWage.recipsqrt = MinWage^(-1/2))
opioids2 <- mutate (opioids2, MinWage.recip = MinWage^(-1))
pairs(opioids2[c("DeathRate", "MinWage", "MinWage.sqrt", "MinWage.log", "MinWage.recipsqrt", "MinWage.recip")])

#DeathRate by VetsNo.sqrt
#DeathRate, HeroinRate, SyntheticRate -- VetsNo
opioids2 <- mutate (opioids2, VetsNo.sqrt = sqrt(VetsNo))
opioids2 <- mutate (opioids2, VetsNo.sq = VetsNo^2)
opioids2 <- mutate (opioids2, VetsNo.log = log(VetsNo))
opioids2 <- mutate (opioids2, VetsNo.recipsqrt = VetsNo^(-1/2))
opioids2 <- mutate (opioids2, VetsNo.recip = VetsNo^(-1))
pairs(opioids2[c("DeathRate", "VetsNo", "VetsNo.sqrt", "VetsNo.log", "VetsNo.recipsqrt", "VetsNo.recip")])

#DeathRate by. RxRate.sqrt
#DeathRate, HeroinRate, SyntheticRate -- RxRate
opioids2 <- mutate (opioids2, RxRate.sqrt = sqrt(RxRate))
opioids2 <- mutate (opioids2, RxRate.sq = RxRate^2)
opioids2 <- mutate (opioids2, RxRate.log = log(RxRate))
opioids2 <- mutate (opioids2, RxRate.recipsqrt = RxRate^(-1/2))
opioids2 <- mutate (opioids2, RxRate.recip = RxRate^(-1))
pairs(opioids2[c("DeathRate", "RxRate", "RxRate.sqrt", "RxRate.log", "RxRate.recipsqrt", "RxRate.recip")])

#DeathRate vs. OpioidRxRate.sqrt
#DeathRate, HeroinRate, SyntheticRate -- OpioidRxRate
opioids2 <- mutate (opioids2, OpioidRxRate.sqrt = sqrt(OpioidRxRate))
opioids2 <- mutate (opioids2, OpioidRxRate.sq = OpioidRxRate^2)
opioids2 <- mutate (opioids2, OpioidRxRate.log = log(OpioidRxRate))
opioids2 <- mutate (opioids2, OpioidRxRate.recipsqrt = OpioidRxRate^(-1/2))
opioids2 <- mutate (opioids2, OpioidRxRate.recip = OpioidRxRate^(-1))
pairs(opioids2[c("DeathRate", "OpioidRxRate", "OpioidRxRate.sqrt", "OpioidRxRate.log", "OpioidRxRate.recipsqrt", "OpioidRxRate.recip")])

#DeathRate vs. VeteranUnemp.sqrt
#DeathRate, HeroinRate, SyntheticRate -- VeteranUnemp
opioids2 <- mutate (opioids2, VeteranUnemp.sqrt = sqrt(VeteranUnemp))
opioids2 <- mutate (opioids2, VeteranUnemp.sq = VeteranUnemp^2)
opioids2 <- mutate (opioids2, VeteranUnemp.log = log(VeteranUnemp))
opioids2 <- mutate (opioids2, VeteranUnemp.recipsqrt = VeteranUnemp^(-1/2))
opioids2 <- mutate (opioids2, VeteranUnemp.recip = VeteranUnemp^(-1))
pairs(opioids2[c("DeathRate", "VeteranUnemp", "VeteranUnemp.sqrt", "VeteranUnemp.log", "VeteranUnemp.recipsqrt", "VeteranUnemp.recip")])

#DeathRate vs. VeteranPoverty.sqrt
#DeathRate, HeroinRate, SyntheticRate -- VeteranPoverty
opioids2 <- mutate (opioids2, VeteranPoverty.sqrt = sqrt(VeteranPoverty))
opioids2 <- mutate (opioids2, VeteranPoverty.sq = VeteranPoverty^2)
opioids2 <- mutate (opioids2, VeteranPoverty.log = log(VeteranPoverty))
opioids2 <- mutate (opioids2, VeteranPoverty.recipsqrt = VeteranPoverty^(-1/2))
opioids2 <- mutate (opioids2, VeteranPoverty.recip = VeteranPoverty^(-1))
pairs(opioids2[c("DeathRate", "VeteranPoverty", "VeteranPoverty.sqrt", "VeteranPoverty.log", "VeteranPoverty.recipsqrt", "VeteranPoverty.recip")])

#DeathRate vs. VeteranDisability.sqrt
#DeathRate, HeroinRate, SyntheticRate -- VeteranDisability
opioids2 <- mutate (opioids2, VeteranDisability.sqrt = sqrt(VeteranDisability))
opioids2 <- mutate (opioids2, VeteranDisability.sq = VeteranDisability^2)
opioids2 <- mutate (opioids2, VeteranDisability.log = log(VeteranDisability))
opioids2 <- mutate (opioids2, VeteranDisability.recipsqrt = VeteranDisability^(-1/2))
opioids2 <- mutate (opioids2, VeteranDisability.recip = VeteranDisability^(-1))
pairs(opioids2[c("DeathRate", "VeteranDisability", "VeteranDisability.sqrt", "VeteranDisability.log", "VeteranDisability.recipsqrt", "VeteranDisability.recip")])

#RxRate, OpioidRxRate -- VeteranUnemp, VeteranPoverty, VeteranDisability
pairs(opioids2[c("DeathRate", "RxRate", "OpioidRxRate.sqrt", "VeteranUnemp", "VeteranPoverty", "VeteranDisability")])

pairs(opioids2[c("DeathRate", "RxRate", "HeroinRate", "SyntheticRate", "OpioidRxRate")])
pairs(opioids2[c("DeathRate", "MeanElevation", "MinWage", "VetsNo")])
pairs(opioids2[c("DeathRate", "VeteranUnemp", "VeteranPoverty", "VeteranDisability")])
pairs(opioids2[c("DeathRate", "RxRate", "HeroinRate", "SyntheticRate", "OpioidRxRate", "MeanElevation", "MinWage", "VetsNo", "VeteranUnemp", "VeteranPoverty", "VeteranDisability")])
#pairs(opioids2[c("DeathRate", "DemParty/(DemParty+RepParty)", "RepParty/(DemParty+RepParty)", "MinWage", "VetsNo")])

model1 <- lm (DeathRate ~ VeteranUnemp.sqrt + OpioidRxRate.sqrt, data = opioids2)
summary (model1)
#
model2 <- lm (DeathRate ~ VeteranPopu + VeteranUnemp.sqrt + VeteranPoverty.sqrt + VeteranDisability.sqrt, data = opioids2)
summary (model2)
#


pairs(opioids2[c("DeathRate", "OpioidRxRate", "VeteranPopu", "VeteranUnemp.sqrt", "VeteranPoverty.sqrt", "VeteranDisability.sqrt")])
pairs(opioids2[c("DeathRate", "OpioidRxRate", "HeroinRate", "SyntheticRate", "VetsNo.sqrt")])

