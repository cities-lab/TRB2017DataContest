setwd("C:/Users/shiwei/Desktop/TRB contest")
setwd("~/Google Drive/TRB Data Contest")
library(foreign)

TAZ <-read.dbf("OUATS_AirSage_TAZ.dbf")
union <- read.dbf("Orlando_union.dbf")
union <- union[order(union$TAZ_ID),]

union$bg_percent <- union$Shape_Area / union$BG_Area

# to exclude tiny calculation errors/useless weights in map union process due to projection differences between TAZ and BG maps
# projection: NAD_1983_StatePlane_Florida_East_FIPS_0901_Feet

# the variables for TAZ is calculated as a weighted summation function of Block Group (BG)
# bg_percent is the area of TAZ or segmented TAZ divide by area of BG(s) it locate 
# the according TAZ varaibles would be (summation if TAZ cover more than one BG)proportional of BG varaibles


# after selecting varaibles, example manipulation
library(dplyr)
sum <- 
  select(union,TAZ_ID,COUNTHU10,TOTPOP10,starts_with("AUTOOWN"),R_LOWWAGEW,R_MEDWAGEW,R_HIWAGEWK,EMPTOT,starts_with("EMP"),
         starts_with("E5"),starts_with("E8"),starts_with("E_"),bg_percent) %>% 
  mutate_each(funs(.*bg_percent),COUNTHU10,TOTPOP10,starts_with("AUTOOWN"),EMPTOT,starts_with("EMP"),
              starts_with("E5"),starts_with("E8"),starts_with("E_"))
TAZ.sum <- sum %>%
  group_by(TAZ_ID) %>%
  summarise_each(funs(sum))

den <- select(union,TAZ_ID,starts_with("D1"),starts_with("D3"),BG_Area,bg_percent) %>%
  mutate_each(funs(.*BG_Area*bg_percent),starts_with("D1"),starts_with("D3"))
TAZ.den <- den %>%
  group_by(TAZ_ID) %>%
  summarise_each(funs(sum))

avg <- select(union, TAZ_ID,D4a,D4c,D4d,D5cei,D5cri,D5dri,D5dei)
TAZ.avg <- avg %>%
  group_by(TAZ_ID) %>%
  summarise_each(funs(mean))

data <- cbind(TAZ.sum,TAZ.den,TAZ.avg)

data <- cbind(data,TAZ)

data <- data %>%
  mutate_each(funs(./TAZ_Area),starts_with("D1"),starts_with("D3"))

cleaned_data <- data %>%
  select(-D5dri,-D5dei,-E_PCTLOWWA,-E_FEDT10,-E_FEDRET10,-E_FEDOFF10,-E_FEDSVC10,-E_FEDENT10)

write.table(cleaned_data,"cleaned data.csv",sep = ",")

# Population/Employment multiply by bg_percent then summation
# D1/D3 density, multiply by bg_area and by bg_percent then summation
# D5 accessibility,average each BG, no proportion

# validation
# summary(data$COUNTHU10 - data$AUTOOWN0 - data$AUTOOWN1 - data$AUTOOWN2P)

cleaned_data <- cleaned_data %>%
  mutate(POPDEN = TOTPOP10/TAZ_Area,
         EMPDEN = EMPTOT/TAZ_Area,
         JH = EMPTOT/TOTPOP10,
         RH = E5_RET10/TOTPOP10,
         RJ = E5_RET10/EMPTOT,
         R_PCTLOWWAGE = R_LOWWAGEW/TOTPOP10,
         R_PCTAUTO0 = AUTOOWN0/COUNTHU10)

library(psych)
print(paR2vari <- fa(cleaned_data[,c(47,51,59:60,68:74)],nfactors=3,rotate="varimax",fm="ml",scores = "regression"))

