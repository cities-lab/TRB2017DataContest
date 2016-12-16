
#setwd("C:/Users/shiwei/Desktop/TRB contest")
#setwd("~/Google Drive/TRB Data Contest")
if (!require("pacman")) {install.packages("pacman"); library(pacman)}
p_load(dplyr, readr, magrittr, stringr, readxl, tidyr, rgdal, ggplot2, foreign)

TAZ <-read.dbf("data/SLD/OUATS_AirSage_TAZ.dbf")
sld.bg_taz <- read.dbf("data/SLD/Orlando_union.dbf")
sld.bg_taz <- sld.bg_taz %>% arrange(TAZ_ID)

# Replace -99999 with 0
sld.bg_taz <- sld.bg_taz %>%
         mutate(D4a=ifelse(D4a == -99999, NA, D4a),
                D4c=ifelse(D4c == -99999, NA, D4c),
                D4d=ifelse(D4d == -99999, NA, D4d),
                D5br=ifelse(D5br == -99999, NA, D5br),
                D5be=ifelse(D5be == -99999, NA, D5be),
                D5dri=ifelse(D5dri == -99999, NA, D5dri),
                D5de=ifelse(D5de == -99999, NA, D5de),
                D5dei=ifelse(D5dei == -99999, NA, D5dei),
                bg_percent = Shape_Area / BG_Area
                )

# to exclude tiny calculation errors/useless weights in map sld.bg_taz process due to projection differences between TAZ and BG maps
# projection: NAD_1983_StatePlane_Florida_East_FIPS_0901_Feet

# the variables for TAZ is calculated as a weighted summation function of Block Group (BG)
# bg_percent is the area of TAZ or segmented TAZ divide by area of BG(s) it locate 
# the according TAZ varaibles would be (summation if TAZ cover more than one BG)proportional of BG varaibles

# after selecting varaibles, example manipulation
bg.popemp <- sld.bg_taz %>% 
  select(TAZ_ID, bg_percent, 
         starts_with("AC"),
         COUNTHU10, TOTPOP10, 
         starts_with("AUTOOWN"), 
         R_LOWWAGEW,R_MEDWAGEW,R_HIWAGEWK,
         EMPTOT, #starts_with("EMP"),
         starts_with("E5"), 
         starts_with("E8"), 
         starts_with("E_")) %>% 
  mutate_each(funs(.*bg_percent), -TAZ_ID, -bg_percent) %>% 
  select(-bg_percent)

taz.popemp <- bg.popemp %>%
  group_by(TAZ_ID) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

taz.d1 <- sld.bg_taz %>% #select(TAZ_ID, bg_percent, AC_UNPR, starts_with("D1")) %>%
  mutate_at(vars(starts_with("D1")), funs(. * AC_UNPR * bg_percent)) %>%
  group_by(TAZ_ID) %>%
  summarise_at(vars(starts_with("D1")), funs(sum(., na.rm=TRUE)))

taz.d3 <- sld.bg_taz %>% #select(TAZ_ID, bg_percent, AC_LAND, starts_with("D3")) %>%
  mutate_at(vars(starts_with("D3")), funs(. * AC_LAND * bg_percent)) %>%
  group_by(TAZ_ID) %>%
  summarise_at(vars(starts_with("D3")), funs(sum(., na.rm=TRUE)))

bg.d4d5 <- select(sld.bg_taz, TAZ_ID, D4a, D4c, D4d, D5cei, D5cri, D5dri, D5dei)
taz.d4d5 <- bg.d4d5 %>%
  group_by(TAZ_ID) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

taz.sld <- taz.popemp %>%
        left_join(taz.d1) %>%
        left_join(taz.d3) %>%
        left_join(taz.d4d5) %>% 
        left_join(TAZ)

# data <- cbind(data,TAZ)
# 
# data <- data %>%
#   mutate_each(funs(./TAZ_Area),starts_with("D1"),starts_with("D3"))

taz.sld <- taz.sld %>%
  mutate_at(vars(starts_with("D1")), funs(./AC_UNPR)) %>% 
  mutate_at(vars(starts_with("D3")), funs(./AC_LAND)) 

# Population/Employment multiply by bg_percent then summation
# D1/D3 density, multiply by bg_area and by bg_percent then summation
# D5 accessibility,average each BG, no proportion

# validation
# summary(data$COUNTHU10 - data$AUTOOWN0 - data$AUTOOWN1 - data$AUTOOWN2P)

taz.sld <- taz.sld %>%
  mutate(POPDEN = TOTPOP10/TAZ_Area,
         EMPDEN = EMPTOT/TAZ_Area,
         JH = EMPTOT/TOTPOP10,
         RH = E5_RET10/TOTPOP10,
         RJ = E5_RET10/EMPTOT,
         R_PCTLOWWAGE = R_LOWWAGEW/TOTPOP10,
         R_PCTAUTO0 = AUTOOWN0/COUNTHU10) %>% 
  select(-D5dri,-D5dei,-E_PCTLOWWA,-E_FEDT10,-E_FEDRET10,-E_FEDOFF10,-E_FEDSVC10,-E_FEDENT10, -starts_with("AC"))

# write.table(taz.sld,"data/taz_sld.csv",sep = ",")
