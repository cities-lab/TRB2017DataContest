# example code of property value project

# regression on 17600 sfh obs.
coords1 <- coordinates(sfh)
IDs1<-row.names(as(sfh, "data.frame"))

#creat weights matrix for k nearest neighbors: use binary weight
sfh_kn4<-knn2nb(knearneigh(coords1, k=4), row.names=IDs1)
sfh_kn4_w <- nb2listw(sfh_kn4)

# create weight matrix for 1 mile distance: use row standardized weight
sfh_kd1<-dnearneigh(coords1, d1=0, d2=5280, row.names=IDs1)
sfh_kd1_w <- nb2listw(sfh_kd1)

# Moran test
moran.test(sfh$sale_price, listw=sfh_kn4_w, alternative="two.sided")
moran.test(sfh$sale_price, listw=sfh_kd1_w, alternative="two.sided") # x is the vector of data frame length

lm.moran.sfh1 <-lm.morantest(price.I_050.sfh,sfh_kn4_w) # calculate Moran's I
lm.moran.sfh2 <-lm.morantest(price.I_050.sfh,sfh_kd1_w)

lmtest.sfh1 <-lm.LMtests(price.I_050.sfh,sfh_kn4_w,test="all") # LM test for spatial dependency
lmtest.sfh2 <-lm.LMtests(price.I_050.sfh,sfh_kd1_w,test="all")
# There are also 5 LM tests:
# The LM test for a missing spatially lagged DV (LMlag)
# The LM test for error dependence (LMerr)
# The robust LM error, which tests for error dependence in the possible presence of a missing lagged dependent variable
# The robust LM lag, which tests for 
# The SARMA test, which tests for both lag and error (not particularly useful because it's high if either SL or SE is present)
# Only use robust forms when BOTH LMErr and LMLag are significant

library(lmtest)
bptest(price.I_050.sfh) # test whether errors are heteroskedastic

load(file="Regression_Result_SFH.RData")

price.I_050.sfh.lag.n4 <-lagsarlm(sale_price ~ age + build_sq_f + avrmv  + school + CBD + walkscore + crime + log_park_ntdis + log_ntdis_advon + R050_advon_s + log_ntdis_bl + R050_bl_s +  log_ntdis_remu + R050_remu_s + log_ntdis_lomu + R050_lomu_s+ factor(sale_year) + seasonality,data=sfh, sfh_kn4_w)
summary(price.Ia.sfh.lag)
price.I_050.sfh.lag.d1 <-lagsarlm(sale_price ~ age + build_sq_f + avrmv  + school + CBD + walkscore + crime + log_park_ntdis + logntdis_advon + R050_advon_s + log_ntdis_bl + R050_bl_s +  log_ntdis_remu + R050_remu_s + log_ntdis_lomu + R050_lomu_s+ factor(sale_year) + seasonality,data=sfh, sfh_kd1_w)
summary(price.I_050.sfh.lag.d1)
price.I_050.sfh.err.n4 <- errorsarlm(sale_price ~ age + build_sq_f + avrmv  + school + CBD + walkscore + crime + log_park_ntdis + log_ntdis_advon + R050_advon_s + log_ntdis_bl + R050_bl_s +  log_ntdis_remu + R050_remu_s + log_ntdis_lomu + R050_lomu_s+ factor(sale_year) + seasonality,data=sfh, sfh_kn4_w)
summary(price.I_050.sfh.err.n4)
price.I_050.sfh.err.d1 <- errorsarlm(sale_price ~ age + build_sq_f + avrmv  + school + CBD + walkscore + crime + log_park_ntdis + log_ntdis_advon + R050_advon_s + log_ntdis_bl + R050_bl_s +  log_ntdis_remu + R050_remu_s + log_ntdis_lomu + R050_lomu_s+ factor(sale_year) + seasonality,data=sfh, sfh_kd1_w)
summary(price.I_050.sfh.err.d1)

# Rho reflects the spatial dependence inherent in our sample data, measuring the average influence on observations by their neighboring observations.  It has a positive effect and is highly significant.
# AIC is lower that linear model, indicating a better model fit.
# However, the LR test value (likelihood ratio) is still significant, indicating that the introduction of the spatial lag term improved model fit, it didn't make the spatial effects go away.

save(sfh,sfh_kn4_w,sfh_kd1_w,lm.moran.sfh1,lm.moran.sfh2,lmtest.sfh1,lmtest.sfh2,file="Regression_Component_SFH.RData")
save(price.I_050.sfh.lag.n4,price.I_050.sfh.lag.d1,price.I_050.sfh.err.n4,price.I_050.sfh.err.d1,file="Regression_Result_SFH.RData")
