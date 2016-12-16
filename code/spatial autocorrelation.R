library(maptools)
library(rgdal)
library(spdep)

map <- readShapeSpatial("data/taz/OUATS_AirSage_TAZ.shp",proj4string=CRS("+init=epsg:26958"))
# projection code found at http://georepository.com/crs_26958/NAD83-Florida-East.html

# convert polygon to points layer
# https://www.nceas.ucsb.edu/scicomp/usecases/computepolygoncentroids

coords <- coordinates(map)
IDs<-row.names(as(map, "data.frame"))

map_nb <- poly2nb(map,queen=F)
map_nb_w <- nb2listw(map_nb,zero.policy = T)

data.taz <- trip.dataset %>%
  group_by(TAZ_ID) %>%
  summarise(trips.sum=sum(Count)) %>%
  left_join(SLD.final, by="TAZ_ID")%>% 
  mutate(trips.density=trips.sum/TAZ_Area)
# 1307 TAZ_ID missing

data.taz <- rbind(data.taz,c(1307,rep(1,78)))

summary(m.taz <- lm(trips.density~ 1, data=data.taz))
summary(m.taz <- lm(trips.density~ ML1 + ML2 + ML3, data=data.taz))

moran.test(data.taz$trips.density, listw=map_nb_w,zero.policy = T)
lmtest.taz <-lm.LMtests(m.taz,map_nb_w,zero.policy = T)

# There are also 5 LM tests:
# The LM test for a missing spatially lagged DV (LMlag)
# The LM test for error dependence (LMerr)
# The robust LM error, which tests for error dependence in the possible presence of a missing lagged dependent variable
# The robust LM lag, which tests for 
# The SARMA test, which tests for both lag and error (not particularly useful because it's high if either SL or SE is present)
# Only use robust forms when BOTH LMErr and LMLag are significant

summary(errorsarlm(trips.density~ ML1+ML2+ML3, data=data.taz,map_nb_w,zero.policy = T))


