# Load required packages 
require(stargazer)

# Source scripts 
source("code/load_data.R")
source("code/clean data.R")

# Factor analysis 
library(psych)
print(paR2vari <- fa(cleaned_data[,c(47,51,59:60,68:74)],nfactors=3,rotate="varimax",fm="ml",scores = "regression"))
SLD.final <- cbind(cleaned_data,paR2vari$scores)

# Data combine 
alldata <- trip.dataset %>%
           left_join(SLD.final, by="TAZ_ID") %>%
           mutate(trips.density=Count/TAZ_Area)

# one hour 
m.hour <- lm(trips.density~ML1 + ML2 + ML3 + weekend +Subscriber_Class + peak, data=alldata)
summary(m.hour)

m.hour.homeworker <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Home Worker"))
summary(m.hour.homeworker)

m.hour.inboundcommuter <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Inbound Commuter"))
summary(m.hour.inboundcommuter)

m.hour.longtermvisitor <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Long Term Visitor"))
summary(m.hour.longtermvisitor)

m.hour.outboundcommuter <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Outbound Commuter"))
summary(m.hour.outboundcommuter)

m.hour.residentworker <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Resident Worker"))
summary(m.hour.residentworker)


m.hour.shorttermvisitor <- lm(trips.density~ML1 + ML2 + ML3 + weekend + peak, data=alldata %>% filter(Subscriber_Class=="Short Term Visitor"))
summary(m.hour.shorttermvisitor)


stargazer(m.hour.homeworker, m.hour.inboundcommuter, m.hour.longtermvisitor, type="text")
stargazer(m.hour.outboundcommuter, m.hour.residentworker, m.hour.residentworker,  type="text")

# aggrgate by Subscriber_Class at day level 
Start_Date.df <- trip.dataset %>%
                 group_by(Start_Date) %>%
                 summarise(weekday=first(weekday),
                           weekend=first(weekend))

trip.dataset.subscriber.day <- trip.dataset %>%
  group_by(TAZ_ID, Start_Date, Subscriber_Class)  %>%
  summarise(trips.sum=sum(Count)) %>%
  left_join(Start_Date.df) %>%
  left_join(SLD.final, by="TAZ_ID") %>% 
  mutate(trips.density=trips.sum/TAZ_Area)

m.subscriber.day <- lm(trips.density~ML1 + ML2 + ML3 + weekend +Subscriber_Class, data=trip.dataset.subscriber.day)
summary(m.subscriber.day)

# Market segmentation 
m.day.homeworker <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Home Worker"))
summary(m.day.homeworker)

m.day.inboundcommuter <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Inbound Commuter"))
summary(m.day.inboundcommuter)

m.day.longtermvisitor <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Long Term Visitor"))
summary(m.day.longtermvisitor)

m.day.outboundcommuter <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Outbound Commuter"))
summary(m.day.outboundcommuter)

m.day.residentworker <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Resident Worker"))
summary(m.day.residentworker)


m.day.shorttermvisitor <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.subscriber.day %>% filter(Subscriber_Class=="Short Term Visitor"))
summary(m.day.shorttermvisitor)


stargazer(m.day.homeworker, m.day.inboundcommuter, m.day.longtermvisitor, type="text")
stargazer(m.day.outboundcommuter, m.day.residentworker, m.day.residentworker,  type="text")


# aggregate at day level 
trip.dataset.day <- trip.dataset %>%
  group_by(TAZ_ID, Start_Date)  %>%
  summarise(trips.sum=sum(Count)) %>%
  left_join(Start_Date.df) %>%
  left_join(SLD.final, by="TAZ_ID")%>% 
  mutate(trips.density=trips.sum/TAZ_Area)

m.day <- lm(trips.density~ML1 + ML2 + ML3 + weekend, data=trip.dataset.day)
summary(m.day)











