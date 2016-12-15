# Source scripts 
source("code/load_data.R")

# time-of-day variation ====
trip.tod <- trip.dataset %>%
  group_by(time) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count)) %>%
  as.data.frame()
trip.tod

ggplot(trip.tod, aes(time, trips.sum)) + geom_point()

boxplot.tod.unlim <- ggplot(trip.dataset, aes(time, Count)) + geom_boxplot() # + coord_flip()
boxplot.tod.unlim

boxplot.tod.lim <- ggplot(trip.dataset, aes(time, Count)) + geom_boxplot()+ 
  coord_cartesian(ylim = c(0, 200)) # +  coord_flip()
boxplot.tod.lim

# Peak period ====
trip.peak <- trip.dataset %>%
  group_by(peak, weekend) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.peak

ggplot(trip.peak, aes(peak, trips.sum)) + geom_point(aes(colour = factor(weekend)))

boxplot.peak.unlim <- ggplot(trip.dataset, aes(peak, Count)) + geom_boxplot() 
boxplot.peak.unlim

boxplot.peak.lim <- ggplot(trip.dataset, aes(peak, Count)) + geom_boxplot(aes(fill = weekend))+ 
  coord_cartesian(ylim = c(0, 200)) 
boxplot.peak.lim

# day to day variation =====
date.df <- trip.dataset %>%
           group_by(date)%>%
           summarise(weekday=first(weekday))

trip.dtd <- trip.dataset %>%
  group_by(date) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))  %>%
  left_join(date.df) %>%
  as.data.frame()
trip.dtd

ggplot(trip.dtd, aes(factor(date), trips.sum)) + geom_point(aes(colour = factor(weekday)))

boxplot.dtd.unlim <- ggplot(trip.dataset, aes(date, Count)) + geom_boxplot() 
boxplot.dtd.unlim

boxplot.dtd.lim <- ggplot(trip.dataset, aes(date, Count)) + geom_boxplot() + coord_cartesian(ylim = c(0, 100))
boxplot.dtd.lim






# week to week variation ====
trip.wtw <- trip.dataset %>%
  group_by(week) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.wtw

boxplot.wtw.unlim <- ggplot(trip.dataset, aes(week, Count)) + geom_boxplot() 
boxplot.wtw.unlim

boxplot.wtw.lim <- ggplot(trip.dataset, aes(week, Count)) + geom_boxplot() + coord_cartesian(ylim = c(0, 100))
boxplot.wtw.lim
plot(trip.wtw$week, trip.wtw$trips.sum)




# Weekday variation =====
trip.aggregation <- trip.dataset %>%
  group_by(weekday) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.aggregation
boxplot.weekday.unlim <- ggplot(trip.dataset, aes(weekday, Count)) + geom_boxplot() 
boxplot.weekday.unlim

boxplot.weekday.lim <- ggplot(trip.dataset, aes(weekday, Count)) + geom_boxplot() + coord_cartesian(ylim = c(0, 100))
boxplot.weekday.lim


ggplot(trip.aggregation, aes(factor(weekday), trips.sum)) + geom_point()
ggplot(trip.aggregation, aes(factor(weekday), trips.avg)) + geom_point()

plot(trip.aggregation$weekday, trip.aggregation$trips.sum)
plot(trip.aggregation$weekday, trip.aggregation$trips.avg)


weekday.freq.df <- data.frame(weekday=factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                              freq=c(4, 5, 5, 4, 4, 4, 4),
                              weekday.num=c(1:7))

trip.aggregation.sum <- trip.dataset %>%
  group_by(weekday) %>%
  summarise(trips.sum=sum(Count)
  ) %>%
  left_join(weekday.freq.df) %>%
  mutate(trips.sum.avg=trips.sum/freq) %>%
  arrange(trips.sum.avg) %>%
  as.data.frame()

ggplot(trip.aggregation.sum, aes(factor(weekday), trips.sum.avg)) + geom_point()

# Weekend ====
trip.weekend <- trip.dataset %>%
  group_by(weekend) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.weekend

boxplot.weekend.unlim <- ggplot(trip.dataset, aes(weekend, Count)) + geom_boxplot() 
boxplot.weekend.unlim

boxplot.weekend.lim <- ggplot(trip.dataset, aes(weekend, Count)) + geom_boxplot() + coord_cartesian(ylim = c(0, 100))
boxplot.weekend.lim

# Calculate trips generation per day during weekday and weekend
weekend.freq.df <- data.frame(weekend=c("Weekday", "Weekend"), 
                              freq=c(22, 8))

trip.weekend.sum <- trip.dataset %>%
  group_by(weekend) %>%
  summarise(trips.sum=sum(Count)) %>%
  left_join(weekend.freq.df) %>%
  mutate(trips.sum.avg=trips.sum/freq)
trip.weekend.sum



# subscriber class ====
trip.subscriber <- trip.dataset %>% 
  group_by(Subscriber_Class) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.subscriber    

boxplot.subscriber.unlim <- ggplot(trip.dataset, aes(Subscriber_Class, Count)) + geom_boxplot(aes(fill = weekday)) 
boxplot.subscriber.unlim

boxplot.subscriber.lim <- ggplot(trip.dataset, aes(Subscriber_Class, Count)) + geom_boxplot(aes(fill = weekday)) + coord_cartesian(ylim = c(0, 100))
boxplot.subscriber.lim

trip.subscriber.weekday <- trip.dataset %>% 
  group_by(Subscriber_Class, weekday) %>%
  summarise(trips.sum=sum(Count)) %>%
  left_join(weekday.freq.df) %>%
  mutate(trips.sum.avg=trips.sum/freq) %>%
  as.data.frame()

trip.subscriber.weekday
ggplot(trip.subscriber.weekday, aes(factor(weekday.num), trips.sum.avg)) + geom_point(aes(colour = factor(Subscriber_Class)))

# ggplot(trip.subscriber.weekday, aes(factor(weekday.num), trips.sum.avg)) +  geom_point() + facet_grid(Subscriber_Class~.)

# Purpose ====
trip.purpose <- trip.dataset %>% 
  group_by(Purpose) %>%
  summarise(trips.min=min(Count),
            trips.avg=mean(Count),
            trips.median=median(Count),
            trips.max=max(Count),
            trips.sum=sum(Count))
trip.purpose

boxplot.purpose.unlim <- ggplot(trip.dataset, aes(Purpose, Count)) + geom_boxplot() 
boxplot.purpose.unlim

boxplot.purpose.lim <- ggplot(trip.dataset, aes(Purpose, Count)) + geom_boxplot() + coord_cartesian(ylim = c(0, 100))
boxplot.purpose.lim

trip.purpose.sum <- trip.dataset %>% 
  group_by(Purpose, weekday) %>%
  summarise(trips.sum=sum(Count))  %>%
  left_join(weekday.freq.df) %>%
  mutate(trips.sum.avg=trips.sum/freq)%>%
  as.data.frame()
trip.purpose.sum
ggplot(trip.purpose.sum, aes(factor(weekday.num), trips.sum.avg)) + geom_point(aes(colour = factor(Purpose)))









