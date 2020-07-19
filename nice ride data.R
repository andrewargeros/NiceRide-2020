rm(list = ls())
library(tidyverse)

bike = read_csv("Nice_ride_trip_history_2017_season.csv")
temp = read_csv("WeatherDailyMinneapolis2017.csv")
loc = read_csv("Nice_Ride_2017_Station_Locations.csv")
member.bike = read_csv("member bike.csv")

loc = loc %>% rename("Station" = Name)

member.bike$PCTmember = member.bike$Member / member.bike$Total

member.bike = merge(member.bike, loc, by="Station")

zips = read_csv("ziplatlong.csv") 

library(geosphere)
mat = distm(member.bike[,c('Longitude','Latitude')], zips[,c('Longitude','Latitude')], fun=distVincentyEllipsoid)
member.bike$Zip = zips$Zip[apply(mat, 1, which.min)]

url = "https://www.incomebyzipcode.com/minnesota/"
incs.d = tibble(member.bike$Zip) %>% rename("Zip" = `member.bike$Zip`)
incs.d$income = rep(NA,length(incs.d$Zip))
incs = c()
for(i in member.bike$Station){ 
  zip.temp = member.bike[which(member.bike$Station==i),"Zip"]
  url.temp = paste0(url, zip.temp)
  urldata = getURL(url.temp)
  inc.temp = str_extract(urldata, "\\d{3},\\d{3}")[1]
  incs.d[i,2] = inc.temp
  incs = append(incs, inc.temp)
}
p = incs.d[1:202, 1]
d = incs.d[203:404, 2]

zipcat = read_csv("zipstocat.csv")
member.bike.temp = cbind(member.bike, zipcat)
member.bike.temp$income[is.na(member.bike.temp$income)] = mean(member.bike.temp$income)
member.bike.temp$Campus = ifelse(member.bike.temp$Zip %in% c(55455, 55414), c(1), 
                                 ifelse(member.bike.temp$Station == "Englewood Ave & N Asbury Street", c(1), c(0)))
member.bike.temp = member.bike.temp[,-10]
member.bike.temp$stp = ifelse(member.bike.temp$Zip %in% c(55114, 
                                                          55108,
                                                          55104,
                                                          55105,
                                                          55116,
                                                          55102,
                                                          55107,
                                                          55119,
                                                          55106,
                                                          55117,
                                                          55130), c(1), c(0))

bike = bike %>% janitor::clean_names(case = "snake")
bike$start_date = lubridate::mdy_hm(bike$start_date)
bike$Date = as.Date(bike$start_date, "%Y-%m-%d %H:%M")
bike$starttime = strftime(bike$start_date, format="%H:%M", tz = Sys.timezone())
ugh = strsplit(bike$starttime, split = ":")
ugh = matrix(unlist(ugh), ncol=2, byrow=TRUE)
ugh = as.data.frame(ugh)
bike = cbind(bike, ugh[,1])
bike = bike %>% rename("starthour" = `ugh[, 1]`)
bike$starthour = as.integer(bike$starthour)
bike$starthour = bike$starthour + 6
bike$start_date = as.Date(bike$start_date, format = "%Y-%m-%d")
bike$end_date = lubridate::mdy_hm(bike$end_date)
bike$end_date = as.Date(bike$end_date, format = "%Y-%m-%d")
bike$account_type = as.factor(bike$account_type)
bike = bike %>% rename("DATE" = start_date)

bike = merge(bike, temp, by="DATE")
bike$one = rep(1, length(bike$DATE))
bike$cost = ((bike$total_duration_seconds/(60*60))*4)+2

twins = read_csv("sportsref.csv")
twins$Date = paste0(twins$Date, ", 2017")
twins$Date = as.Date(twins$Date, "%A, %B %d, %Y")
twins$`W/L` = as.factor(twins$`W/L`)
twins$Opp = as.factor(twins$Opp)

bike.twins = inner_join(bike, twins, by = "Date")



joyride = bike[which(bike$start_station == bike$end_station), ]

bike.date = bike %>% 
  group_by(DATE) %>% 
  summarise(tripcount = sum(one),
            temp = mean(TMAX),
            secs = mean(total_duration_seconds))

bike.gameday = bike.twins %>% 
  filter(start_station == "Target Field Station" |
           end_station == "Target Field Station"    & 
          X3 != "@") %>% 
  group_by(Date, Opp, `W/L`) %>% 
  summarise(ntrips = sum(one),
            mean(Attendance))
bike.gameday$W = ifelse(bike.gameday$`W/L`=="W" | bike.gameday$`W/L`=="W-wo", c(1), c(0))
bike.gameday$Div = ifelse(bike.gameday$Opp %in% c("DET", "KCR", "CLE", "CHW"), c(1), c(0))
bike.date$day = weekdays(bike.date$DATE)
bike.date$wknd = ifelse(bike.date$day %in% c("Friday", "Saturday", "Sunday"), c(1), c(0)) 
bike.date$freeze = ifelse(bike.date$temp <= 32, c(1), c(0))

bike.day = bike.date %>% group_by(day) %>% summarise(tripcount = mean(tripcount))
bike.day$wknd = ifelse(bike.day$day %in% c("Friday", "Saturday", "Sunday"), c(1), c(0))
sum(bike.day[which(bike.day$wknd==1),c("tripcount")])/sum(bike.day$tripcount)
bike.day$day = factor(bike.day$day, levels= c("Sunday", "Monday", 
                                             "Tuesday", "Wednesday",
                                             "Thursday", "Friday",
                                             "Saturday"))

weekendavg = bike.day %>% 
  group_by(wknd) %>% 
  summarise(tripavg = mean(tripcount)) 
weekendavg$wknd = ifelse(weekendavg$wknd==1, c("Weekends"), c("Weekdays"))
weekendavg$prorata = weekendavg$tripavg
weekendavg$prorata[[1]] = weekendavg$tripavg[[1]]/4
weekendavg$prorata[[2]] = weekendavg$tripavg[[2]]/3

member.bike.t2 = bike
member.bike.t2 = member.bike.t2 %>% rename("Station" = start_station)
member.bike.t2 = merge(member.bike.t2, member.bike.temp, by = "Station")
member.bike.t2$counter = rep(1, nrow(member.bike.t2))
mean(member.bike.t2[which(member.bike.t2$Campus==1), "total_duration_seconds"])
mean(member.bike.t2[which(member.bike.t2$Campus==0), "total_duration_seconds"])
sum(member.bike.t2[which(member.bike.t2$Campus==1 & member.bike.t2$starthour>20), "counter"])
sum(member.bike.t2[which(member.bike.t2$Campus==0 & member.bike.t2$starthour>20), "counter"])
plt_col = "violet"
gg1 = ggplot(bike.day, aes(x = day, y = tripcount)) + 
  geom_col(color = plt_col, fill = plt_col) +
  xlab("Day of Week") + ylab("Average # of Trips") +
  labs(title = "Average Trips by Day of Week")
gg1

gg2 = ggplot(weekendavg, aes(x = wknd, y = prorata)) +
  geom_col(color = plt_col, fill = plt_col, width = .6) + 
  xlab("Type of Day") + ylab("Average # of Trips") +
  labs(title = "Average Trips by Weekdays vs Weekends")
gg2

lm0 = lm(Total ~ (income/1000), data = member.bike.temp)
summary(lm0)

lm1 = lm(tripcount ~ temp + freeze, data = bike.date)
summary(lm1)

lm2 = lm(secs ~ tripcount + temp, data = bike.date)
summary(lm2)

lm3 = lm(Total ~ PCTmember + `Total docks`, data = member.bike) 
summary(lm3)

member.bike.best = member.bike %>% 
  arrange(desc(Total))
member.bike.docks = member.bike %>% 
  arrange(desc(`Total docks`))

member.bike$zscore.total = (member.bike$Total-mean(member.bike$Total))/sd(member.bike$Total)
member.bike.best.s = head(member.bike.best$Station, 10)
member.bike.docks.s = head(member.bike.docks$Station, 10)
comp = tibble(member.bike.best.s, 
              member.bike.docks.s)
comp = comp %>% rename(
  "MostPopular" = member.bike.best.s,
  "MostDocks" = member.bike.docks.s
)
comp.stat = tibble(letters = member.bike.best.s,
                   count = unlist(map(member.bike.best.s, function(x) sum(member.bike.docks.s %in% x)))) %>% 
  rename("Station" = letters)

comp.stat = merge(comp.stat, member.bike, by = "Station")
comp.stat = comp.stat[,c("Station",
                         "count",
                         "PCTmember",
                         "Total",
                         "Total docks", 
                         "zscore.total")]

comp.docks = tibble(letters = member.bike.docks.s,
                    count = unlist(map(member.bike.docks.s, function(x) sum(member.bike.best.s %in% x)))) %>% 
  rename("Station" = letters)

comp.docks = merge(comp.docks, member.bike, by = "Station")

comp.docks = comp.docks[,c("Station",
                           "count",
                           "PCTmember",
                           "Total",
                           "Total docks", 
                           "zscore.total")]









