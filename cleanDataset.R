deaths <- read.csv("JournalistDeaths.csv", na.strings=c("","NA"))[,1:17]
deaths
deaths <- na.omit(deaths)
deaths
write.csv(deaths, file = 'JournalistDeaths.csv')