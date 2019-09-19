deaths <- read.csv("JournalistDeaths.csv", na.strings=c("","NA"))[,1:17]
deaths
deaths <- na.omit(deaths)
deaths
deaths <- deaths[,c(2:17)]
write.csv(deaths, file = 'JournalistDeaths.csv')
