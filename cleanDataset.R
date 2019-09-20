deaths <- read.csv("JournalistDeaths.csv", na.strings=c("","NA"))[,1:17]
deaths$Tortured <- as.character(deaths$Tortured)
for (i in 1:nrow(deaths)) {
  deaths[i,17] <- substr(deaths[i,17], 1, nchar(deaths[i,17])-1)
}
deaths <- na.omit(deaths)
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
