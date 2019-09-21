deaths <- read.csv("JournalistDeaths.csv", stringsAsFactors=F, na.strings=c("","NA"))[,1:17]
deaths$Tortured <- as.character(deaths$Tortured)
for (i in 1:nrow(deaths)) {
  deaths[i,17] <- gsub(x = deaths[i, 17], pattern = "\n", replacement = "")
    #substr(deaths[i,17], 1, nchar(deaths[i,17])-1)
}
deaths <- na.omit(deaths)
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")
deaths
for (i in 1:nrow(deaths)) {
  for (j in 1:ncol(deaths)) {
    deaths <- deaths[ grep("na", deaths[i,j], invert = TRUE) , ]
  }
}
deaths
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
