deaths <- read.csv("JournalistDeaths.csv", stringsAsFactors=F, na.strings=c("","NA"))[,1:17]
deaths$Tortured <- as.character(deaths$Tortured)
for (i in 1:nrow(deaths)) {
  deaths[i,17] <- gsub(x = deaths[i, 17], pattern = "\n", replacement = "")
    #substr(deaths[i,17], 1, nchar(deaths[i,17])-1)
}
deaths <- na.omit(deaths)
deaths
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")
nrow(deaths)
for (i in 1:nrow(deaths)) {
  for (j in 1:ncol(deaths)) {
    na_value <- sum(deaths[i,j] == "na", na.rm = TRUE)
    if (na_value == 1) {
      deaths <- deaths[-i,]
    }
  }
}
nrow(deaths)
deaths
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
