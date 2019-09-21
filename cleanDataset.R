deaths <- read.csv("JournalistDeaths.csv", stringsAsFactors=F, na.strings=c("","NA"))[,1:17]
deaths$Tortured <- as.character(deaths$Tortured)
for (i in 1:nrow(deaths)) {
  deaths[i,17] <- gsub(x = deaths[i, 17], pattern = "\n", replacement = "")
}
deaths <- na.omit(deaths)
deaths[complete.cases(deaths),]
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")
for (i in 1:nrow(deaths)) {
  for (j in 1:ncol(deaths)) {
    na_value <- sum(str_trim(deaths[i,j]) == "na", na.rm = TRUE)
    if (na_value == 1) {
      deaths <- deaths[-i,]
    }
  }
}
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
