# Read in csv file
deaths <- read.csv("JournalistDeaths.csv", stringsAsFactors=F, na.strings=c("","NA"))[,1:17]

# Remove "/n" symbol from values in last column of dataset
deaths$Tortured <- as.character(deaths$Tortured)
for (i in 1:nrow(deaths)) {
  deaths[i,17] <- gsub(x = deaths[i, 17], pattern = "\n", replacement = "")
}

# Omit records containing null values
deaths <- na.omit(deaths)
deaths[complete.cases(deaths),]
for (i in 1:nrow(deaths)) {
  for (j in 1:ncol(deaths)) {
    na_value <- sum(str_trim(deaths[i,j]) == "na", na.rm = TRUE)
    if (na_value == 1) {
      deaths <- deaths[-i,]
    }
  }
}

# Rename column names that have "." separating words
names(deaths) <- gsub(x = names(deaths), pattern = "\\.", replacement = " ")

# Write updated dataset to csv file
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
