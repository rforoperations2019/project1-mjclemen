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


# Extract years from first column by finding 4 consecutive numbers
# Logic found on stackoverflow: https://stackoverflow.com/questions/48121585/extract-year-from-string-and-append-to-dataframe/48121686
extractYear <- function(string) {
  year <- regmatches(string, regexec("[0-9]{4}", string))
  sapply(year, function(x) {
    if(length(x) > 0){
      x <- as.numeric(x)
    } else {
      x <- "Unknown"
    }
  })
}

# Apply function to first column in dataset to find the year of each record
deaths$Date <- extractYear(deaths$Date)

# Remove records with unknown dates
for(i in 1:nrow(deaths)) {
  unknown_value <- sum(str_trim(deaths[i,"Date"]) == "Unknown", na.rm = TRUE)
  if(unknown_value == 1) {
    deaths <- deaths[-i,]
  }
  if (str_trim(deaths[i, "Type of Death"]) == "Suspected Source of Fire:") {
    deaths <- deaths[-i,]
  }
  
}

# Rename Date column to "Year of Death" to match what the values are
names(deaths)[1] <- "Year of Death"

# Uncovering unknown information in the data. Renaming values that equal the column name to instead
# be marked as "Unknown"
deaths$Name[deaths$Name == "Name"] <- "Unknown"
deaths$Sex[deaths$Sex == "Sex"] <- "Unknown"
deaths$`Country Killed`[deaths$`Country Killed` == "Country Killed"] <- "Unknown"
deaths$Organization[deaths$Organization == "Organization"] <- "Unknownn"
deaths$Nationality[deaths$Nationality == "Nationality"] <- "Unknown"
deaths$Medium[deaths$Medium == "Medium"] <- "Unknown"
deaths$Job[deaths$Job == "Job"] <- "Unknown"
deaths$Coverage[deaths$Coverage == "Coverage"] <- "Unknown"
deaths$Freelance[deaths$Freelance == "Freelance"] <- "Unknown"
deaths$`Local Foreign`[deaths$`Local Foreign` == "Local/Foreign"] <- "Unknown"
deaths$`Source of Fire`[deaths$`Source of Fire` == "Source of Fire"] <- "Unknown"
deaths$`Source of Fire`[deaths$`Source of Fire` == "Unknown Fire"] <- "Unknown"
deaths$`Type of Death`[deaths$`Type of Death` == "Type of Death"] <- "Unknown"
deaths$`Impunity  for Murder`[deaths$`Impunity  for Murder` == "Impunity for Murder"] <- "Unknown"
deaths$`Impunity  for Murder`[deaths$`Impunity  for Murder` == "Impunity (for Murder)"] <- "Unknown"
deaths$`Impunity  for Murder`[deaths$`Impunity  for Murder` == "Complete Impunity"] <- "Yes"
deaths$`Taken Captive`[deaths$`Taken Captive` == "Taken Captive"] <- "Unknown"
deaths$Threatened[deaths$Threatened == "Threatened"] <- "Unknown"
deaths$Tortured[deaths$Tortured == "Tortured"] <- "Unknown"

# Write updated dataset to csv file
write.csv(deaths, file = 'JournalistDeaths.csv', row.names = FALSE)
