#############################################################
### Exploring the number of visitors for each year
# Read CSV
NP_visit <- read.csv("Annual_NP_Visitation.csv")

# Replace NA with 0
TRV <-NP_visit$TRV
TRV[is.na(TRV)] <- 0
NP_visit$TRV <- TRV

# Define unique year values
year <- unique(NP_visit$Year)

# Empty vector for each year of NP visitors
NP <- rep(NA, length(year))

# Sum for each year
for(i in 1:length(year)) {
  NP_filter <- NP_visit[NP_visit$Year == year[i],]
  NP[i] <- sum(NP_filter$TRV)
}
# Check values
NP

# Time series plot
plot(year, NP, type='l', main="National Park Visitors per Year",
     xlab="Year", ylab="Number of Visitors")

#############################################################
### Gather the visitor count and species count for each park
# Read the master list
NP_master <- read.csv("species_parks.csv")

# The number of species at each park
NP_list <- unique(NP_master$Park.Name)

# Empty vector for each year of NP visitors
NP_species <- rep(NA, length(NP_list))
NP_visitors <- rep(NA, length(NP_list))
NP_acres <- rep(NA, length(NP_list))

# Sum for each park
library(plyr)
library(data.table)

# Filter number of species for each park
for(i in 1:length(NP_list)) {
  NP_filter <- NP_master[NP_master$Park.Name == NP_list[i],]
  NP_species[i] <- nrow(NP_filter)
}

# Filter visitor count from 2016 for each park
for(i in 1:length(NP_list)) {
  NP_filter <- NP_master[NP_master$Park.Name == NP_list[i],]
  NP_visitors[i] <- NP_filter$TRV[1]
}

# Filter acres
for(i in 1:length(NP_list)) {
  NP_filter <- NP_master[NP_master$Park.Name == NP_list[i],]
  NP_acres[i] <- NP_filter$Acres[1]
}

# Create data table
psva_count <- data.table(
  Park = NP_list,
  Species = NP_species,
  Acres = NP_acres,
  Visitors = NP_visitors
)

#############################################################
### Address NA values
# Sequoia and Kings Canyon National Park
# Take the the two park counts and sum together
KC <- dplyr::filter(NP_visit, ParkName == "Kings Canyon NP" & Year == 2016)
KC <- KC$TRV
SQ <- dplyr::filter(NP_visit, ParkName == "Sequoia NP" & Year == 2016)
SQ <- SQ$TRV
KCSQ <- KC+SQ
# Visitor count for both parks
KCSQ
psva_count$Visitors[psva_count$Park == "Sequoia and Kings Canyon National Parks"] <- KCSQ

# Wrangell - St Elias National Park and Preserve
# Grab value from the National Park Service site
WSE <- 79047
psva_count$Visitors[psva_count$Park == "Wrangell - St Elias National Park and Preserve"] <- WSE

# Gates Of The Arctic National Park and Preserve
# Grab value from the National Park Service site
GoA <- 10047
psva_count$Visitors[psva_count$Park == "Gates Of The Arctic National Park and Preserve"] <- GoA

## Put these values in the master table
NP_master$TRV[NP_master$Park.Name == "Sequoia and Kings Canyon National Parks"] <- KCSQ
NP_master$TRV[NP_master$Park.Name == "Wrangell - St Elias National Park and Preserve"] <- WSE

#############################################################
### Add number of species for each park in the master table
for (i in 1:length(NP_list)) {
  NP_filter <- NP_master$Park.Name == NP_list[i]
  for (j in 1:length(NP_master$Park.Name)) {
    if (NP_filter[j] == TRUE) {
      NP_master$Species.Count[j] <- psva_count$Species[i]
    }
  }
}

# Write file
write.csv(NP_master, "species_parks_v2.csv")
write.csv(psva_count, "species_parks_subset.csv")
