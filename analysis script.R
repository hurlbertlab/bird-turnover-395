## ENEC 395: Climate and avian turnover

# Library
library(dplyr)

# Read in Breeding Bird Census (BBC) data
bbc_censuses = read.csv("bbc_censuses.csv", header = TRUE, sep = ",")
bbc_counts = read.csv("bbc_counts.csv", header = TRUE, sep = ",")
bbc_sites = read.csv("bbc_sites.csv", header = TRUE, sep = ",")

# Filter sites ( >= 2 census years, >= 10 years apart)
bbcCensusCount <- data.frame(siteID_uni = unique(bbc_censuses$siteID), count = NA, time_range = NA)

for (s in 1: length(bbcCensusCount$siteID_uni)) {
  tmpCount = filter(bbc_censuses, siteID, siteID == bbcCensusCount$siteID_uni[s])
  tmpTime = max(tmpCount$year) - min(tmpCount$year)
  
  bbcCensusCount$count[s] = nrow(tmpCount)
  bbcCensusCount$time_range[s] = tmpTime
  }

bbcCensusCount = filter(bbcCensusCount, count, count>= 2) 
bbcCensusCount = filter(bbcCensusCount, time_range, time_range>= 10)