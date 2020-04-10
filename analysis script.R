## ENEC 395: Climate and avian turnover script 3
# Final products: histogram pdf, bbcSpeciesCount: table of bbc aou counts 
# filtered by relevant sites and breeders, counts_list: list of tables of bbs data 
# for each bbc site, removed transient

library(dplyr)
library(sf)
library(ggplot2)
# Checking temporal distribution of surveys
# must run filtering scripts first and save objects for counts_list

bbcSites.3 = read.csv("bbcSitesFin2.csv")
bbsCounts.3 = read.csv("subset_bbsCounts.csv")
bbsRoutes.3 = read.csv("subset_bbsRoutes.csv")

bbc_counts = read.csv("bbc-data/bbc_counts.csv", na.strings = c("+", "LU"), stringsAsFactors = FALSE)


pdf(file = "bbs_hist.pdf")
for(i in 1:length(counts_list)) {
  hist(counts_list[[i]]$year, breaks = 20, xlab = "year", 
       main = paste("Site ", i))
}
dev.off()
#####



#####
pdf(file = "bbs_hist_byroute.pdf")
for(i in 1:length(counts_list)) {
  
  par(mfrow = c(1,2))
  
  occurrence1 = bbsWeather %>% 
    filter(stateroute %in% counts_list[[i]]$stateroute, year >= bbcSitesFin$y1[i] - 2 & year <= bbcSitesFin$y1[i] +2)  
  
  occurrence2 = bbsWeather %>% 
    filter(stateroute %in% counts_list[[i]]$stateroute, year >= bbcSitesFin$y2[i] - 2 & year <= bbcSitesFin$y2[i] +2) 
  
  barplot(table(occurrence1$stateroute), xlab = "Y1 state route surveys", main = paste("Site ", i, ":", bbcSitesFin$State[i]))
  barplot(table(occurrence2$stateroute), xlab = "Y2 state route surveys")
  
}
dev.off()
par(mfrow = c(1, 1))

pdf(file = "map routes and surveys.pdf")
us_states2163 = st_transform(us_states, 2163)
us_map = tm_shape(us_states2163) + 
  tm_polygons() + 
  tm_layout(frame = FALSE) 

us_map = us_map + 
  tm_shape(sf_bbcSites)+
  tm_dots(col = "red", size = .07, shape = 8)
  
for( n in 1: length(dist_list)) {
  us_map = us_map + 
    tm_shape(dist_list[[n]]) +
    tm_dots()
}

print(us_map)

dev.off()

# assign bbs landcover

landcover_US = read.csv("fragmentation_indices_nlcd_simplified.csv")
newcode <- data.frame(class = seq(1,9), 
                                   legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                              "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))
landcover_US_2001 = landcover_US %>%
  select(file:prop.landscape) %>%
  filter(year == "2001") %>%
  filter(stateroute %in% bbsRoutes.3$stateroute)

landcover_US_2001.legend = left_join(landcover_US_2001, newcode, by = "class")

landcover_Can = read.csv("fragmentation_indices_canada.csv")
legend_can = read.csv("canada-landcover-legend.txt")

landcover_Can_2001 = landcover_Can %>% 
  select(file:prop.landscape) %>%
  filter(year == "2000") %>%
  filter(stateroute %in% bbsRoutes.3$stateroute) 

landcover_Can_2001.legend = left_join(landcover_Can_2001, legend_can, by = "class") %>%
  rename(legend = label) %>%
  select(-definition)

# combine US and Canada landcover, choose BBS roots to match 

landcover = bind_rows(landcover_Can_2001.legend, landcover_US_2001.legend)

bbsroute.landcover = landcover %>% group_by(stateroute) %>% filter(prop.landscape == max(prop.landscape))

bbcSites.3 = rename(bbcSites.3, bbc_site = X)

bbsroute.landcover = left_join(bbsroute.landcover, bbsRoutes.3, by = "stateroute")# %>% select(-X) 
  
bbsroute.landcover_fin = left_join(bbsroute.landcover, bbcSites.3, by = "bbc_site") %>%
  select(c(year:class, prop.landscape:sitename, state:elev_m)) %>% group_by(bbc_site)

pair = data.frame(bbc_site = 1:17, bbc_siteID = bbcSites.3$siteID,
                  stateroute = c("47019", "18009", "72028", "14016", "14016",
                                 "18009", "46030", "18009", "80002", "18009",
                                 "61064", "14047", "61121", "68001", "80002", "68220", "14016"))

# compare abundance of individuals 

bbcSpeciesCount = bbc_counts %>%
  filter(siteID %in% bbcSites.3$siteID) %>%
  filter(status == "breeder")
bbcSpeciesCount$count[is.na(bbcSpeciesCount$count)] = 0

replace.counts = as.numeric(bbcSpeciesCount$count)
bbcSpeciesCount.num = bbcSpeciesCount %>% mutate(count.int = replace.counts)


pair.counts = data.frame(bbc = integer(), species1 = integer(), individ1 = numeric(), bbs = integer(), species2 = integer(), individ2 = numeric())
for (n in 1:nrow(pair)) {
  bbc = pair$bbc_siteID[n]
  bbs = pair$stateroute[n]
  
  df1 = filter(bbcSpeciesCount.num, bbcSpeciesCount.num$siteID == bbc)
  df2 = filter(bbsCounts.3, bbsCounts.3$stateroute == bbs)
  
  species1 = nrow(df1)
  individ1 = sum(df1$count.int)
  
  species2 = nrow(filter(bbsCounts.3, bbsCounts.3$stateroute == bbs))
  individ2 = sum(df2$speciestotal)
  
  df = data.frame(bbc, species1, individ1, bbs , species2, individ2)
  pair.counts = rbind(pair.counts, df)

}

# for loop calculating # of species for subsets of bbs with same number of individuals as bbc
#subsets
