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

bbc_counts = read.csv("bbc-data/bbc_counts.csv")

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



# next steps


bbcSpeciesCount = bbc_counts %>%
  filter(siteID %in% bbcSitesFin) %>%
  filter(status == "breeder")

# assign bbs landcover

landcover_US = read.csv("fragmentation_indices_nlcd_simplified.csv")
newcode <- data.frame(class = seq(1,9), 
                                   legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                              "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))
landcover_US_2001 = landcover_US %>%
  select(file:prop.landscape) %>%
  filter(year == "2001") %>%
  filter(stateroute %in% bbsRoutes.3$stateroute)
# change route to stateroute once subset is run again

landcover_US_2001.legend = left_join(landcover_US_2001, newcode, by = "class")

bbsroute.landcover = landcover_US_2001.legend %>% group_by(stateroute) %>% filter(prop.landscape == max(prop.landscape))

bbcSites.3 = rename(bbcSites.3, bbc_site = X)

bbsroute.landcover = left_join(bbsroute.landcover, bbsRoutes.3, by = "stateroute") %>% select(-X) 
  
bbsroute.landcover_fin = left_join(bbsroute.landcover, bbcSites.3, by = "bbc_site") %>%
  select(c(year:class, prop.landscape:sitename, state:elev_m)) %>% group_by(bbc_site)

