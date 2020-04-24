######################################
## ENEC 395: Avian community similarity: script 3
# Ellie Kremer
# 12/3/2019
######################################

library(dplyr)
library(sf)
library(ggplot2)
library(spData)
library(rnaturalearth)
library(rnaturalearthdata)
library(tmap)
library(raster)
library(rgeos)

# Read in filtered BBC data
bbcSites.3 = read.csv("bbcSitesFin2.csv")
bbsCounts.3 = read.csv("subset_bbsCounts.csv")
bbsRoutes.3 = read.csv("subset_bbsRoutes.csv")

# Read in raw BBC survey data

bbc_counts = read.csv("bbc-data/bbc_counts.csv", stringsAsFactors = FALSE) 
for(n in 1: length(bbc_counts$count)) {
  if(bbc_counts$count[n] == "+") {bbc_counts$count[n] = .25}
  if(bbc_counts$count[n] == "LU") {bbc_counts$count[n] = 1.0}
}

## Assign BBS site landcover types

# Step 1: Read in raster and match classes to given types from legend

landcover_US = read.csv("fragmentation_indices_nlcd_simplified.csv")

newcode <- data.frame(class = seq(1,9), 
                                   legend = c("Open water", "Urban", "Barren", "Forest", "Shrubland", 
                                              "Agricultural", "Grasslands", "Wetlands", "Perennial ice, snow"))

# Step 2: Filter to landcover from selected years and combine US and Canada data

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

landcover = bind_rows(landcover_Can_2001.legend, landcover_US_2001.legend)

# Step 3: Assign landcover to BBS sites based on landcover type covering greatest proportion

bbsroute.landcover = landcover %>% group_by(stateroute) %>% filter(prop.landscape == max(prop.landscape))

bbcSites.3 = rename(bbcSites.3, bbc_site = X)

bbsroute.landcover = left_join(bbsroute.landcover, bbsRoutes.3, by = "stateroute")# %>% select(-X) 
  
bbsroute.landcover_fin = left_join(bbsroute.landcover, bbcSites.3, by = "bbc_site") %>%
  select(c(year:class, prop.landscape:sitename, state:elev_m)) %>% group_by(bbc_site)

pair = data.frame(bbc_site = 1:17, bbc_siteID = bbcSites.3$siteID, y1 = bbcSites.3$y1, y2 = bbcSites.3$y2,
                  stateroute = c("47019", "18009", "72028", "14016", "14016",
                                 "18009", "46030", "18009", "80002", "18009",
                                 "61064", "14047", "61121", "68001", "80002", "68220", "14016"))

# Compare abundance of individuals observed at BBC and BBS sites

bbcSpeciesCount = bbc_counts %>%
  filter(siteID %in% bbcSites.3$siteID) %>%
  filter(status == "breeder") 

bbcSpeciesCount$count[is.na(bbcSpeciesCount$count)] = 0
bbcSpeciesCount$count = as.numeric(bbcSpeciesCount$count)
bbcSpeciesCount$count = ceiling(bbcSpeciesCount$count)

pair.counts = data.frame(bbc = integer(), species1.y1 = integer(), individ1.y1 = integer(), species1.y2 = integer(), individ1.y2 = integer(),
                         bbs = integer(), bbs.y1 = integer(), species2.y1 = integer(), individ2.y1 = numeric(),
                         bbs.y2 = integer(), species2.y2 = integer(), individ2.y2 = integer())
for (n in 1:nrow(pair)) {
  bbc = pair$bbc_siteID[n]
  bbs = pair$stateroute[n]
  y1 = pair$y1[n]
  y2 = pair$y2[n]
  
  
  df1.y1 = filter(bbcSpeciesCount, bbcSpeciesCount$siteID == bbc,
               bbcSpeciesCount$year == y1) 
  df1.y2 = filter(bbcSpeciesCount, bbcSpeciesCount$siteID == bbc,
                  bbcSpeciesCount$year == y2)
  
  df2 = bbsCounts.3 %>% filter(bbsCounts.3$stateroute == bbs)
  
  df2.y1 = filter(df2, df2$year == y1 - min(abs(unique(df2$year) - y1)))
  bbs.y1 =  y1 - min(abs(unique(df2$year) - y1))
  if(sum(df2.y1$speciestotal) == 0) {
    df2.y1 = filter(df2, df2$year == y1 + min(abs(unique(df2$year) - y1)))
    bbs.y1 = y1 + min(abs(unique(df2$year) - y1))
  }
  df2.y2 = filter(df2, df2$year == y2 - min(abs(unique(df2$year) - y2)))
  bbs.y2 = y2 - min(abs(unique(df2$year) - y2))
  if(sum(df2.y2$speciestotal) == 0) {
    df2.y2 = filter(df2, df2$year ==  y2 + min(abs(unique(df2$year) - y2)))
    bbs.y2 = y2 + min(abs(unique(df2$year) - y2))
  }
  
  species1.y1 = nrow(df1.y1)
  individ1.y1 = sum(df1.y1$count)
  species1.y2 = nrow(df1.y2)
  individ1.y2 = sum(df1.y2$count)
  
  species2.y1 = nrow(df2.y1)
  individ2.y1 = sum(df2.y1$speciestotal)
  species2.y2 = nrow(df2.y2)
  individ2.y2 = sum(df2.y2$speciestotal)
  
  df = data.frame(bbc, species1.y1, individ1.y1, species1.y2, individ1.y2,
                  bbs, bbs.y1, species2.y1, individ2.y1, bbs.y2, species2.y2, individ2.y2)
  pair.counts = rbind(pair.counts, df)
}

# For loop calculating # of individuals present in BBC then generating subsets
# of BBS surveys with same # of individuals

# Calculating Jaccard similarity coefficient for each subset and saving to calculate mean value for each BBS site
# J = # species shared / total # unique species

output = data_frame(siteID = integer(), stateroute = factor(), mean.J = double(), J.sd = double())

for (s in 1:nrow(pair.counts)) {
  bbs.y1.all = bbsCounts.3 %>% filter(year == pair.counts$bbs.y1[s], stateroute == pair.counts$bbs[s])
  bbs.y2.all = bbsCounts.3 %>% filter(year == pair.counts$bbs.y2[s], stateroute == pair.counts$bbs[s])
  
  J.vals = vector()
  for (i in 1:100) {
    y1.subset = sample_n(bbs.y1.all, size = pair.counts$individ1.y1[s], replace = TRUE, weight = bbs.y1.all$speciestotal)
    y2.subset = sample_n(bbs.y2.all, size = pair.counts$individ1.y2[s], replace = TRUE, weight = bbs.y2.all$speciestotal)
    
    tmp.y1.species = y1.subset$aou
    tmp.y2.species = y2.subset$aou
    
    sharedspp = sum(tmp.y1.species %in% tmp.y2.species)
    if (length(tmp.y1.species) > length(tmp.y2.species)) {
      sharedspp = sum(tmp.y2.species %in% tmp.y1.species)}
    totalspp = length(tmp.y1.species) + length(tmp.y2.species) - sharedspp
    
    J.vals[i] = sharedspp/totalspp
    J.sd = sd(J.vals)
  }
  tmpOut = data.frame(siteID = pair.counts$bbc[s], stateroute = pair.counts$bbs[s], J = mean(J.vals), sd = J.sd)
  output = rbind(output, tmpOut)
}

# Calculating Jaccard similarity value for each BBC site

bbc.J = vector()
for (l in 1: nrow(pair.counts)) {
  tmp.y1.species = filter(bbcSpeciesCount, siteID == pair.counts$bbc[l], year == bbcSites.3$y1[l])$species
  tmp.y2.species = filter(bbcSpeciesCount, siteID == pair.counts$bbc[l], year == bbcSites.3$y2[l])$species
  
  sharedspp = sum(tmp.y1.species %in% tmp.y2.species)
  if (length(tmp.y1.species) > length(tmp.y2.species)) {
    sharedspp = sum(tmp.y2.species %in% tmp.y1.species)}
  totalspp = length(tmp.y1.species) + length(tmp.y2.species) - sharedspp
  
  bbc.J[l] = sharedspp/totalspp
}
output$bbc.J = bbc.J 

# Combining and simplifying output results table

output = left_join(output, bbcSites.3, by = "siteID") 

routes_simple = data_frame(stateroute = bbsRoutes.3$stateroute, bbs.lat = bbsRoutes.3$latitude, bbs.long = bbsRoutes.3$longitude) %>%
  filter(stateroute %in% output$stateroute) %>% unique()
routes_simple$stateroute = factor(routes_simple$stateroute)

output = left_join(output, routes_simple, by = "stateroute")
simple_output = data_frame(BBC_site = output$sitename, BBC_ID = output$siteID,
                           BBS_Stateroute = output$stateroute, BBC_J = output$bbc.J,
                           BBS_J = output$J, Year_1 = output$y1, Year_2 = output$y2)

df.m = data.frame(Type = rep(c("bbs", "bbc"), each = 17), J = c(output$J, output$bbc.J))

## Analysis of results

# paired t-test

t.test(output$J, output$bbc.J, paired = TRUE)

# rough draft figures

pdf(file = "similarity_figures.pdf")

# Boxplot comparing mean values of Jaccard similarity

ggplot(data = df.m, aes(x = Type, y = J)) +
  geom_boxplot(aes(fill = Type)) +
  labs(x = "Site type", y = "Jaccard Similarity", title = "Mean similarity at BBC and BBS Sites")

# Linear regression modeling the relationship between J similarity at BBC and BBS sites

J.linmod = lm(J ~ bbc.J, data = output)

# Plot comparing linear regression model to 1:1 ratio

J.comp = ggplot(data = output, aes(x = bbc.J, y = J)) + geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(a=0, b=1, col = "red") +
  labs(title = "Linear regression compared to null hypothesis")
J.comp 

# Scatter plot comparing J for BBS and BBC with landcover and state factors included 

J.factors = ggplot(data = output, aes(x = bbc.J, y = J, color = landcover, shape = state)) + 
  geom_point() +
  geom_abline(a=0, b=1, col = "red") +
  geom_errorbar(aes(ymin = J-sd, ymax = J+sd)) +
  labs(x = "BBC J", y = "BBS J", title = "Jaccard Similarity at BBC and BBS sites") 
J.factors


dev.off()

# Map of BBC sites with final BBS site pair including landcover factor

pdf(file = "paired_site_map.pdf")
sf_bbcSites = st_as_sf(bbcSites.3,
                       coords = c("longitude", "latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
sf_bbsSites = st_as_sf(output,
                       coords = c("bbs.long", "bbs.lat"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

worldmap = ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
northam_cropped = st_crop(worldmap, xmin = -170, xmax = -50,
                          ymin = 25, ymax = 53)

us_states2163 = st_transform(northam_cropped, 2163)
us_map = tm_shape(us_states2163) + 
  tm_polygons() + 
  tm_layout(frame = FALSE) 

us_map = us_map + 
    tm_shape(sf_bbsSites) +
    tm_dots(size = .5, col = "landcover")
us_map = us_map + 
  tm_shape(sf_bbcSites)+
  tm_dots(size = .1, shape = 8) 
us_map = us_map + 
  tm_add_legend(type = "symbol", labels = c("BBC", "BBS"), shape = c(8,1))

print(us_map)

dev.off()

# Write final data files

write.csv(output, file = "jaccard_calc_table.csv")
write.csv(pair.counts, file = "pair_counts.csv")
write.csv(simple_output, file = "simple_results.csv")
write.csv(output, file = "full_results.csv")
