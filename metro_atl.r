# ACS GA SEED
library(acs)
library(maps)
library(ggplot2)
library(tigris)
library(dplyr)
library(htmlwidgets)
library(leaflet)
api.key.install(key="45433e3f7efa61c93be9898a0340bfa7ed8eca07")
acs.tables.install()

inc.tbl<-acs.lookup(endyear=2014, keyword=c("family","income"), dataset="acs")

ga.seed.counties <- fips.county[c(418,420, 431, 447, 454),]

ga.seed.map<-block_groups(state = "GA", county = ga.seed.counties[,3], cb=TRUE)

ga.seed <- geo.make(state="GA", county=ga.seed.counties[,3], tract = "*", block.group = "*" )

ga.seed.total.pop <- acs.fetch(geo = ga.seed, table.number="B01003", endyear = 2014)
ga.seed.mfi <- acs.fetch(geo = ga.seed, table.number="B19113", endyear = 2014)


ga.seed.income_df <- data.frame(state=str_pad(ga.seed.mfi@geography$state, 2, "left", pad="0"),
                               county=str_pad(ga.seed.mfi@geography$county, 3, "left", pad="0"),
                               tract=str_pad(ga.seed.mfi@geography$tract, 6, "left", pad="0"),
                               blkgrp=str_pad(ga.seed.mfi@geography$blockgroup, 1, "left", pad="0"),
                               MFI = ga.seed.mfi@estimate,
                        stringsAsFactors = FALSE)

ga.seed.income_df$GEOID <- with(ga.seed.income_df, paste0(state, county, tract, blkgrp))

income_merged <- geo_join(ga.seed.map, ga.seed.income_df, "GEOID", "GEOID")

#remove water
income_merged <- income_merged[income_merged$ALAND>0,]

popup <- paste0("GEOID: ", income_merged$GEOID, "<br>", "Median Family Income: ", round(income_merged$B19113_001,0))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = income_merged$B19113_001
)


map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = income_merged,
              fillColor = ~pal(B19113_001),
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal,
            values = income_merged$B19113_001,
            position = "bottomright",
            title = "Median Family Income (acs 2010-2014",
            labFormat = labelFormat(suffix = "%"))

saveWidget(map3, file="map3.html", selfcontained=TRUE)
