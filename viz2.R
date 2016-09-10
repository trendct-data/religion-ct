library(RColorBrewer)

df_county_demographics$value <- df_county_demographics$total_population
choro = CountyChoropleth$new(df_county_demographics)
choro$set_zoom("connecticut")
choro$title = "Where Do People in Connecticut Live?"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Population", colours = brewer.pal(8, "GnBu"))
choro$render()

df_county_demographics$value <- df_county_demographics$median_rent
choro = CountyChoropleth$new(df_county_demographics)
choro$set_zoom("connecticut")
choro$title = "Median Rent in Connecticut Counties"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Median Rent ($)", colours = brewer.pal(8, "GnBu"))
choro$render() + geom_text(data = data.frame(long = -72.926889, lat = 41.305532, label = "hm"),
                           aes(long, lat, label = label, group = NULL), color = "black", size = 3)

df_county_demographics$value <- df_county_demographics$percent_white
choro = CountyChoropleth$new(df_county_demographics)
choro$set_zoom("connecticut")
choro$title = "How Much of Connecticut's Population Is White?"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Percent", colours = brewer.pal(8, "GnBu"))
choro$render() + geom_text(data = data.frame(long = -72.926889, lat = 41.305532, label = "Salt Lake"),
                           aes(long, lat, label = label, group = NULL), color = "black", size = 3)

counties <- counties[counties$STABBR == "CT",]
counties$CNTYNAME <- as.character(counties$CNTYNAME)
counties$CNTYNAME <- sub("County", "", counties$CNTYNAME)
counties$CNTYNAME <- sub("\\s+$", "", counties$CNTYNAME)

counties <- counties[,!sapply(counties, function(x) all(is.na(x)))]

myDF <- as.data.frame(cbind(counties$CNTYNAME, counties$FIPS, counties$POP2010, 
                            counties$TOTRATE, counties$LDSRATE, counties$MPRTRATE, 
                            counties$EVANRATE, counties$CATHRATE, counties$OTHRATE, 
                            counties$ORTHRATE, counties$BPRTRATE))
colnames(myDF) <- c("county", "region", "population", "total", "LDS", 
                    "mainline", "evangelical", "catholic", "originalother", 
                    "orthodox", "blackprot")
myDF[,2:11] <- lapply(myDF[,2:11], as.character)
myDF[,2:11] <- lapply(myDF[,2:11], as.numeric)
myDF[is.na(myDF)] <- 0

myDF <- mutate(myDF, other = (originalother - LDS + orthodox + blackprot))
myDF <- mutate(myDF, allchurchytypes = 
                 (LDS + mainline + evangelical + catholic + other))
myDF <- mutate(myDF, difference = (total - allchurchytypes))
c(mean(myDF$difference), sd(myDF$difference))

myDF <- mutate(myDF, unclaimed = (1000. - allchurchytypes))

myDF$value <- myDF$LDS
choro = CountyChoropleth$new(myDF)
choro$set_zoom("connecticut")
choro$title = "LDS Adherents by County"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence rate\n(per 1000 population)", 
                                          colours = brewer.pal(8, "YlGnBu"))
choro$render() + geom_text(data = data.frame(long = -72.926889, lat = 41.305532, label = "Salt Lake"),
                           aes(long, lat, label = label, group = NULL), color = "black", size = 3)


myDF$value <- myDF$unclaimed
choro = CountyChoropleth$new(myDF)
choro$set_zoom("connecticut")
choro$title = "Nonadherents by County"
choro$set_num_colors(1)
choro$ggplot_scale = scale_fill_gradientn(name = "Nonadherence rate\n(per 1000 population)", 
                                          colours = brewer.pal(8, "YlGnBu"))
choro$render() + geom_text(data = data.frame(long = -72.926889, lat = 41.305532, label = "Salt Lake"),
                           aes(long, lat, label = label, group = NULL), color = "black", size = 3)

library(gridExtra)
myDF$value <- myDF$catholic
choro1 = CountyChoropleth$new(myDF)
choro1$set_zoom("connecticut")
choro1$title = "Catholics by County"
choro1$set_num_colors(1)
choro1$ggplot_scale = scale_fill_gradientn(name = "Adherence rate\n(per 1000 population)", 
                                           colours = brewer.pal(8, "YlGnBu"))
myDF$value <- myDF$evangelical
choro2 = CountyChoropleth$new(myDF)
choro2$set_zoom("connecticut")
choro2$title = "Evangelicals by County"
choro2$set_num_colors(1)
choro2$ggplot_scale = scale_fill_gradientn(name = "Adherence rate\n(per 1000 population)", 
                                           colours = brewer.pal(8, "YlGnBu"))

grid.arrange(choro1$render(), choro2$render(), ncol = 2)


#

library(reshape2)
meltedDF <- melt(myDF[,c(1,5:8,12,15)])
colnames(meltedDF) <- c("county", "religion", "rate")
meltedDF$religion <- as.factor(meltedDF$religion)
meltedDF$religion <- factor(meltedDF$religion,
                            levels(meltedDF$religion)[c(1,6,4,3,5,2)])

ggplot(meltedDF[(meltedDF$county == "Hartford"| meltedDF$county == "Fairfield"),], 
       aes(x = religion, y = rate, group = county, fill = religion)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~county, ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="none", axis.title.x = element_blank()) +
  ylab("Adherence rate (per 1000 population)") + 
  ggtitle("Two Most Populous Counties In Connecticut") +
  scale_x_discrete(labels=c("LDS", "Unclaimed", "Catholic", "Evangelical",
                            "Mainline", "Other"))

ggplot(meltedDF, aes(x = religion, y = rate, group = county, fill = religion)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~county, ncol = 6) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="none", axis.title.x = element_blank(), 
        axis.text.x= element_text(angle=45, hjust = 1)) +
  ylab("Adherence rate (per 1000 population)") + 
  ggtitle("Religions in Connecticut Counties") +
  scale_x_discrete(labels=c("LDS", "Unclaimed", "Catholic", "Evangelical",
                            "Mainline", "Other"))