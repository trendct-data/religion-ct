library(foreign)
counties <- read.spss("ReligiousMembershipCounties2010.SAV", 
                      to.data.frame = TRUE)

library(choroplethr)
library(ggplot2)
library(viridis)
counties$region <- counties$FIPS
counties$value <- counties$TOTRATE
counties[(counties$value > 1000),'value'] <- 1000.
choro = CountyChoropleth$new(counties)
choro$title = "Total Religious Adherents by County"
choro$set_num_colors(1)
choro$set_zoom("connecticut")
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
choro$render()

##

library(gridExtra)
counties[is.na(counties)] <- 0
counties$value <- counties$EVANRATE
choro1 = CountyChoropleth$new(counties)
choro1$set_zoom("connecticut")
choro1$title = "Evangelical Protestants by County"
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
counties$value <- counties$MPRTRATE
choro2 = CountyChoropleth$new(counties)
choro2$set_zoom("connecticut")
choro2$title = "Mainline Protestants by County"
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))

grid.arrange(choro1$render(), choro2$render(), ncol = 2)

##
data(df_county_demographics)
df_county_demographics$value <- df_county_demographics$percent_black
choro1 = CountyChoropleth$new(df_county_demographics)
choro1$set_zoom("connecticut")
choro1$title = "Connecticut' Black Population"
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "Percent population", colours = viridis(32))
counties$value <- counties$BPRTRATE
choro2 = CountyChoropleth$new(counties)
choro2$set_zoom("connecticut")
choro2$title = "Black Protestants by County"
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))

grid.arrange(choro1$render(), choro2$render(), ncol = 2)


##

library(dplyr)
countiesjoin <- counties %>% filter(STABBR == "CT") %>% 
  select(CNTYNAME, STABBR, FIPS, BPRTRATE) %>%
  left_join(df_county_demographics, by = c("FIPS" = "region"))
myCor <- cor.test(countiesjoin$BPRTRATE, countiesjoin$percent_black)

countiesjoin <- counties %>% filter(STABBR == "CT") %>% 
  select(CNTYNAME, STABBR, FIPS, CATHRATE) %>%
  left_join(df_county_demographics, by = c("FIPS" = "region"))
myCor <- cor.test(countiesjoin$CATHRATE, countiesjoin$percent_white)