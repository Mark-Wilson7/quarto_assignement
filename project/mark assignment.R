install.packages("tidyverse")
install.packages("plotly")
library(plotly)
library(tidyverse)

# reading in excel
unicef_indicator_1_7_ <- read_csv("unicef_indicator_1 (7).csv")
ind2 <- select(unicef_indicator_1_7_, country, alpha_2_code, alpha_3_code, numeric_code, year, obs_value)
unicef_metadata_5_ <- read_csv("unicef_metadata (5).csv")
meta2 <- select(unicef_metadata_5_, country, alpha_2_code, alpha_3_code, numeric_code, year, life_expectancy)
data_join <-full_join(ind2,meta2,by=c("country","numeric_code","year") )
ind_metaL<-left_join(ind2,meta2,by=c("country","numeric_code","year","alpha_2_code","alpha_3_code"))
#selecting data for 2022
ind_metaL_2022<-ind_metaL %>%
  filter(year==2022)
# reading in world map 
map_world<-map_data("world")

map_data1<-full_join(ind_metaL_2022,map_world,by=c("country"="region"),relationship = "many-to-many")
#create map
ggplot(map_data1)+
  aes(x=long,y=lat,group=group,fill=obs_value)+
  geom_polygon()+ 
  labs(x="", y="",title= "Percentage of population using non-piped improved drinking water in 2022", fill="% of population")





#Barchart
#selecting top 5 countries highest percentage in 2022
top5_2022<-arrange(ind_metaL_2022,desc(obs_value)) %>% 
  head(5) %>% 
  select(country,numeric_code)
#joining to obtain all years data for the 5 countries in 2022 identified 
top5c_yr <-left_join(top5_2022,ind_metaL,by=c("country","numeric_code") )
#bar chart of percentage of population using non-piped improved drinking water over years  
top5c_yr %>% 
  ggplot()+
  aes(country, obs_value, fill=country)+
  geom_col()+
  facet_wrap(~year)+
  labs(x="", y="% non-piped improved drinking water",
       title= "Evolution of population using non-piped improved drinking water \n from 2000-2022 for top 5 countries identifed in 2022", 
       fill="Country")+
  theme_classic()+
  theme(axis.text.x = element_blank())
#barchart 2
top10c_yr %>% 
  ggplot()+
  aes(country, obs_value, fill=country)+
  geom_col()+
  facet_wrap(~year)+
  labs(x="", y="% non-piped improved drinking water",
       title= "Evolution of population using non-piped improved drinking water \n from 2000-2022 for top 5 countries identifed in 2022", 
       fill="Country")+
  theme_classic()+
  theme(axis.text.x = element_blank())


#time series
timeseries1<-top5c_yr%>%
  ggplot()+
  aes(year,life_expectancy,group=country,color=country)+
  geom_line()+
  labs(x="Life expectancy (years)",
       y="Year",
       title= "Life expectancy over time of the countries identified as top 5 \n with the highest percentage using non-piped improved \n drinking water in 2022 ",
       fill="Country")+
  theme_classic()
ggplotly(timeseries1)

#timesseries 2 top 10 countries
timeseries2<-top10c_yr%>%
  ggplot()+
  aes(year,life_expectancy,group=country,color=country)+
  geom_line()+
  labs(x="Life expectancy (years)",
       y="Year",
       title= "Life expectancy over time of the countries identified as top 10 \n with the highest percentage using non-piped improved \n drinking water in 2022 ",
       fill="Country")+
  theme_classic()
ggplotly(timeseries2)

#joining data to show top 10 in 2022
#selecting top 10countries highest percentage in 2022
top10_2022<-arrange(ind_metaL_2022,desc(obs_value)) %>% 
  head(10) %>% 
  select(country,numeric_code)
#joining to obtain all years data for the 5 countries in 2022 identified 
top10c_yr <-left_join(top10_2022,ind_metaL,by=c("country","numeric_code") )




#scatterplot
top<-ind_metaL %>%
  filter(year==2000)
ggplot(ind_metaL_2000)+
  aes(obs_value,life_expectancy)+
  geom_point()+
  facet_wrap(~year)+
  labs(
    x="% non-piped improved drinking water",
    y="life expectancy",
    title = "Comparing the life expextancy with percentage of population using \n non-piped imporved drinking water for the selected 5 countires"
  ) 
#scatterplot 2
ggplot(top10c_yr)+
  aes(obs_value,life_expectancy)+
  geom_point()+
  facet_wrap(~year)+
  labs(
    x="% non-piped improved drinking water",
    y="life expectancy",
    title = "Comparing the life expextancy with percentage of population using \n non-piped imporved drinking water for the selected 5 countires"
  ) 





