library(tidyverse) 
library(ggplot2)
library(dplyr)
library(datasets)
require(usmap)
require(openintro)
require(maps)


covidus=read.csv("/Users/scorpion/Documents/STID/S3/ R Shiny/Projet/data/us_states_covid19_daily.csv")

abb=covidus$state
region=abbr2state(abb)

covidus2=cbind(covidus,region)
covidus2['fips'] <- fips(covidus2$state)
states=map_data("state")


####Aggregating the data ####

states2=states %>%
  group_by(region) %>%
  summarise(lat = max(lat),long=max(long))


covid_agg=covidus2 %>%
  group_by(region,state) %>%
  summarise(total_pos = sum(positive,na.rm = TRUE),total_neg=sum(negative,na.rm = TRUE),
            total_nt=sum(pending,na.rm = TRUE))
covid_agg$region=tolower(covid_agg$region)

map.df <- merge(covid_agg,states2, by="region", all.x = T)



####total positive cases in the US ####

plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Positive Cases", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Positive cases of Covid", caption = "Source: @SRK")




####total positive cases in West US####
plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("CA", "NV", "ID", "OR", "WA")) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Positive Cases", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Positive cases of Covid in Western US", caption = "Source: @SRK")



####total positive cases in northeast region ###
plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = .northeast_region) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Positive Cases", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Positive cases of Covid in North East US", caption = "Source: @SRK")



####total non treated cases in US####
plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "total_nt", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Not tested cases of Covid19 in US", caption = "Source: @SRK")




### Subsetting the data for highly infected states ####
sub_us=subset(covidus2 , subset =(covidus2$state %in% c('MA','TX','WA','CA','FL','CO','NY')))
####Infected patients growth in 7 states
p <- ggplot(data = sub_us, aes(x = date, y = positive),color= state)

p + geom_line(aes(group = state)) + facet_wrap(~ state)

### death trend in 7 states 
p <- ggplot(data = sub_us, aes(x = date, y = death))

p + geom_line(aes(group = state)) + facet_wrap(~ state)

