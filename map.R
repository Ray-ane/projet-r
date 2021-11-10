#------------------------------------------------------------#
#                         IMPORTATION                        #
#------------------------------------------------------------#

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(datasets)
require(usmap)
require(openintro)
require(maps)


covidus=read.csv("us_states_covid19_daily.csv")

abb=covidus$state

region=abbr2state(abb)

covidus2=cbind(covidus,region)
covidus2['fips'] <- fips(covidus2$state)
states=map_data("state")


#### Agrégation des données ####

states2=states %>%
  group_by(region) %>%
  summarise(lat = max(lat),long=max(long))


covid_agg=covidus2 %>%
  group_by(region,state) %>%
  summarise(total_pos = sum(positive,na.rm = TRUE),total_neg=sum(negative,na.rm = TRUE),
            total_nt=sum(pending,na.rm = TRUE))
covid_agg$region=tolower(covid_agg$region)



map.df <- merge(covid_agg,states2, by="region", all.x = T)


#### Nombre total de cas positifs aux États-Unis ####

plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Cas positive", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Cas positifs de Covid", caption = "Source: .....")




#### Nombre total de cas positifs dans l'ouest des États-Unis ####

plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("CA", "NV", "ID", "OR", "WA")) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Cas positive", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Cas positifs de Covid dans l'ouest des États-Unis", caption = "Source : ....")





####  Nombre total de cas positifs dans la région nord-est ###

plot_usmap(data = map.df, values = "total_pos",   labels=TRUE,include = c("PA", "NJ", "CT", "NY", "MA" , "RI" , "VT" , "NH" , "ME")) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "Cas positive", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Cas positifs de Covid dans le nord-est des États-Unis", caption = "Source: .... ")






plot_usmap(data = map.df, values = "total_pos",   labels=TRUE) + 
  scale_fill_continuous( low = "white", high = "orange", 
                         name = "total_nt", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Not tested cases of Covid19 in US", caption = "Source: @SRK")




### Sous-ensemble des données pour les États fortement infectés ####

sub_us=subset(covidus2 , subset =(covidus2$state %in% c('MA','TX','WA','CA','FL','CO','NY')))


####  Croissance des patients infectés dans 7 états

p <- ggplot(data = sub_us, aes(x = date, y = positive),color= state)

p + geom_line(aes(group = state)) + facet_wrap(~ state)

### Tendance des décès dans 7 états

p <- ggplot(data = sub_us, aes(x = date, y = death))

p + geom_line(aes(group = state)) + facet_wrap(~ state)

