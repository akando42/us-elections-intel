library(politicaldata)
library(highcharter)
library(dplyr)


## ARREST DATA
data("USArrests", package = "datasets")
USArrests <- mutate(USArrests, "woe-name" = rownames(USArrests))

hcmap(map = "countries/us/us-all", data = USArrests,
      joinBy = "woe-name", value = "UrbanPop", name = "Urban Population")


## UNPRESIDENTIAL LMAO
pres_poll_by_state <- politicaldata::pres_results
unpresidential_poll_by_district <- politicaldata::house_results


pres_by_state <- pres_poll_by_state %>%
  filter(year%in%2015:2017) %>%
  filter(state != 'DC')

## Ploting on Map
hcmap(map = "countries/us/us-all", data = pres_by_differences,
      joinBy = "state", value = "scaled", name = "WTF",
      download_map_data = FALSE) 

## By DISTRICT
unpresidential_by_district <- unpresidential_poll_by_district %>%
  filter(year%in%2017:2019) %>%
  filter(state_abb != 'DC')

## Transforming state name here. 
states <- us$features
pres_poll_by_state$state = tolower(states.properties[match(pres_poll_by_state$state,states.properties.postalcode)]) 
unpresidential_by_district <- mutate(unpresidential_by_district, difference = (dem-rep)^2*10000)

hcmap(map = "countries/us/custom/us-113-congress", data=unpresidential_by_district, 
      joinBy = "district", value="difference", name="Interesting Districts")


pres <- politicaldata::pres_results

pres <- pres %>% 
  filter(year%in% 2008:2016)


# we need the full name of the state to join with the map

# minmum sample per district

unpresidential_by_district <- mutate(unpresidential_by_district, sample_size = sqrt(sample_size))
unpresidential_by_district <- mutate(unpresidential_by_district, sample_size_2_percent = (0.02/1.96)^2/(dem*rep))

