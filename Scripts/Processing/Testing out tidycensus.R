###### Explore tidycensus #####


## At the moment, must install from gitHub
library(devtools)
dev_mode(TRUE)
# install_github("hadley/ggplot2")
library("ggplot2", lib.loc="~/R-dev")
# dev_mode(FALSE)


require(tidyverse)
require(sf)
library(viridis)
library(rvest)
require(tidycensus)
options(tigris_use_cache = TRUE)



if(grepl('EI',Sys.info()["nodename"] )) 
{BASE = file.path('Z:/user/') } else {
  BASE = file.path('Volumes/ei-edl01/user/')
}


WORK.OUT = file.path(BASE,'ajk41/Solar Incentive NBER/Data/OUTPUT',Sys.Date())
dir.create(WORK.OUT, recursive=T)

GIS.files = file.path(BASE,'ajk41/GIS files')
mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
census_api_key(mykey)


########### 
## https://walkerke.github.io/tidycensus/articles/basic-usage.html
## http://strimas.com/r/tidy-sf/
# get_decennial
# get_acs



test = get_decennial(geography = c('state'),variables = "H043A001", year = c(1990)) #--> single year at a time

test %>%  #--> huh. tidy frames can go straight into ggplots
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()



## ACS with geometry
v15 <- load_variables(2015, "acs5", cache = TRUE)
                          #--> note: no year??
test = get_acs(state = "CA", county='Orange', geography = "tract", variables='B07013_001', geometry=F)  # drop the 'E' off the end - tidycensus gets E and MOE

geotest = get_acs(state = "CA", county='Orange', geography = "tract", variables='B07013_001', geometry=T)  # drop the 'E' off the end - tidycensus gets E and MOE
geotest = get_acs(state = "CA", county='Orange', geography = "tract", variables='B07013_001', geometry=T, cb=F)  # cb=F gives you TIGERlines instead of census cartographic. No idea difference.

geotest %>%
  ggplot(aes(fill = estimate, color = estimate)) +   #--> fill is fill, color is border (make same for no-border)
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


gt.2015 = get_acs(state='CA', county='Orange', geography = 'block group', variables = 'B25003_001', geometry=T)
# congressional district, etc. don't seem to work.  (see ?get_acs "geometry" for what works: state, county, tract, block group, block and zcta (lowercase))
# Geographies have to line-up with the ORDER of the >'s at http://api.census.gov/data/2015/acs5/geography.html
# That is, if it says "state>combined statistical area", then I can only supply a state, not a county.
# Though the website says "block group" requires state and county and tract; only needs state and county. So...not perfect?
#  get_acs() and get_decennial() tell you a bit more.

# use endyear=201X for acs year
gt.2015 = get_acs(state='CA', county='Orange', geography = 'block group', variables = 'B25003_001', geometry=T)
gt.2012 = get_acs(state='CA', county='Orange', endyear=2013, geography = 'block group', variables = 'B25003_001', geometry=T)
range(gt.2012$estimate)
range(gt.2015$estimate) #--> they're different. endyear works!

gt.2015 %>%
  ggplot(aes(fill = estimate, color = estimate)) +   #--> fill is fill, color is border (make same for no-border)
  geom_sf() + 
 # coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")



## let's try multiple subvariables
v15 <- load_variables(2015, "acs5", cache = TRUE)
View(v15) #-> filter on "tenure"
## in B2500X (see v15), the total households is in B25009_001; the total owner-occupied is B25009_002; and the breakdown by # of people in household is B25009_003:B25009_9.
###    so let's test to see if they sum correctly within B25009_002 (owner-occupied) using summary_var
households = get_acs(state='CA', county='Orange', endyear=2015, geography = 'block group', variables = 'B25009_002', summary_var = 'B25009_001', geometry=T)

usevars = paste0('B25009_00', 3:9)
households = get_acs(state='CA', county='Orange', endyear=2015, geography = 'block group', variables = usevars, summary_var = 'B25009_002', geometry=T)

xx = households %>% group_by(NAME) %>% summarize(summaryvar = summary_est[1], actual_sum = sum(estimate))  # Yup!


## OK - so it takes some manual thinking, but summary_var is perfect for the "denominator" in shares!



############
## Now, how do we keep the full FIPS? I want to be able to ID a state>county>census block group
###   It may be that NAME is unique, or I can pull out Census Tract with a string exp.
###   Or, use 'keep_geo_vars'
households = get_acs(state='CA', county='Orange', endyear=2015, geography = 'block group',
                     variables = usevars, summary_var = 'B25009_002', 
                     geometry=T, keep_geo_vars = T)
head(households)
## Awesome - STATEFP, COUNTYFP, TRACTCE, and BLKGRPCE form the full FIPS.
## AFFGEOID starts with the geography hierarchy (e.g. block gropu is '150'), then four 0's (not sure), then US, then the concatenated STATEFP(2)/COUNTYFP(3)/TRACTCE(6)/BLKGRPCE(1)
## Note that they're all characters!




#### Plotting with these multiple variables
## either subset the data using variable=='B25009_00X'
## or facet over ~variable:
households %>%
  filter(variable=='B25009_005') %>%
  ggplot(aes(fill = estimate, color = estimate)) +   #--> fill is fill, color is border (make same for no-border)
  geom_sf() + 
  # coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")


households %>%
  ggplot(aes(fill = estimate, color = estimate)) +   #--> fill is fill, color is border (make same for no-border)
  geom_sf() + 
  facet_wrap(~variable) +
  # coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "inferno") + 
  scale_color_viridis(option = "inferno")






