################
## Explore and compile census data
################

## PART I
## This script loads up tidycensus
## Imports HOLC maps
## Imports HOLC add'l data (added 9-25-2017, pending)
## Creates using.fips, which is all of the state-county FIPS that touch the HOLC maps
## Imports 2015 Census Block Group (and maybe tract?) info
## Creates using.BG.index, which is all of the block groups with HOLC polys over them.
## Intersects BG with HOLC grades (A,B,C,D)
## And identifies those CBG which are >80% in one single grade (maybe ">80% in/not-in red?)

## PART II
## Loads up 1990, 2000 census data on home heating fuel: v4: 1990 census no longer has an API. Thanks, Trump.
## Loads up 1980 census data from NHGIS extract (only for CDP/SMA/MCD or something like that)
## And (attempts) to merge 1980 data.


# v4: incorporate zip code to see if CA RASS is helpful

#### Part I ####
#.libPaths('Z:/user/ajk41/jlib')
# library(devtools)
# dev_mode(TRUE)
# install_github("hadley/ggplot2", lib='~/R-dev')
# install_github("r-spatial/sf")
# dev_mode(FALSE)

require(sf)
library(viridis)
library(rvest)
require(tidycensus)
require(sp)
require(rgeos)
require(rgdal)
require(raster)
require(tigris)
require(mapview)
require(geojsonsf)
require(censusapi)
require(acs) #--> new innovation. Better looking up using acs.lookup(table.name = 'FUEL', case.sensitive=F)
options(tigris_use_cache = TRUE)

# tigris cache, for future ref (can add .zip files manually! folder is hidden, type in address using 'save as...') C:\Users\ajk41\AppData\Local\tigris
# http://old.socialexplorer.com/pub/reportdata/MetaBrowser.aspx?survey=ACS2015_5yr&ds=ACS15_5yr&header=True
# https://api.census.gov/data/2000/sf3/variables.html (navigate with this; add ".html" to anything JSON-like

# Justin Madron's github: https://github.com/americanpanorama/HOLC_Area_Description_Data/blob/master/Form_Schema/AD-Form-3.png



WORK.OUT = file.path(BASE,'ajk41/Low Income Energy/Data/OUTPUT',Sys.Date())
dir.create(WORK.OUT, recursive=T)

# mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
# census_api_key(mykey, install=T)
# Run once: Sys.setenv(CENSUS_KEY="ff90006e6d5f1960ef3bbb37579766bb563b1121")



#######
## Read in HOLC
#######
DATA.IN = file.path(BASE, 'ajk41','Low Income Energy','Data','Found Data')

#--> The 2018 polygons (7545 polys)
# HOLCold = readOGR(dsn=file.path(DATA.IN,'HOLC Maps','holc_polygons','2016 Download'), layer='holc_polygons', stringsAsFactors=F); HOLCold = st_as_sf(HOLCold); HOLCold$area_description_data = NA


#--> The 2020 polygons (8878)
# HOLC = st_read(dsn = file.path(DATA.IN, 'HOLC Maps','holc_polygons','fullshpfile','shapefile'), layer='holc_ad_data', stringsAsFactors=F) %>%
#   dplyr::rename(HOLC_Grade = holc_grade, ID = holc_id)
#   
#--> 2021 polygons (8878, same as 2020)

#--> The 2020 polygons in GEOjson (faster read)
HOLC = HOLCinit = geojson_sf(file.path(DATA.IN,  'HOLC Maps','holc_polygons','fullDownload04172021.geojson')) %>% # this one has more complete data in json in area_description_data (compared to shapefile's area_descr, also in json)
  dplyr::rename(ID = holc_id) %>%
  dplyr::mutate(GRADE = toupper(as.character(holc_grade))) %>%
  dplyr::mutate(GRADE = ifelse(GRADE=='E', 'D', GRADE)) %>% st_make_valid() %>%
  dplyr::rename(polygon_id = neighborhood_id)

HOLC = HOLC %>%
  dplyr::select(state, city, polygon_id, name, ID, GRADE)

color.map =  c("A" = "green", "B" = "blue", "C" = "yellow", "D" = "red")

######
## Read in US
######
US = st_as_sf(states()) %>% dplyr::filter(!NAME%in%c('Alaska','Hawaii','United States Virgin Islands','Guam','Puerto Rico','Commonwealth of the Northern Mariana Islands','American Samoa')) %>%
  st_transform(st_crs(HOLC)) %>% mutate(STATE_NAME = as.factor(as.character(NAME)))

USzip = st_as_sf(readRDS(file.path(GIS.files, "USA_Zip_Code_Boundaries_v104_zip_poly.rds"))) %>% 
  dplyr::select(zip = czip, NAME, STATE, POPULATION, POP_SQMI) %>% 
  st_transform(st_crs(HOLC)) %>%
  st_make_valid()

# cityform = fread('https://raw.githubusercontent.com/americanpanorama/HOLC_Area_Description_Data/master/Form_Schema/City_Form_ids.csv') # 10-08-2020 copy saved
# cityform[form_id<=1|form_id==9675, form_id:=NA] # 0's are missing forms, 1's are descriptive forms, 9675 is ???

cityform = fread('https://raw.githubusercontent.com/americanpanorama/HOLC_Area_Description_Data/master/City_Form_Codebook.csv')
cityform[form_id<=1|form_id==9675, form_id:=NA] # 0's are missing forms, 1's are descriptive forms, 9675 is ???
cityform[, schema:=case_when(
                            form_id==1939 ~ 'AD-4',
                            form_id%in%c(19370826,
                                         19370601) ~ 'AD-3',
                            form_id==19370203 ~ 'AD-2',
                            form_id==19371001 ~ 'AD-1',
                            city_id==174 ~ 'AD-3',  # Madison, WI uses both 8-26-37 and 6-1-37, but both are AD-3's
                            TRUE ~ as.character(NA))]


HOLC = HOLC %>%
  left_join(cityform, by=c('city','state'))



######
## HOLC.d, URichmond's digitized data from surveyors.
# HOLC.d = read_csv(file=file.path(DATA.IN, 'HOLC Maps','holc_ad_data10082020.csv')) 

HOLC.d = fread('https://raw.githubusercontent.com/americanpanorama/HOLC_Area_Description_Data/master/holc_ad_data.csv')

HOLC.d = HOLC.d %>%
 dplyr::filter(polygon_id!='' & polygon_id!='4 & 5') %>%  # 140 + 1 dropped
 #dplyr::mutate(polygon_id = ifelse(is.na(polygon_id), 4956, as.integer(polygon_id))) %>%
 dplyr::mutate(polygon_id = as.integer(polygon_id)) %>%
 # dplyr::filter(!is.na(polygon_id)) %>%
  arrange(polygon_id, cat_id, sub_cat_id, order) %>%
  left_join(HOLC %>% st_set_geometry(NULL) %>% dplyr::select(polygon_id, form_id, schema, city, state), by='polygon_id') %>%
  dplyr::filter(!is.na(schema))



jSH <- function(polyd, ci, sci=NA, or=NA){
  # if(NROW(unique(polyd[,'polygon_id']))>1) stop('invalid input: only one polygon_id at a time!')
  res = polyd[which(polyd$cat_id==ci),]
  if(!is.na(sci)) res = res[which(res$sub_cat_id==sci),]
  if(!is.na(or))  res = res[which(res$order==or),]
  if(NROW(res)==0) return(NA)
  if(NROW(res)>1){print(paste0('Hey, multiples in ',polyd$polygon_id[1],' at ',ci,'-',sci,' order: ', or,'--',paste0(res, collapse = '::')))
                  res = res[order(-res)][1]}
 # if(NROW(res)>1) stop(paste0('Hey, multiples in ',polyd$polygon_id[1],' at ',ci,'-',sci,' order: ', or))
  return(res$data)
}


AD1 = HOLC.d[schema=='AD-1',]
AD2 = HOLC.d[schema=='AD-2',]
AD3 = HOLC.d[schema=='AD-3',]
AD4 = HOLC.d[schema=='AD-4',]


AD1.proc = rbindlist(lapply(split(AD1, f = AD1$polygon_id), function(zz){
                                              data.frame(
                                              Black_YN =     jSH(zz,2,'d',1),
                                              Black_PCT =    jSH(zz,2,'d',2),
                                              Foreign_PCT =  jSH(zz,2,'c',1),
                                              Relief_YN =    jSH(zz,2,'f',NA),
                                              FamIncome =    jSH(zz,2,'b',NA),
                                              OOH_PCT   =    jSH(zz,3,'f',1),
                                              OCC_PCT   =    jSH(zz,3,'e',1),
                                              Rent29    =    jSH(zz,3,'m',1),
                                              Rent35    =    jSH(zz,3,'n',2), 
                                              Rent3739 =     jSH(zz,3,'o',2),
                                              Price29   =    jSH(zz,3,'h',1),
                                              Price35 =      jSH(zz,3,'i',2),
                                              Price3739=     jSH(zz,3,'j',2),
                                              Repair_Predom =jSH(zz,3,'d',1),
                                              Repair_Other = jSH(zz,3,'d',2),
                                              Construction_Predom = jSH(zz,3,'b',1),
                                              Construction_Other  = jSH(zz,3,'b',2),
                                              AverageHomeAge =jSH(zz,3,'c',1),
                                              Avail_Mortgage = jSH(zz, 4, 'a',1),
                                              TaxRate = NA,
                                              schema = 'AD-1')}), idcol = 'polygon_id')[,polygon_id:=as.integer(polygon_id)]


AD2.proc = rbindlist(lapply(split(AD2, f = AD2$polygon_id), function(zz){
                                              data.frame(
                                                Black_YN =     jSH(zz,5,'d',1),
                                                Black_PCT =    jSH(zz,5,'d',2),
                                                Foreign_PCT =  jSH(zz,5,'c',2),
                                                Relief_YN =    jSH(zz,5,'f',NA),
                                                FamIncome =    jSH(zz,5,'b',NA),
                                                OOH_PCT   =    jSH(zz,8,'c',NA),
                                                OCC_PCT   =    jSH(zz,8,'b',NA),
                                                Rent29    =    ifelse(zz$city[1]=='Lima', jSH(zz,7,'1',3), jSH(zz,7,NA,'3')),
                                                Rent35    =    ifelse(zz$city[1]=='Lima', jSH(zz,7,'2',5), jSH(zz,7,NA,'9')),
                                                Rent3739  =    ifelse(zz$city[1]=='Lima', jSH(zz,7,'3',5), jSH(zz,7,NA,'16')),
                                                # Rent29    =    jSH(zz,7,'1',3),
                                                # Rent35    =    jSH(zz,7,'2',5),
                                                # Rent3739  =    jSH(zz,7,'3',5),
                                                Price29   =    ifelse(zz$city[1]=='Lima', jSH(zz,7,'1',1),jSH(zz,7,NA,'1')),
                                                Price35 =      ifelse(zz$city[1]=='Lima', jSH(zz,7,'2',2),jSH(zz,7,NA,'6')),
                                                Price3739=     ifelse(zz$city[1]=='Lima', jSH(zz,7,'3',2),jSH(zz,7,NA,'13')),
                                                Repair_Predom =jSH(zz,6,'d',NA),
                                                Repair_Other = NA,
                                                Construction_Predom = jSH(zz,6,'b',NA),
                                                Construction_Other  = NA,
                                                AverageHomeAge =jSH(zz,6,'c',NA),
                                                Avail_Mortgage = jSH(zz,12,'a',NA),
                                                TaxRate = NA,
                                                schema = 'AD-2')}), idcol = 'polygon_id')[,polygon_id:=as.integer(polygon_id)]




AD3.proc = rbindlist(lapply(split(AD3, f = AD3$polygon_id), function(zz){
                                            data.frame(
                                              Black_YN =     jSH(zz,5,'d','1'),
                                              Black_PCT =    jSH(zz,5,'d','2'),
                                              Foreign_PCT =  jSH(zz,5,'c','2'),
                                              Relief_YN =    jSH(zz,5,'f',NA),
                                              FamIncome =    jSH(zz,5,'b',NA),
                                              OOH_PCT   =    jSH(zz,8,'c',NA),
                                              OCC_PCT   =    jSH(zz,8,'b',NA),
                                              Rent29    =    jSH(zz,7,NA,'3'),
                                              Rent35    =    jSH(zz,7,NA,'9'),
                                              Rent3739 =     jSH(zz,7,NA,'16'),
                                              Price29   =    jSH(zz,7,NA,'1'),
                                              Price35 =      jSH(zz,7,NA,'6'),
                                              Price3739=     jSH(zz,7,NA,'13'),
                                              Repair_Predom =jSH(zz,6,'d',NA),
                                              Repair_Other = NA,
                                              Construction_Predom = jSH(zz,6,'b',NA),
                                              Construction_Other  = NA,
                                              AverageHomeAge =jSH(zz,6,'c',NA),
                                              Avail_Mortgage = jSH(zz,12,'a',NA),
                                              TaxRate = NA,
                                              schema = 'AD-3')}), idcol = 'polygon_id')[,polygon_id:=as.integer(polygon_id)]



AD4.proc = rbindlist(lapply(split(AD4, f = AD4$polygon_id), function(zz){
                                            data.frame(
                                              Black_YN =     jSH(zz,1,'d',NA),
                                              Black_PCT =    jSH(zz,1,'d',NA),
                                              Foreign_PCT =  jSH(zz,1,'c',1),
                                              Relief_YN =    NA,
                                              FamIncome =    NA,
                                              OOH_PCT   =    jSH(zz,2,'f',1),
                                              OCC_PCT   =    jSH(zz,2,'e',1),
                                              Rent29    =    NA,  # the 1939 form didn't look at 1929 rents at all
                                              Rent35    =    NA,  # Nothing on 1935 either
                                              Rent3739 =     jSH(zz,2,'n',2),
                                              Price29   =    NA,
                                              Price35 =      NA,
                                              Price3739=     jSH(zz,2,'i',2),
                                              Repair_Predom =jSH(zz,2,'d',1),
                                              Repair_Other = jSH(zz,2,'d',2),
                                              Construction_Predom = jSH(zz,2,'b',1),
                                              Construction_Other  = jSH(zz,2,'b',2),
                                              AverageHomeAge =jSH(zz,2,'c',1),
                                              Avail_Mortgage = jSH(zz,6,NA,NA),
                                              TaxRate = jSH(zz,7,'2'),
                                              schema = 'AD-4')}), idcol = 'polygon_id')[,polygon_id:=as.integer(polygon_id)]

HOLC.dd = rbindlist(list(AD1.proc, AD2.proc, AD3.proc, AD4.proc), fill = T)


#######
## Begin the proces of cleaning data...
#######


## Black_YN, Black_PCT --> NBlack_YN, NBlack_PCT; N means NEW! I think. 11-5-2018
##### SKip to load, below 
# template = unique(HOLC.dd[,c('Black_YN','Black_PCT')])
# template$NBlack_YN = as.character(NA)
# template$NBlack_PCT = as.character(NA)
#   template = edit(template)
  # "nominal" = 2% (by 2-3 records with "nominal" and a percentage)
  # "small" = 5%
  # "few" = 1%
  # "scattered" = 2-5%
  # "several" = 3%
  # "some" = 3%3

# write.csv(template, file=file.path(WORK.OUT, 'Black_Presence_v3_mapping.csv'), row.names=F) # orig in September 27, 2017; copied to 4-18 in Low Income Energy
# write.csv(data.frame(Black_YN = setdiff(HOLC.dd$Black_YN, template$Black_YN)), file=file.path(WORK.OUT, 'Black_Presence_v2_mapping.csv'))
# write.csv(HOLC.dd %>% ungroup() %>% dplyr::select(Black_YN, Black_PCT) %>% distinct(), file=file.path(WORK.OUT, 'Black_Presence_v3_mapping_out.csv'))

First_Black_PCT_dictionary = c('D-4 - 25%; D-5 - 0%' = '12%',
                               'D-9 - 50%; D-10 - 50%; D-11 - 90%' = '60')

Black_PCT_dictionary = c('1/2' = '50',
                         '1/4' = '25',
                         '1/2 of 1' = '1',
                         '1/10' = '10',
                         '1/5' = '20',
                         '80 and increasing' = '80',
                         'None' = '0',
                         'none' = '0',
                         'Nil' = '0',
                         'nil' = '0',
                         'x' = '0',
                         'N/a' = '0',
                         'NA' = '0',
                         '-' = '0',
                         '--' = '0',
                         '\\*' = '0',
                         'N/A' = '0',
                         '6-8'='7',
                         'trace' = '1',
                         '3 to 4' = '3',
                         'Few' = '1',
                         'few' = '1',
                         'Some' = '3',
                         'some' = '3',
                         'Less than 1' = '2',
                         'Substantial' = '30','substantial' = '30',
                         'One' = '1', 'two' = '2',
                         'No' = '0', 'Nominal' = '3', '0minal' = '3',
                         '50 of 1' = '1',
                         'On Pershing Avenue only.' = '2',
                         'Small' = '3')
 
template = HOLC.dd %>% ungroup() %>% dplyr::select(Black_YN, Black_PCT) %>% distinct() %>% 
  dplyr::mutate(NBlack_YN = Black_YN, NBlack_PCT = gsub('\\%|\\+|\\$','',Black_PCT)) %>%
  dplyr::mutate(NBlack_PCT = str_replace_all(string = NBlack_PCT, pattern = First_Black_PCT_dictionary)) %>%
  dplyr::mutate(NBlack_PCT = sapply(strsplit(split = '-', x = .$NBlack_PCT), '[', 1)) %>%
  dplyr::mutate(NBlack_PCT = str_replace_all(string = NBlack_PCT, pattern = Black_PCT_dictionary)) %>%
  dplyr::mutate(NBlack_PCT = str_extract(NBlack_PCT, '\\d+')) %>%
  dplyr::mutate(NBlack_YN = ifelse(grepl(pattern = 'Yes|^yes|light|Several|10|few|Few|Fwe|fwe|fam\\.|^37|^1|^85|^2|Nom|East|30|10|amil|cattered|^Along|^Negro|^One|^one|Many|^Some|^3 or', x = .$NBlack_YN), 'TRUE', .$NBlack_YN)) %>%
  dplyr::mutate(NBlack_YN = ifelse(grepl(pattern = '^No|^NO|^no|hreat|restricted|-|\\*|^0|\\$0|^See |^Threaten', x = .$NBlack_YN), 'FALSE', .$NBlack_YN)) %>%
  dplyr::mutate(NBlack_YN = ifelse(NBlack_YN=='', 'FALSE',NBlack_YN)) %>%
  dplyr::mutate(NBlack_YN = as.logical(NBlack_YN)) %>%
  dplyr::mutate(NBlack_match = ifelse(is.na(NBlack_YN) & is.na(NBlack_PCT), FALSE, TRUE)) %>%
  dplyr::mutate(NBlack_PCT = as.numeric(NBlack_PCT)) %>%
  dplyr::mutate(NBlack_PCT = ifelse(NBlack_PCT>100, NBlack_PCT/10, NBlack_PCT))

HOLC.dd = merge(HOLC.dd, y = template, by = c('Black_YN','Black_PCT'), all.x=T, all.y=F)[is.na(NBlack_match),NBlack_match:=FALSE]



#########################################
## Rent --> RentXX_Mean  (ok, accidentally coded it template.fam, but it's actually rent.)
## 
template.rent = tibble(Rent = unique(c(as.character(HOLC.dd$Rent29),
                                          as.character(HOLC.dd$Rent35),
                                          as.character(HOLC.dd$Rent3739)))) %>%
  dplyr::mutate(Rent_original = Rent) %>%
  dplyr::mutate(Rent = str_replace(string = Rent, pattern ='\\${1}',replacement = '')) %>%
  dplyr::mutate(Rent = str_replace(string = Rent, pattern = '\\s?1\\\2|\\s?1/2', replacement = '.5')) %>%
  dplyr::mutate(Rent = ifelse(str_detect(Rent, '1929|1932|1935|1939|1937|100%|p\\.a'), NA, Rent)) %>%
  tidyr::extract(Rent,
                 c('Rent_lower','Rent_upper'),
                 regex = '(\\d{1,3}\\.?\\d{0,2})\\D*(\\d{0,3}\\.?\\d{0,2})', remove = F) %>%
  dplyr::mutate_at(vars(Rent_lower, Rent_upper), as.numeric) %>%
  dplyr::mutate_at(vars(Rent_lower, Rent_upper), ~ifelse(.x >=1000, .x/12, .x)) %>%
  dplyr::mutate(Rent_Mean = rowMeans(cbind(Rent_lower, Rent_upper), na.rm=T)) %>%
  dplyr::mutate(Rent_match = ifelse(!is.na(Rent_Mean),  TRUE, FALSE))

# 
# 
# template.fam = unique(as.character(HOLC.dd$Rent35))
# template.fam = data.frame(Rent = unique(c(template.fam, unique(as.character(HOLC.dd$Rent3739)))))
# 
# template.fam$IV1 = F
# template.fam$IV1[grepl('room|unheated|Unheated|;|per side|foreclosed|9000.00|9/16/2016|^N$|^S$|^S S$|Practically|rms|Less than|substantiate|area|Heated|^rent$|^rentals$|Static', x=template.fam$Rent)|is.na(template.fam$Rent)] = T
# 
# template.fam$NR = F
# template.fam$NR[grepl('^\\*$|no|No|Few|few|Owner|owner|None|Undeveloped|N/A|n/a|--|^-$', x=template.fam$Rent)] = T
# 
# template.fam.use = template.fam[!template.fam$IV1 & !template.fam$NR,]
# template.fam.other = template.fam[template.fam$IV1|template.fam$NR,]
#   template.fam.other$lower = NA
#   template.fam.other$upper = NA
#   template.fam.other$mean = NA
# 
# template.fam.use$lower = gsub(pattern='\\$|\\*|\\%|\\&|up|Up| and|\\(a\\)', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 1))
# template.fam.use$lower = gsub(pattern=' 1/2', '.5', template.fam.use$lower)
# template.fam.use$lower = as.numeric(template.fam.use$lower)
# template.fam.use$lower[which(template.fam.use$lower>=1000)] = template.fam.use$lower[which(template.fam.use$lower>=1000)]/100
# 
# template.fam.use$upper = gsub(pattern='\\$|\\*|\\%|\\&|up', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 2))
# template.fam.use$upper = gsub(pattern=' 1/2', '.5', template.fam.use$upper)
# template.fam.use$upper = sapply(strsplit(trimws(as.character(template.fam.use$upper)), ' |,'), '[', 1)  #--> if the upper range has add'l notes, this drops 'em.
# template.fam.use$upper = gsub(pattern='5.o0', '5.00', template.fam.use$upper)
# template.fam.use$upper = as.numeric(template.fam.use$upper)
# template.fam.use$upper[which(!is.na(template.fam.use$upper) & template.fam.use$upper>=1000)] = template.fam.use$upper[which(!is.na(template.fam.use$upper) & template.fam.use$upper>=1000)]/100
# 
# template.fam.use$upper[which(is.na(template.fam.use$upper))] = template.fam.use$lower[which(is.na(template.fam.use$upper))]  #--> if no upper, use lower for upper.
# template.fam.use$mean = (template.fam.use$lower+template.fam.use$upper)/2
# 
# template.fam = rbind(template.fam.use, template.fam.other)
# names(template.fam) = c('Rent','IV1','NR','Rent_Lower','Rent_Upper','Rent_Mean')
# template.rent = template.fam
#   # write.csv(template.rent, file=file.path(WORK.OUT, 'Rent_crosswalk_v1.csv'), row.names=F) 
#   # template.rent = read_csv(file=file.path("Z:/user/ajk41/Solar Incentive NBER/Data/OUTPUT/2017-09-27/Rent_crosswalk_v1.csv"))
# template.rent = template.rent %>% dplyr::select(Rent, NR,Rent_Mean, Rent_Lower) %>%
#   dplyr::mutate(Rent_match = TRUE)


# Merge 29, 35 and 3739 rent:
HOLC.dd = HOLC.dd %>%
  left_join(template.rent %>% dplyr::select(Rent29 = Rent_original, Rent_Mean_29 = Rent_Mean), by = c('Rent29')) %>%
  left_join(template.rent %>% dplyr::select(Rent35 = Rent_original, Rent_Mean_35 = Rent_Mean), by = c('Rent35')) %>%
  left_join(template.rent %>% dplyr::select(Rent3739 = Rent_original, Rent_Mean_3739 = Rent_Mean), by = c('Rent3739'))




## Family Income ##
## Not all have FamIncome.
template.fam = tibble(inc = unique(HOLC.dd$FamIncome)) %>%
  dplyr::mutate(FamIncome_original = inc) %>%
  dplyr::mutate(inc = gsub(x = inc, pattern = '\\s?M', ',000')) %>%
  dplyr::mutate(inc = gsub(x = inc, pattern = ',', replace = '')) %>%
  tidyr::extract(inc, c('inc_lower','inc_upper'), regex = '(\\d{2,})\\D+(\\d*\\.?\\d*)', remove = T, convert = T) %>%
  dplyr::mutate(inc_lower = ifelse(inc_lower<100 & inc_upper >1000, inc_lower*10, inc_lower)) %>%
  dplyr::mutate(inc_lower = ifelse(inc_lower<500, NA, inc_lower)) %>%
  dplyr::mutate(Minc = rowMeans(cbind(as.numeric(inc_lower), as.numeric(inc_upper)), na.rm=T)) %>%
  setDT()

# Merge into HOLC.dd
HOLC.dd = merge(x=HOLC.dd, y=unique(template.fam[,.(Minc, FamIncome = FamIncome_original)]), by = 'FamIncome', all.x=T, all.y=F) 





## Repair Quality
# Only 54 are missing
template.rep = data.frame(rp = as.character(unique(HOLC.dd$Repair_Predom)), stringsAsFactors=F)

excellent = grepl("xcellent", template.rep$rp)
good = grepl("Good|good|God|god|pride of ownership", template.rep$rp)
fair = grepl("Fair|fair|soon fail|Fari|Spotty|settle|minor repairs|Fine|fine|Medium", template.rep$rp)
poor = grepl("Poor|poor|errible|need of repair|dilapadated|major repair|bad|Bad", template.rep$rp)

template.rep$repair_class = "Unk"
template.rep$repair_class[excellent] = 'Excellent'
template.rep$repair_class[good] = 'Good'
template.rep$repair_class[fair] = 'Fair'
template.rep$repair_class[poor] = 'Poor'

template.rep$Repair_match = TRUE

HOLC.dd = merge(x=HOLC.dd, y=template.rep, by.x='Repair_Predom', by.y='rp', all.x=T, all.y=F) %>% 
  replace_na(list(Repair_match = FALSE)) 
rm(template.rep)



## Relief families
# 1,094 missing :(
# don't do yet.
# 
# 
# 
## Average Home Age (New April 2021)
HOLC.dd = HOLC.dd %>%
  dplyr::mutate(AverageHomeAge_orig = AverageHomeAge) %>%
  tidyr::extract(AverageHomeAge_orig, c('age_low','age_high'),
                 regex = '(\\d+)\\D+(\\d*)', convert = T) %>%
  dplyr::mutate(age_low =  ifelse(age_low>200, NA, age_low),
                age_high = ifelse(age_high>200, NA, age_high)) %>%
  dplyr::mutate(AverageHomeAge = rowMeans(cbind(age_low, age_high), na.rm = T))



#--------------------
#####
## Merge HOLC.dd to HOLC (spatial) 
## Subset out BG, and then allocate HOLC data spatially; aggregate to BG.
#####
HOLC.dd$ExtraData = as.logical(T)
HOLC = merge(x=HOLC, y=HOLC.dd[,.(polygon_id, NBlack_YN, NBlack_PCT, Rent29_Mean = Rent_Mean_29, Rent35_Mean = Rent_Mean_35, Rent3739_Mean = Rent_Mean_3739,
                                  Minc, # Relief_YN,
                                  repair_class, AverageHomeAge, ExtraData)], by='polygon_id', all.x=T, all.y=F) %>% 
  replace_na(list(ExtraData = F))



######
## County-FIPS is already in TIGRIS cache (this thing is useful!)
## So can conjure it up with just counties() (insteadl of counties(state='XX',...))

##--> Get FIPS(county) that touch HOLC
FIPS = st_as_sf(counties()) %>% st_transform(st_crs(HOLC))
HOLC = HOLC %>% st_join(FIPS %>% dplyr::select(STATEFP, COUNTYFP), largest=T) %>%
  dplyr::mutate(STCO = paste0(STATEFP, COUNTYFP))

# Save in list below with BG ZCTA full.


####---- Get BG and ZCTA data and shapefiles to intersect----####

##--> Download data BY block group for each FIPS that touches (taking unique)  # ex: B25003_002 is "renter-occupied housing unit"; _001 is total housing units
# lv = load_variables(dataset = 'acs5', year=2018, cache=TRUE)
# lv %>% dplyr::filter(grepl('HEAT', concept, ignore.case = T))

##!! NOTE: acs package has ways of combining (correctly) the block.group levels. See the 2014 pdf at the end!!! Make a HOLC-nbhd defined collection of BG's and combine that way, then query using acs.fetch instead of Tidycensus?


using.fips = unique(HOLC[,c('STATEFP','COUNTYFP','STCO')] %>% st_set_geometry(NULL) %>% dplyr::filter(!is.na(STATEFP)))


BG = map(split(using.fips, 1:NROW(using.fips)), function(z){
  sfp = as.character(z$STATEFP)
  scp = as.character(z$COUNTYFP)
  #if(sfp=='12' & scp=='086') return(NA)
    print(paste0(sfp, "-", scp))
    get_acs(state=sfp, county=scp, geography='block group', year=2018, 
                  variables = c(paste0('B25040_0',formatC(1:10, width=2, flag='0')),
                                paste0('B02001_0',formatC(1:10, width=2, flag='0')),
                                'B19013_001'), 
            output='wide', geometry=T, keep_geo_vars = T) %>%  # , summary_var = 'B25040_001'
      dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE, AFFGEOID, NAME.x, B25040_001E:B19013_001E) %>%
      st_transform(st_crs(HOLC))
})

BG = do.call(rbind, BG)


##--> get census zip
ZCTA =  get_acs(geography='zcta', state = NULL, county = NULL, year = 2019, # 2018 no longer pulls ZCTAs?
        variables = c(paste0('B25040_0',formatC(1:10, width=2, flag='0')),
                      paste0('B02001_0',formatC(1:10, width=2, flag='0')),
                      'B19013_001'), 
        output='wide', geometry=T, keep_geo_vars = T) %>%  # , summary_var = 'B25040_001'
  dplyr::select(GEOID, AFFGEOID10, NAME.zip = NAME, B25040_001E:B19013_001E) %>%
  st_transform(st_crs(HOLC)) %>%
  st_join(counties(cb = TRUE) %>% dplyr::mutate(STCO = paste0(STATEFP, COUNTYFP)) %>% dplyr::select(STCO) %>% st_transform(st_crs(ZCTA)), largest = T)



varDictionary = c('B25040_001E' = 'TotalFuel',
                  'B25040_002E' = 'UtilityGas',
                  'B25040_003E' = 'LPGas',
                  'B25040_004E' = 'Electricity',
                  'B25040_005E' = 'FuelOil',
                  'B25040_006E' = 'Coal',
                  'B25040_007E' = 'Wood',
                  'B25040_008E' = 'Solar',
                  'B25040_009E' = 'OtherFuel',
                  'B25040_010E' = 'NoFuel',
                  'B02001_001E' = 'TotalRace',
                  'B02001_002E' = 'White',
                  'B02001_003E' = 'Black',
                  'B02001_004E' = 'NativeAmerican',
                  'B02001_005E' = 'Asian',
                  'B02001_006E' = 'NativeHawaiian',
                  'B02001_007E' = 'OtherRaceOne',
                  'B02001_008E' = 'TwoOrMore',
                  'B02001_009E' = 'TwoOrMoreOther',
                  'B02001_010E' = 'ThreeOrMore',
                  'B19013_001E' = 'MedIncome2018')

names(BG) = str_replace_all(string = names(BG), pattern = varDictionary)
names(ZCTA) =  str_replace_all(string = names(ZCTA), pattern = varDictionary)


BG = BG %>% dplyr::select(-contains(c('B250','B020','B190'))) %>%
  dplyr::mutate_at(.vars = vars(UtilityGas:NoFuel), ~ .x/TotalFuel ) %>%
  dplyr::mutate_at(.vars = vars(White:ThreeOrMore), ~ .x/TotalRace) 


ZCTA = ZCTA %>% dplyr::select(-contains(c('B250','B020','B190'))) %>% dplyr::filter(TotalFuel>0) %>%
  dplyr::mutate_at(.vars = vars(UtilityGas:NoFuel), ~ .x/TotalFuel ) %>% 
  dplyr::mutate_at(.vars = vars(White:ThreeOrMore), ~ .x/TotalRace) 


BG = BG %>%
  dplyr::mutate(OtherRace = NativeAmerican+NativeHawaiian+OtherRaceOne+TwoOrMore+TwoOrMoreOther+ThreeOrMore) %>%
  dplyr::mutate(OtherSubstandard = Coal + Wood + LPGas + OtherFuel + NoFuel,
                OtherSubstandardNarrow = Coal + NoFuel,
                OtherSubstandardMed = Coal + LPGas + FuelOil + OtherFuel + NoFuel,
                OtherSubstandardWide = Coal + Wood + OtherFuel + LPGas + FuelOil + NoFuel) %>%
  dplyr::select(-Coal, -Wood, -OtherFuel, -NoFuel, -NativeAmerican, -NativeHawaiian, -OtherRaceOne, -TwoOrMore, -TwoOrMoreOther, -ThreeOrMore) %>%
  dplyr::select(STATEFP:NAME.x, UtilityGas:Solar, OtherSubstandard:OtherSubstandardWide, White:Asian, OtherRace, MedIncome2018, TotalFuel, TotalRace)


ZCTA = ZCTA %>%
  dplyr::mutate(OtherRace = NativeAmerican+NativeHawaiian+OtherRaceOne+TwoOrMore+TwoOrMoreOther+ThreeOrMore) %>%
  dplyr::mutate(OtherSubstandard = Coal + Wood + LPGas + OtherFuel + NoFuel,
                OtherSubstandardNarrow = Coal + NoFuel,
                OtherSubstandardMed = Coal + LPGas + FuelOil + OtherFuel + NoFuel,
                OtherSubstandardWide = Coal + Wood + OtherFuel + LPGas + FuelOil + NoFuel) %>%
  dplyr::select(-Coal, -Wood, -OtherFuel, -NoFuel, -NativeAmerican, -NativeHawaiian, -OtherRaceOne, -TwoOrMore, -TwoOrMoreOther, -ThreeOrMore) %>%
  dplyr::select(GEOID:NAME.zip, STCO, UtilityGas:Solar, OtherSubstandard:OtherSubstandardWide, White:Asian, OtherRace, MedIncome2018, TotalFuel, TotalRace)





##--> Get list of BG rows that also touch HOLC
using.BG.index = unlist(lapply(st_intersects(BG, HOLC), function(z) NROW(z)>=1))  #--> gets an index of all the BG's that have a HOLC over them
BG = BG[using.BG.index,] 
BG = BG %>%
  dplyr::mutate(BG_AREA = as.numeric(st_area(BG)))

using.ZCTA.index = unlist(lapply(st_intersects(ZCTA, HOLC, sparse = TRUE), function(z) NROW(z)>=1))  #--> gets an index of all the BG's that have a HOLC over them
ZCTA = ZCTA[using.ZCTA.index,] 
ZCTA = ZCTA %>%
  dplyr::mutate(ZCTA_AREA = as.numeric(st_area(ZCTA)))


##--> Separate BG and attach to HOLC
# BG.int = st_intersection(BG, HOLC %>% group_by(GRADE) %>% dplyr::summarize(.)) %>% 
#           dplyr::mutate(shareArea = as.numeric(st_area(.))/BG_AREA,
#                        Maj = shareArea>.80) %>%
#           dplyr::arrange(AFFGEOID, GRADE)

# ZCTA.int = st_intersection(ZCTA, HOLC %>% group_by(GRADE) %>% dplyr::summarize(.)) %>% 
#   dplyr::mutate(shareArea = as.numeric(st_area(.))/ZCTA_AREA,
#                 Maj = shareArea>.80) %>%
#   dplyr::arrange(AFFGEOID10, GRADE)

US = states(cb=TRUE) %>%
  dplyr::filter(STUSPS!='AK') %>%
  st_union() %>%
  st_transform(st_crs(HOLC))

rest.of.US = st_difference(US, HOLC %>% st_union()) %>%
  st_sf() %>% # this lets me treat it as having data attached (without it, just st_difference, wasn't tibble)
  dplyr::mutate(polygon_id = NA)

HOLC.augmented = bind_rows(list(HOLC, rest.of.US)) %>%
  dplyr::select(-STATEFP, -COUNTYFP, -STCO)


BG.int.full = st_intersection(BG, HOLC.augmented) %>% 
  dplyr::mutate(segmentArea = as.numeric(st_area(.))) %>%
  dplyr::arrange(AFFGEOID, GRADE)

ZCTA.int.full = st_intersection(ZCTA, HOLC.augmented) %>%  #--> should this have an outside-of-HOLC polygon so that intersection captures zip's on the edge?
  dplyr::mutate(segmentArea = as.numeric(st_area(.))) %>%
  dplyr::arrange(AFFGEOID10, GRADE)


saveRDS(list(BG.int.full = BG.int.full, ZCTA.int.full = ZCTA.int.full, HOLC.int.full = HOLC), file.path(WORK.OUT, paste0('BG_ZCTA_HOLC_int----',Sys.Date(),'.rds')))



### End here. Moved analysis to Analysis file.
