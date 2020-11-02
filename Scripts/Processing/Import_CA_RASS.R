

###################
### Import CA RASS
###################





# Initial: only 2009 RASS (2019 coming soon)
# Data sent by Glen Sharp, CA PUC to Duke email in 2017
    # shared with Yixuan Gao, student MSU AFRE
# Glen will send 2019 data when ready (reminded 10-5-2020)



#### Set packages ####
require(tidyverse)
require(tidycensus)
require(tigris)
require(haven)
require(readr)
require(readxl)
require(skimr)
require(broom)
require(Mapview)
options(tigris_use_cache = TRUE)

# 
# mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
# census_api_key(mykey, install=T)


WORK.OUT = file.path(BASE, 'ajk41','Low Income Energy','OUTPUT',paste0('OUTPUT_',Sys.Date()))
dir.create(WORK.OUT)


PLOT.OUT = file.path(BASE, 'ajk41','Low Income Energy','Scripts','Plots and Outputs')
dir.create(PLOT.OUT)


DATA.IN = file.path(BASE, 'ajk41','Low Income Energy','Data','Found Data')


# 
# HOLC = readOGR(dsn=file.path(DATA.IN,'HOLC Maps','holc_polygons'), layer='holc_polygons', stringsAsFactors=F)
# HOLC = st_as_sf(HOLC) 
# 
# HOLC$GRADE = toupper(as.character(HOLC$HOLC_Grade)) #--> HOLC letter has some odd entries. In LA, there's an I-8 on the east side of town. But grade of that is D, so HOLC$HOLC_grade is right.
# HOLC$GRADE[which(HOLC$GRADE=="E")] = "D"
# HOLC$ID = HOLC$HOLC_ID
# HOLC$polygon_idu = paste0(HOLC$polygon_id, '-', HOLC$ID)
# HOLC = HOLC[,c('map_id','polygon_id','polygon_idu','ID','GRADE','name')] # map-polygon is unique; sheets is # of sheets that comprise a city, I think
# color.map =  c("A" = "green", "B" = "blue", "C" = "yellow", "D" = "red")
# 
# 
CAzip = st_as_sf(readRDS(file.path(GIS.files, "USA_Zip_Code_Boundaries_v104_zip_poly.rds"))) %>%
  dplyr::select(zip = czip, NAME, STATE, POPULATION, POP_SQMI) %>% dplyr::filter(STATE=='CA') %>%
  st_transform(st_crs(HOLC))
# 
# 


###---> BG.int, ZCTA.int and HOLC all processed in Importing_HOLC_v4.R

BGZ = readRDS(jLoad(WORK.OUT, 'BG_ZCTA_HOLC_int'))
BG.int.full = BGZ[[1]]
ZCTA.int.full = BGZ[[2]]
HOLC = BGZ[[3]] # readRDS(jLoad(WORK.OUT, 'HOLC_with_data')) # 10-12



#### Import RASS data ####
RASS09e = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','ddn_electricbillingdatamodels.csv'), na.strings = c('99')) #?97
RASS09g = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','ddn_gasbillingdatamodels.csv'), na.strings = c('99'))
RASS09s = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','Survdata.csv'), na.strings = c('99'))

#### Survey data (one obs. per household)
s09 = RASS09s[,.(IDENT, servzip, avginc, homeage, EUTIL, NGUTIL, CZT24, NAC_KWH, NAC_Therms, res, rescnt, kids, adults, seniors, NGSERV, OWNRENT, YRS_RES, BUILTYR, SQFT, PAYHEAT, PHTNGCNT, PHTNGFWL, PHTNGRAD, PHTNGFP, PHTNGOTH, PHTELBSB, PHTELCRH, PHTELCHP, PHTELWHP, PHTELPOR, PHTELOTH, PHTBGCNT)][,zip:=sprintf('%05d', servzip)]



CAzip = CAzip %>% left_join(s09 %>% dplyr::filter(!is.na(servzip)) %>% group_by(zip) %>% summarize(countRASS = n()), by='zip')
# predannuse is the NAC-predicted (modeled) annualized consumption in a typical year
# annuse is the annualized consumption observed. Methods aren't clear, but I think this is use, but normalized to 365 days.
# predannuse is a nice, quick way of getting at electricity consumption, but r1...r21, d1...d21 might have more insight
  # Household-by-household model of HDD response (CDD, but not as important; heating is primary, cooling is gravy).
  # Do households of equal income but in different colored areas (red/yellow) tend to have more consumption per HDD. If so, is it in low income when these are most positive?
  #     Hard to disentangle "I like it warmer" from "my junk is inefficient"


CAzip.disagg  = st_intersection(CAzip %>% dplyr::mutate(ZIP_AREA = as.numeric(st_area(.))) %>% st_make_valid(), HOLC %>% st_make_valid()) %>% 
  mutate(INT_AREA = as.numeric(st_area(.)),
  SHARE    = INT_AREA/ZIP_AREA) %>% arrange(zip) %>%
  group_by(zip, NAME, STATE, POPULATION) %>%
  dplyr::summarize(UGRADE = NROW(unique(GRADE)),
                   GRADE = paste(unique(GRADE), collapse = ', '))





# ^^^ prior added to Analysis_v3spin.R 10-28-2020




### Attempt at household-level HDD response estimation:
e09.use = melt(RASS09e , id.vars = c('IDENT'), measure = patterns("^r","^d","^u","^hdd[0-9]+","^cdd[0-9]+"), value.name = c('r','d','u','hdd','cdd')) %>%
  dplyr::filter( !is.na(r) & !is.na(d) & !is.na(u)) %>%
  dplyr::mutate(r = mdy(r)) %>%
  dplyr::arrange(IDENT, variable, r) %>%
  group_by(IDENT) %>%
  nest()

e09.use$mod = lapply(e09.use$data, function(x){
 y = try(lm(u~hdd, data=x[x$hdd>0,]))
 if('try-error'%in%class(y)) return(NA)
 return(broom::tidy(y))})


e09.use$hddcoef = unlist(lapply(e09.use$mod, function(y) {
  if(is.na(y)) return(NA)
  as.numeric(y[2,'estimate'])}))

