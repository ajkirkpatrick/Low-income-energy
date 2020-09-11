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
## Loads up 1990, 2000 census data on home heating fuel
## Loads up 1980 census data from NHGIS extract (only for CDP/SMA/MCD or something like that)
## And (attempts) to merge 1980 data.




#### Part I ####
.libPaths('Z:/user/ajk41/jlib')
# library(devtools)
# dev_mode(TRUE)
# install_github("hadley/ggplot2", lib='~/R-dev')
# install_github("r-spatial/sf")
# dev_mode(FALSE)

require(plyr)
require(tidyverse)
require(sf)
library(viridis)
library(rvest)
require(tidycensus)
require(sp)
require(rgdal)
require(raster)
require(tigris)
options(tigris_use_cache = TRUE)
library("ggplot2", lib.loc="~/R-dev")
# require(ggplot2)
# tigris cache, for future ref (can add .zip files manually! folder is hidden, type in address using 'save as...') C:\Users\ajk41\AppData\Local\tigris
# http://old.socialexplorer.com/pub/reportdata/MetaBrowser.aspx?survey=ACS2015_5yr&ds=ACS15_5yr&header=True
# https://api.census.gov/data/2000/sf3/variables.html (navigate with this; add ".html" to anything JSON-like

if(grepl('EI',Sys.info()["nodename"] )) 
{BASE = file.path('Z:/user/') } else {
  BASE = file.path('Volumes/ei-edl01/user/')
}


WORK.OUT = file.path(BASE,'ajk41/Solar Incentive NBER/Data/OUTPUT',Sys.Date())
dir.create(WORK.OUT, recursive=T)

GIS.files = file.path(BASE,'ajk41/GIS files')
mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
census_api_key(mykey, install=T)





#######
## Read in HOLC
#######
DATA.IN = file.path("C:\\Users\\ajk41\\Box Sync\\Home Folder ajk41\\BoxSync\\Projects\\Low income and Energy\\Data search\\Found Data")
HOLC = readOGR(dsn=file.path(DATA.IN,'HOLC Maps','holc_polygons'), layer='holc_polygons', stringsAsFactors=F)
#--> send to FusionTable for EarthEngine:  writeOGR(spTransform(HOLC, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")), dsn='/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/FusionTableOut/HOLC.kml', layer="HOLC", driver="KML", overwrite_layer=TRUE)
HOLC = st_as_sf(HOLC)

HOLC$GRADE = toupper(as.character(HOLC$HOLC_Grade)) #--> HOLC letter has some odd entries. In LA, there's an I-8 on the east side of town. But grade of that is D, so HOLC$HOLC_grade is right.
HOLC$GRADE[which(HOLC$GRADE=="E")] = "D"
HOLC$ID = HOLC$HOLC_ID
HOLC = HOLC[,c('map_id','polygon_id','ID','GRADE','name')] # map-polygon is unique; sheets is # of sheets that comprise a city, I think

######
## Read in US
######
US = st_as_sf(readOGR(file.path(GIS.files, "states_21basic"), 'states', stringsAsFactors=F))
US = US[which(!US$STATE_NAME%in%c('Alaska','Hawaii')),]
US$STATE_NAME = as.factor(as.character(US$STATE_NAME))
US = st_transform(US, st_crs(HOLC))
plot(US[,'STATE_NAME'])

## Get state for HOLC ##
HOLC[,c('STATE_ABBR','STATE_FIPS')] = US[sapply(st_intersects(HOLC, US), '[', 1), c('STATE_ABBR','STATE_FIPS'), drop=T] #--> ahh, drop=T here drops the sfc/geom column.
# temp = aggregate(HOLC, by=list(HOLC$GRADE), do_union=T, function(z)  unique(z))








######
## HOLC.d, URichmond's digitized data from surveyors.
## Read in add'l info and merge to polygon_id (new 9-25-2017)
HOLC.d = read_csv(file=file.path('C:/Users/ajk41/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Data search/Found Data/HOLC Maps/holc_ad_data.csv')) %>%
          arrange(polygon_id, cat_id, sub_cat_id, `_order`)

# Fix an error
HOLC.d[is.na(HOLC.d$polygon_id),'polygon_id'] = 4956

## Need to determine which AD form (1, 2/3, 4) is implied by cat_id, sub_cat_id, `_order`
## Note that cat_id and `_order` are numeric; sub_cat_id is not. Try to use only max(cat_id) and min(_order)
jCategorize <- function(ci){
  max.ci = ifelse(sum(is.na(ci))==length(ci), NA, max(ci, na.rm=T))
  if(is.na(max.ci)) return(NA)
  ifelse(max.ci==6, 1,
    ifelse(max.ci==9|max.ci==10, 4, 
       ifelse(max.ci==15, 2, 
          NA)))
}

jSH <- function(polyd, ci, sci=NA, or=NA){
  # if(NROW(unique(polyd[,'polygon_id']))>1) stop('invalid input: only one polygon_id at a time!')
  res = polyd[which(polyd$cat_id==ci),]
  if(!is.na(sci)) res = res[which(res$sub_cat_id==sci),]
  if(!is.na(or))  res = res[which(res$`_order`==or),]
  if(NROW(res)==0) return(NA)
  return(res$data)
}

jExtract.HOLC <-function(da){
  ddply(.data = da,.variables='polygon_id', function(zz){
    fc = zz$form_category[1]
    if(fc==1){
      return(unique(data.frame(
        form_category = fc,
        Report_GRADE = jSH(zz,6,NA,2),
        Black_YN =     jSH(zz,2,'d',1),
        Black_PCT =    jSH(zz,2,'d',2),
        Foreign_PCT =  jSH(zz,2,'c',1),
        Relief_YN =    jSH(zz,2,'f',NA),
        FamIncome =    jSH(zz,2,'b',NA),
        OOH_PCT   =    jSH(zz,3,'f',1),
        OCC_PCT   =    jSH(zz,3,'e',1),
        Rent35    =    jSH(zz,3,'m',1),
        Price35 =      jSH(zz,3,'i',1),
        Rent3739 =     jSH(zz,3,'o',2),
        Price3739=     jSH(zz,3,'j',2),
        Repair_Predom =jSH(zz,3,'d',1),
        Repair_Other = jSH(zz,3,'d',2),
        Construction_Predom = jSH(zz,3,'b',1),
        Construction_Other  = jSH(zz,3,'b',2),
        Avail_Mortgage = jSH(zz, 4, 'a',1),
        TaxRate = NA)))} #end fc==1
    
    if(fc==2|fc==3){ 
      if(sum(is.na(zz[which(zz$cat_id==7),'sub_cat_id']))>0){
        return(unique(data.frame(
          form_category = fc,
          Report_GRADE = jSH(zz,1,NA,2),
          Black_YN =     jSH(zz,5,'d',1),
          Black_PCT =    jSH(zz,5,'d',2),
          Foreign_PCT =  jSH(zz,5,'c',2),
          Relief_YN =    jSH(zz,5,'f',NA),
          FamIncome =    jSH(zz,5,'b',NA),
          OOH_PCT   =    jSH(zz,8,'c',1),
          OCC_PCT   =    jSH(zz,8,'b',1),
          Rent35    =    jSH(zz,7,NA,10),
          Price35 =      jSH(zz,7,NA,7 ),
          Rent3739 =     jSH(zz,7,NA,17),
          Price3739=     jSH(zz,7,NA,14),
          Repair_Predom =jSH(zz,6,'d',NA),
          Repair_Other = NA,
          Construction_Predom = jSH(zz,6,'b',NA),
          Construction_Other  = NA,
          Avail_Mortgage = jSH(zz, 12, 'a', NA),
          TaxRate = NA)))} else { # end fc==2|3 (a)
        return(unique(data.frame(
          form_category = fc,
          Report_GRADE = jSH(zz,1,NA,2),
          Black_YN =     jSH(zz,5,'d',1),
          Black_PCT =    jSH(zz,5,'d',2),
          Foreign_PCT =  jSH(zz,5,'c',2),
          Relief_YN =    jSH(zz,5,'f',NA),
          FamIncome =    jSH(zz,5,'b',NA),
          OOH_PCT   =    jSH(zz,8,'c',1),
          OCC_PCT   =    jSH(zz,8,'b',1),
          Rent35    =    jSH(zz,7, 2, 6), #
          Price35 =      jSH(zz,7, 2, 3), #
          Rent3739 =     jSH(zz,7, 3, 6), #
          Price3739=     jSH(zz,7, 3, 3), #
          Repair_Predom =jSH(zz,6,'d',NA),
          Repair_Other = NA,
          Construction_Predom = jSH(zz,6,'b',NA),
          Construction_Other  = NA,
          Avail_Mortgage = jSH(zz, 12, 'a', NA),
          TaxRate = NA)))} # end fc==2|3 (b)  
    }

    if(fc==4){    
      return(unique(data.frame(
        form_category = fc,
        Report_GRADE = jSH(zz,9,NA,2),
        Black_YN =     NA,
        Black_PCT =    jSH(zz,1,'d',1),
        Foreign_PCT =  jSH(zz,1,'c',1),
        Relief_YN =    NA,
        FamIncome =    NA,
        OOH_PCT   =    jSH(zz,2,'f',1),
        OCC_PCT   =    jSH(zz,2,'e',1),
        Rent35    =    jSH(zz,2,'l',1),
        Price35 =      jSH(zz,2,'g',1),
        Rent3739 =     jSH(zz,2,'n',2),
        Price3739=     jSH(zz,2,'i',2),
        Repair_Predom =jSH(zz,2,'d',1),
        Repair_Other = jSH(zz,2,'d',2),
        Construction_Predom = jSH(zz,2,'b',1),
        Construction_Other  = jSH(zz,2,'b',2),
        Avail_Mortgage = jSH(zz, 6,NA,NA),
        TaxRate = jSH(zz,7,NA,2))))} # end fc==4
  } # end ddply function
  ) # end ddply call
} # close jExtract.HOLC function



## Add best-guess at form category (1, 2(/3), 4)
## and extract the data using the form_cateogry mappings in jExtract.HOLC
HOLC.d = HOLC.d %>% group_by(polygon_id) %>% mutate(form_category = jCategorize(cat_id))
HOLC.dd = jExtract.HOLC(HOLC.d)
duplicated.polys = HOLC.dd$polygon_id[duplicated(HOLC.dd$polygon_id)]
duplicated.polys

# For now, drop duplicates. Later, will want to fix them up:
HOLC.dd = HOLC.dd %>% mutate(dups = duplicated(polygon_id)) %>% filter(dups==0)



#######
## Begin the proces of cleaning data...
#######


## Black_YN, Black_PCT --> NBlack_YN, NBlack_PCT
##### SKip to load, below 
# template = unique(HOLC.dd[,c('Black_YN','Black_PCT')])
# template$NBlack_YN = as.character(NA)
# template$NBlack_PCT = as.character(NA)
#   template = edit(template)
  # "nominal" = 2% (by 2-3 records with "nominal" and a percentage)
  # "small" = 5%
  # "few" = 1%
  # "scattered" = 2-5%
# write.csv(template, file=file.path(WORK.OUT, 'Black_Presence_v1_mapping.csv'), row.names=F)
template = read_csv(file.path("Z:/user/ajk41/Solar Incentive NBER/Data/OUTPUT/2017-09-27/Black_Presence_v1_mapping.csv"), na = c("NA"))
HOLC.dd = merge(x=HOLC.dd, y=template, by = c('Black_YN','Black_PCT'), all.x=T, all.y=F)




## Rent --> RentXX_Mean  (ok, accidentally coded it template.fam, but it's actually rent.)
template.fam = unique(as.character(HOLC.dd$Rent35))
template.fam = data.frame(Rent = unique(c(template.fam, unique(as.character(HOLC.dd$Rent3739)))))

template.fam$IV1 = F
template.fam$IV1[grepl('room|unheated|Unheated|;|per side|foreclosed|9000.00|9/16/2016|^N$|^S$|^S S$|Practically|rms|Less than|substantiate|area|Heated|^rent$|^rentals$|Static', x=template.fam$Rent)|is.na(template.fam$Rent)] = T

template.fam$NR = F
template.fam$NR[grepl('^\\*$|no|No|Few|few|Owner|owner|None|Undeveloped|N/A|n/a|--|^-$', x=template.fam$Rent)] = T

template.fam.use = template.fam[!template.fam$IV1 & !template.fam$NR,]
template.fam.other = template.fam[template.fam$IV1|template.fam$NR,]
  template.fam.other$lower = NA
  template.fam.other$upper = NA
  template.fam.other$mean = NA

template.fam.use$lower = gsub(pattern='\\$|\\*|\\%|\\&|up', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 1))
template.fam.use$lower = gsub(pattern=' 1/2', '.5', template.fam.use$lower)
template.fam.use$lower = as.numeric(template.fam.use$lower)
template.fam.use$lower[template.fam.use$lower>=1000] = template.fam.use$lower[template.fam.use$lower>=1000]/100

template.fam.use$upper = gsub(pattern='\\$|\\*|\\%|\\&|up', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 2))
template.fam.use$upper = gsub(pattern=' 1/2', '.5', template.fam.use$upper)
template.fam.use$upper = sapply(strsplit(trimws(as.character(template.fam.use$upper)), ' |,'), '[', 1)  #--> if the upper range has add'l notes, this drops 'em.
template.fam.use$upper = gsub(pattern='5.o0', '5.00', template.fam.use$upper)
template.fam.use$upper = as.numeric(template.fam.use$upper)
template.fam.use$upper[!is.na(template.fam.use$upper) & template.fam.use$upper>=1000] = template.fam.use$upper[!is.na(template.fam.use$upper) & template.fam.use$upper>=1000]/100

template.fam.use$upper[is.na(template.fam.use$upper)] = template.fam.use$lower[is.na(template.fam.use$upper)]  #--> if no upper, use lower for upper.
template.fam.use$mean = (template.fam.use$lower+template.fam.use$upper)/2

template.fam = rbind(template.fam.use, template.fam.other)
names(template.fam) = c('Rent','IV1','NR','Rent_Lower','Rent_Upper','Rent_Mean')
template.rent = template.fam
  # write.csv(template.rent, file=file.path(WORK.OUT, 'Rent_crosswalk_v1.csv'), row.names=F) 
  # template.rent = read_csv(file=file.path("Z:/user/ajk41/Solar Incentive NBER/Data/OUTPUT/2017-09-27/Rent_crosswalk_v1.csv"))
template.rent = template.rent %>% dplyr::select(Rent, NR,Rent_Mean, Rent_Lower)

# Merge 35 and 3739 rent:
HOLC.dd = merge(x = HOLC.dd, y=template.rent, by.x='Rent35', by.y='Rent', all.x=T, all.y=F)
HOLC.dd$Rent35_Mean = HOLC.dd$Rent_Mean
HOLC.dd$NR35 = HOLC.dd$NR
HOLC.dd = HOLC.dd %>% dplyr::select(-Rent_Mean, -Rent_Lower, -NR)

HOLC.dd = merge(x = HOLC.dd, y=template.rent, by.x='Rent3739', by.y='Rent', all.x=T, all.y=F)
HOLC.dd$Rent3739_Mean = HOLC.dd$Rent_Mean
HOLC.dd$NR3739 = HOLC.dd$NR
HOLC.dd = HOLC.dd %>% dplyr::select(-Rent_Mean, -Rent_Lower, -NR)



## Family Income ##
## Not all have FamIncome.
template.fam = data.frame(inc = unique(HOLC.dd[,c('FamIncome')]), stringsAsFactors=F)
template.fam$Ninc = template.fam$inc
template.fam$Ninc = gsub(pattern='2.5M', '2,500', as.character(template.fam$Ninc))
template.fam$Ninc = gsub(pattern='7.5 M', '7,500', as.character(template.fam$Ninc))
template.fam$Ninc = gsub('M', ',000', gsub(' M', 'M', template.fam$Ninc))
template.fam$Ninc[grepl('down|egro|^\\*$|N/A|Static|Factory |ntermediate', template.fam$Ninc)] = NA
template.fam$Ninc = gsub('Over|over|\\$|and up|\\& up$|\\& upward|and upward', '', template.fam$Ninc)
template.fam$Ninc = gsub(' average|per year|upward|up$|up.$|,|\\&|\\*', '', template.fam$Ninc)

template.fam$Linc = sapply(strsplit(template.fam$Ninc, ' *- *| to | t0 |--'), '[', 1)
template.fam$Linc = as.numeric(sapply(strsplit(trimws(template.fam$Linc), ' '), '[', 1))
template.fam$Hinc = sapply(strsplit(template.fam$Ninc, ' *- *| to | t0 |--'), '[', 2)
template.fam$Hinc = as.numeric(sapply(strsplit(template.fam$Hinc, ' '), '[', 1))

template.fam$Hinc[is.na(template.fam$Hinc)] = template.fam$Linc[is.na(template.fam$Hinc)]  # missing H? Replace it with L
template.fam$Linc[template.fam$Linc==0 & !is.na(template.fam$Linc)] = template.fam$Hinc[template.fam$Linc==0 & !is.na(template.fam$Linc)]  # if Linc is 0, replace with Hinc (some wer '0-1000')

for(i in 1:4){
  template.fam$Linc[!is.na(template.fam$Hinc) & template.fam$Linc<template.fam$Hinc/100] = template.fam$Linc[!is.na(template.fam$Hinc) & template.fam$Linc<template.fam$Hinc/100]*10
}

template.fam$Minc = (template.fam$Hinc + template.fam$Linc)/2  # no save.; Minc = Mean Income

# Merge into HOLC.dd
HOLC.dd = merge(x=HOLC.dd, y=template.fam[,c('inc','Minc')], by.x='FamIncome', by.y='inc', all.x=T, all.y=F)





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

HOLC.dd = merge(x=HOLC.dd, y=template.rep, by.x='Repair_Predom', by.y='rp', all.x=T, all.y=F) %>% arrange(polygon_id)
rm(template.rep)



## Relief families
# 1,094 missing :(
# don't do yet.



#--------------------
#####
## Merge HOLC.dd to HOLC (spatial) 
## Subset out BG, and then allocate HOLC data spatially; aggregate to BG.
#####
HOLC.dd$ExtraData = as.logical(T)
HOLC = merge(x=HOLC, y=HOLC.dd[,c('polygon_id','NBlack_YN','NBlack_PCT','Rent35_Mean','Rent3739_Mean','Minc','repair_class','ExtraData')], by='polygon_id', 
             all.x=T, all.y=F)

# WATTS.SG = c(7547, 7550) #polygon_id  # LA: map_id==16
# xx = HOLC %>% filter(map_id==16 & polygon_id %in% WATTS.SG)

xx = HOLC %>% filter(map_id==68) #--> 68 is MD, which has lots of info digitized.
ggplot(data=xx, aes(fill=GRADE)) + geom_sf()







######
## County-FIPS is already in TIGRIS cache (this thing is useful!)
## So can conjure it up with just counties() (insteadl of counties(state='XX',...))
# temp = aggregate(HOLC, by=list(HOLC$GRADE), do_union=T, function(z)  unique(z))
FIPS = st_as_sf(counties())
FIPS = st_transform(FIPS, st_crs(HOLC))


##--> Get FIPS(county) that touch HOLC
using.fips.index = unique(unlist(st_intersects(HOLC, FIPS)))
using.fips = FIPS[using.fips.index,]
using.fips = split(using.fips[,c('STATEFP','COUNTYFP')], f=1:NROW(using.fips))


##--> Download data BY block group for each FIPS that touches (taking unique)  # ex: B25003_002 is "renter-occupied housing unit"; _001 is total housing units

#--> OLD: using 2015 BG from tigris. Instead, use 1990 BG definitions (should be very similar)
# ttemp = lapply(using.fips, function(z){
#   print(paste0(z$STATEFP, '-', z$COUNTYFP))
#   get_acs(state=z$STATEFP, county=z$COUNTYFP, geography='block group', endyear=2015, variables = c('B25003_002'), summary_var = 'B25003_001', cb=F, keep_geo_vars = T, geometry=T)
# })

#--> Get BG here:


BG = map(using.fips, function(z){
  sfp = as.character(z$STATEFP)
  scp = as.character(z$COUNTYFP)
  if(sfp=='12' & scp=='086') return(NA)
    print(paste0(sfp, "-", scp))
    get_decennial(state=sfp, county=scp, geography='block group', year=1990, sumfile='sf3',
                  variables = c('P080A001',paste0('H030000',formatC(1:9, width=1, flag='0'))), output='wide', geometry=T, keep_geo_vars = T) %>% 
      dplyr::select(ST, CO, TRACT, BG, GEOID, NAME.x, P080A001, H0300001:H0300009, geometry)
})

BG = do.call(rbind, BG[!is.na(BG)])
BG = st_transform(BG, st_crs(HOLC))

##--> Plot just CT
# 
# BG %>%
#   filter(grepl(x=NAME, pattern='Connecticut')) %>%
#   ggplot(aes(fill = estimate/summary_est, color = estimate/summary_est)) +   #--> fill is fill, color is border (make same for no-border)
#   geom_sf() + 
#   scale_fill_viridis(option = "magma") + 
#   scale_color_viridis(option = "magma")


##--> Get list of BG rows that also touch HOLC
using.BG.index = unlist(lapply(st_intersects(BG, HOLC), function(z) NROW(z)>=1))  #--> gets an index of all the BG's that have a HOLC over them
BG = BG[using.BG.index,] 
BG = BG %>% mutate(BG_AREA       = as.numeric(st_area(.)),
                   summary_value = as.numeric(rowSums(.[,grep("H03", names(.)), drop=T]))) %>%
            mutate(YEAR          = 1990,
                   MedIncome1990 = P080A001,
                   Pct.gas       = H0300001/summary_value,
                   Pct.electric  = H0300003/summary_value,
                   Pct.other = (summary_value-H0300001-H0300003)/summary_value,
                   Pct.woodcoalker = (H0300002+H0300004+H0300005+H0300006)/summary_value,
                   Pct.none = H0300009/summary_value) %>% 
            arrange(GEOID) %>%
            dplyr::select(YEAR, ST, CO, TRACT, BG, GEOID, BG_AREA, MedIncome1990, Pct.gas:Pct.none, summary_value, geometry)


BG.int = st_intersection(BG, st_buffer(HOLC %>% group_by(GRADE) %>% summarize(.), dist=0)) %>% 
          mutate(shareArea = as.numeric(st_area(.)/BG_AREA),
                       Maj = shareArea>.80) %>%
          arrange(GEOID, GRADE)

BG.maj = BG.int %>% filter(shareArea>.80) 

ggplot(BG.int %>% filter(Maj==T &  ST=='24'), aes(col=GRADE)) + geom_sf()


# saved as temp_2.RData #
# save.image("Z:/user/ajk41/Low Income Energy/Low Income Energy Rproj/temp_2.RData")
# load("Z:/user/ajk41/Low Income Energy/Low Income Energy Rproj/temp_2.RData")

## Pull and compile relevant household heating fuel data ##
# 2015 ACS:  DP04_0062E is all occupied Hh; DP04_0063...71E are counts by type
#            Where 63 is gas; 65 is electricity; 71 is no fuel.
#    Damnit, 2015 ACS doesn't do block group on any of the energy fields.
#    https://api.census.gov/data/2000/sf3/variables.html (navigate with this; add ".html" to anything JSON-like)


BG.maj.expanded = st_intersection(BG.maj, HOLC %>% dplyr::select(-GRADE)) %>% mutate(INT_AREA = as.numeric(st_area(.)),
                                                                                     SHARE    = INT_AREA/BG_AREA) %>% arrange(GEOID)


insert.narm = F
BG.maj.collapsed = BG.maj.expanded %>% group_by(YEAR, ST, CO, TRACT, BG, GEOID, GRADE, map_id) %>%
                                        summarize(summary_value = mean(summary_value, na.rm=insert.narm),
                                                  Pct.gas = mean(Pct.gas, na.rm=insert.narm),
                                                  Pct.electric = mean(Pct.electric, na.rm=insert.narm),
                                                  Pct.other = mean(Pct.other, na.rm=insert.narm),
                                                  Pct.woodcoalker = mean(Pct.woodcoalker, na.rm=insert.narm),
                                                  Pct.none = mean(Pct.none, na.rm=insert.narm),
                                                  MedIncome1990 = mean(MedIncome1990, na.rm=insert.narm),
                                                  ID = paste(ID, collapse = ', '),
                                                  names = paste(name, collapse = ', '),
                                                  repair_class = paste(repair_class, collapse = ', '),
                                                  Rent35_Mean = sum(Rent35_Mean * (SHARE/sum(SHARE)), na.rm=insert.narm),
                                                  Rent3739_Mean = sum(Rent3739_Mean * (SHARE/sum(SHARE)), na.rm=insert.narm),
                                                  Minc = sum(Minc*(SHARE/sum(SHARE)), na.rm=insert.narm),
                                                  NBlack_PCT = sum(as.numeric(NBlack_PCT)*(SHARE/sum(SHARE)), na.rm=insert.narm)) %>%
                                      arrange(GEOID) %>% ungroup()

BG.maj.collapsed = BG.maj.collapsed %>% mutate(GRADEf = factor(GRADE, levels=c('C','D','B','A')))


robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}

#Woodcoalker = LP, wood, coal, kerosene.

# http://www.drewdimmery.com/robust-ses-in-r/
# use:
BGMC = BG.maj.collapsed %>% filter(!is.na(Minc) & !is.na(MedIncome1990) & !is.na(Pct.none) & Minc<30000)
res1 = robust.se(lm(Pct.none ~ GRADEf + as.factor(map_id), data=BGMC), BGMC$ST)[[2]]
res2 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]
res3 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) + poly(MedIncome1990, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]

stargazer(res1, res2, res3, dep.var.caption='Pct. no heating fuel', omit='map_id', omit.labels= c('City FE'), notes = 'Robust SE clustered by state')

# some ideas: just use wood+coal? justify no county fips FE? Need more rent data.

require(stargazer)
require(sandwich)
require(lmtest)
require(plm)

## Heartland
BGMC = BG.maj.collapsed %>% filter(!is.na(Minc) & !is.na(MedIncome1990) & !is.na(Pct.none) & Minc<30000) # there are some not-possible incomes in here.
res1 = robust.se(lm(Pct.none ~ GRADEf + as.factor(map_id), data=BGMC), BGMC$ST)[[2]]
res2 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]
res3 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) + poly(MedIncome1990, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]

stargazer(res1, res2, res3, dep.var.caption='Pct. no heating fuel', omit='map_id', omit.labels= c('City FE'), notes = 'Robust SE clustered by state')


saveRDS(BG.maj.collapsed, file=file.path(WORK.OUT, 'Heartland_Data_snapshot_10_2_2017.rds'))
boxplot(Minc ~ GRADE, BGMC) #BGMC is cut off at $30k.
 




 P080A001


summary(lm(Pct.other ~ GRADEf + Minc + Rent3739_Mean, data=BG.maj.collapsed))

summary(lm(Pct.none ~ GRADEf + Minc + as.factor(paste0(ST, CO)), data=BG.maj.collapsed))





## year 2000 Decennial Census 
ttemp2000 = reduce(map(using.fips, function(z){
  print(paste0(z$STATEFP, "-", z$COUNTYFP))
  get_decennial(state=z$STATEFP, county=z$COUNTYFP, geography='block group', year=2000, sumfile='sf3',
                variables = paste0('H0400',formatC(2:10, width=2, flag='0')), summary_var = 'H040001', output='wide', geometry=F)
                                            }), rbind) %>% 
                   mutate(YEAR = 2000,
                          Pct.gas = H040002/summary_value,
                          Pct.electric = H040004/summary_value,
                          Pct.other = (summary_value-H040002-H040004)/summary_value,
                          Pct.none = H040010/summary_value) %>%
                    dplyr::select(GEOID, YEAR, Pct.gas, Pct.electric, Pct.other, Pct.none) %>%
                    arrange(GEOID)

## year 1990 Decennial Census
ttemp1990 = reduce(map(using.fips, function(z){
  print(paste0(z$STATEFP, "-", z$COUNTYFP))
  get_decennial(state=z$STATEFP, county=z$COUNTYFP, geography='block group', year=1990, sumfile='sf3',
                variables = paste0('H030000',formatC(1:9, width=1, flag='0')), output='wide', geometry=F)
}), rbind)  %>% mutate(summary_value = rowSums(.[1:9])) %>%
  mutate(YEAR = 1990,
         Pct.gas = H0300001/summary_value,
         Pct.electric = H0300003/summary_value,
         Pct.other = (summary_value-H0300001-H0300003)/summary_value,
         Pct.none = H0300009/summary_value) %>%
  dplyr::select(GEOID, YEAR, Pct.gas, Pct.electric, Pct.other, Pct.none) %>%
  arrange(GEOID)

## year 1980 decennial census (with help from NHGIS)
ttemp1980 = read_csv(file=file.path('Z:/user/ajk41/Low Income Energy/Census Data/1980_NHGIS/nhgis0008_csv/nhgis0008_ds107_1980_blck_grp_01598.csv')) %>%
            mutate(GEOID = paste0(STATEA, COUNTYA, formatC(as.numeric(TRACTA), width=6, flag="0"), BLCK_GRPA)) %>%
            group_by(GEOID, YEAR) %>% summarize(DEWAA001 = sum(DEWAA001, DEWAB001, na.rm=T),   ##--> add together all of the tract's counts for each utility reported, and add in the "rural area" counts as well.
                                                DEWAA002 = sum(DEWAA002, DEWAB002, na.rm=T),  ###--> the multiple records per GEOID is a result of tracts that pass over CDA's, Places, etc.
                                                DEWAA003 = sum(DEWAA003, DEWAB003, na.rm=T),
                                                DEWAA004 = sum(DEWAA004, DEWAB004, na.rm=T),
                                                DEWAA005 = sum(DEWAA005, DEWAB005, na.rm=T),
                                                DEWAA006 = sum(DEWAA006, DEWAB006, na.rm=T),
                                                DEWAA007 = sum(DEWAA007, DEWAB007, na.rm=T),
                                                DEWAA008 = sum(DEWAA008, DEWAB008, na.rm=T)) %>% ungroup(.) %>%
                          mutate(summary_value = rowSums(.[grep("DEWAA", names(.))]),
                                 Pct.gas = DEWAA001/summary_value,
                                 Pct.electric = DEWAA003/summary_value,
                                 Pct.other = (summary_value-DEWAA001-DEWAA003)/summary_value,
                                 Pct.none = DEWAA008/summary_value) %>%
      dplyr::select(GEOID, YEAR, Pct.gas, Pct.electric, Pct.other, Pct.none) %>%
      arrange(GEOID)
            

BG.int = merge(x=BG.int, y=bind_rows(list(ttemp2000, ttemp1990, ttemp1980)), by = 'GEOID', all.x=T, all.y=F)
# summary(lm(Pct.none ~ as.factor(YEAR) + as.factor(GRADE) + as.factor(paste0(STATEFP, COUNTYFP)), data=BG.int %>% filter(Maj==T)))  #-> well, this is a good start.

## Taking just the "major" block-groups (those with >80% of overlap with a single GRADE)
BG.maj = BG.int %>% filter(Maj==T)

for(st in unique(STATES???))
st = 24

tBG = BG.maj %>% filter(STATEFP==st)
tHOLC = HOLC %>% filter(STATE_FIPS==st)






