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


#--> The 2020 polygons (8877)
# HOLC = st_read(dsn = file.path(DATA.IN, 'HOLC Maps','holc_polygons','fullshpfile','shapefile'), layer='holc_ad_data', stringsAsFactors=F) %>%
#   dplyr::rename(HOLC_Grade = holc_grade, ID = holc_id)

#--> The 2020 polygons in GEOjson (faster read)
HOLC = HOLCinit = geojson_sf(file.path(DATA.IN,  'HOLC Maps','holc_polygons','fullDownload10072020.geojson')) %>% # this one has more complete data in json in area_description_data (compared to shapefile's area_descr, also in json)
  dplyr::rename(ID = holc_id) %>%
  dplyr::mutate(GRADE = toupper(as.character(holc_grade))) %>%
  dplyr::mutate(GRADE = ifelse(GRADE=='E', 'D', GRADE)) %>%
  dplyr::select(state, city, polygon_id = neighborhood_id, name, ID, GRADE) %>% # area_description_data comes from sep. sheet (easier than parsing)
  st_make_valid()

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

cityform = fread('https://raw.githubusercontent.com/americanpanorama/HOLC_Area_Description_Data/master/Form_Schema/City_Form_ids.csv') # 10-08-2020 copy saved
cityform[form_id<=1|form_id==9675, form_id:=NA] # 0's are missing forms, 1's are descriptive forms, 9675 is ???

HOLC = HOLC %>% 
  left_join(cityform, by=c('city','state'))



######
## HOLC.d, URichmond's digitized data from surveyors.
# HOLC.d = read_csv(file=file.path(DATA.IN, 'HOLC Maps','holc_ad_data10082020.csv')) 

HOLC.d = fread('https://raw.githubusercontent.com/americanpanorama/HOLC_Area_Description_Data/master/holc_ad_data.csv')

HOLC.d = HOLC.d %>%
  dplyr::filter(polygon_id!='' & polygon_id!='4 & 5') %>%
  dplyr::mutate(polygon_id = ifelse(is.na(polygon_id), 4956, as.integer(polygon_id))) %>%
  dplyr::filter(!is.na(polygon_id)) %>%
  arrange(polygon_id, cat_id, sub_cat_id, order) %>%
  left_join(HOLC %>% st_set_geometry(NULL) %>% dplyr::select(polygon_id, form_id,city, state), by='polygon_id')



## Need to determine which AD form (1, 2/3, 4) is implied by cat_id, sub_cat_id, `_order`
## Note that cat_id and `_order` are numeric; sub_cat_id is not. Try to use only max(cat_id) and min(_order)
jCategorize <- function(ci){
  max.ci = ifelse(sum(is.na(ci))==length(ci), NA, max(ci, na.rm=T))
  if(is.na(max.ci)) return(as.integer(NA))
  return(ifelse(max.ci==6, 1,
    ifelse(max.ci==9|max.ci==10, 4, 
       ifelse(max.ci==15, 2, 
          as.integer(NA)))))
}

jSH <- function(polyd, ci, sci=NA, or=NA){
  # if(NROW(unique(polyd[,'polygon_id']))>1) stop('invalid input: only one polygon_id at a time!')
  res = polyd[which(polyd$cat_id==ci),]
  if(!is.na(sci)) res = res[which(res$sub_cat_id==sci),]
  if(!is.na(or))  res = res[which(res$order==or),]
  if(NROW(res)==0) return(NA)
  return(res$data)
}

jExtract.HOLC <-function(da){
  lapply(split(da, f=da$polygon_id), function(zz){ # 10-01-37 Form-1
    fc = zz$form_id[1]
    if(is.na(fc)) return(data.frame(form_category=as.numeric(NA)))
    
    if(fc==1|fc==19371001){
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
    
    if(fc==2|fc==3|fc==19370203|fc==19370826|fc==19370601|fc==6234766){ # I think 1937-06-01 is the same as 02-03; merge compared to categorize sure seems like 6234766 is form 2
      if(sum(is.na(zz[which(zz$cat_id==7),'sub_cat_id']))>0){ # 02-03-37 Form 2
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

    if(fc==4|fc==1939){     # probably form 4, but is that 1939? Left over 19370601 and 6234766
      return(unique(data.frame(
        form_category = fc,
        Report_GRADE = jSH(zz,9,NA,2),
        Black_YN =     NA,
        Black_PCT =    jSH(zz,1,'d',1), # (1, 'd', NA)
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
    
    return(data.frame(form_category=as.numeric(NA)))
  } # end ddply function
  ) # end lapply call
} # close jExtract.HOLC function


HOLC.dd = HOLC.d %>%
  group_by(polygon_id, city, state) %>%
  nest() 

HOLC.dd$dataExtract =  lapply(HOLC.dd$data, function(zz){ # 10-01-37 Form-1
  fc = zz$form_id[1]
  if(is.na(fc)) return(data.frame(form_category=as.numeric(NA)))
  
  if(fc==1|fc==19371001){
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
  
  if(fc==2|fc==3|fc==19370203|fc==19370826|fc==19370601|fc==6234766){ # I think 1937-06-01 is the same as 02-03; merge compared to categorize sure seems like 6234766 is form 2
    if(sum(is.na(zz[which(zz$cat_id==7),'sub_cat_id']))>0){ # 02-03-37 Form 2
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
  
  if(fc==4|fc==1939){     # probably form 4, but is that 1939? Left over 19370601 and 6234766
    return(unique(data.frame(
      form_category = fc,
      Report_GRADE = jSH(zz,9,NA,2),
      Black_YN =     NA,
      Black_PCT =    jSH(zz,1,'d',1), # (1, 'd', NA)
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
  
  return(data.frame(form_category=as.numeric(NA)))
            }) 

HOLC.dd = HOLC.dd %>%
  unnest(dataExtract) %>%
  dplyr::select(-data)

# duplicated.polys = HOLC.dd$polygon_id[duplicated(HOLC.dd$polygon_id)]
# duplicated.polys

# For now, drop duplicates. Later, will want to fix them up:
HOLC.dd = HOLC.dd %>% mutate(dups = duplicated(polygon_id)) %>% dplyr::filter(dups==0) %>% dplyr::select(-dups)



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
  dplyr::mutate(NBlack_match = T)

HOLC.dd = merge(x=HOLC.dd, y=template, by = c('Black_YN','Black_PCT'), all.x=T, all.y=F) %>%
  replace_na(list(NBlack_match = FALSE)) 

HOLC.dd %>%
  dplyr::filter(NBlack_match==F) %>% View()




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

template.fam.use$lower = gsub(pattern='\\$|\\*|\\%|\\&|up|Up| and|\\(a\\)', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 1))
template.fam.use$lower = gsub(pattern=' 1/2', '.5', template.fam.use$lower)
template.fam.use$lower = as.numeric(template.fam.use$lower)
template.fam.use$lower[which(template.fam.use$lower>=1000)] = template.fam.use$lower[which(template.fam.use$lower>=1000)]/100

template.fam.use$upper = gsub(pattern='\\$|\\*|\\%|\\&|up', '', sapply(strsplit(as.character(template.fam.use$Rent), split=' to|-'), '[', 2))
template.fam.use$upper = gsub(pattern=' 1/2', '.5', template.fam.use$upper)
template.fam.use$upper = sapply(strsplit(trimws(as.character(template.fam.use$upper)), ' |,'), '[', 1)  #--> if the upper range has add'l notes, this drops 'em.
template.fam.use$upper = gsub(pattern='5.o0', '5.00', template.fam.use$upper)
template.fam.use$upper = as.numeric(template.fam.use$upper)
template.fam.use$upper[which(!is.na(template.fam.use$upper) & template.fam.use$upper>=1000)] = template.fam.use$upper[which(!is.na(template.fam.use$upper) & template.fam.use$upper>=1000)]/100

template.fam.use$upper[which(is.na(template.fam.use$upper))] = template.fam.use$lower[which(is.na(template.fam.use$upper))]  #--> if no upper, use lower for upper.
template.fam.use$mean = (template.fam.use$lower+template.fam.use$upper)/2

template.fam = rbind(template.fam.use, template.fam.other)
names(template.fam) = c('Rent','IV1','NR','Rent_Lower','Rent_Upper','Rent_Mean')
template.rent = template.fam
  # write.csv(template.rent, file=file.path(WORK.OUT, 'Rent_crosswalk_v1.csv'), row.names=F) 
  # template.rent = read_csv(file=file.path("Z:/user/ajk41/Solar Incentive NBER/Data/OUTPUT/2017-09-27/Rent_crosswalk_v1.csv"))
template.rent = template.rent %>% dplyr::select(Rent, NR,Rent_Mean, Rent_Lower) %>%
  dplyr::mutate(Rent_match = TRUE)


# Merge 35 and 3739 rent:
HOLC.dd = merge(x = HOLC.dd, y=template.rent, by.x='Rent35', by.y='Rent', all.x=T, all.y=F) %>%
  replace_na(list(Rent_match = FALSE)) %>%
  dplyr::rename(Rent_match35 = Rent_match)

HOLC.dd$Rent35_Mean = HOLC.dd$Rent_Mean
HOLC.dd$NR35 = HOLC.dd$NR
HOLC.dd = HOLC.dd %>% dplyr::select(-Rent_Mean, -Rent_Lower, -NR)

HOLC.dd = merge(x = HOLC.dd, y=template.rent, by.x='Rent3739', by.y='Rent', all.x=T, all.y=F)%>%
  replace_na(list(Rent_match = FALSE)) %>%
  dplyr::rename(Rent_match3739 = Rent_match)

HOLC.dd$Rent3739_Mean = HOLC.dd$Rent_Mean
HOLC.dd$NR3739 = HOLC.dd$NR
HOLC.dd = HOLC.dd %>% dplyr::select(-Rent_Mean, -Rent_Lower, -NR)



## Family Income ##
## Not all have FamIncome.
template.fam = data.frame(inc = unique(HOLC.dd[,c('FamIncome')]), stringsAsFactors=F)
template.fam$Ninc = template.fam$inc
template.fam$Ninc = gsub(pattern='2.5M', '2,500', as.character(template.fam$Ninc))
template.fam$Ninc = gsub(pattern='7.5 M', '7,500', as.character(template.fam$Ninc))
template.fam$Ninc = gsub(pattern='1.2-2.1 M', '1200-2100', as.character(template.fam$Ninc))
template.fam$Ninc = gsub(pattern='5000to15000', '5000-15000', as.character(template.fam$Ninc))
template.fam$Ninc = gsub('M', ',000', gsub(' M', 'M', template.fam$Ninc))
template.fam$Ninc[grepl('down|egro|^\\*$|N/A|Static|Factory |ntermediate', template.fam$Ninc)] = NA
template.fam$Ninc = gsub('Over|over|\\$|and up|\\& up$|\\& upward|and upward|Upward|Under|under|not over|average', '', template.fam$Ninc)
template.fam$Ninc = gsub(' average|per year|upward|up$|up.$|,|\\&|\\*', '', template.fam$Ninc)

template.fam$Linc = sapply(strsplit(template.fam$Ninc, ' *- *| to | t0 |--'), '[', 1)
template.fam$Linc = as.numeric(sapply(strsplit(trimws(template.fam$Linc), ' '), '[', 1))
template.fam$Hinc = sapply(strsplit(template.fam$Ninc, ' *- *| to | t0 |--'), '[', 2)
template.fam$Hinc = as.numeric(sapply(strsplit(template.fam$Hinc, ' '), '[', 1))

template.fam$Hinc[is.na(template.fam$Hinc)] = template.fam$Linc[is.na(template.fam$Hinc)]  # missing H? Replace it with L
template.fam$Linc[template.fam$Linc==0 & !is.na(template.fam$Linc)] = template.fam$Hinc[template.fam$Linc==0 & !is.na(template.fam$Linc)]  # if Linc is 0, replace with Hinc (some wer '0-1000')
template.fam$Linc[is.na(template.fam$Linc)] = template.fam$Hinc[is.na(template.fam$Linc)]

for(i in 1:4){
  template.fam$Linc[!is.na(template.fam$Linc) & template.fam$Linc<template.fam$Hinc/100] = template.fam$Linc[!is.na(template.fam$Linc) & template.fam$Linc<template.fam$Hinc/100]*10
}

template.fam = template.fam %>%
  dplyr::mutate(Linc = ifelse(Linc<=10, Linc*1000, Linc),
                Hinc = ifelse(Hinc<250, Hinc*1000, Hinc)) %>%
  dplyr::mutate(Linc = ifelse(Linc==10003000, 2000, Linc),
                Hinc = ifelse(Hinc==10003000, 2000, Hinc))

template.fam$Minc = (template.fam$Hinc + template.fam$Linc)/2  # no save.; Minc = Mean Income
template.fam$Minc_match = TRUE

# Merge into HOLC.dd
HOLC.dd = merge(x=HOLC.dd, y=unique(template.fam[,c('inc','Minc','Minc_match')]), by.x='FamIncome', by.y='inc', all.x=T, all.y=F) %>%
  replace_na(list(Minc_match = FALSE)) 





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



#--------------------
#####
## Merge HOLC.dd to HOLC (spatial) 
## Subset out BG, and then allocate HOLC data spatially; aggregate to BG.
#####
HOLC.dd$ExtraData = as.logical(T)
HOLC = merge(x=HOLC, y=HOLC.dd[,c('polygon_id','NBlack_YN','NBlack_PCT','Rent35_Mean','Rent3739_Mean','Minc','repair_class','NBlack_match','Rent_match35','Rent_match3739',
                                  'Minc_match','Repair_match','ExtraData')], by='polygon_id', all.x=T, all.y=F) %>% 
  replace_na(list(ExtraData = F))

# WATTS.SG = c(7547, 7550) #polygon_id  # LA: map_id==16
# xx = HOLC %>% dplyr::filter(map_id==16 & polygon_id %in% WATTS.SG)

xx = HOLC %>% dplyr::filter(state=='MI') # dplyr::filter(map_id==69) #--> 68 is MD, which has lots of info digitized.
ggplot(data=xx, aes(fill=GRADE)) + geom_sf() + scale_fill_manual(values = color.map)





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
ZCTA =  get_acs(geography='zcta', year=2018, 
        variables = c(paste0('B25040_0',formatC(1:10, width=2, flag='0')),
                      paste0('B02001_0',formatC(1:10, width=2, flag='0')),
                      'B19013_001'), 
        output='wide', geometry=T, keep_geo_vars = T) %>%  # , summary_var = 'B25040_001'
  dplyr::select(GEOID, AFFGEOID10, NAME.zip = NAME, B25040_001E:B19013_001E) %>%
  st_transform(st_crs(HOLC))



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


ZCTA = ZCTA %>% dplyr::select(-contains(c('B250','B020','B190'))) %>%
  dplyr::mutate_at(.vars = vars(UtilityGas:NoFuel), ~ .x/TotalFuel ) %>%
  dplyr::mutate_at(.vars = vars(White:ThreeOrMore), ~ .x/TotalRace) 


BG = BG %>%
  dplyr::mutate(OtherRace = NativeAmerican+NativeHawaiian+OtherRaceOne+TwoOrMore+TwoOrMoreOther+ThreeOrMore) %>%
  dplyr::mutate(OtherSubstandard = Coal + Wood + OtherFuel + NoFuel) %>%
  dplyr::select(-Coal, -Wood, -OtherFuel, -NoFuel, -NativeAmerican, -NativeHawaiian, -OtherRaceOne, -TwoOrMore, -TwoOrMoreOther, -ThreeOrMore) %>%
  dplyr::select(STATEFP:NAME.x, UtilityGas:Solar, OtherSubstandard, White:Asian, OtherRace, MedIncome2018, TotalFuel, TotalRace)


ZCTA = ZCTA %>%
  dplyr::mutate(OtherRace = NativeAmerican+NativeHawaiian+OtherRaceOne+TwoOrMore+TwoOrMoreOther+ThreeOrMore) %>%
  dplyr::mutate(OtherSubstandard = Coal + Wood + OtherFuel + NoFuel) %>%
  dplyr::select(-Coal, -Wood, -OtherFuel, -NoFuel, -NativeAmerican, -NativeHawaiian, -OtherRaceOne, -TwoOrMore, -TwoOrMoreOther, -ThreeOrMore) %>%
  dplyr::select(GEOID:NAME.zip, UtilityGas:Solar, OtherSubstandard, White:Asian, OtherRace, MedIncome2018, TotalFuel, TotalRace)





##--> Get list of BG rows that also touch HOLC
using.BG.index = unlist(lapply(st_intersects(BG, HOLC), function(z) NROW(z)>=1))  #--> gets an index of all the BG's that have a HOLC over them
BG = BG[using.BG.index,] 
BG = BG %>%
  dplyr::mutate(BG_AREA = as.numeric(st_area(BG)))

using.ZCTA.index = unlist(lapply(st_intersects(ZCTA, HOLC), function(z) NROW(z)>=1))  #--> gets an index of all the BG's that have a HOLC over them
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

BG.int.full = st_intersection(BG, HOLC) %>% 
  dplyr::mutate(segmentArea = as.numeric(st_area(.))) %>%
  dplyr::arrange(AFFGEOID, GRADE)



ZCTA.int.full = st_intersection(ZCTA, HOLC) %>%
  dplyr::mutate(segmentArea = as.numeric(st_area(.))) %>%
  dplyr::arrange(AFFGEOID10, GRADE)


saveRDS(list(BG.int.full, ZCTA.int.full, HOLC), file.path(WORK.OUT, paste0('BG_ZCTA_HOLC_int----',Sys.Date(),'.rds')))



### End here. Moved analysis to Analysis file.
