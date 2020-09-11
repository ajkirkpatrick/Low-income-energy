############
## Low Income Energy
## Merge Census Tract (AHS) data
############
require(sp)
require(rgeos)
require(rgdal)
require(raster)
require(data.table)

EI = grep("ei-", list.dirs("/Volumes", recursive=F), value=T)[1]

WORK.OUT = file.path("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output",paste0("OUT_",Sys.Date()))
dir.create(WORK.OUT, recursive=T)   

DATA.IN = file.path('/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Data search/Found Data')

GISshp = file.path(EI, 'user/ajk41/GIS files')


#### Load in data ####
##--- CT and BG maps ---##
CT = readRDS(file=file.path("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/OUT_2017-02-27/All_Int_1990_shapes_CT_2-27-2017.rds"))
BG = readRDS(file=file.path("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/OUT_2017-02-22/All_Int_1990_shapes_BG_2-22-2017.rds"))
        BG = do.call(rbind, lapply(BG, function(z){
                      spChFIDs(z) <- z$id
                      return(z)}))
##--- Census Data ---###
#--> I tried using Am. Housing Survey (AHS), which is microdata at census tract level, but it ANONYMIZES :(
# AHS90msa = fread(input="/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Data search/Found Data/AHS/ahs90msa.csv") #--> MSA only
C90 = fread(input="/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Data search/Found Data/Census 1990/nhgis0005_ds120_1990_blck_grp.csv")



#### Set GIS parameters: ####
in.CRS = CRS('+init=epsg:4326')  # for XY
USE.PRJ = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs') #-Albers No. America

## Load background maps and census maps:
US = readOGR(file.path(GISshp, "states_21basic"), 'states', stringsAsFactors=F)
US = US[which(!US$STATE_NAME%in%c('Alaska','Hawaii')),]
US$STATE_NAME = as.factor(as.character(US$STATE_NAME))
US = spTransform(US, USE.PRJ)


##### See if anything can be merged ####
BG = merge(x=BG, y=C90[,.(GISJOIN, EUY001, EUY002, EUY003, EUY004, EUY005, ESS001, EST001, ESU001)], by='GISJOIN', all=F)
BG@data[,'TOT_POP'] = apply(BG@data[,grep("EU",names(BG), value=T)], MARGIN=1, sum, na.rm=T)
BG$PCT_WHITE = BG$EUY001/BG$TOT_POP
BG$PCT_BLACK = BG$EUY002/BG$TOT_POP
BG$PCT_AMIND = BG$EUY003/BG$TOT_POP
BG$PCT_ASIAN = BG$EUY004/BG$TOT_POP
BG$PCT_OTHER = BG$EUY005/BG$TOT_POP

summary(lm(PCT_BLACK ~ A + B + C + D, BG@data))
summary(lm(ESS001 ~ A + B + C + D, BG@data)) # lower quartile


