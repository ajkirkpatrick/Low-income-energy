require(rgeos)
require(raster)
require(sp)
require(rgdal)
require(maptools)
require(raster) #--> for crop
require(cleangeo)
require(maps) #--> state.fips
require(snowfall)
###########################
if(sum(grepl('Volumes', dir(".")))>=1) {BOX.PROJECT = file.path("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects")
                                        GISshp = file.path(grep('ei',dir(file.path('/Volumes'), full.names=T), value=T),'user/ajk41/GIS files')
                                            } else {
                                        BOX.PROJECT = file.path("C:/Users/ajk41/Box Sync/Home Folder ajk41/BoxSync/Projects")
                                        GISshp = file.path(huh)}


WORK.OUT = file.path(BOX.PROJECT, "Low income and Energy/Output",paste0("OUT_",Sys.Date()))
dir.create(WORK.OUT, recursive=T)   

DATA.IN = file.path(BOX.PROJECT, 'Low income and Energy/Data search/Found Data')





#### Set GIS parameters: ####
in.CRS = CRS('+init=epsg:4326')  # for XY in SS data
USE.PRJ = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs') #-Albers No. America

## Load background maps and census maps:
US = readOGR(file.path(GISshp, "states_21basic"), 'states', stringsAsFactors=F)
US = US[which(!US$STATE_NAME%in%c('Alaska','Hawaii')),]
US$STATE_NAME = as.factor(as.character(US$STATE_NAME))
US = spTransform(US, USE.PRJ)


#### Load in HOLC data (small!) ####
HOLC = readOGR(dsn=file.path(DATA.IN,'HOLC Maps','holc_polygons'), layer='holc_polygons', stringsAsFactors=F)
#--> send to FusionTable:  writeOGR(spTransform(HOLC, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")), dsn='/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/FusionTableOut/HOLC.kml', layer="HOLC", driver="KML", overwrite_layer=TRUE)
HOLC = spTransform(HOLC, USE.PRJ)
HOLC@data[,c('STATE','STATE_FIPS')] = HOLC%over%US[,c('STATE_ABBR','STATE_FIPS')][,c('STATE_ABBR','STATE_FIPS')]
HOLC$GRADE = toupper(as.character(HOLC$HOLC_Grade)) #--> HOLC letter has some odd entries. In LA, there's an I-8 on the east side of town. But grade of that is D, so HOLC$HOLC_grade is right.
HOLC$GRADE[which(HOLC$GRADE=="E")] = "D"
HOLC$ID = HOLC$HOLC_ID
HOLC@data = HOLC@data[,c('STATE','STATE_FIPS','map_id','polygon_id','ID','GRADE','name')] # map-polygon is unique; sheets is # of sheets that comprise a city, I think

## Create polygons for HOLC city extents, names are map_id:
HOLC_list = split(HOLC, f=HOLC$map_id)
HOLC_extents = do.call(rbind, lapply(HOLC_list, function(city){
 city_extent = as(extent(city), 'SpatialPolygons')
 proj4string(city_extent) = proj4string(city)
 city_extent = spChFIDs(city_extent, as.character(city$map_id)[1])
}))

#--> HOLC$GRADE is  color. Not sure what "E" is.
HOLC_union = gUnaryUnion(HOLC, id=HOLC$GRADE)
HOLC_union = gSimplify(HOLC_union, tol=0)
HOLC_union$GRADE = row.names(HOLC_union)
HOLC_union$color = c('green','blue','yellow','red')

#### Get Block Group map and calculate area of overlap ####
## 1990 block group map (should be 211k or more block groups)
# BG = readOGR(dsn = file.path('/Volumes/Le Drive/R overflow/Census Shapefiles/nhgis0002_shape/nhgis0002_shapefile_tl2000_us_blck_grp_1990'), layer='US_blck_grp_1990.shp')
BG = readOGR(dsn = file.path(GISshp,'nhgis0001_shape/nhgis0001_shapefile_tl2000_us_blck_grp_1990'),layer='US_blck_grp_1990', stringsAsFactors=F)
BG = spTransform(BG, USE.PRJ)


BG$STATE_FIPS = substr(BG$FIPSSTCO, 1, 2)
BG = BG[which(BG$STATE_FIPS%in%unique(HOLC$STATE_FIPS)),]


#2910 ('3599') is bad
BG = BG[-c(2910),]

#--> put this aside for now:
CT = gUnaryUnion(BG, id=paste0(BG$FIPSSTCO,"-",BG$TRACT))
CT$tID = as.character(row.names(CT))
CT$FIPSSTCO = sapply(strsplit(x=CT$tID, split='-'),'[',1)
CT$STATE_FIPS = substr(CT$tID, 1, 2)
CT$TRACT = sapply(strsplit(CT$tID, split='-'), '[', 2)
#--> Not taking CT any further, BG seems like the more reasonable level (unless data is only at Tract level)
      #--> yup, only at census tract for now...
      #--> Take this down to below BG



#### Process BlockGroup intersection with HOLC ####
BGi = gIntersects(gUnaryUnion(HOLC_extents), BG, byid=T)
BG = BG[BGi[,1,drop=T],]

BGi = gIntersects(gUnaryUnion(HOLC), BG, byid=T)
BG = BG[BGi[,1,drop=T],]

## Get pct of each BG that overlaps with HOLC area ##
jGetGrade<-function(bg, holc_union){
  holc_union = crop(holc_union, extent(bg))
  t.int = try(gIntersection(bg, holc_union, byid=c(T,T)))  
  if(class(t.int)=='try-error') return('Unable to intersect bg and holc')
  bg$id = row.names(bg)
  bg$AREA = gArea(bg, byid=T)
  t.area = data.frame(gArea(t.int, byid=T))
  names(t.area) = c('AREA')
  t.area$GRADE = sapply(strsplit(row.names(t.area), " "), '[', 2)
  t.area$id = sapply(strsplit(row.names(t.area), " "), '[', 1)
  t.area = reshape(t.area,  idvar = 'id', timevar = 'GRADE', direction='wide')
  names(t.area)[2:NCOL(t.area)] = sapply(strsplit(names(t.area)[2:NCOL(t.area)], split = "AREA."), '[', 2)
  add.fields = LETTERS[1:4][!LETTERS[1:4]%in%names(t.area)]
  for(i in add.fields){
    t.area[,i] = as.numeric(NA) 
  }
  t.area = t.area[,c('id','A','B','C','D')]
  if(NROW(unique(t.area$id))!=NROW(t.area)) return('Non-unique values for ID')
  row.names(t.area) = t.area$id
  t.area[is.na(t.area)] = 0
  
  bg =(merge(x=bg, y=t.area, by='id', all.x=T, all.y=T)) #--> see if any y's with unmatched x's show up...
  bg$N = bg$AREA - apply(bg@data[,LETTERS[1:4]], MARGIN=1, sum, na.rm=T)
  
  bg$PCT_A = bg$A/bg$AREA
  bg$PCT_B = bg$B/bg$AREA
  bg$PCT_C = bg$C/bg$AREA
  bg$PCT_D = bg$D/bg$AREA
  bg$PCT_N = round(bg$N/bg$AREA,5)
  return(bg)
}

BGx = split(BG, f=BG$FIPSSTCO)
sfInit(parallel=T, 7)
sfExport('jGetGrade')
sfExport('HOLC_union')
sfLibrary(raster)
sfLibrary(rgeos)
sfLibrary(sp)

ptm = Sys.time()
BGx = sfLapply(BGx, function(bgx){
  return(jGetGrade(bgx, HOLC_union))})
Sys.time()-ptm


saveRDS(BGx, file=file.path(WORK.OUT, 'All_Int_1990_shapes_BG_2-22-2017.rds'))
BGx = readRDS(file.path(BOX.PROJECT, "Low income and Energy/Output/OUT_2017-02-22/All_Int_1990_shapes_BG_2-22-2017.rds"))

BG = do.call(rbind, lapply(BGx, function(z){
  spChFIDs(z) <- z$id
  return(z)}))

rm(BGx)


##--> At this point, BG contains all the measures of intersection for every BG.
##--  At an optimum, every BG would have mostly *one* grade with it: mostly A, mostly D, etc.
##--  How do I measure this? I'd like to compare how well BG's "bin" to how well census tracts might "bin".
##--  I'm mostly interested in the "D" areas, so...
hist(BG$PCT_D, breaks=100)
quantile(BG$PCT_D, c(.1, .25, .75, .9), na.rm=T)


#### Now, process census tract intersection with HOLC ####
CTi = gIntersects(gUnaryUnion(HOLC_extents), CT, byid=T)
CT = CT[CTi[,1,drop=T],]

CTi = gIntersects(gUnaryUnion(HOLC), CT, byid=T)
CT = CT[CTi[,1,drop=T],]

CTx = split(CT, f=CT$STATE_FIPS)
sfInit(parallel=T, 7)
sfExport('jGetGrade')
sfExport('HOLC_union')
sfLibrary(raster)
sfLibrary(rgeos)
sfLibrary(sp)

ptm = Sys.time()
CTx = sfLapply(CTx, function(ctx){
  return(jGetGrade(ctx, HOLC_union))})
Sys.time()-ptm
sfStop(); gc()

CT = do.call(rbind, lapply(CTx, function(z){
  spChFIDs(z) <- z$id
  return(z)}))
rm(CTx)

saveRDS(CT, file=file.path(WORK.OUT, 'All_Int_1990_shapes_CT_2-27-2017.rds'))
CT = readRDS(file.path(BOX.PROJECT, "Low income and Energy/Output/OUT_2017-02-27/All_Int_1990_shapes_CT_2-27-2017.rds"))







#### -- Intersect block groups and HOLC polys, mapping the BG with the largest overlap to each HOLC ---####
## Get all intersecting block groups, then ID the largest for eac
## calculating the percent of the HOLC that is in the BG, and the percent of the BG that is in the HOLC
xx = over(HOLC, BG[,c('FIPSSTCO','TRACT','GROUP','STFID','STATE_FIPS')], returnList=T)
names(xx) = row.names(HOLC)
  
for(i in 1:length(xx)){
  print(i)
  if(i%in%c(3835)){  #--> this is a strange island off Duluth that the map rotated to fit in orientation, so it doesn't line up with BG.
    xx[[i]] = xx[[(i-1)]][1,]
    xx[[i]][1,] = rep(NA, NCOL(xx[[i]][1,]))
    xx[[i]]$HOLC_ID = names(xx)[i]
    xx[[i]]$IS_BG = T
    next 
  }
  temp.intersect = gIntersection(HOLC[i,], BG[which(BG$STFID%in%xx[[i]]$STFID),], byid=T)
  temp.intersect = spTransform(temp.intersect, USE.PRJ)
  xx[[i]]$INT_BG = gArea(temp.intersect, byid=T)   #--> area of each intersection between HOLC and BG (one # for each BG)
  xx[[i]]$PCT_INT_BG = xx[[i]]$INT_BG/gArea(HOLC[i,], byid=F) #--> % of area of HOLC falling in each BG (one # for each BG)
  xx[[i]]$IS_BG = xx[[i]]$PCT_INT_BG==max(xx[[i]]$PCT_INT_BG)  #--> The plurality BG (the BG that contains the largest portion of HOLC)
  xx[[i]]$PCT_INT_HOLC = as.numeric(NA)
  xx[[i]]$PCT_INT_HOLC = xx[[i]]$INT_BG/gArea(BG[which(BG$STFID%in%xx[[i]]$STFID),], byid=T)  #--> The share of each BG that overlaps this HOLC (one # for each BG)
  xx[[i]]$HOLC_ID = names(xx)[i]  
  xx[[i]]$BG_WT = xx[[i]]$PCT_INT_BG*xx[[i]]$PCT_INT_HOLC  #--> A measure of how overlapped each BG/HOLC is (1 is perfect - it means BG and HOLC are exactly equal)
  xx[[i]]$BG_WT = xx[[i]]$BG_WT/sum(xx[[i]]$BG_WT)
}

BG.HOLC.map = rbindlist(lapply(xx, function(z) return(z[z$IS_BG==T, c('HOLC_ID','PCT_INT_BG','FIPSSTCO','TRACT','GROUP','STFID')])))
HOLC@data = data.frame(HOLC@data, BG.HOLC.map[match(row.names(HOLC), BG.HOLC.map$HOLC_ID),])







match(BG.HOLC.map
