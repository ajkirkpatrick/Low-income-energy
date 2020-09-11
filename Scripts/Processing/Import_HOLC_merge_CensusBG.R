require(rgeos)
require(raster)
require(sp)
require(rgdal)
require(maptools)
require(raster) #--> for crop
require(cleangeo)
require(maps) #--> state.fips

###########################
WORK.OUT = file.path("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output",paste0("OUT_",Sys.Date()))
dir.create(WORK.OUT, recursive=T)   

DATA.IN = file.path('/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Data search/Found Data')

GISshp = file.path('/Volumes/ei-edl01-3/user/ajk41/GIS files')




#### Set GIS parameters: ####
in.CRS = CRS('+init=epsg:4326')  # for XY in SS data
USE.PRJ = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs') #-Albers No. America

## Load background maps and census maps:
US = readOGR(file.path(GISshp, "states_21basic"), 'states')
US = US[which(!US$STATE_NAME%in%c('Alaska','Hawaii')),]
US$STATE_NAME = as.factor(as.character(US$STATE_NAME))
US = spTransform(US, USE.PRJ)


## Load in HOLC data (small!)
HOLC = readOGR(dsn=file.path(DATA.IN,'HOLC Maps','holc_polygons'), layer='holc_polygons')
HOLC = spTransform(HOLC, USE.PRJ)
HOLC@data[,c('STATE','STATE_FIPS')] = HOLC%over%US[,c('STATE_ABBR','STATE_FIPS')][,c('STATE_ABBR','STATE_FIPS')]
HOLC$GRADE = toupper(as.character(HOLC$HOLC_Grade)) #--> HOLC letter has some odd entries. In LA, there's an I-8 on the east side of town. But grade of that is D, so HOLC$HOLC_grade is right.
HOLC$GRADE[which(HOLC$GRADE=="E")] = "D"
HOLC$ID = HOLC$HOLC_ID
HOLC@data = HOLC@data[,c('STATE','STATE_FIPS','map_id','polygon_id','ID','GRADE','name')] # map-polygon is unique; sheets is # of sheets that comprise a city, I think

## Create polygons for HOLC city extents:
HOLC_list = split(HOLC, f=HOLC$map_id)
HOLC_extents = do.call(rbind, lapply(HOLC_list, function(city){
 city_extent = as(extent(city), 'SpatialPolygons')
 proj4string(city_extent) = proj4string(city)
 city_extent = spChFIDs(city_extent, as.character(city$map_id)[1])
}))

HOLC_FIPS = as.character(unique(HOLC$STATE_FIPS))


## 1990 block group map (should be 211k or more block groups)
BG = readShapePoly(file.path('/Volumes/Le Drive/R overflow/Census Shapefiles/nhgis0002_shape/nhgis0002_shapefile_tl2000_us_blck_grp_1990/US_blck_grp_1990.shp'))
# BG = readShapePoly(file.path('/Volumes/ei-edl01-3/user/ajk41/GIS files/nhgis0001_shape/nhgis0001_shapefile_tl2000_us_blck_grp_1990/US_blck_grp_1990.shp'))
BG@proj4string = CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
BG$STATE_FIPS = substr(BG$FIPSSTCO, 1, 2)
BG = BG[which(BG$STATE_FIPS%in%unique(HOLC_FIPS)),]


report <- clgeo_CollectionReport(BG)
summary <- clgeo_SummaryReport(report)
issues <- report[report$valid == FALSE,]
nv <- clgeo_SuspiciousFeatures(report)
mysp <- BG[nv,]

# Now, clean up the problems:
mysp.clean <- clgeo_Clean(mysp, verbose = TRUE)
report.clean <- clgeo_CollectionReport(mysp.clean)
summary.clean <- clgeo_SummaryReport(report.clean)
summary.clean #looks good
BG[nv,] = mysp.clean

BG1 = rbind(BG[-nv,],mysp.clean)
BG1 = spTransform(BG1, USE.PRJ)

BG1@data[,c('FIPSSTCO','TRACT','GROUP')] = apply(BG1@data[,c('FIPSSTCO','TRACT','GROUP')], MARGIN=2, as.integer)
BG1@data$STFID = as.numeric(BG1@data$STFID)
BG1@data = BG1@data[,c('FIPSSTCO','TRACT','GROUP','STFID','STATE_FIPS')]

# do some repairs:
# slot(ll, "polygons") <- lapply(slot(ll, "polygons"), checkPolygonsHoles)
# test = gIsValid(ll)
# BG = crop(BG, extent(gBuffer(US, width=1000, byid=F)))#-> this should clip to US extent. I think it's OK to ignore missing CRS


BG2 = split(BG1, f=1:NROW(BG1))
require(snowfall)
sfInit(parallel=T, 6)
sfExport('HOLC_extents')
sfLibrary(rgeos)
sfLibrary(sp)
  BG2 = sfLapply(BG2, function(x){
    temp = try(gIntersects(x, HOLC_extents, byid=F)==T)
    if(class(temp)=='try-error') temp = NA
   x@data$INTS = temp
  return(x)
  })
  sfStop()
BG3 = do.call(rbind, BG2)
bad.blockgroup = row.names(BG3[is.na(BG3$INTS),]) #--> "3599" in SW/gulf coast Alabama. Does not overlap with HOLC
BG3$INTS[is.na(BG3$INTS)]=F
saveRDS(BG3, file=file.path(WORK.OUT, 'BlockGroup_Int_HOLC_2-21-17.rds'))
BG3 = readRDS(file=file.path('/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/OUT_2017-02-20/BlockGroup_Int_HOLC_2-21-17.rds'))
BG = BG3[which(BG3$INTS==T),]
##-->BG is now all INTERSECTING BlockGroup EXTENTS thus far.

BGx = split(BG, f=row.names(BG))
sfInit(parallel=T, 6)
sfExport('HOLC')
sfLibrary(rgeos)
sfLibrary(sp)
  ptm = Sys.time()
  BGx = sfLapply(BGx,function(x) {
    gIntersects(x, HOLC, byid=F)})
  Sys.time()-ptm
sfStop()
BG = BG[names(BGx[BGx==T]),]

saveRDS(BG, file=file.path(WORK.OUT, 'All_Int_1990_BG_2-21-2017.rds'))
BG = readRDS("/Users/ajkirk/Box Sync/Home Folder ajk41/BoxSync/Projects/Low income and Energy/Output/OUT_2017-02-20/All_Int_1990_BG_2-21-2017.rds")

data(state.fips)
state.fips = unique(state.fips[,c('fips','abb')])
BG$fips = as.numeric(BG$STATE_FIPS)
BG = merge(x = BG, y=state.fips[,c('fips','abb')], by="fips", all.x=T, all.y=F)
BG$STATE_ABBR = BG$abb
BG$abb = NULL

BG$AREA = gArea(BG, byid=T)


#--> Now it's just those that actually intersect one or more HOLC

## Test 1: total up share of each BG covering A, B, C, D (Green/Blue/Yellow/Red) and regress:
#--> BG is only those census blockgroups (1990) that touch HOLC's
#--> HOLC$GRADE is  color. Not sure what "E" is.
HOLC_union = gUnaryUnion(HOLC, id=HOLC$GRADE)
HOLC_union = gSimplify(HOLC_union, tol=0)
  HOLC_union$GRADE = row.names(HOLC_union)
  HOLC_union$color = c('green','blue','yellow','red')

jGetGrade<-function(bg, holc_union){
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
  bg$PCT_N = bg$N/bg$AREA
    return(bg)
}

test.res = jGetGrade(BG[which(BG$FIPSSTCO==54039),], HOLC_union)

BGx = split(BG, f=BG$STATE_ABBR) 
sfInit(parallel=T, 7)
sfExport('jGetGrade')
sfExport('HOLC_union')
sfLibrary(rgeos)
sfLibrary(sp)

ptm = Sys.time()
BGx = sfLapply(BGx,function(x) {
  jGetGrade(x, HOLC_union)})
Sys.time()-ptm
sfStop()


saveRDS(BGx, file=file.path(WORK.OUT, 'All_BG_1990_AreaOfIntersect_2-22-2017.rds'))
  












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
  xx[[i]]$INT_BG = gArea(temp.intersect, byid=T) 
  xx[[i]]$PCT_INT_BG = xx[[i]]$INT_BG/gArea(HOLC[i,], byid=F)
  xx[[i]]$IS_BG = xx[[i]]$PCT_INT_BG==max(xx[[i]]$PCT_INT_BG)
  xx[[i]]$PCT_INT_HOLC = as.numeric(NA)
  xx[[i]]$PCT_INT_HOLC = xx[[i]]$INT_BG/gArea(BG[which(BG$STFID%in%xx[[i]]$STFID),], byid=T)
  xx[[i]]$HOLC_ID = names(xx)[i]
  xx[[i]]$BG_WT = xx[[i]]$PCT_INT_BG*xx[[i]]$PCT_INT_HOLC
  xx[[i]]$BG_WT = xx[[i]]$BG_WT/sum(xx[[i]]$BG_WT)
}

