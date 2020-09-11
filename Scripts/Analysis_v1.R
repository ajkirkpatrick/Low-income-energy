########
## Analysis for Redlining
########




require(rgdal)
require(rgeos)
require(sp)
require(sf)
require(stargazer)
library(viridis)
library(rvest)
require(tidycensus)
require(sp)
require(rgdal)
require(raster)
require(tigris)
require(snow)
require(doSNOW)
require(parallel)
require(foreach)
require(iterators)
options(tigris_use_cache = TRUE)


cl <- snow::makeCluster(rep('localhost',20), type='SOCK')
registerDoSNOW(cl)




# Load files --------------------------------------------------------------
WORK.OUT = file.path(BASE,'ajk41/Low Income Energy/Data/OUTPUT',Sys.Date())
dir.create(WORK.OUT, recursive=T)

Processed_HOLC = file.path(BASE, 'ajk41/Low Income Energy/Data/Zillow/Processed_HOLC')

mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
census_api_key(mykey, install=T)

BG.int = readRDS(jLoad(WORK.OUT, 'BG_int'))
HOLC = readRDS(jLoad(WORK.OUT, 'HOLC_with_data'))



# Get Zillow --------------------------------------------------------------

Z = foreach(z = iter(list.files(file.path(Processed_HOLC), pattern='.rds', full.names=T), by='cell'),
            .errorhandling = 'pass', .packages = c('dplyr','data.table','sf','rgdal','rgeos','sp','tidyverse'),
            .noexport = c('z'),
            .verbose=T) %dopar% {
             z = readRDS(z) %>% st_sf() 
             z = z[which(!is.na(st_coordinates(z)[,1] & !is.na(st_coordinates(z)[,2]))),]
             z = z %>% dplyr::select(-ImportParcelID, 
                                     -(PropertyHouseNumber:PropertyStreetPostDirectional), 
                                     -(PropertyAddressUnitDesignator:PropertyAddressUnitNumber),
                                     -PropertyBuildingNumber, -MailAddressMatchCode)  %>%
               dplyr::filter(!is.na(TotalRooms)) %>%
               st_join(HOLC %>% dplyr::select(polygon_id, map_id, NBlack_YN, NBlack_PCT, Rent35_Mean, Rent3739_Mean, Minc)) %>%
               dplyr::filter(!is.na(polygon_id) & !is.na(map_id))
            } # end dopar foreach

HhHOLC = do.call(rbind, Z) 
HhHOLC.small = HhHOLC %>% dplyr::filter(!is.na(Minc) & !is.na(NBlack_YN)) %>%
                dplyr::mutate(NoHeat = HeatingTypeorSystemStndCode=='NO', id = 1:n()) %>%
                st_join(., BG.int %>% dplyr::select(ST, GEOID, MedIncome1990, Pct.gas, Pct.electric, Pct.none))
                
HhHOLC.rsmall = HhHOLC.small %>% dplyr::filter(!is.na(GEOID))


Hh = HhHOLC.rsmall
lm1 = lm(NoHeat ~ GRADE + as.factor(paste0(map_id)), data=Hh)
lm2 = lm(NoHeat ~ GRADE + poly(Minc, 2) + as.factor(map_id), data=Hh)
lm3 = lm(NoHeat ~ GRADE + bs(Minc, 3)  + bs(MedIncome1990,3) + as.factor(map_id), data=HhHOLC.small)
lm4 = lm(NoHeat ~ GRADE + bs(Minc, 3)  + bs(MedIncome1990,3) + bs(NBlack_PCT, 3) + as.factor(map_id), data=HhHOLC.small)


samp = sample(NROW(HhHOLC), 500000, replace=F)





Hh = HhHOLC[!is.na(Minc) & !is.na(Rent35_Mean) & !is.na(NBlack_PCT),]
Hh = Hh[sample(.N, 500000, replace=F),]

lm1 = lm(NoHeat ~ GRADE + as.factor(paste0(map_id)), data=Hh)
lm2 = lm(NoHeat ~ GRADE + poly(Minc, 2) + as.factor(map_id), data=Hh)
lm3 = lm(NoHeat ~ GRADE + poly(Minc, 2) + poly(Rent35_Mean, 3) +  as.factor(map_id), data=Hh)
lm4 = lm(NoHeat ~ GRADE + poly(Minc, 2) + poly(Rent35_Mean, 3) + poly(NBlack_PCT, 3) + as.factor(map_id), data=Hh)

summary(lm1, robust=T)



res1 = robust.se(lm(NoHeat ~ GRADE + as.factor(map_id), data=Hh), Hh[,ST])
res2 = robust.se(lm(NoHeat ~ GRADE + poly(Minc, 2) + as.factor(paste0(map_id)), data=Hh), Hh[,ST])
res3 = robust.se(lm(Pct.none ~ GRADE + poly(Minc, 2) + poly(MedIncome1990, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]


# Census BG based analysis incl. Heartland  -------------------------------

BG.maj = BG.int %>% dplyr::filter(shareArea>.80) 

ggplot(BG.int %>% dplyr::filter(Maj==T &  ST=='24'), aes(col=GRADE)) + geom_sf()


# saved as temp_2.RData #
# save.image("Z:/user/ajk41/Low Income Energy/Low Income Energy Rproj/temp_2.RData")
# load("Z:/user/ajk41/Low Income Energy/Low Income Energy Rproj/temp_2.RData")

## Pull and compile relevant household heating fuel data ##
# 2015 ACS:  DP04_0062E is all occupied Hh; DP04_0063...71E are counts by type
#            Where 63 is gas; 65 is electricity; 71 is no fuel.
#    Damnit, 2015 ACS doesn't do block group on any of the energy fields.
#    https://api.census.gov/data/2000/sf3/variables.html (navigate with this; add ".html" to anything JSON-like)

require(stargazer)
require(sandwich)
require(lmtest)
require(plm)


#--> Interpolate BG data over HOLC polygons
BG.maj.expanded = st_intersection(BG.maj, HOLC %>% dplyr::select(-GRADE)) %>% mutate(INT_AREA = as.numeric(st_area(.)),
                                                                                     SHARE    = INT_AREA/BG_AREA) %>% arrange(GEOID)


insert.narm = F
BG.maj.collapsed = BG.maj.expanded %>% group_by(YEAR, ST, CO, TRACT, BG, GEOID, GRADE, map_id) %>%
  dplyr::summarize(summary_value = mean(summary_value, na.rm=insert.narm),
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
  dplyr::arrange(GEOID) %>% ungroup()

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
BGMC = BG.maj.collapsed %>% dplyr::filter(!is.na(Minc) & !is.na(MedIncome1990) & !is.na(Pct.none) & Minc<30000)
res1 = robust.se(lm(Pct.none ~ GRADEf + as.factor(map_id), data=BGMC), BGMC$ST)[[2]]
res2 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]
res3 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) + poly(MedIncome1990, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]

stargazer(res1, res2, res3, dep.var.caption='Pct. no heating fuel', omit='map_id', omit.labels= c('City FE'), notes = 'Robust SE clustered by state')

# some ideas: just use wood+coal? justify no county fips FE? Need more rent data.


## Heartland
BGMC = BG.maj.collapsed %>% filter(!is.na(Minc) & !is.na(MedIncome1990) & !is.na(Pct.none) & Minc<30000) # there are some not-possible incomes in here.
res1 = robust.se(lm(Pct.none ~ GRADEf + as.factor(map_id), data=BGMC), BGMC$ST)[[2]]
res2 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]
res3 = robust.se(lm(Pct.none ~ GRADEf + poly(Minc, 2) + poly(MedIncome1990, 2) +as.factor(paste0(map_id)), data=BGMC), BGMC$ST)[[2]]

stargazer(res1, res2, res3, dep.var.caption='Pct. no heating fuel', omit='map_id', omit.labels= c('City FE'), notes = 'Robust SE clustered by state')



