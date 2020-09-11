##################
### Zillow import
### for HOLC data
##################


require(sf)
library(viridis)
library(rvest)
require(tidycensus)
require(sp)
require(rgdal)
require(raster)
require(tigris)
require(readr)
require(readxl)
options(tigris_use_cache = TRUE)

WORK.OUT = file.path(BASE,'ajk41/Low Income Energy/Data/OUTPUT',Sys.Date())
dir.create(WORK.OUT, recursive=T)

Processed_HOLC = file.path(BASE, 'ajk41/Low Income Energy/Data/Zillow/Processed_HOLC')

dir <- file.path(BASE, 'ajk41/Low Income Energy/Data/Zillow/Raw downloads/20180107_Transaction_and_current_assessor/20180107')  #--> for using Zillows code, below

mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
census_api_key(mykey, install=T)

HOLC = readRDS(jLoad(WORK.OUT, 'HOLC_with_data'))  # "Importing_HOLC_v2.R"
HOLC.grade = HOLC %>% group_by(GRADE) %>% summarize(.)

FIPS = st_as_sf(counties())
FIPS = st_transform(FIPS, st_crs(HOLC))


##--> Get FIPS(county) that touch HOLC
use.STATE_FIPS = unique(HOLC %>% pull(STATE_FIPS))
use.STCOFIPS = c(unique(HOLC %>% pull(STCOFIPS)), '01085')

redo = F



#################################
#### Begin Zillow processing ####
if(redo==T){  # this is the whole loading of zillow process; taken from ZTRAX github
  
#####################################################################################
#  Building a simple hedonic dataset from ZTRAX
#   The purpose of this code is to demonstrate the structure and some of the nuance
#   of the ZTRAX dataset. Ultimately, individual researchers are responsible
#   for all data cleaning and the subsequent results using ZTRAX. See the detailed ZTRAX
#   documentation for all available variables and variable descriptions. 
#
#  Skylar Olsen, PhD
#  Zillow Senior Economist 
#  2016-03-05
#####################################################################



td<- tempdir()

## These lines set several options
options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors


#############################################################################################################
#############################################################################################################
### IMPORTANT: These files are very large. While prototyping, limit the number of rows you load.
###            When ready, change prototyping to FALSE.
#############################################################################################################
#############################################################################################################

prototyping <- F

if(prototyping){
  rows2load <- 10000
}else{
  rows2load <- -1L
}


######################################################################
###  Create property attribute table
#    Need 3 tables
#    1) Main table in assessor database
#    2) Building table
#    3) BuildingAreas

#  Pull in layout information
layoutZAsmt <- read_excel(file.path(dir, 'layout.xlsx'), sheet = 1)
layoutZTrans <- read_excel(file.path(dir, 'layout.xlsx'), 
                           sheet = 2,
                           col_types = c("text", "text", "numeric", "text", "text"))

col_namesMain <- layoutZAsmt[layoutZAsmt$TableName == 'utMain', 'FieldName']
col_namesBldg <- layoutZAsmt[layoutZAsmt$TableName == 'utBuilding', 'FieldName']
col_namesBldgA <- layoutZAsmt[layoutZAsmt$TableName == 'utBuildingAreas', 'FieldName']
col_namesMail <- layoutZAsmt[layoutZAsmt$TableName == 'utMailAddress','FieldName']



######################################################################
### begin loop:

for(STFIPS in use.STATE_FIPS){
  
cat(paste0('Processing state fips: ', STFIPS, '\n'))  
  # if(file.exists(file.path(Processed_HOLC, paste0('HOLC_',STFIPS,'.rds')))) {
  #   print(paste0(STFIPS,' exists already. Next state'))
  #   next
  # }

use.zipfile = file.path(dir, paste0(STFIPS, '.zip'))
######################################################################
# Pull address, geographic, lot size, and tax data from main table
con <- unzip(zipfile = use.zipfile, files = 'ZAsmt\\Main.txt', exdir = td)
base <- read.table(con,
                   nrows = rows2load,                    
                   sep = '|',
                   header = FALSE,
                   stringsAsFactors = FALSE,             
                   skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                   comment.char="",                           # tells R not to read any symbol as a comment
                   quote = "" ,                                # this tells R not to read quotation marks as a special symbol
                   col.names = col_namesMain$FieldName
)                                          

base <- as.data.table(base)
base <- base[FIPS %in% as.integer(use.STCOFIPS), list(RowID, ImportParcelID, 
                     FIPS, State, County, 
                     PropertyFullStreetAddress,
                     PropertyHouseNumber, PropertyHouseNumberExt, PropertyStreetPreDirectional, PropertyStreetName, PropertyStreetSuffix, PropertyStreetPostDirectional,
                     PropertyCity, PropertyState, PropertyZip,
                     PropertyBuildingNumber, PropertyAddressUnitDesignator, PropertyAddressUnitNumber,
                     PropertyAddressLatitude, PropertyAddressLongitude, PropertyAddressCensusTractAndBlock, 
                     NoOfBuildings,
                     LotSizeAcres, LotSizeSquareFeet,
                     TaxAmount, TaxYear)]

# Keep only one record for each ImportPropertyID. 
# ImportParcelID is the unique identifier of a parcel. Multiple entries for the same ImportParcelID are due to updated records.
# The most recent record is identified by the greatest LoadID. 
#   **** This step may not be necessary for the published dataset as we intend to only publish the updated records, but due dilligence demands we check. 

length(unique(base$ImportParcelID))  # Number of unique ImportParcelIDs
dim(base)[1]                         # Number of rows in the base dataset

if( length(unique(base$ImportParcelID)) != dim(base)[1] ){
  
  #Example: Print all entries for parcels with at least two records.
  base[ImportParcelID %in% base[duplicated(ImportParcelID), ImportParcelID], ][order(ImportParcelID)]
  
  setkeyv(base, c("ImportParcelID", "LoadID"))  # Sets the index and also orders by ImportParcelID, then LoadID increasing
  keepRows <- base[ ,.I[.N], by = c("ImportParcelID")]   # Creates a table where the 1st column is ImportParcelID and the second column 
  # gives the row number of the last row that ImportParcelID appears.
  base <- base[keepRows[[2]], ] # Keeps only those rows identified in previous step
  
}
unlink(con)
######################################################################
#### Load most property attributes
con <- unzip(zipfile = use.zipfile, files = 'ZAsmt\\Building.txt', exdir = td)

bldg <- read.table(con,
                   nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                   sep = '|',
                   header = FALSE,
                   stringsAsFactors = FALSE,             
                   skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                   comment.char="",                           # tells R not to read any symbol as a comment
                   quote = "",                                # this tells R not to read quotation marks as a special symbol
                   col.names = col_namesBldg$FieldName
) 

bldg <- as.data.table(bldg)

bldg <- bldg[FIPS %in% as.integer(use.STCOFIPS) , list(RowID, NoOfUnits, BuildingOrImprovementNumber, 
                                                         YearBuilt, EffectiveYearBuilt, YearRemodeled,
                                                         NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms, 
                                                         FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
                                                         HeatingTypeorSystemStndCode,
                                                         PropertyLandUseStndCode)]


#  Reduce bldg dataset to Single-Family Residence, Condo's, Co-opts (or similar)

bldg <- bldg[PropertyLandUseStndCode %in% c('RR101',  # SFR
                                            'RR999',  # Inferred SFR
                                            # 'RR102',  # Rural Residence   (includes farm/productive land?)
                                            'RR104',  # Townhouse
                                            'RR105',  # Cluster Home
                                            'RR106',  # Condominium
                                            'RR107',  # Cooperative
                                            'RR108',  # Row House
                                            'RR109',  # Planned Unit Development
                                            'RR113',  # Bungalow
                                            'RR116',  # Patio Home
                                            'RR119',  # Garden Home
                                            'RR120'), # Landominium
             ]
unlink(con)

######################################################################
#### Load building squarefoot data
con <- unzip(zipfile = use.zipfile, files = 'ZAsmt\\BuildingAreas.txt', exdir = td)
sqft <- read.table(con,
                   nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                   sep = '|',
                   header = FALSE,
                   stringsAsFactors = FALSE,             
                   skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                   comment.char="",                           # tells R not to read any symbol as a comment
                   quote = "",                                # this tells R not to read quotation marks as a special symbol
                   col.names = col_namesBldgA$FieldName
)


sqft <- as.data.table(sqft)
sqft = sqft[FIPS %in% as.integer(use.STCOFIPS),]

# Counties report different breakdowns of building square footage and/or call similar concepts by different names.
# The structure of this table is to keep all entries reported by the county as they are given. See 'Bldg Area' table in documentation.
# The goal of this code is to determine the total square footage of each property. 
# We assume a simple logic to apply across all counties here. Different logic may be as or more valid.
# The logic which generates square footage reported on our sites is more complex, sometimes county specific, and often influenced by user interaction and update. 

sqft <- sqft[BuildingAreaStndCode %in% c('BAL',  # Building Area Living
                                         'BAF',  # Building Area Finished
                                         'BAE',  # Effective Building Area
                                         'BAG',  # Gross Building Area
                                         'BAJ',  # Building Area Adjusted
                                         'BAT',  # Building Area Total
                                         'BLF'), # Building Area Finished Living
             ]

table(sqft$BuildingOrImprovementNumber, useNA='ifany')  # BuildingOrImprovementNumber > 1  refers to additional buildings on the parcel. 

sqft <- sqft[ , list(sqfeet = max(BuildingAreaSqFt, na.rm = T)), by = c("RowID", "BuildingOrImprovementNumber")]


unlink(con)

###############################################################################
# In addition, pull utMailAddress and merge (get absentee landlords)

con <- unzip(zipfile = use.zipfile, files = 'ZAsmt\\MailAddress.txt', exdir = td)
mail <- read.table(con,
                   nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                   sep = '|',
                   header = FALSE,
                   stringsAsFactors = FALSE,             
                   skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                   comment.char="",                           # tells R not to read any symbol as a comment
                   quote = "",                                # this tells R not to read quotation marks as a special symbol
                   col.names = col_namesMail$FieldName
)


mail <- as.data.table(mail)

mail <- mail[FIPS %in% as.integer(use.STCOFIPS) , list(RowID, MailHouseNumber, MailFullStreetAddress, MailCity,
                                                       MailState, MailZip, MailZip4, MailAddressMatchCode,FIPS)]
mail[,MailFIPS:=FIPS]
mail[,FIPS:=NULL]

stopifnot(sum(duplicated(mail$RowID))==0)
unlink(con)


###############################################################################
#   Merge previous three datasets together to form attribute table

attr <- merge(base, bldg, by = "RowID", all.x=T, all.y=F)
attr <- merge(attr, sqft, by = c("RowID", "BuildingOrImprovementNumber"), all.x=T, all.y=F)
attr <- merge(attr, mail, by = "RowID", all.x=T, all.y=F)

stopifnot(sum(duplicated(attr$ImportParcelID))==0)  #--> ZTrans has unique ID with TransID, has M:1 with ImportParcelID


attr = attr %>% dplyr::filter(!is.na(PropertyAddressLongitude)&!is.na(PropertyAddressLatitude)) %>%
        st_as_sf(., coords = c('PropertyAddressLongitude','PropertyAddressLatitude'), crs=4326, agr='constant', na.fail=F) %>%
        st_transform(crs = st_crs(HOLC))  %>%
        mutate(GRADE = unlist(lapply(st_intersects(., HOLC.grade), function(x){
                                              ifelse(is_empty(x), NA, HOLC.grade[x[1],'GRADE'])}))) %>%
        dplyr::filter(!is.na(GRADE))

# attr is now just those properties that are in HOLC rated polygons.





###############################################################################
###############################################################################
#  Load transaction dataset.
#     Need two tables
#      1) PropertyInfo table provided ImportParcelID to match transaction to assessor data loaded above
#      2) Main table in Ztrans database provides information on real estate events

col_namesProp <- layoutZTrans[layoutZTrans$TableName == 'utPropertyInfo', 'FieldName']
col_namesMainTr <- layoutZTrans[layoutZTrans$TableName == 'utMain', 'FieldName']


###############################################################################
#   Load PropertyInfo table for later merge

con <- unzip(zipfile = use.zipfile, files = 'ZTrans\\PropertyInfo.txt', exdir = td)

propTrans <- read.table(con,
                        nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                        sep = '|',
                        header = FALSE,
                        stringsAsFactors = FALSE,             
                        skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                        comment.char="",                           # tells R not to read any symbol as a comment
                        quote = "",                                # this tells R not to read quotation marks as a special symbol
                        col.names = col_namesProp$FieldName
)
unlink(con)

propTrans <- as.data.table(propTrans)

propTrans <- propTrans[FIPS %in% as.integer(use.STCOFIPS) & ImportParcelID %in% attr$ImportParcelID , list(TransId, PropertySequenceNumber, LoadID, ImportParcelID)] #--> ASmt has ImportParcelID but not TransID or PropertySeuenceNumber or LoadID. Link RowID<--1:1-->ImportParcelID<--M:1-->TransID

# Keep only one record for each TransID and PropertySequenceNumber. 
# TransID is the unique identifier of a transaction, which could have multiple properties sequenced by PropertySequenceNumber. 
# Multiple entries for the same TransID and PropertySequenceNumber are due to updated records.
# The most recent record is identified by the greatest LoadID. 
#   **** This step may not be necessary for the published dataset as we intend to only publish most updated record. 

setkeyv(propTrans, c("TransId", "PropertySequenceNumber", "LoadID"))
keepRows <- propTrans[ ,.I[.N], by = c("TransId","PropertySequenceNumber")] #--> had PropertySequenceNumber in the grouping by. That's not right!
propTrans <- propTrans[keepRows[[3]], ]
propTrans[ , LoadID:= NULL]

# Drop transactions of multiple parcels (transIDs associated with PropertySequenceNumber > 1)

dropTrans <- unique(propTrans[PropertySequenceNumber > 1, TransId])
propTrans <- propTrans[!(TransId %in% dropTrans) & ImportParcelID%in%attr$ImportParcelID, ]   # ! is "not"



#######################################################################################
#  Load main table in Ztrans database, which provides information on real estate events
con <- unzip(zipfile = use.zipfile, files = 'ZTrans\\Main.txt', exdir = td)

trans <-as.data.table(read.table(con,
                        nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                        sep = '|',
                        header = FALSE,
                        stringsAsFactors = FALSE,             
                        skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column 
                        comment.char="",                           # tells R not to read any symbol as a comment
                        quote = "",                                # this tells R not to read quotation marks as a special symbol
                        col.names = col_namesMainTr$FieldName
))
unlink(con)

trans <- trans[FIPS %in% as.integer(use.STCOFIPS) & TransId %in% propTrans$TransId, list(TransId, LoadID,
                                                         RecordingDate, DocumentDate, SignatureDate, EffectiveDate,
                                                         SalesPriceAmount, LoanAmount,
                                                         SalesPriceAmountStndCode, LoanAmountStndCode,
                                                         LenderName, LenderTypeStndCode, LenderIDStndCode, LenderDBAName, DBALenderTypeStndCode, DBALenderIDStndCode,
                                                         LenderMailFullStreetAddress, LenderMailCity, LenderMailState, LenderMailZip, LenderMailZip4,
                                                         LoanTermYears,InitialInterestRate,
                                                         TitleCompanyName, TitleCompanyIDStndCode,
                                                         # These remaining variables may be helpful to, although possibly not sufficient for, data cleaning. See documentation for all possible variables.
                                                         DataClassStndCode, DocumentTypeStndCode,      
                                                         PartialInterestTransferStndCode, IntraFamilyTransferFlag, TransferTaxExemptFlag,
                                                         PropertyUseStndCode, AssessmentLandUseStndCode,
                                                         OccupancyStatusStndCode)]

# Keep only one record for each TransID. 
# TransID is the unique identifier of a transaction. 
# Multiple entries for the same TransID are due to updated records.
# The most recent record is identified by the greatest LoadID. 
#   **** This step may not be necessary for the published dataset as we intend to only publish most updated record. 

setkeyv(trans, c("TransId", "LoadID"))
keepRows <- trans[ ,.I[.N], by = "TransId"]
trans <- trans[keepRows[[2]], ]
trans[ , LoadID:= NULL]

#  Keep only events which are deed transfers (excludes mortgage records, foreclosures, etc. See documentation.)

trans <- trans[DataClassStndCode %in% c('D', 'H'), ]

###############################################################################
#   Merge previous two datasets together to form transaction table

ptm = Sys.time()
transComplete <- merge(propTrans, trans, by = "TransId") %>% 
  as_tibble() %>%
  dplyr::filter(ImportParcelID %in% attr$ImportParcelID) %>%
  nest(-ImportParcelID,.key='trans') #-----------------------------------> nested on ImportParacelID to merge in line ~405 to make list-column.
Sys.time()-ptm


###############################################################################
#  Finally, merge 1:M attr (property attributes) to transComplete (transaction details)
ptm = Sys.time()
xx = as.tibble(attr) %>% left_join(transComplete, 'ImportParcelID')
ptm - Sys.time()

saveRDS(xx, file.path(Processed_HOLC, paste0('HOLC_',STFIPS,'.rds')))

rm(xx);rm(trans);rm(transComplete);rm(attr)
gc()
} 

if(exists('td')) {unlink(td)
  rm(td)}

} # end "if redo"


############################
#### Load in .rds files ####
############################
require(snowfall)
sfInit(10, parallel=T)
sfLibrary(sf)
sfLibrary(dplyr)

Z = do.call(rbind,sfClusterApplyLB(list.files(file.path(Processed_HOLC), pattern='.rds', full.names = T),
                         function(z) {z = readRDS(z) %>% st_sf()
                                      z = z[which(!is.na(st_coordinates(z)[,1] & !is.na(st_coordinates(z)[,2]))),]
                                           z %>% dplyr::select(-ImportParcelID, 
                                                              -(PropertyHouseNumber:PropertyStreetPostDirectional), 
                                                              -(PropertyAddressUnitDesignator:PropertyAddressUnitNumber),
                                                              -PropertyBuildingNumber, -MailAddressMatchCode)  %>%
                                             dplyr::filter(!is.na(TotalRooms)) %>%
                                             st_sf()
                                            # z %>%    mutate(GRADE = st_join(.[,c('RowID',  'geometry')], HOLC.grade) %>% 
                                            #                   group_by(RowID) %>% summarize(GRADE2 = first(GRADE)) %>% pull(GRADE2))
                         }))






