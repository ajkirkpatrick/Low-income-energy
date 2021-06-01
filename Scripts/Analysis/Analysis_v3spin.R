
#' ---
#' title: "Energy Insecurity and Redlined America"
#' author: A. Justin Kirkpatrick
#' output:
#'   pdf_document:
#'     keep_tex: true
#'     extra_dependencies: ["bbm", "threeparttable","float","booktabs"]
#' ---
#'
#'

#+ echo=FALSE, warning=FALSE, error=FALSE, message=FALSE, cache = FALSE

########
## Analysis for Redlining
########

require(rgdal)
require(rgeos)
require(sp)
require(sf)
require(mapview)
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
require(fixest)
require(modelsummary)
require(gtable)
require(knitr)
require(kableExtra)
require(units)
require(prettyunits)
require(scales)
require(DescTools)
require(broom)
require(here)
options(tigris_use_cache = TRUE)

cacheoption = TRUE





# numCores = 2
# 
# if(NROW(showConnections())!=numCores){ # faster core construction
#   if(NROW(showConnections())>0) stopCluster(cl)
#   cl <- parallel::makeCluster(rep('localhost',numCores), type='PSOCK')
#   registerDoSNOW(cl)}


#########################
#########################
#' # Thanks: Heartland 2017, Brian Murray and NicInstitute
#' 
#' # Introduction
#' 
#' 
#' 
#' The energy insecurity literature contains evidence that low-income households frequently face
#' excessive energy bills, despite their income limitations, that initially seem counter-intuitive
#' (Hernaandez et al., 2014; Hernaandez and Bird, 2010). Poliycmakers have expressed concern not just that low-income
#' households spend a larger share of income on energy services, a measure known as *energy burden*, but that low-income households are
#' paying more to attain the *same* level of energy services relative to other income groups. The notion
#' of this inequity in "energy access" - the ability to purchase energy services such as home heating or cooling - has
#' been the object of programs that aim to improve residential energy efficiency through retrofits (CITE),
#' or provide subsidies for newer, higher-efficiency appliances (CITE). As part of increasing concerns
#' about income inequality, energy burdens and energy access inequity have become notable issues. This paper
#' explores the role of historic housing discrimination known as "redlining" and its role
#' in exacerbating energy access issues.
#' 
#' That a high energy burden is common amongst low-income households is not surprising as energy burden is, 
#' by definition, decreasing in income. The existence of retrofit programs and home heating
#' assistance programs like LIHEAP show that policymakers view home heating as a necessity. Evidence
#' shows that inadequate home heating can cause or worsen existing medical issues (CITE), and
#' can affect child development and achievement (CITE). However, these programs can be
#' expensive, suffer from economic dead weight loss due to poor targeting (CITE), under-deliver
#' promised results (Fowlie et al.), can interact with environmental and environmental justice goals negatively by subsidizing consumption
#' of energy (Levinson?), and are econommic "second best" to programs that successfully target the 
#' source of the burden directly.
#' 
#' A racial component exists even beyond income-based energy inequity. Even when controlling for poverty rates, Reames
#' (2016b) finds that minority-dominated census block-groups and those block-groups with
#' high measures of racial segregation also tend to have lower (worse) energy efficiency and 
#' thus spend a greater total amount for the same level of energy services relative
#' to non-minority households. That is, even
#' conditional on poverty, minority areas have lower-efficiency housing stock relative to
#' non-minority areas. Not only is energy insecurity an issue, but so too is this "energy inequity",
#' which I define as "the disproportional incidence of energy insecurity in heavily-minority areas
#' relative to non-minority areas of similar income." 
#' 
#' This energy inequity is primarily attributable to the housing stock, which is less energy effcient
#' in low income and minority neighborhoods. Previous work in this area has found negative
#' correlations between energy efficiency and racial characteristics at the census block-group
#' level (Reames, 2016b), and many have noted the disproportionate energy burden faced by
#' low-income households. With tight budget constraints, low-income households are more
#' likely to consume lower-priced housing. Naturally, lower priced housing stock will be deficient
#' in some areas -- maintenance may be less frequent or non-existent, structural problems may
#' plague the home, and the home may have un-remediated lead paint or asbestos. Similarly,
#' it is more likely that the home is poorly insulated, has single-pane windows, and has dated,
#' inefficient systems for heating and cooling. These characteristics are common amongst low-income 
#' housing, and are intertwined with poor child health and development outcomes, and
#' with food insecurity (Cook et al., 2008).
#' 
#' This brings about the questions central to this paper: First, is there evidence that racial differences in
#' energy access exist once income and wealth are accounted for? Second, what drives the wedge between the energy
#' efficiency of the housing stock available to (or chosen by) minorities and the energy efficiency of 
#' the housing stock available to (or chosen by) non-minorities of similar economic status?
#' 
#' This paper posits a hypothesis for one potential cause of this wedge and why it persists over time. 
#' I propose that institutional discrimination in the
#' 1930s-1970s forced African-Americans and other minorities into segregated "redlined" communities and, 
#' over this period, the housing stock in these communities, though similar at one point in time, developed differently
#' relative to the housing stock of non-redlined communities of similar socio-economic standing and demographic
#' composition. This historical racism has persistent long-term effects on housing choices and housing quality
#' that explains, at least in part, the energy inequity we observe today.
#' 
#' To this end, I explore the extent to which
#' minority households face energy inequity - that is, the extent to which minority households pay a higher amount for the same
#' level of energy services such as home heating relative to non-minorities of similar socio-economic status, which I do by 
#' examining data on home energy consumption, race, and income. This requires careful attention to the nature of energy
#' expenditures. High levels of energy expenditure may be a sign that a household values warm temperatures during Winter
#' months and chooses to spend a larger share of income on home heating. High levels of energy expenditure may also be the result
#' of households who choose a home with very low energy efficiency and who must spend a great
#' deal simply to keep the interior temperature at a low but livable temperature. In short, one can spend a lot of money
#' to heat to 75 degrees farenheit in an efficient home, and one can spend a lot of money heating to 60 degrees farenheit 
#' in a very inefficient home. The two are observationally equivalent when examining only monthly bills and outside temperature.
#' I disentangle these two by leveraging data from the 
#' California Residential Appliance Saturation Survey (RASS) which includes consumer's reported thermostat setpoint,
#' combined with a simple model of household energy consumption. The model allows me to separate unobserved characteristics of the household 
#' from home heating efficiency, permitting unbiased estimation of a household's home heating response to cold-weather shocks. By focusing on
#' the energy consumption response after controlling for the household's thermostat setpoint, I am able to measure the home's energy
#' efficiency level, rather than the household's preference for indoor temperatures or direct constraints on budgets.
#' 
#' Using original maps from the Homeowners Loan Corporation (HOLC) which designated "redlined" areas along with "yellow", "green" and "blue" areas
#' in more than 170 cities in the US in 1933-36 \ref{fig:HOLC1}, 
#' combined with RASS survey data from 2009, I test the hypothesis that areas designated Grade D (or "red"), which were considered "appropriate" for minorities to purchase homes, have
#' more frequent substandard heating systems and are more inefficient in heating, evidenced by higher cost responses to cold weather shocks after controlling
#' for thermostat setpoints. As noted by (CITE SPENCER), the HOLC maps did not randomly assign neighborhoods grades and therefore any current differences in 
#' the simple mean of energy efficiency outcomes between areas of different grades should not be causally attributed to redlining policy. Specifically, areas of lower construction
#' quality, lower rent, older and smaller homes, and areas with pre-existing minority populations in 1933-36 were more likely to be graded as "red." To address
#' the potential for bias, I control for these observables by using original survey data recorded by HOLC surveyors in 1933-36 that recorded the
#' justification for grading. Notably, there is considerable overlap in rents, the presence of minority populations, income, and other observables between "red" and "yellow"
#' HOLC grades, leaving specific designation by the surveyor to be nearly random. Conditional on these observables and assuming that unobserved differences correlated with HOLC grade do not still apply in the current period, then
#' "red" grades are as good as randomly assigned. I use this conditionally as-good-as-random assignment to estimate causal effects of HOLC "redlining" on current
#' energy inefficiency and energy inequity.
#'
#+ r dispfig1, echo=F, cache=F, fig.align='center', out.width='90%', fig.caption = 'Example of a HOLC map (1933) showing neighborhood designations in Durham, NC. Red areas were designated "appropriate" for lending to minorities, while other areas were not.'

knitr::include_graphics(here('Images','HOLC_example_1.png'))

#' 
#' This paper does not address the dynamics of residential sorting between 1933 and the current period. While the practice of "redlining" came to a statutory end
#' with the Civil Rights Act of 1968, it was not until 1977's Community Reinvestment Act that lenders were required to detail the extent of their lending
#' in all areas in which they operated, including in heavily-minority "redlined" neighborhoods. *De facto*, rather than *de jure* housing discrimination continued 
#' well beyond that date, and still continues today, albeit in different ways (see Christensen + Timmins, ...). In a housing
#' market with no frictions and no discrimination, post-institutional redlining would have seen a re-sorting of households by preferences for housing stock. In
#' this perfect case, observed differences in energy inequity would be the result of households sorting in to inefficient units, and housing prices adjusting to
#' reflect the cost of heating. In effect, the most inefficient units would be rented by those who have little preference for warm tempertures. At the same time,
#' poverty is known to be cyclic, and low-income populations that rely heavily on family and community may have, in effect, high fixed costs of moving outside of a neighborhood.
#' For young families especially, remaining close to (grand)parents may necessitate remaining in the same neighborhood for multiple generations. To the extent that a policy 
#' in place through 1977 "fixed" a family's location by precluding moves to other areas with better housing stock, the same families may face high costs of moving 
#' today. Thus, redlining policy may have a *hysteresis* effect today. A full sorting model of residential choice is beyond this paper, though not infeasible under the
#' right data circumstances.
#' 
#' This paper contributes to two separate literatures. The first literature is the body of work that examines the role of historic discriminatory economic policies on 
#' current outcomes. In this area, this paper is most similar to Aaronson and Mazumder (???), which specifically examines the effect of HOLC redlining on credit and home ownership, 
#' Aaronson and Mazumder use a boundary difference-in-differences to show that households located 
#' inside HOLC redlined areas had less access to credit and were less likely to own their home relative to similar households located just outside the boundary. That 
#' striking result motivates this paper -- one of the mechanisms by which energy inequity can manifest is through declining investment in available housing
#' stock either by owner-occupants or by absentee landlords. Energy inequity is, at least in theory, a direct result of the effect shown by Aaronson and Mazumder. 
#' The second literature
#' examines household electricity consumption responses to temperature or price shocks in the context of energy expenditures. 
#' Largely motivated by the need to accurately predict future electricity consumption under future prices, demographics, or climate (Ang. and Auffhamer 2009), 
#' these studies have examined the consumption response to varying temperature or prices. Few, however,
#' have incorporated a model of consumption that allows for endogenous preferences for thermostat setpoints. An exception is Brewer (2019), who estimates a "bliss point"
#' in a structural model of joint residence and thermostat setpoint choice, though this paper examines moral hazard in landlord-paid heating. 
#' The paper most similar to this is Doremus, Jacqz, and Johnston (2020), who examine household energy consumption expenditures in response to variation in temperature
#' by income. However, the authors similarly do not allow for endogenous thermostat setpoints that reflect unobserved tastes. 
#' 
#' Section 2 defines energy inequity and introduces a simple model of home heating costs. It further shows how unobserved home efficiency characteristics, 
#' which may vary between redlined and non-redlined neighborhoods when historic housing discrimination still impacts current energy burdens,  
#' can be estimated from energy consmption data with a known thermostat setpoint. Section 3 introduces the data. 
#' Section 4 discusses the estimation strategy. Section 5 presents results and Section 6 concludes.
#' 
#' 
#' Some citations: Aroonreuengsawat and Auffhammer: reponse to weather shocks, but does not endogenize the thermostat setpoint
#' Levinson 2013 (JEBO) on building codes: not using thermostats, just decomposing aggregate (state, region) decrease in PerCap energy by demos, moving, buildings, etc. Does use RECS, but only to get CA and NON-CA HDD responses (no thermostat)
#' Chong 2013: estimates response but no thermostat
#' ## Details and description
#' 
#' # 2. Energy Inequity and Housing Discrimination
#' 
#' 
#' ## Energy inequity
#' 
#' PROBABLY CUT THISE WHOLE THING::::Energy burden is defined as the share of total household income spent on energy purchases not including transport. The low-income
#' energy burden is the noted phenomenon where low-income households have a disproportionate likelihood of having a high proportion
#' of household income devoted to energy purchases (Byrne 1986; Baxter 1998). In practice, energy burdens above 10\% are worrisome,
#' and many households have burdens of 20% or more (Baxter, 1988). Middle and upper-income households tend to spend 5% or less of 
#' household income on energy services (Hernandez and Bird, 2010). High energy burden indicates an inability to afford
#' basic energy services (heating, refrigeration), particularly during periods of volatility in energy prices. High prices
#' or high volatility can lead to a "heat or eat" scenario (Battachayra, Haider, Currie) where households must trade
#' off between heating and other necessities.
#' 
#' Energy inequity can be defined as the gap between the cost to obtain a set level of energy services for a minority household
#' relative to a non-minority household of equal socioeconomic status and means. Energy services more broadly can be defined as
#' adequate lighting, heating, cooling, refrigeration, and sanitation. In this paper, I use home heating temperature as the primary
#' energy serice discussed. The definition of energy inequity acknowledges that
#' different households may be income-based differences in the selection of housing that lead to differences in 
#' the cost of energy services. Even beyond income-based constraints, heterogeneous preferences correlated
#' with race or ethnicity may still foment differences in total energy expenditures. By measuring the cost
#' to households to obtain a common level of energy services, the measure accounts for these differences. Remaining
#' differences are not explained by preferences or income. 
#' 
#' 
#' #### Cite severity of problem
#' 
#' 
#' #### Obvious reason: being poor and short-sighted
#' 
#' #### Conditional on income, problem still persists!
#' 
#' #### Potential explanations (brief)
#' This section is brief. Full literature review comes later. Or maybe the full lit for why is here?
#' 
#' #### Causal chain is really long
#' 
#' #### Potential chain (redlining--> lack of ownership --> why stay in area?)
#' 
#' ## This paper here...
#' 
#' #### Explores the role of redlining in explaining energy burden
#' By isolating the energy outcomes that can only be explained by redlining
#' 
#' #### Plausibly exogenous variation
#' Conditional on similar (but not red-lined) nearby neighborhoods. Observables.
#' 
#' #### Extracted 1930's data to understand redlining
#' Survey data created by federal HOLC employees in 1936-1939 provides neighborhood-specific data on observable characteristics that can be used to identify the effects of redlining. The designation of red- and yellow-lined areas (as well as green and blue) aggregate away important variation between each grading. Not all redlined areas are identical, nor are all yellow areas. Survey data recorded as part of the redline designation process provides important, but largely unusued, sources of variation. Within-grade variation in housing is common with reported average rents, median income, repair status of housing, share of housing developed, and construction type. Prior to being redlined, areas with high percentages of low-income or minority populations were more likely to have lower rents and lower housing quality. Frequently, the reason for a low-income or minority area's location was associated with the quality of the geography or proximity to natural features - low-lying areas that frequently flooded or areas too steep for conventional building were generally populated by lower-income individuals. These same features also foment low investment in housing stock, and can explain current inefficiencies in  housing regardless of the HOLC grade. With HOLC survey data in hand, I can control for these features and separate out the effect of HOLC redlining from the determinants of HOLC grading.
#' 
#' #### Identification key: there are neighborhoods with identical economics and racial composition where some surveyors designated them red and some designated yellow.
#' 
#' #### Poor whites vs. poor blacks
#' 
#' #### Historic data merged to block groups to leverage modern outcomes
#' To assess current energy outcomes, I merge original HOLC neighborhoods to modern census block groups, extracting ACS and census indicators of high energy burden. Census block groups match the granularity of HOLC neighborhoods reasonably well. However, boundaries do not tend to coincide exactly, requiring some aggregation.  Once linked, I compare modern energy outcomes with HOLC grading, conditioning on observable characteristics both in the 1930's (selection), and in the present. While an individual household-level analysis would provide the clearest evidence, household-level energy consumption data is limited for privacy reasons, especially at the spatial resolution of HOLC neighborhoods, which can have as few as 100 homes in them.
#' 
#' 
#' #### 
#' 
#' # Data
#' This is the section with the data. 
#' And the section where we process the data




#+ process-1, echo=FALSE, cache = TRUE, warning=F, message=F
    

    # Load files --------------------------------------------------------------
    WORK.OUT = file.path(BASE,'ajk41/Low Income Energy/Data/OUTPUT',Sys.Date())
    dir.create(WORK.OUT, recursive=T)
    
    Processed_HOLC = file.path(BASE, 'ajk41/Low Income Energy/Data/Zillow/Processed_HOLC')
    
    # mykey = "ff90006e6d5f1960ef3bbb37579766bb563b1121"
    # census_api_key(mykey, install=T)
    
    BGZ = readRDS(jLoad(WORK.OUT, 'BG_ZCTA_HOLC_int'))
    BG.int.full = BGZ[[1]]
    ZCTA.int.full = BGZ[[2]]
    HOLC = BGZ[[3]] # readRDS(jLoad(WORK.OUT, 'HOLC_with_data')) # 10-12
    
    # Set in Importing_HOLC_v4.R:
    #  dplyr::mutate(OtherSubstandard = Coal + Wood + LPGas + OtherFuel + NoFuel,
    #    OtherSubstandardNarrow = Coal + NoFuel,
    #    OtherSubstandardMed = Coal + LPGas + FuelOil + OtherFuel + NoFuel,
    #    OtherSubstandardWide = Coal + Wood + OtherFuel + LPGas + FuelOil + NoFuel) %>%
         
        
    
    # Dissolve to BG with data on share in each GRADE ---------
    # Summarize (dissolve) BG and ZCTAs and average HOLC-data within.
    #         - Hopefully a reasonable number of ~100% in A,B,C,or D.
    BG.int = BG.int.full %>% 
      group_by_at(vars(AFFGEOID, TRACTCE, BLKGRPCE, UtilityGas:TotalRace)) %>%   # see https://github.com/r-spatial/sf/wiki/migrating
      summarize(NBlack_YN = max(NBlack_YN, na.rm=T),
                NBlack_PCT = weighted.mean(as.numeric(NBlack_PCT), w = segmentArea, na.rm=T),
                Rent35_Mean = weighted.mean(Rent35_Mean, w=segmentArea, na.rm=T), 
                Rent3739_Mean = weighted.mean(Rent3739_Mean, w = segmentArea, na.rm=T), 
                Minc = weighted.mean(Minc, w=segmentArea, na.rm=T),
                GRADE.A  = sum(segmentArea[GRADE=='A'], na.rm=T)/sum(segmentArea),
                GRADE.B  = sum(segmentArea[GRADE=='B'], na.rm=T)/sum(segmentArea),
                GRADE.C  = sum(segmentArea[GRADE=='C'], na.rm=T)/sum(segmentArea),
                GRADE.D  = sum(segmentArea[GRADE=='D'], na.rm=T)/sum(segmentArea),
                GRADE.NA = sum(segmentArea[is.na(GRADE)], na.rm=T)/sum(segmentArea),
                GRADE.max = replace_na(GRADE[which.max(segmentArea)], 'X'),
                share.max = max(segmentArea, na.rm=T)/sum(segmentArea)) %>% # Note: check that no AFFGEOID are duplicated
      dplyr::mutate(STATEFP = gsub(pattern = '.*US([0-9]{2})([0-9]{3}).*', '\\1', x=AFFGEOID),
                    COUNTYFP = gsub(pattern = '.*US([0-9]{2})([0-9]{3}).*', '\\2', x=AFFGEOID),
                    STCO =     gsub(pattern = '.*US([0-9]{5}).*', '\\1', x=AFFGEOID)) %>%
      dplyr::mutate(isD = GRADE.D>.8,
                    GRADE.max = factor(GRADE.max, levels = c('C','D','B','A','X')),
                    MedIncome1936 = Minc/1000,
                    MedIncome2018 = MedIncome2018/1000)
    
    
    ZCTA.int = ZCTA.int.full %>% dplyr::filter(TotalFuel>0) %>%
      group_by_at(vars(AFFGEOID10, NAME.zip, UtilityGas:TotalRace)) %>%
      summarize(NBlack_YN = max(NBlack_YN, na.rm=T), 
                NBlack_PCT = weighted.mean(as.numeric(NBlack_PCT), w = segmentArea, na.rm=T),
                Rent35_Mean = weighted.mean(Rent35_Mean, w=segmentArea, na.rm=T), 
                Rent3739_Mean = weighted.mean(Rent3739_Mean, w = segmentArea, na.rm=T), 
                Minc = weighted.mean(Minc, w=segmentArea, na.rm=T),
                GRADE.A   = sum(segmentArea[GRADE=='A'], na.rm=T)/sum(segmentArea),
                GRADE.B   = sum(segmentArea[GRADE=='B'], na.rm=T)/sum(segmentArea),
                GRADE.C   = sum(segmentArea[GRADE=='C'], na.rm=T)/sum(segmentArea),
                GRADE.D   = sum(segmentArea[GRADE=='D'], na.rm=T)/sum(segmentArea),
                GRADE.NA  = sum(segmentArea[is.na(GRADE)], na.rm=T)/sum(segmentArea),
                GRADE.max = replace_na(GRADE[which.max(segmentArea)], 'X'),
                share.max = max(segmentArea, na.rm=T)/sum(segmentArea)) %>% # Note: check that no AFFGEOID are duplicated
      st_join(counties(cb=TRUE) %>% dplyr::select(STCO = GEOID) %>% st_transform(st_crs(HOLC)), largest=TRUE) %>%
      dplyr::mutate(STATEFP = substr(x=STCO, start=1, stop=2),
                    COUNTYFP = substr(x = STCO, start = 3, stop=5),
                    isD = GRADE.D>.8,
                    GRADE.max = factor(GRADE.max, levels = c('C','D','B','A','X')),
                    MedIncome1936 = Minc/1000,
                    MedIncome2018 = MedIncome2018/1000,
                    zip = as.character(sapply(strsplit(NAME.zip, ' '), '[', 2)))
    
    
    # stopifnot(sum(duplicated(ZCTA.int$AFFGEOID10))==0)
    stopifnot(sum(duplicated(BG.int$AFFGEOID))==0)
    stopifnot(sum(duplicated(ZCTA.int$AFFGEOID10))==0)

    # end----
#' 
#' 
#+ process-2, echo=FALSE, cache = TRUE, warning=F, message=F
    
    #### Import RASS data ####
    RASS09e = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','ddn_electricbillingdatamodels.csv'), na.strings = c('99')) #?97
    RASS09g = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','ddn_gasbillingdatamodels.csv'), na.strings = c('99'))
    RASS09s = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2009','Survdata.csv'), na.strings = c('99'))
    
    RASS19s = fread(file.path(BASE, 'ajk41','Low Income Energy','Data','CA RASS','CA RASS 2019','Final19_SW_CleanedSurvey.csv'), na.strings = c('99'))
    
    #### Survey data (one obs. per household)
    s09 = RASS09s[,.(IDENT, servzip, avginc, homeage, HOHETH, EDUC, EUTIL, NGUTIL, CZT24, HMRNSET, HDAYSET, HEVNSET, HNITESET,
                     NAC_KWH, NAC_Therms, res, rescnt, kids, adults, seniors, NGSERV, OWNRENT, YRS_RES, BUILTYR, 
                     SQFT, PAYHEAT, PHTNGCNT, PHTNGFWL, PHTNGRAD, PHTNGFP, PHTNGOTH, PHTELBSB, PHTELCRH, PHTELCHP, 
                     PHTELWHP, PHTELPOR, PHTELOTH, PHTBGCNT)][,c('zip','wave','ethnicity'):=list(sprintf('%05d', servzip), 2009,
                                                                                                 factor(HOHETH, levels = c(1:7),
                                                                                                        labels = c('Native American','Asian and Pacific Islander',
                                                                                                                   'Black','Hispanic','White','Other','Mixed')))]
    
    s19 = RASS19s[,.(IDENT, servzip, avginc, homeage, HOHETH, EDUC, EUTIL = eutil, NGUTIL, CZT24, HMRNSET, HDAYSET, HEVNSET, HNITESET,
                     NAC_KWH, NAC_Therms, res, rescnt, kids, adults, seniors, NGSERV, OWNRENT, YRS_RES, BUILTYR, 
                     SQFT, PAYHEAT, PHTNGCNT, PHTNGFWL, PHTNGRAD, PHTNGFP, PHTNGOTH, PHTELBSB, PHTELCRH, PHTELCHP, 
                     PHTELWHP, PHTELPOR, PHTELOTH, PHTBGCNT)][,c('zip','wave','ethnicity'):=list(sprintf('%05d', servzip), 2019,
                                                                                                 factor(HOHETH, levels = c(1:7, 97),
                                                                                                        labels = c('Native American','Asian Pacific Islander',
                                                                                                                   'Black','Hispanic','White','Other','Mixed',
                                                                                                                   'No Answer')))]
    
    
    #### Consumption data (nest to IDENT)
    e09.use = melt(RASS09e , id.vars = c('IDENT'), measure = patterns("^r","^d","^u","^hdd[0-9]+","^cdd[0-9]+"), value.name = c('r','d','u','hdd','cdd')) %>%
      dplyr::filter( !is.na(r) & !is.na(d) & !is.na(u)) %>%
      dplyr::mutate(r = mdy(r),
                    u = as.numeric(gsub(',','',u))) %>%
      dplyr::filter(!is.na(u) & !is.na(hdd) & !is.na(cdd)) %>%
      dplyr::arrange(IDENT, variable, r) %>%
      dplyr::select(IDENT, r, d, hdd, cdd, u) %>%
      group_by(IDENT) %>% dplyr::filter(n()>1 & sd(u)>0) %>%
      nest(edata = -IDENT)
      
    
    g09.use = melt(RASS09g , id.vars = c('IDENT'), measure = patterns("^r","^d","^u","^hdd[0-9]+","^cdd[0-9]+"), value.name = c('r','d','u','hdd','cdd')) %>%
      dplyr::filter( !is.na(r) & !is.na(d) & !is.na(u) & d>=0) %>%
      dplyr::mutate(r = mdy(r),
                    u = as.numeric(gsub(',','',u))) %>%
      dplyr::filter(!is.na(u) & !is.na(hdd) & !is.na(cdd)) %>%
      dplyr::arrange(IDENT, variable, r) %>%
      dplyr::select(IDENT, r, d, hdd, cdd, u) %>%
      group_by(IDENT) %>% dplyr::filter(n()>1 & sd(u)>0) %>%
      nest(gdata = -IDENT)
    
    
    ### Household-level CDD and HDD responses in electricity and gas:
    e09.use  = e09.use %>%
      dplyr::mutate(coefrun = map(edata, ~feols(u ~ hdd + cdd, weights=.$d, data = ., warn=F, notes = F) %>%
                                    tidy() %>%
                                    dplyr::select(term, estimate) %>%
                                    spread(term, estimate))) %>%
      unnest(coefrun) %>%
      dplyr::select(IDENT, intercept.e = `(Intercept)`, cdd.e = cdd, hdd.e = hdd, edata) %>%  ungroup() %>%
      dplyr::mutate_at(.vars = vars(intercept.e:hdd.e), .funs = ~ Winsorize(., probs=c(.02, .98), na.rm=T))
    
    g09.use  = g09.use %>%
      dplyr::mutate(coefrun = map(gdata, ~feols(u ~ hdd + cdd, weights=.$d, data = ., warn = F, notes = F) %>%
                                    tidy() %>%
                                    dplyr::select(term, estimate) %>%
                                    spread(term, estimate))) %>%
      unnest(coefrun) %>%
      dplyr::select(IDENT, intercept.g = `(Intercept)`, cdd.g = cdd, hdd.g = hdd, gdata) %>% ungroup() %>%
      dplyr::mutate_at(.vars = vars(intercept.g:hdd.g), .funs = ~ Winsorize(., probs=c(.02, .98), na.rm=T))
    
    
    all09.use =  inner_join(s09, full_join(e09.use, g09.use, by=c('IDENT')), by='IDENT')
    # avginc,
    # cdd.e -> coef response to cooling degree days (should be positive); 
    # cdd.g -> coef response to cooling degree days (should be negative or zero)
    # hdd.e -> coef response to heating degree days (should be positive esp if e heat)
    # hdd.g -> coef response to heating degree days (should be very positive)
    
    #--> CAZ: one record per zip code (283 zips). Each record has census (race, income TotalFuel etc.) plus HOLC (aggregated NBlack, Rent, etc.) and share of GRADE
    ##        Plus GRADE.max and share.max.
    ##        adata has household RASS responses, and those have in them gdata and edata
    CAZ = ZCTA.int %>% 
      st_set_geometry(NULL) %>% as_tibble() %>% 
      inner_join(as_tibble(all09.use) %>% group_by(zip) %>% nest(adata = -(zip)), by='zip') %>%
      dplyr::mutate(zipcount = map_int(adata, .f = ~ NROW(.)))
    
    ##--> CAS: unnested version of CAZ without a/e/gdata, with one row per household and Hh level (not consumption) data.
    ##         For household fuel source regressions.
    CAS = ZCTA.int %>% 
      st_set_geometry(NULL) %>% as_tibble() %>% 
      inner_join(bind_rows(s09, s19), by='zip')

                 
                 
    
    
#' 
#' ## Census blockgroups
#' 
#  /*   COMMENT OUT THIS SECTION
#' HOLC grading and survey data are available at the neighborhood level, where the average neighborhood size is approximately `r prettyNum(round(mean(st_area(BG.int)/(1000^2)), 2), big.mark=',')` square kilometers. 
#' 
#' 
#' ### Substandard Heating Fuel by Income and Ethnicity
#' 
#' I first examine the incidence of energy burden on minority populations. 
#' A model of home selection would clearly predict a relationship between energy burden and income as low-income individuals trade off 
#' energy efficient (yet higher cost) housing for lower-cost but energy-inefficient housing. 
#' Since a home's energy efficiency is part of a bundle of attributes, households with low income may often trade 
#' energy efficiency for other properties, such as larger square footage. 
#' Lower efficiency homes may be more expensive to heat to a comfortable standard. 
#' However, households with severe budget constraints may select a low-efficiency house and select to spend as little as possible on heating, 
#' resulting in very low inside temperatures, lower housing costs, and low energy expenditures. An examination of heating fuel source
#' does not speak directly to energy burden, but rather identifies income or racial groups that tend to 
#' have poor heating infrastructure and thus either bear low indoor temperatures during cold weather, or spend
#' higher amounts using old, inefficient, or piecemeal heating sources like space heaters.
#' 
#' At the block-group level, I regress the fraction of households reporting substandard home heating fuel. I define
#' substandard home heating fuel in multiple ways:
#' 
#' \begin{itemize}
#' \item[Wide]: Coal, Wood, Liquid Propane Gas, Fuel Oil, Other, or None
#' \item[Med]: Coal, Liquid Gas, Fuel Oil, Other or None
#' \item[Original]:  Coal. Wood, Liquid Propane Gas, Other, or None
#' \item[Narrow]: Coal and None 
#' \end{itemize}
#' 
#' 
#' \begin{eqnarray*}
#' SubstandardHeating_{i} = \beta_0 + \beta_1 MedInc_b + \beta_2 X_b + \beta_3 MedInc_b X_b + \Gamma_{c(b)} \epsilon_i  \nonumber
#' \end{eqnarray*}
#' 
#' Here, $X_b$ includes median income in 2018 and shares of Black, White, Asian and Other Race, plus interactions. Because the 
#' data is at the census block-group level $b$, I include county FIPS fixed effects for county $c(b)$. Census block-groups do not
#' perfectly align with HOLC neighborhoods. I calculate the share of each block-group within a HOLC neighborhood and include only those census
#' block-groups that coincide more than 80\% with a single HOLC grade. In Column 9, below, I increase the treshold to 95\%. This specification
#' does not use HOLC grade, but 
#' 
#' 
#+ substandard-est-1, echo=FALSE, fig.width=7, cache = TRUE, result='as.is', message=F, warning=F

    inc2.blank = feols(fml = OtherSubstandard ~ MedIncome2018*Black + MedIncome2018*White + Asian + OtherRace | STCO, 
                      data = BG.int %>% dplyr::filter(TotalFuel>298) %>% st_set_geometry(NULL))
    inc1.blank = feols(fml = OtherSubstandard ~ MedIncome2018 | STCO, 
                      data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))
    
    inc2.wide = feols(fml = OtherSubstandardWide ~ MedIncome2018*Black + MedIncome2018*White + Asian + OtherRace | STCO, 
                       data = BG.int %>% dplyr::filter(TotalFuel>298) %>% st_set_geometry(NULL))
    inc1.wide = feols(fml = OtherSubstandardWide ~ MedIncome2018 | STCO, 
                       data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))
    
    inc2.med = feols(fml = OtherSubstandardMed ~ MedIncome2018*Black + MedIncome2018*White + Asian + OtherRace | STCO, 
                      data = BG.int %>% dplyr::filter(TotalFuel>298) %>% st_set_geometry(NULL))
    inc1.med = feols(fml = OtherSubstandardMed ~ MedIncome2018 | STCO, 
                      data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))
    
    inc2.narrow = feols(fml = OtherSubstandardNarrow ~ MedIncome2018*Black + MedIncome2018*White + Asian + OtherRace | STCO, 
                     data = BG.int %>% dplyr::filter(TotalFuel>298) %>% st_set_geometry(NULL))
    inc1.narrow = feols(fml = OtherSubstandardNarrow ~ MedIncome2018 | STCO, 
                     data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))
    #inc95.narrow = feols(fml = OtherSubstandardNarrow ~ MedIncome2018 | STCO, 
    #                    data = BG.int %>% dplyr::filter(share.max>.95 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))
    
    
    modelsummary(models = list(inc1.wide, inc2.wide,inc1.blank, inc2.blank, 
                               inc1.med, inc2.med, inc1.narrow, inc2.narrow), se = 'cluster',
                 output = 'kableExtra',  # instead of latex so that I can use kable_styling(latex_options = 'scale_down') for the table
                 stars=TRUE, fmt = '%.5f',
                 title = 'Share of Households with Substandard Fuel by Ethnicity and Income',
                 notes = 'Robust SE clustered by FIPS county') %>%  # output as kableExtra
      add_header_above(c('Variable' = 1, 'Wide' = 2, 'Original' = 2, 'Medium' = 2, 'Narrow' = 3)) %>%
      kable_styling(latex_options = 'scale_down') # to use this kableExtra option!
    
    #---end
 
#' #### Results
#' 
#' Nothing particularly good here. Odd that all ethnicities have a negative effect - am I missing something here?
#' 
#  */
#' 
#' 
#' \newpage
#' 
#' ### Substandard heating fuel by HOLC grade
#' 
#+ substandard-est-2, echo=FALSE, fig.width=7, result='as.is', message=F, warning=F, cache = TRUE
#
  # With OthersubstandardNarrow #
BG.list1 = list(
  
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) + MedIncome2018 + MedIncome1936 + Rent3739_Mean | STCO,
                  data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL)),
    
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) + MedIncome2018*MedIncome1936 + Rent3739_Mean | STCO,
                  data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))  ,  
    
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) + MedIncome2018*MedIncome1936 + Rent3739_Mean | STCO,
              data = BG.int %>% dplyr::filter(share.max>.95 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL)),

  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black + White + Asian | STCO, 
              data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black*as.factor(GRADE.max) + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black + White + Asian | STCO, 
              data = BG.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL)),
  
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
              data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardNarrow ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
              data = BG.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL))
)

modelsummary(models = BG.list1, se = 'cluster',
             output = 'kableExtra',  # instead of latex so that I can use kable_styling(latex_options = 'scale_down') for the table
             stars=TRUE, fmt = '%.5f',
             add_rows = tibble::tribble(~term, ~one, ~two, ~three, ~f, ~fi, ~si, ~se, ~ei,
                                        'HOLC Grade Threshold', '80%','80%', '95%', '80%', '80%', '95%', '80%', '95%'),
             title = 'Share of Households with OtherSubstandardNarrow Fuel (Coal and none) by HOLC Grade\\label{tab:substandardnarrow1}',
             notes = list('Robust SE clustered by FIPS county','Columns 1, 2, 3, 5 are blokgroups with >80% of total area in one HOLC grade; 4, 6, and 8 are >95%')) %>%  # output as kableExtra
  kable_styling(latex_options = 'scale_down') # to use this kableExtra option!

#'
#' #### Results
#' 
#' Column 1 allows an additive effect for median income in 2018 and 1936 (reported from HOLC surveys), while 2-8 allow an interaction. 
#' 
#' As expected, current median blockgroup income predicts a lower share of homes with substandard heating fuel across all models. 
#' Median income in 1936 predicts \textbf{higher} prevalance of substandard heating in higher 1936 median income areas. 
#' 
#' Columns 4-7 include controls for current racial composition. 
#' These results consistently find that higher percentages of Blacks are associated with lower likelihood of substandard heating fuel. \textbf{EXPLAIN!!}
#' 
#' The coefficient on HOLC Grade D is positive and significant across almost all specifications, indicating that, conditional on a rich set of controls 
#' including 1936 characteristics to control for selection on observables, areas that were graded "D" by the HOLC are more likely to have substandard 
#' heating fuels in 2018 relative to those graded "C".




#+ substandard-est-3, echo=FALSE, fig.width=7, result='as.is', message=F, warning=F, cache = TRUE

      # Try with OtherSubstandardWide #
BG.list2 = list(
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) + MedIncome2018 + MedIncome1936 + Rent3739_Mean | STCO,
        data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL)),
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) + MedIncome2018*MedIncome1936 + Rent3739_Mean | STCO,
        data = BG.int %>% dplyr::filter(share.max>.8 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))  ,  
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) + MedIncome2018*MedIncome1936 + Rent3739_Mean | STCO,
        data = BG.int %>% dplyr::filter(share.max>.95 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL)),
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black*as.factor(GRADE.max) + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL)),
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
  
  feols(fml = OtherSubstandardWide ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
        data = BG.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL))
)

modelsummary(models = BG.list2, se = 'cluster',
             output = 'kableExtra',  # instead of latex so that I can use kable_styling(latex_options = 'scale_down') for the table
             stars=TRUE, fmt = '%.5f',
              add_rows = tibble::tribble(~term, ~one, ~two, ~three, ~f, ~fi, ~si, ~se, ~ei,
                                    'HOLC Grade Threshold', '80%','80%', '95%', '80%', '80%', '95%', '80%', '95%'),
             title = 'Share of Households with OtherSubstandardWide Fuel (Coal + Wood + OtherFuel + LPGas + FuelOil + NoFuel) by HOLC Grade',
             notes = list('Robust SE clustered by FIPS county','Columns 1, 2, 3, 5 are blokgroups with >80% of total area in one HOLC grade; 4, 6, and 8 are >95%')) %>%  # output as kableExtra
  kable_styling(latex_options = 'scale_down') # to use this kableExtra option!

#' 
#' #### Results
#' The same phenomenon is not observed when defining substandard heating as including LP gas and fuel oil. In many parts
#' of the country, LP gas and fuel oil are commonplace, and can be relatively efficient and desirable. It is not wholly
#' unexpected for this to be insignificant.
#' 

#+ substandard-est-4zcta, eval = FALSE, echo=FALSE, fig.width=7, result='as.is', message=F, warning=F, cache = cacheoption
    # NOT EVAL: Eval is turned off, no ZCTA's are in the data after dropping NA's from HOLC and filtering down to 80% coverage.
    # ZCTA is just not viable. Perhaps for CA with RASS data?
# ZCTA.list= list(
#   
#   feols(fml = OtherSubstandard ~ as.factor(GRADE.max) + MedIncome2018 + MedIncome1936 + Rent3739_Mean | STATEFP ,
#         data = ZCTA.int %>% dplyr::filter(share.max>.7 & !is.na(GRADE.max) & GRADE.max!='X') %>% st_set_geometry(NULL))   ,
# 
#   
#   feols(fml = OtherSubstandard ~ as.factor(GRADE.max) +  MedIncome2018 + MedIncome1936 + Rent3739_Mean | STATEFP, 
#         data = ZCTA.int %>% dplyr::filter(share.max>.7  & GRADE.max!='X') %>% st_set_geometry(NULL))  
#   
#   # feols(fml = OtherSubstandard ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + Black + White + Asian | STCO, 
#   #       data = ZCTA.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL)),
#   
# #  feols(fml = OtherSubstandard ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
#   #      data = ZCTA.int %>% dplyr::filter(share.max>.8  & GRADE.max!='X') %>% st_set_geometry(NULL))  ,
#   
# #  feols(fml = OtherSubstandard ~ as.factor(GRADE.max) +  MedIncome2018*MedIncome1936 + Rent3739_Mean + NBlack_YN + NBlack_PCT + Black + White + Asian | STCO, 
#     #    data = ZCTA.int %>% dplyr::filter(share.max>.95 & GRADE.max!='X' ) %>% st_set_geometry(NULL))
# )
# 
# modelsummary(models = ZCTA.list, se = 'cluster',
#              output = 'kableExtra',  # instead of latex so that I can use kable_styling(latex_options = 'scale_down') for the table
#              stars=TRUE, fmt='%.5f',
#              title = 'Share of Households with OtherSubstandardWide Fuel (Coal + Wood + OtherFuel + LPGas + FuelOil + NoFuel) (by ZCTA)',
#              notes = 'Robust SE clustered by FIPS county') %>%  # output as kableExtra
#   kable_styling(latex_options = 'scale_down') # to use this kableExtra option!

#' 
#' #### Results using Zip Code Tabulation Area (ZCTA)
#' 
#' Zipcode Tabulation Areas are coarser than census blockgroups. 
#' Due to this, there are fewer ZCTAs that fall predominantly in one HOLC grade, 
#' resulting in a smaller sample size. Of the `r comma(NROW(unique(ZCTA.int$AFFGEOID10)))` zip 
#' codes that touch on one or more HOLC neighborhoods, `r ZCTA.int %>% st_set_geometry(NULL) %>% dplyr::filter(share.max>.8 & GRADE.max!='X' & !is.na(GRADE.max)) %>% distinct(AFFGEOID10) %>% count() %>% pull(n) %>% comma()` zip codes have greater than 80% within one HOLC grade. Of these, `r ZCTA.int %>% st_set_geometry(NULL) %>% dplyr::filter(GRADE.max=='D' & share.max>.8 & GRADE.max!='X' & !is.na(GRADE.max)) %>% distinct(AFFGEOID10) %>% count() %>% pull(n) %>% comma()` are Grade D (red). Of these, only `r ZCTA.int %>% st_set_geometry(NULL) %>% dplyr::filter(GRADE.max=='D' & share.max>.8 & GRADE.max!='X' & !is.na(GRADE.max) &!is.na(MedIncome1936) &  !is.na(Rent3739_Mean)) %>% distinct(AFFGEOID10) %>% count() %>% pull(n) %>% comma()` zip code(s) have HOLC survey data including median income, presence of minorities, and rent data for 1936-38. This precludes the use of zip code aggregations to estimate HOLC grading on current substandard heating fuel.
#'
#'
#'
#'
#' ## Household-level energy consumption (CA-RASS)
#' 
#' Aggregation can mask important heterogeneity in household energy consumption. We use household-level monthly consumption data geolocated to the zip-code level to identify HOLC-zone specific 
#' energy consumption responses to cooling and heating events. 
#' 
#' Household energy consumption data is retrieved from the California Residential Appliance Saturation Survey (RASS) for 2009 and (hopefully) 2019. The RASS
#' is commissioned by the California Public Utilities Commission and implemented by DNV GL Energy Insights. The survey contains information on household
#' energy consumption including home age, primary heating fuel source, and thermostat setpoint. The survey also includes the household's zip code, 
#' household characteristics including income and number of children, and merges two years of billing information obtained direclty from the gas and electric
#' utilities serving the household. The survey sample consists of `r NROW(s09)` households sampled from all over California.
#' 
#' We are interested in household's energy consumption response to increases in heating degree days. We focus on households that rely primarily on electric heating. For each household, we
#' estimate a consumption response function that summarizes the household's change in electricity consumption per change in monthly heating degree days. 
#' This measure will be larger if a household consumes more energy when temperatures are lower, and smaller if a household consumes less energy when temperatures drop.
#' I allow this response to vary bsed on the HOLC grade that covers the plurality of the zip code in which the household lies. I use only those zip codes in which greater than 80\% 
#' of the zip code is within one specific HOLC grade.
#'  
#' *A priori*, it is not clear whether the interaction of heating degree days and HOLC-designated redlining should be positive or negative. If a household 
#' prefers to remain warm and comfortable on a cold night, then expenditures will be higher when temperatures are lower. Similarly, if a household in a redlined area
#' maintains the same indoor temperature setpoint but has a home with lower energy ratings or is otherwise less efficient, then energy expenditures will be greater as well.
#' If expenditures are not greater, then it may be that the household trades off comfort by lowering the indoor temperature in order to keep expenditures low, or
#' it may be that the home is very efficient, and more energy is not needed to maintain a constant temperature. This ambiguity confounds interpretation of estimates.
#' 
#'
#' Table (below) reports the results from a regression of the form:
#' 
#' $$hdd^e_h = \beta_0 + \sum 1(grade_h=g)\beta_g + \beta_{inc} avgincome_h + cdd^e_h + \gamma^{CZ} + \varepsilon$$
#' 
#' Where $hdd^e$ is the electricity consumption response to one additional heating degree day for household $h$ located in HOLC grade $g$. $cdd^e$ is the household's electricity consumption response
#' to an additional cooling degree day. Table (below that) shows results for $hdd^g$. $\gamma^{CZ}$ are climate-zone fixed effects.
#' 
#' 
#+ RASS-1setup, echo=F, cache=TRUE, warning=F, message = F
all09.use = inner_join(s09, full_join(e09.use, g09.use, by=c('IDENT')), by='IDENT') %>% 
  left_join(ZCTA.int %>% st_set_geometry(NULL) %>% 
              dplyr::mutate(servzip = as.integer(zip)) %>%
              dplyr::select(GRADE.max, GRADE.A:GRADE.NA, MedIncome1936, NBlack_PCT, NBlack_YN, Rent3739_Mean, Minc, share.max, servzip),
                                    by = 'servzip')

SET.codebook = data.frame(HNITESET = c(1:7, 97),
                          NITESET = c('Off','<55','55-60','60-65','65-70','70-75','>75','Unk')) %>%
  dplyr::mutate(NITESET = factor(NITESET, levels = c('Off','<55','55-60','60-65','65-70','70-75','>75','Unk')))

all09.use = all09.use %>% left_join(SET.codebook, by = 'HNITESET') %>%
  replace_na(list(NITESET = 'Unk')) %>%
  dplyr::mutate(NITESET = factor(NITESET)) %>%
  left_join(SET.codebook %>% dplyr::select(HDAYSET = HNITESET, DAYSET = NITESET), by = 'HDAYSET') %>%
  replace_na(list(DAYSET = 'Unk')) %>%
  dplyr::mutate(DAYSET = factor(DAYSET))

all09.use = all09.use %>% dplyr::mutate(GRADE.maxl = factor(GRADE.max, levels = c('C','X','D','A','B')))

#'
#'
#+ RASS-1ex, echo=F, cache=TRUE, warning=F, message = F

  # Do the responses (coefs. cdd.e, cdd.g, hdd.e, hdd.g) vary by redlining?
  # # What if we control for cdd.e response (AC usage, which tells us about building envelope)
modelsummary(list(feols(hdd.g ~ GRADE.max + avginc | CZT24, all09.use %>% dplyr::filter(share.max >.80 & GRADE.max%in%c('C','D','X')) %>% unnest(gdata) %>% dplyr::filter(PHTNGCNT==1|PHTNGRAD==1) %>% dplyr::mutate(GRADE.max = fct_relevel(GRADE.max, 'X','D','C')), cluster = 'CZT24'),
                  feols(hdd.g ~ GRADE.max + avginc | CZT24, all09.use %>% dplyr::filter(share.max >.80 & GRADE.max%in%c('C','D','X')) %>% unnest(gdata) %>% dplyr::filter(PHTNGCNT==1|PHTNGRAD==1) %>% dplyr::mutate(GRADE.max = fct_relevel(GRADE.max, 'X','D','C')), cluster = 'CZT24')),
             stars=T,
             title = 'Regression of heating-degree day gas consumption response on HOLC grade and income\\label{tab:responsegas1}',
             notes = c('Using only households with gas as primary heating fuel','Omitted grade is refactored to be "X" '))


modelsummary(list(feols(hdd.e ~ as.factor(GRADE.max) + avginc | CZT24, all09.use %>% dplyr::filter(share.max >.80) %>% unnest(edata) %>% dplyr::filter(PHTELCRH==1|PHTELBSB==1)%>% dplyr::mutate(GRADE.max = fct_relevel(GRADE.max, 'X','D','C')), cluster = 'CZT24'),
                  feols(hdd.e ~ as.factor(GRADE.max) + avginc + cdd.e | CZT24, all09.use %>% dplyr::filter(share.max >.80) %>% unnest(edata) %>% dplyr::filter(PHTELCRH==1|PHTELBSB==1)%>% dplyr::mutate(GRADE.max = fct_relevel(GRADE.max, 'X','D','C')), cluster = 'CZT24')),
             stars=T,
             title = 'Regression of heating-degree day electricity consumption response on HOLC grade and income\\label{tab:responseelectric1}',
             notes = c('Using only households with electricity as primary heating fuel','Omitted grade is actually "X" as all "C" is lost in gas'))

#'
#'
#' Coefficient results in \ref{tab:responsegas1}, Model (1) indicate that households located inside the HOLC Grade D (red) areas 
#' have significantly higher gas responses relative to those in HOLC ungraded areas within the urban boundary and relative
#' to those in HOLC Grade C. The omitted factor in \ref{tab:responsegas1} is ungraded so that results on Grade D can be 
#' compared directly to \ref{tab:responseelectric1}, which has insufficient data to compare to Grade C.
#' 
#' Table \ref{tab:responseelectric1} shows significantly lower electricity responses.
#' In each case, only households that report primary heating fuel of gas (first table) and electricity (second table) are included. 
#' The ambiguity in effect is clear in examining Table \ref{tab:responseelectric1}, which shows a significantly *lower* 
#' effect within the HOLC red (D) areas. In Grade D (red) areas, households using natural gas for their primary heating fuel
#' tend to have lower response to heating degree days relative to those in Grade C.
#' 
#' Lower response to heating degree days can be generated by two competing explanations. First, households in Grade D areas may
#' have more efficient homes and heating systems. With a sealed building envelope and modern gas heaters, the cost
#' to maintain the temperature inside may be low relative to areas with drafty windows and older heating systems.
#' A competing explanation is that Grade D areas are far less efficient, heating is more expensive on the margin, and
#' households in these less efficient homes compensate by lowering the thermostat setpoint. A home set at 55 degrees overnight
#' will consume less energy than a home set at 75 degrees, even if it is less efficient. 
#' 
#' To address this confounding, I leverage the household's response to the RASS survey question on thermostat setpoint and build a \
#' model of household energy consumption.
#' 
#' ### A model of heating energy consumption
#' Let heating energy consumed be determined by $H(\tau_i, T_i; \phi_i)$, where $\tau_i$ is the household's thermostat setpoint, $T_i$ is the outside ambient temperature around household $i$, and $\phi_i$ is a parameter summarizing the envelope and appliance efficiency
#' of household $i$. Households with more efficient insulation, dual-pane windows, and higher energy star rated heating systems will have a lower value of $\phi_i$. In a simple 
#' linear form, the cost of heating a home with setpoint $\tau_i$ in month $J$ is:
#' 
#' \begin{eqnarray}
#' H = \phi_0 + \phi_i \sum_{d=1}^{D_m}(\tau_i - T_{id}) \label{eq:hdd1}
#' \end{eqnarray}
#' 
#' The sum of ambient outdoor temperature deviations $T_{id}$ from the thermostat setpoint $\tau_i$ represents the sum of the temperature gradient between indoor and outdoor temperature.
#' We do not observe the ambient outdoor temperature for each household, but do observe the heating degree days, which are $\sum_{d=1}^{D_m}(\tau_{r(i)} - T_{id})$, where $\tau_r$ is the 
#' heating degree days (HDD) reference temperature. The reference temperature varies by climate zones but is not disclosed in the data. However, it is
#' constant for each household across all time periods in the data. Rewriting \ref{eq:hdd1}:
#' 
#' \begin{eqnarray}
#' H &=& \phi_0 + \phi_i \sum_{d=1}^{D_m}\tau_i + \phi_i \sum_{d=1}^{D_m} \tau_r - \phi_i \sum_{d=1}^{D_m} T_{id} - \phi_i \sum_{d=1}^{D_m} \tau_r + \epsilon \nonumber \\
#'  &=& \phi_0 + \phi_i \sum_{d=1}^{D_m}(\tau_{r(i)} - T_{id}) + \phi_i \sum_{d=1}^{D_m}(\tau_i-\tau_{r(i)}) + \epsilon \nonumber \\
#'  &=& \phi_0 + \phi_i \sum_{d=1}^{D_m}(HDD_{id}) + \kappa_{i} + \epsilon \label{eq:hdd2}
#' \end{eqnarray}
#' 
#' The household thermostat setpoint $\tau_i$ is time invariant. Thus, the household fixed effect absorbs the difference between the reference temperature and the thermostat
#' setpoint. If heating costs are not linear in deviations from $\tau_{r(i)}$, $\phi_i$ will capture both the household-specific efficiency, as well as the difference in 
#' heating costs per HDD conditional on $\tau_i$. For instance, if a household maintains a very high $\tau_i>\tau_{r(i)}$ and faces convex costs in $T_j$, $\phi_i$ will 
#' capture this difference. We estimate $\phi_i$ over HOLC grades and household income.  
#' In our most flexible specification, we estimate a common $\phi_i$ for each HOLC grade and for each value of $\tau_i$ reported in the data. 
#' 
#' Conditioning on a specific overnight or daytime thermostat setting forecloses
#' the possibility that households adjust thermostat setting downward to reduce consumption since the adjustment is answered in the question. 
#' Household consumption response functions for those that save on heating by setting the thermostat
#' setpoint very low are identified by variation conditional on their setpoint. By allowing each setpoint bin 
#' (<55, 55-60, 60-65, 65-70, 70-75, 75+) to have a separate estimate of response to heating degree days, 
#' I capture the effect of heating degree days separate from thermostat setpoint adjustments. Once properly conditioned, 
#' the difference between HOLC Grade D (red) and HOLC Grade C (yellow) housing can be estimated.
#' 
#' $$usage^g_h = \phi_0 + \phi_1 hdd_h + \phi_2 hdd_h*(grade_h=D) + \phi_3 averageincome_h*hdd_h + \phi_4 cdd^e_h*hdd_h + \sum_{s=1}^{S} hdd_h*\theta^s + \kappa_h + \varepsilon$$
#' 
#' Where $usage^g_h$ is the monthly observed gas usage, $hdd_h$ is the household's monthly heating degree days, $s\in S$ are the temperature setpoint bins, and $\kappa_h$ is a vector of household 
#' fixed effects.

#+ RASS-1b, message=F, warning=F, echo = F, cache = TRUE

# What if we pool all of the monthly bill observations? Turns out, there are very few (if any) NG-using Hh's in D!  dplyr::filter(cdd<=10 & (PHTNGCNT==1|PHTNGRAD==1))
all09.use.pooled = all09.use %>% unnest(edata) %>% dplyr::filter(share.max > .90 & cdd<=0 & (PHTELCRH==1|PHTELBSB==1)) %>%
  dplyr::mutate(NITESET = relevel(NITESET, ref = '>75'),
                DAYSET = relevel(DAYSET, ref = '>75'),
                ethnicity = relevel(ethnicity, ref = 'White'),
                avginc1000 = avginc/1000)

modelsummary(list(
  feols(u ~ hdd + hdd:GRADE.max + hdd:avginc | IDENT,             weights=~d, data = all09.use.pooled, warn = F), # Do redlined areas have diff. responses?
  feols(u ~ hdd + hdd:GRADE.max + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled, warn = F) , # Do redlined areas have diff. responses controlling for building envelope (w/AC usage)
  feols(u ~ hdd + hdd:NITESET + hdd:GRADE.max + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:DAYSET + hdd:GRADE.max + hdd:avginc + hdd:cdd.e | IDENT,  weights=~d, data = all09.use.pooled, warn = F) ,# Or daytime setting
  feols(u ~ hdd + hdd:DAYSET + hdd:NITESET + hdd:GRADE.max + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled, warn = F),
  feols(u ~ hdd + hdd:DAYSET + hdd:NITESET + hdd:GRADE.max + hdd:avginc  | IDENT, weights=~d, data = all09.use.pooled, warn = F),
  feols(u ~ hdd + hdd:DAYSET:as.factor(CZT24) + hdd:NITESET:as.factor(CZT24) + hdd:GRADE.max + hdd:avginc  | IDENT, weights=~d, data = all09.use.pooled, warn = F),
  feols(u ~ hdd + hdd:DAYSET + hdd:NITESET + hdd:GRADE.max + hdd:avginc  | IDENT, weights=~d, data = all09.use.pooled %>% dplyr::filter(DAYSET !='Unk' & NITESET !='Unk'), warn = F)),# Or daytime setting
  stars = T,
  # coef_omit = c('CZT24'),
  title = 'Regression of electricity consumption on heating degree days, interacted with HOLC grade and income, conditional on thermostat setpoint\\label{tab:electricresponsethemset1}',
  notes = c('Using only households with electricity as primary heating fuel','Omitted grade is "C"'),
  output = 'kableExtra') %>%
  kable_styling(latex_options = 'scale_down')



#'
#'
#' Results have the expected sign - an increase in the heating degree days leads to an increase in electricity consumption across each of the specifications. 
#' Households located inside a HOLC Grade D (red) area show around two to three times the consumption per hdd relative to the omitteed category, Grade C. 
#' Unfortunately, too few households lie in HOLC areas with reported covariates necessary to control
#' for unobservables that may have affected the treatment (assignment to HOLC red) and the current outcome (home efficiency / gas consumption per hdd).  
#' Column (3) through (7) control for the reported nighttime and daytime temperature setpoints. In both 3 and 4, the main effect remains (and increases in magnitude) - conditional on a target setpoint, 
#' an increase in $hdd$ leads to an increase in usage. 
#' Although insignificant, the interaction for the two lowest bins, 55-60 and 60-65, is negative, indicating that an increase in $hdd$ for households with very low overnight setpoints
#' leads to smaller increases in ELECTRICITY consumption relative to the omitted category, which is "off/other". As expected, households with very high overnight setpoints (>75) have very high 
#' consumption responses to $hdd$. In the last column, unknown thermostat setpoints are dropeed from the data. The effect of HOLC Grade D (red) persists.
#' 
#' Notably, households within the HOLC red (D) area continue to exhibit two to three times the consumption response relative to households in the omitted category, even accounting for
#' potentially heterogeneous preferences for overnight temperature setpoints. This indicates that households in HOLC red areas are not simply exhibiting greater preference
#' for overnight comfort, but rather face higher expenditures simply to maintain one constant temperature.
#'
#'
#'
#'
#+ RASS-1bGas, message=F, warning=F, echo = F, cache = TRUE
all09.use.pooled.g = all09.use %>% unnest(gdata) %>% dplyr::filter(share.max > .90 & cdd<=0 & (PHTNGCNT==1|PHTNGRAD==1)) %>%
  dplyr::mutate(NITESET = relevel(NITESET, ref = '>75'),
                DAYSET = relevel(DAYSET, ref = '>75'),
                ethnicity = relevel(ethnicity, ref = 'White'),
                GRADE.maxl = fct_relevel(GRADE.max, 'C','X','D'),
                avginc1000 = avginc/1000)

modelsummary(list(
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:avginc | IDENT,             weights=~d, data = all09.use.pooled.g, warn = F), # Do redlined areas have diff. responses?
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled.g, warn = F) , # Do redlined areas have diff. responses controlling for building envelope (w/AC usage)
  feols(u ~ hdd + hdd:NITESET + hdd:GRADE.maxl + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled.g, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:DAYSET + hdd:GRADE.maxl + hdd:avginc + hdd:cdd.e | IDENT,  weights=~d, data = all09.use.pooled.g, warn = F) ,# Or daytime setting
  feols(u ~ hdd + hdd:DAYSET + hdd:NITESET + hdd:GRADE.maxl + hdd:avginc  | IDENT, weights=~d, data = all09.use.pooled.g, warn = F),
  feols(u ~ hdd + hdd:DAYSET + hdd:NITESET + hdd:GRADE.maxl + hdd:avginc + hdd:cdd.e | IDENT, weights=~d, data = all09.use.pooled.g, warn = F)),# Or daytime setting
  stars = T,
  title = 'Regression of natural gas consumption on heating degree days, interacted with HOLC grade and income, conditional on thermostat setpoint\\label{tab:gasresponsethemset1}',
  notes = c('Using only households with natural gas as primary heating fuel','Omitted grade is "C"'),
  output = 'kableExtra') %>%
  kable_styling(latex_options = 'scale_down')

#'
#' Unfortunately, we **lose all Grade D households in natural gas**.
#'
#'
#'
#' #### Household level energy by ethnicity
#' 
#' This section examines the coefficient of response across ethnicity, as well as HOLC grade and thermostat set points.
#' 
#+ RASS-1c, echo=F, message = F, warning=F, cache = TRUE
modelsummary(list(
  feols(u ~ hdd + hdd:GRADE.max + hdd:ethnicity + hdd:avginc1000  | IDENT, weights=~d, data = all09.use.pooled, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:GRADE.max + hdd:ethnicity + hdd:NITESET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:GRADE.max + hdd:ethnicity + hdd:DAYSET + hdd:avginc1000  | IDENT, weights=~d, data = all09.use.pooled, warn = F)  ,# Or daytime setting
  feols(u ~ hdd + hdd:GRADE.max + hdd:ethnicity + hdd:NITESET + hdd:DAYSET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled, warn = F)
  # Not enough observations - feols(u ~ hdd + hdd:GRADE.max:ethnicity + hdd:ethnicity + hdd:NITESET + hdd:DAYSET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled, warn = F) 
), 
stars = T,
title = 'Regression of electricity consumption on heating degree days, interacted with HOLC grade, ethnicity, and income, conditional on thermostat setpoint\\label{tab:electricresponsethemset2}',
notes = c('Using only households with electricity as primary heating fuel','Omitted grade is "C"'),
output = 'kableExtra') %>%
  kable_styling(latex_options = 'scale_down')


          # doesn't look so great when cdd.e is included as its missing for many
          # this is just electric; what about g?
           
#'
#' Results surprisingly show that Black households with electricity as a primary heating fuel have lower responses to increases in heating degree days 
#' relative to White households in two of the four specifications. 
#' A similar effect holds for Hispanic households, though in no specifications are the results statistically significant
#' for Hispanic households. Conditioning on separate effects for nighttime and daytime thermostat setpoints reduces the magnitude of the
#' coefficient of response for households in HOLC grade red (D), but the point estimate is still positive. 
#'
#'
#+ RASS-1cgas, echo=F, message = F, warning = F, cache = TRUE


modelsummary(list(
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:ethnicity + hdd:avginc1000               | IDENT, weights=~d, data = all09.use.pooled.g, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:ethnicity + hdd:NITESET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled.g, warn = F) , # Do redlined areas have diff. responses cond. on setpoint
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:ethnicity + hdd:DAYSET + hdd:avginc1000  | IDENT, weights=~d, data = all09.use.pooled.g, warn = F)  ,# Or daytime setting
  feols(u ~ hdd + hdd:GRADE.maxl + hdd:ethnicity + hdd:NITESET + hdd:DAYSET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled.g, warn = F)
  # NOt enough observations - feols(u ~ hdd + hdd:GRADE.max:ethnicity + hdd:ethnicity + hdd:NITESET + hdd:DAYSET + hdd:avginc1000 | IDENT, weights=~d, data = all09.use.pooled, warn = F) 
), stars = T, 
title = 'Regression of gas consumption on heating degree days, interacted with HOLC grade, ethnicity, and income, conditional on thermostat setpoint\\label{tab:gasresponsethemset2}',
output = 'kableExtra',
notes = c('Using only households with natural gas as primary heating fuel','Omitted grade is "C"')) %>%
  kable_styling(latex_options = 'scale_down')

#' Table \ref{tab:gasresponsethemset2} shows an insufficient number of gas households in HOLC grade D (red) areas which precludes us from seeing the effect of HOLC red on response to heating degree days.
#' Results focused on ethnicity show that Hispanic households are less responsive to heating degree days conditional on daytime and nighttime 
#' thermostat setpoints. A similar result for Black households is not significant.



/*
  
  rmarkdown::render(rstudioapi::getSourceEditorContext()$path, 
                    output_file = paste0(format(Sys.time(), '%F--%H%M'), '_', gsub(pattern = '\\.R', '.pdf', 
                                                                                   basename(rstudioapi::getSourceEditorContext()$path))))

*/


#' # End
#' 
###################
###################
###################


