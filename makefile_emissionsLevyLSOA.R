# runs the emissions levy template code

# packages ----
library(dkUtils) # see https://github.com/dataknut/dkUtils
myLibs <- c("data.table",
            "here",
            "skimr")

dkUtils::loadLibraries(myLibs)

# functions ----
source(here::here("R", "functions.R"))

makeReport <- function(filter){
  # > run report ----
  rmdFile <- "simulating_local_emissions_levy_template" # not the full path
  title = "Data notebook: Simulating a local emissions levy to fund local energy effiency retrofit"
  subtitle <- paste0("Focus on: ", filter)
  authors = "Ben Anderson"
  # default = html
  rmarkdown::render(input = paste0(here::here("rmd", rmdFile), ".Rmd"),
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors,
                                  filter = filter,
                                  rmdFile = rmdFile),
                    output_file = paste0(here::here("docs/"), rmdFile, "_",filter,".html")
  )
}

# parameters ----
rParams <- list() # params used in this R script

# load the datasets we use and do generic data processing here ----

# > ONS LSOA lookups ----
lsoa_lookup <- data.table::fread(paste0("~/Dropbox/data/UK_lookups/", "lsoa_lookup.csv.gz"))

# > ONS MSOA income (total) - small area estimates ----
# Contents									
# 
# Total annual household income by middle layer super output area (MSOA), England and Wales, financial year ending March 2018 (£)									

msoa_income2018 <- data.table::fread(paste0("~/Dropbox/data/UK_ONS/income_MSOA/", "totalannualincome2018.csv"))
msoa_income2018[, MSOA11CD := `MSOA code`]
msoa_income2018[, MSOA11NM := `MSOA name`]
msoa_income2018[, region := `Region name`]
msoa_income2018[, msoa_tot_annual_income_2018 := `Total annual income (\xa3)`]
setkey(msoa_income2018, MSOA11CD)

# > DCLG/DLUC IMD 2019 ----
lsoa_imd2019 <- data.table::fread(here::here("data", "LSOA_Indices_of_Multiple_Deprivation_2019.csv.gz")
)
lsoa_imd2019[, LAD11NM := LADnm]
lsoa_imd2019[, IMD_Decile_label := ifelse(IMD_Decile == 1, "1 (10% most deprived)",IMD_Decile)]
lsoa_imd2019[, IMD_Decile_label := ifelse(IMD_Decile == 10, "10 (10% least deprived)",IMD_Decile_label)]
lsoa_imd2019[, IMD_Decile_label := factor(IMD_Decile_label, levels = c("1 (10% most deprived)","2","3","4","5","6","7","8","9","10 (10% least deprived)"))]

#> LSOA BEIS fuel poverty ----
lsoa_beis_fp <- data.table::fread(here::here("data", "LSOA_Fuel_Poverty_2019.csv.gz"))

#> LSOA BEIS energy data ----

gasF <- here::here("data", "LSOA_GAS_2018.csv.gz")
gasDT <- data.table::fread(gasF)
gasDT[,LAD11NM := `Local Authority Name`]
gasDT[,LSOA11CD := `Lower Layer Super Output Area (LSOA) Code`]
setkey(gasDT, LSOA11CD, LAD11NM)

elecF <- here::here("data", "LSOA_ELEC_2018.csv.gz")
elecDT <- data.table::fread(elecF)
elecDT[,LAD11NM := `Local Authority Name`]
elecDT[,LSOA11CD := `Lower Layer Super Output Area (LSOA) Code`]
setkey(elecDT, LSOA11CD, LAD11NM)

energy_2018DT <- elecDT[gasDT]
energy_2018DT[, nGasMeters := `Number of consuming meters`]
energy_2018DT[, nElecMeters := `Total number of domestic electricity meters`]
setkey(energy_2018DT, LSOA11CD)

#> CREDS data ----
# covers England https://www.carbon.place/about/ at LSOA level
lsoa_creds_pbcc <- data.table::fread(here::here("data", "LSOA_CREDS_PBCC_data.csv.gz"))
# % of total emissions due to gas & elec & other heat
lsoa_creds_pbcc[, CREDS_energy_pc := 100*((elec_percap_2018 + gas_percap_2018 + other_heat_percap_2011)/total_kgco2e_percap)]

nrow(lsoa_creds_pbcc)

# % of household (spaces?) with mainly elec
lsoa_creds_pbcc[, CREDS_pc_Heating_Electric_2011 := 100*(pHeating_Electric/(pHeating_None + pHeating_Gas + pHeating_Electric + pHeating_Oil + pHeating_Solid+ pHeating_Other))]

lsoa_creds_pbcc[, LAD11NM := LAD17NM] # for matching - beware name changes

# calc CREDS totals
lsoa_creds_pbcc[, CREDS_total_kgco2e := total_kgco2e_percap * pop_2018]

lsoa_creds_pbcc[, CREDS_gas_kgco2e2018 := gas_percap_2018 * pop_2018]
lsoa_creds_pbcc[is.na(CREDS_gas_kgco2e2018), CREDS_gas_kgco2e2018 := 0] # fix NA where no gas recorded


lsoa_creds_pbcc[, CREDS_elec_kgco2e2018 := elec_percap_2018 * pop_2018]
lsoa_creds_pbcc[is.na(CREDS_elec_kgco2e2018), CREDS_elec_kgco2e2018 := 0]

lsoa_creds_pbcc[, CREDS_otherEnergy_kgco2e2011 := other_heat_percap_2011 * pop_2018]
lsoa_creds_pbcc[is.na(CREDS_otherEnergy_kgco2e2011), CREDS_otherEnergy_kgco2e2011 := 0]

lsoa_creds_pbcc[, CREDS_allHomeEnergy_kgco2e2011 := CREDS_gas_kgco2e2018 + CREDS_elec_kgco2e2018 + CREDS_otherEnergy_kgco2e2011]

lsoa_creds_pbcc[, CREDS_car_kgco2e2018 := car_percap_2018 * pop_2018]
lsoa_creds_pbcc[is.na(CREDS_car_kgco2e2018), CREDS_car_kgco2e2018 := 0]
lsoa_creds_pbcc[, CREDS_van_kgco2e2018 := van_percap_2018 * pop_2018]
lsoa_creds_pbcc[is.na(CREDS_van_kgco2e2018), CREDS_van_kgco2e2018 := 0]
lsoa_creds_pbcc[, CREDS_Whole_House_Detached := Whole_House_Detached]
lsoa_creds_pbcc[, CREDS_Whole_House_Semi := Whole_House_Semi]
lsoa_creds_pbcc[, CREDS_Whole_House_Terraced := Whole_House_Terraced]
lsoa_creds_pbcc[, CREDS_Whole_House_Flat := Flat_PurposeBuilt + Flat_Converted + Flat_Commercial]

lsoa_creds_pbcc[, CREDS_epc_Whole_House_Detached := type_house_detached] 
lsoa_creds_pbcc[, CREDS_epc_Whole_House_Semi := type_house_semi]
lsoa_creds_pbcc[, CREDS_epc_Whole_House_Terraced := type_house_midterrace + type_house_endterrace]
lsoa_creds_pbcc[, CREDS_epc_Whole_House_Flat := type_flat + type_maisonette]
# that leaves type_parkhome, type_bungalow, type_other - how to code?

#> CENSUS 2011 data ----
lsoa_2011_tenure <- data.table::fread(path.expand("~/Dropbox/data/UK_census/2011/2011-LSOA-QS405EW-tenure.csv"))
lsoa_2011_tenure[, LSOA21CD := `geography code`]
setkey(lsoa_2011_tenure,LSOA21CD)
lsoa_2011_tenure[, c2011_pc_social_rent := 100*`Tenure: Social rented: Total; measures: Value`/`Tenure: All categories: Tenure; measures: Value`]
lsoa_2011_tenure[, c2011_pc_private_rent := 100*`Tenure: Private rented: Total; measures: Value`/`Tenure: All categories: Tenure; measures: Value`]
lsoa_2011_tenure[, c2011_pc_owned := 100*`Tenure: Owned: Total; measures: Value`/`Tenure: All categories: Tenure; measures: Value`]


lsoa_2011_acc_type <- data.table::fread(path.expand("~/Dropbox/data/UK_census/2011/2011-LSOA-QS402EW-dwelling-type.csv"))
lsoa_2011_acc_type[, LSOA21CD := `geography code`]
setkey(lsoa_2011_acc_type,LSOA21CD)
# recode to match EPC counts
lsoa_2011_acc_type[, c2011_detached := `Dwelling Type: Unshared dwelling: Whole house or bungalow: Detached; measures: Value`]
lsoa_2011_acc_type[, c2011_semi := `Dwelling Type: Unshared dwelling: Whole house or bungalow: Semi-detached; measures: Value`]
lsoa_2011_acc_type[, c2011_terrace := `Dwelling Type: Unshared dwelling: Whole house or bungalow: Terraced (including end-terrace); measures: Value`] # NB - need to combine EPC terraced & end-terrace
lsoa_2011_acc_type[, c2011_flat := `Dwelling Type: Unshared dwelling: Flat, maisonette or apartment: Total; measures: Value`] # NB combine EPC flat & maisonette
lsoa_2011_acc_type[, c2011_pc_detached := c2011_detached/`Dwelling Type: All categories: Accommodation type; measures: Value`]
lsoa_2011_acc_type[, c2011_pc_semi := c2011_semi/`Dwelling Type: All categories: Accommodation type; measures: Value`]
lsoa_2011_acc_type[, c2011_pc_terrace := c2011_terrace/`Dwelling Type: All categories: Accommodation type; measures: Value`]
lsoa_2011_acc_type[, c2011_pc_flat := c2011_flat/`Dwelling Type: All categories: Accommodation type; measures: Value`]


# may match to CREDS "Whole_House_Detached"          "Whole_House_Semi"              "Whole_House_Terraced"         
# "Flat_PurposeBuilt"             "Flat_Converted"                "Flat_Commercial"               "Caravan"    

#> CENSUS 2021 data ----
#  won't all match as LSOA boundary changes
lsoa_2021_ch <- data.table::fread(path.expand("~/Dropbox/data/UK_census/2021/2021-LSOA-central-heating-custom-filtered-2023-06-22T16 13 37Z.csv"))
lsoa_2021_ch[,LSOA21CD := `Lower layer Super Output Areas Code`]
setkey(lsoa_2021_ch, LSOA21CD)

lsoa_2021_ch_m <- melt(lsoa_2021_ch[, .(LSOA21CD, `Type of central heating in household (13 categories)`, Observation)], 
                            id.vars = c("LSOA21CD", "Type of central heating in household (13 categories)"))
lsoa_2021_ch_m[, c2021_nhhs :=  sum(value), keyby = .(LSOA21CD)]

lsoa_2021_elec_ch <- lsoa_2021_ch_m[`Type of central heating in household (13 categories)` == "Electric only"]
lsoa_2021_elec_ch[, c2021_pc_elec_ch_2021 := 100 * value/c2021_nhhs]

# Build analytic dataset ----
# reduced variable set for simplicity

# start from the lookup as it has all LSOAs/SOAs

setkey(lsoa_lookup, MSOA11CD)

# add MSOA income - this is England and Wales
# this will duplicate total income across all LSOAs within each MSOA
dt <- lsoa_lookup[, .(LSOA11CD, LSOA11NM, MSOA11CD, MSOA11NM, WD20CD, RUC11, 
                      oacSuperGroupName = `Supergroup Name`, region = `Region/Country`)][msoa_income2018[, .(MSOA11CD, msoa_tot_annual_income_2018)]]
table(dt$region)

# add Census 2011 data
setkey(dt, LSOA11CD)
dt <- dt[lsoa_2011_tenure[, .(LSOA11CD, c2011_pc_social_rent, c2011_pc_private_rent, c2011_pc_owned)]]
dt <- dt[lsoa_2011_acc_type[, .(LSOA11CD, c2011_detached, c2011_semi, c2011_terrace, c2011_flat,
                                c2011_pc_detached, c2011_pc_semi, c2011_pc_terrace, c2011_pc_flat)]]


# add IMD at LSOA level - this is only England
lsoa_imd2019[, LSOA11CD := lsoa11cd]
setkey(lsoa_imd2019, LSOA11CD)

dt <- dt[lsoa_imd2019]
table(dt$region)


# add energy data from BEIS (not via CREDS)
# gives meter counts

# add so that all rows are kept
dt <- energy_2018DT[dt]
table(dt$region)

# add CREDS data - also only England
credsLsoaDT <- lsoa_creds_pbcc[, .(LAD11NM, WD18NM, LSOA11CD = LSOA11, 
                                        CREDS_total_kgco2e,CREDS_gas_kgco2e2018,CREDS_elec_kgco2e2018,
                                        CREDS_otherEnergy_kgco2e2011,CREDS_allHomeEnergy_kgco2e2011,
                                        CREDS_car_kgco2e2018,CREDS_van_kgco2e2018,
                                        pop_2018, CREDS_energy_pc, CREDS_pc_Heating_Electric_2011,
                                   CREDS_Whole_House_Detached, CREDS_Whole_House_Semi, CREDS_Whole_House_Terraced, CREDS_Whole_House_Flat,
                                        epc_total, epc_newbuild, epc_score_avg, epc_A, epc_B,
                                        epc_C, epc_D, epc_E, epc_F, epc_G,
                                   CREDS_epc_Whole_House_Detached, CREDS_epc_Whole_House_Semi, CREDS_epc_Whole_House_Terraced, CREDS_epc_Whole_House_Flat,
                                   type_house_detached, type_house_semi, type_house_midterrace, type_house_endterrace, type_parkhome, type_bungalow,
                                   type_flat, type_maisonette, type_other)]

setkey(credsLsoaDT, LSOA11CD)
# keep only rows that match to CREDS (England)
mergedLSOA_Data_Eng <- dt[credsLsoaDT]
nrow(mergedLSOA_Data_Eng)

# why are some LSOAs missing from the energy data?
head(mergedLSOA_Data_Eng[is.na(nElecMeters), .(LAD11NM, LSOA11CD, LSOA01NM, nElecMeters, nGasMeters,
                                           CREDS_total_kgco2e, CREDS_elec_kgco2e2018, CREDS_gas_kgco2e2018,
                                           CREDS_otherEnergy_kgco2e2011, CREDS_allHomeEnergy_kgco2e2011)])

# directly check LAs included (first 6 rows of table)
head(table(mergedLSOA_Data_Eng$LAD11NM))

# test linkage of Census 2021 data - suspect some LSOAs have changed

sotonLSOA_DT <- mergedLSOA_Data_Eng[LAD11NM %like% "Southampton"]
setkey(sotonLSOA_DT, LSOA11CD)
setkey(lsoa_2021_elec_ch, LSOA21CD)
test <- lsoa_2021_elec_ch[sotonLSOA_DT[, .(LSOA11CD, WD18NM, CREDS_pc_Heating_Electric_2011)]]
summary(test) # NAs are LSOAs that didn't match
library(ggplot2)
ggplot2::ggplot(test, aes(x = CREDS_pc_Heating_Electric_2011 ,y = c2021_pc_elec_ch_2021)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

#> check data ----
skimr::skim(mergedLSOA_Data_Eng)


# check data

head(mergedLSOA_Data_Eng[, .(CREDS_Whole_House_Detached, c2011_detached, 
                             CREDS_Whole_House_Semi, c2011_semi,
                             CREDS_Whole_House_Terraced, c2011_terrace,
                             CREDS_Whole_House_Flat, c2011_flat)])

head(mergedLSOA_Data_Eng[, .(type_house_detached, type_house_semi, type_house_midterrace, type_house_endterrace, type_parkhome, type_bungalow,
                             type_flat, type_maisonette, type_other)])

# run the report
# > set filter here ----
# if it's "All English LSOAs" we don't filter
# we could use a list of all LAs but that would take a while...

# filterList <- c("Southampton","Newham", "Winchester","Portsmouth", "Eastleigh",
#                 "Wight", "New Forest", "Havant")
# 
# for(filter in filterList){
#   message("Filter: ", filter)
#   rParams$filter <- filter # gets used in rmd
#   makeReport(filter = filter)
# }
# 
# makeReport(filter = "All English LSOAs")

makeReport(filter = "Southampton")
makeReport(filter = "Winchester")
makeReport(filter = "Islington")
# makeReport(filter = "All English LSOAs")


