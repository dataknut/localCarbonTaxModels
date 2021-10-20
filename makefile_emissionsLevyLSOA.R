# runs the emissions levy template code

# packages ----
library(dkUtils) # see https://github.com/dataknut/dkUtils
myLibs <- c("data.table",
            "here")

dkUtils::loadLibraries(myLibs)

# functions ----
source(here::here("R", "functions.R"))

makeReport <- function(f,filter){
  # default = html
  rmarkdown::render(input = paste0(here::here("rmd", f), ".Rmd"),
                    params = list(title = title,
                                  subtitle = subtitle,
                                  authors = authors),
                    output_file = paste0(here::here("docs/"), f, "_",filter,".html")
  )
}

# parameters ----
rParams <- list() # params used in R or Rmd scripts

# > set filer ----
rParams$filter <- "Newham" # search string used to filter LA name thus: LAD11NM %like% <filter>
rParams$filter <- "All English LSOAs"
# if it's "All English LSOAs" we don't filter

# load the datasets we use and do generic data processing here ----

# > LSOA lookups ----
lsoa_lookup <- data.table::fread(paste0("~/Dropbox/data/UK_census/lookups/", "lsoa_lookup.csv"))
# > IMD 2019 ----
lsoa_imd2019 <- data.table::fread(here::here("data", "LSOA_Indices_of_Multiple_Deprivation_2019.csv.gz")
)
lsoa_imd2019[, LAD11NM := LADnm]
lsoa_imd2019[, IMD_Decile_label := ifelse(IMD_Decile == 1, "1 (10% most deprived)",IMD_Decile)]
lsoa_imd2019[, IMD_Decile_label := ifelse(IMD_Decile == 10, "10 (10% least deprived)",IMD_Decile_label)]
lsoa_imd2019[, IMD_Decile_label := factor(IMD_Decile_label, levels = c("1 (10% most deprived)","2","3","4","5","6","7","8","9","10 (10% least deprived)"))]

#> fuel poverty ----
lsoa_beis_fp <- data.table::fread(here::here("data", "LSOA_Fuel_Poverty_2019.csv.gz"))

#> energy data ----

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
lsoa_creds_pbcc <- data.table::fread(here::here("data", "LSOA_CREDS_PBCC_data.csv.gz"))
# % of total emissions due to gas & elec & other heat
lsoa_creds_pbcc[, energy_pc := 100*((elec_percap_2018 + gas_percap_2018 + other_heat_percap_2011)/total_kgco2e_percap)]

nrow(lsoa_creds_pbcc)

# % of household (spaces?) with mainly elec
lsoa_creds_pbcc[, pc_Heating_Electric := 100*(pHeating_Electric/(pHeating_None + pHeating_Gas + pHeating_Electric + pHeating_Oil + pHeating_Solid+ pHeating_Other))]

lsoa_creds_pbcc[, LAD11NM := LAD17NM] # for matching - beware name changes

# calc totals ----
lsoa_creds_pbcc[, CREDStotal_kgco2e := total_kgco2e_percap * pop_2018]
lsoa_creds_pbcc[, CREDSgas_kgco2e2018 := gas_percap_2018 * pop_2018]
lsoa_creds_pbcc[, CREDSelec_kgco2e2018 := elec_percap_2018 * pop_2018]
lsoa_creds_pbcc[, CREDSotherEnergy_kgco2e2011 := other_heat_percap_2011 * pop_2018]
lsoa_creds_pbcc[, CREDSallHomeEnergy_kgco2e2011 := CREDSotherEnergy_kgco2e2011 + CREDSgas_kgco2e2018 + CREDSelec_kgco2e2018 + CREDSotherEnergy_kgco2e2011]
lsoa_creds_pbcc[, CREDScar_kgco2e2018 := car_percap_2018 * pop_2018]
lsoa_creds_pbcc[, CREDSvan_kgco2e2018 := van_percap_2018 * pop_2018]

# get analytic dataset ----
# reduced variable set for simplicity

# start from the lookup as it has all LSOAs/SOAs

setkey(lsoa_lookup, LSOA11CD)

# add IMD - this is only England
lsoa_imd2019[, LSOA11CD := lsoa11cd]
setkey(lsoa_imd2019, LSOA11CD)

dt <- lsoa_imd2019[lsoa_lookup[, .(LSOA11CD, LSOA11NM, WD20CD, RUC11, oacSuperGroupName = `Supergroup Name`, region = `Region/Country`)]]

# add energy
setkey(dt, LSOA11CD)

dt <- dt[energy_2018DT]

# add CREDS data - also only England
credsLsoaDT <- lsoa_creds_pbcc[, .(LAD11NM, WD18NM, LSOA11CD = LSOA11, 
                                        CREDStotal_kgco2e,CREDSgas_kgco2e2018,CREDSelec_kgco2e2018,
                                        CREDSotherEnergy_kgco2e2011,CREDSallHomeEnergy_kgco2e2011,
                                        CREDScar_kgco2e2018,CREDSvan_kgco2e2018,
                                        pop_2018, energy_pc, pc_Heating_Electric,
                                        epc_total, epc_newbuild, epc_A, epc_B,
                                        epc_C, epc_D, epc_E, epc_F, epc_G)]

setkey(credsLsoaDT, LSOA11CD)
# keep only rows that match to CREDS (England)
credsLsoaDT <- dt[credsLsoaDT]

# directly check LAs included (first 6 rows of table)
head(table(credsLsoaDT$LAD11NM))

# filter data here if needed ----

if(rParams$filter == "All English LSOAs"){
  selectedLsoasDT <- credsLsoaDT 
  message("Filter: ", rParams$filter)
} else { # filter
  message("Filter: ", rParams$filter)
  selectedLsoasDT <- credsLsoaDT[LAD11NM %like% rParams$filter]
}
message("Selected rows:")
nrow(selectedLsoasDT)

# >> run report ----
rmdFile <- "simulating_local_emissions_levy_template" # not the full path
title = "Simulating a local emissions levy to fund local energy effiency retrofit"
subtitle <- paste0("Focus on: ", rParams$filter)
authors = "Ben Anderson"

makeReport(f = rmdFile, filter = rParams$filter)

