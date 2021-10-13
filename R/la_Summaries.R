# uses https://data.gov.uk/dataset/723c243d-2f1a-4d27-8b61-cdb93e5b10ff/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics-2005-to-2019

library(data.table)
library(ggplot2)

params <- list()

params$dataPath <- "~/Dropbox/data/beis/laCarbon/"

laDT <- data.table::fread(paste0(params$dataPath, "2005-19_Local_Authority_CO2_emissions.csv"))

names(laDT)

head(laDT)

esdcDT <- laDT[`Local Authority` %like% "East Suffolk"] 

nrow(esdcDT)

esdcDT[, .(n = .N), keyby = .(`Local Authority`, `Calendar Year`)]

ggplot2::ggplot(esdcDT, aes(x = `Calendar Year`, y = `Territorial emissions (kt CO2)`,
                            fill = `LA CO2 Sub-sector`)) +
  geom_col(position = "stack")

data.table::fwrite(esdcDT, file = paste0(params$dataPath, "2005-19_Local_Authority_CO2_emissions_ESDC.csv"))
