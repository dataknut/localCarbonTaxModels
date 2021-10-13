# uses https://data.gov.uk/dataset/723c243d-2f1a-4d27-8b61-cdb93e5b10ff/uk-local-authority-and-regional-carbon-dioxide-emissions-national-statistics-2005-to-2019

library(data.table)
library(RColorBrewer)
library(ggplot2)

params$dataPath <- "~/Dropbox/data/beis/laCarbon/"

laDT <- data.table::fread(paste0(params$dataPath, "2005-19_Local_Authority_CO2_emissions.csv"))

names(laDT)

head(laDT)

esdcDT <- laDT[`Local Authority` %like% "East Suffolk"] 

nrow(esdcDT)

# test
esdcDT[, .(n = .N), keyby = .(`Local Authority`, `Calendar Year`)]

# colours borrowed from https://git.soton.ac.uk/twr1m15/la_emissions_viz/-/blob/master/shiny/app.R
# for details, use set for each sector
industry_pal <- brewer.pal(n = 8, name = "Greys")[4:8]    # industry greys, 5 categories
domestic_pal <- brewer.pal(n = 4, name = "Blues")[2:4]    # domestic blues, 3 categories
transport_pal <- brewer.pal(n = 6, name = "Oranges")[2:6] # transport oranges, 5 categories
lulucf_pal <- brewer.pal(n = 9, name = "Greens")[4:9]     # lulucf greens, 6 categories

# for details, combine sets
detailed_pal <- c(industry_pal, domestic_pal, transport_pal, lulucf_pal)

ggplot2::ggplot(esdcDT, aes(x = `Calendar Year`, y = `Territorial emissions (kt CO2)`,
                            fill = `LA CO2 Sub-sector`,
                            colour = `LA CO2 Sub-sector`)) +
  geom_col(position = "stack") 
  # scale_fill_manual(values = detailed_pal) - currently breaks as wrong number of categories?
  

data.table::fwrite(esdcDT, file = paste0(params$dataPath, "2005-19_Local_Authority_CO2_emissions_ESDC.csv"))
