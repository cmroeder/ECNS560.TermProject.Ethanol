library(tidyverse)

#Pull corn prices from NASS
Sys.setenv(nass_key = "insert NASS key")

url = "https://quickstats.nass.usda.gov/"
endpoint = "api/api_GET/"
params = list(
  key = Sys.getenv("nass_key"),
  format = "JSON",
  source_desc = "SURVEY",
  statisticcat_desc = "PRICE RECEIVED",
  short_desc = "CORN, GRAIN - PRICE RECEIVED, MEASURED IN $ / BU",
  domain_desc = "TOTAL",
  agg_level_desc = "STATE",
  year__GE = "1960",
  freq_desc = "ANNUAL"
)

response = httr::GET(url = url, path = endpoint, query = params)
response

json = response |>
  httr::content("text") |>
  jsonlite::fromJSON()

cornprices = as_tibble(json$data)
cornprices

saveRDS(cornprices, "Data/Cleaning/dirtycornprices.rds")
