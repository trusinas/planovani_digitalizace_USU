library(tidyverse)
library(jsonlite)
library(lubridate)
library(writexl)

# požadované agendy
vyber <- c("A345")

# stažení dat
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/sluzby.json", "data/sluzby.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/agendy.json", "data/agendy.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/ovm.json", "data/ovm.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/kategorie_ovm.json", "data/kategorie_ovm.json")

# zpracování dat
source("R/etl.R", encoding = "UTF-8")

# TODO: přidání vzorce
df <- df %>% 
  mutate(x3 =xl_formula("=A2*(B2+3)"))

# uložení
write_xlsx(sluzby, paste0("output/ukony_", paste(vyber, collapse = "_"), ".xlsx"))

