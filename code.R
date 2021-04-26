library(tidyverse)
library(jsonlite)
library(lubridate)
library(glue)
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

# TODO: info
info <- tibble("Informace, jak používat tabulku" = "")

# TODO: přidání vzorce
# nefunguje
vzorec <- tibble(x1 = c("návratnost (let)", "iniciální investiční výdaj (Kč)", "roční přínosy (Kč)", 
                   "roční provozní výdaje (Kč)", "", "roční přínosy pro VS (Kč)", 
                   "průměrná mzda ve VS (Kč)", "průměrná mzda ve VS na minutu (Kč)", 
                   "", "roční přínosy pro klienty (Kč)", "hrubý domácí produkt na obyvatele (Kč)", 
                   "HDP na obyvatele na minutu (Kč)"),
                 x2 = 1)
vzorec$x2[1] <- xl_formula('=B2/(B6+B10-B4)')
vzorec$x2[2] <- 30000000
vzorec$x2[3] <- xl_formula("=B6+B10")
vzorec$x2[4] <- 6000000
vzorec$x2[6] <- xl_formula("=SUMA('seznam úkonů'!T:T)")
vzorec$x2[7] <- 40572
vzorec$x2[8] <- xl_formula("=2*B7/173,92/60")
vzorec$x2[10] <- xl_formula("=SUMA('seznam úkonů'!U:U)")
vzorec$x2[11] <- 524867
vzorec$x2[12] <- xl_formula("=B11/2008/60")

# funguje x problém s názvy sloupců
sluzby <- sluzby.bak
l <- nrow(sluzby)
l <- l + 1
sluzby <- sluzby %>% 
  mutate(
    'přínosy úředník' = xl_formula(glue('=O{2:l}*(P{2:l}*7.78+R{2:l})')),
    'přínosy klient' = xl_formula(glue('=O{2:l}*(P{2:l}*4.36+S{2:l})'))
    )
write_xlsx(sluzby, "output/test.xlsx")

# TODO: přejmenování sloupců - ke službám přidat názvy

# TODO: přidání seznamu problematických úkonů

# uložení

write_xlsx(list("info" = info, "seznam úkonů" = sluzby, "návratnost" = vzorec), paste0("output/ukony_", paste(vyber, collapse = "_"), ".xlsx"), col_names = TRUE)

